-- | Value analysis over the basic-block AST.
--
-- The pass follows what the local scalar variables of a body may hold and
-- reports a condition whose value it already knows (VAE-001), since one of the
-- paths such a condition guards is then never taken. A condition may be
-- decided by a constant of the module as much as by an assignment or by the
-- branch it sits in, so the pass is seeded with the constant environment the
-- folding builds and runs after it, apart from the rest of the basic-block
-- checks.
--
-- What a variable holds is a set of at most 'valueLimit' values or, once the
-- set has outgrown that, the interval its declared type allows; a variable
-- with no entry in the map is unknown. Where paths meet the two sets are
-- joined, and a variable falls from a set to the interval of its type and from
-- there to unknown, so the chain of a variable has a height of three and the
-- walk of a loop settles after a bounded number of turns, with no widening
-- sequence to tune.
--
-- A condition is evaluated with the evaluator of
-- 'ControlFlow.ConstFolding', which is what keeps the arithmetic of this pass
-- and the arithmetic of the folding the same one. That evaluator reads a
-- single value, so a variable known to hold one answers a lookup and a
-- variable known to hold several answers nothing. An evaluation that does not
-- succeed leaves the pass silent, which is the only direction an error of the
-- transpiler may be wrong in.
module ControlFlow.ValueAnalysis (runValueAnalysisCheck) where

import Control.Monad (unless)
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S

import Configuration.Platform (Platform)
import ControlFlow.BasicBlocks.AST
import ControlFlow.BasicBlocks.Traversal
    (Child(..), expressionChildren, simpleBlockChildren, rootIdent)
import ControlFlow.ConstFolding (evalConstExpression, runConstFolding)
import ControlFlow.ConstFolding.Monad (ConstFoldEnv(..))
import ControlFlow.ValueAnalysis.Errors
import ControlFlow.Dataflow
import Core.Utils (intRange)
import Semantic.Types (SemanticAnn, getObjectSAnns)
import Utils.Annotations

-- | How many values a set may hold before it gives way to the interval of the
-- type. Raising it costs turns of the fixed point of a loop, since an
-- @x = x + 1@ adds one value per turn before it reaches the limit.
valueLimit :: Int
valueLimit = 8

-- | One of the values a variable may hold, which 'setValue' keeps down to a
-- boolean or an integer. Two of them are compared by the value alone: two
-- paths that reach the same number agree whether or not it was written the
-- same way, and the type a constant carries is not part of what they agree on.
newtype Value = Value { valueConst :: Const SemanticAnn }

instance Eq Value where
  left == right = compare left right == EQ

instance Ord Value where
  compare = comparing (rank . valueConst)

    where

      rank (B b) = (0 :: Int, if b then 1 else 0 :: Integer)
      rank (I (TInteger v _) _) = (1, v)
      rank _ = (2, 0)

integerOf :: Value -> Maybe Integer
integerOf (Value (I (TInteger v _) _)) = Just v
integerOf _ = Nothing

-- | What is known of a variable at a point of a path: the values it may hold,
-- written out one by one while they are few, and as an interval with both ends
-- included once they are not.
data Values =
    Discrete (S.Set Value)
  | Interval Integer Integer
  deriving Eq

-- | The values of a variable together with the interval its declared type
-- allows, which is where a set that outgrows 'valueLimit' lands. Taking the
-- convex hull of the set instead would let a loop that adds one at a time
-- climb to the top of the type one value per turn; going to the interval of
-- the type at once bounds the chain at three. A boolean has no interval and
-- never needs one, since two values do not reach the limit.
data Known = Known
  {
    typeRange :: Maybe (Integer, Integer)
  , varValues :: Values
  }
  deriving Eq

-- | What belongs to the path being walked: the local variables whose value the
-- path constrains. A variable that is not here is unknown, so joining two paths
-- keeps the entries they both constrain and drops the rest.
newtype ValueAnalysisPath = ValueAnalysisPath { known :: M.Map Identifier Known }
  deriving Eq

instance Lattice ValueAnalysisPath where
  joinPath left right =
    ValueAnalysisPath (M.mapMaybe id (M.intersectionWith agree (known left) (known right)))

    where

      agree x y = Known (typeRange x) <$> joinValues (typeRange x) (varValues x) (varValues y)

-- | Two sets whose union stays under the limit join exactly. Past the limit
-- the union gives way to the interval of the type, and so does a join in which
-- an interval takes part and neither side contains the other; a variable whose
-- type has no interval falls to unknown instead.
joinValues :: Maybe (Integer, Integer) -> Values -> Values -> Maybe Values
joinValues range (Discrete left) (Discrete right)
  | S.size both <= valueLimit = Just (Discrete both)
  | otherwise = wholeType range

  where

    both = S.union left right

joinValues range left right
  | left `covers` right = Just left
  | right `covers` left = Just right
  | otherwise = wholeType range

wholeType :: Maybe (Integer, Integer) -> Maybe Values
wholeType = fmap (uncurry Interval)

covers :: Values -> Values -> Bool
covers (Interval lo hi) (Interval lo' hi') = lo <= lo' && hi' <= hi
covers (Interval lo hi) (Discrete vs) = all inside (S.toList vs)

  where

    inside value = maybe False (\v -> lo <= v && v <= hi) (integerOf value)

covers (Discrete left) (Discrete right) = right `S.isSubsetOf` left
covers (Discrete _) (Interval _ _) = False

-- | What belongs to the program, and no branch takes back.
data ValueAnalysisGlobal = ValueAnalysisGlobal
  {
    -- | The constants the module sees, as the folding left them. They are what
    -- makes this pass answer for the case a condition is constant outright.
    moduleConsts :: M.Map Identifier (Const SemanticAnn),
    platform :: Platform,
    -- | Whether the walk is inside a loop whose state has not settled yet, in
    -- which case what is known of a variable may still fall and a finding may
    -- not survive the next turn.
    settling :: Bool
  }

type ValueAnalysisMonad = DataflowM ValueAnalysisPath ValueAnalysisGlobal ValueAnalysisError

-- | The one value a variable holds, when the path pins it to one. A set of
-- several values and an interval answer nothing, which leaves the pass where
-- it would be with the variable unknown.
singleValue :: Known -> Maybe (Const SemanticAnn)
singleValue entry = case varValues entry of
  Discrete values | [Value value] <- S.toList values -> Just value
  _ -> Nothing

-- | The value of an expression, when the constants of the module and what this
-- path knows of the local variables determine it.
--
-- The evaluation is the one of the folding, seeded with the local variables
-- pinned to a single value: such a variable answers a lookup exactly as a
-- module constant does, and one that is unknown is absent, which makes the
-- evaluation fail and the pass keep quiet. The two names cannot be confused,
-- since a local may not shadow a global (SE-081).
valueOf :: Expression SemanticAnn -> ValueAnalysisMonad (Maybe (Const SemanticAnn))
valueOf expr = do
  global <- getGlobal
  locals <- known <$> getPath
  let env = ConstFoldEnv
        (M.union (M.mapMaybe singleValue locals) (moduleConsts global))
        (platform global)
  return $ case runConstFolding env (evalConstExpression expr) of
    Left _ -> Nothing
    Right (value, _) -> Just value

-- | The interval the type of a variable allows, which is where its set of
-- values goes once the set outgrows the limit. A type that is not an integer
-- one has none.
rangeOfType :: TerminaType SemanticAnn -> ValueAnalysisMonad (Maybe (Integer, Integer))
rangeOfType ty = do
  plt <- platform <$> getGlobal
  return (intRange plt ty)

-- | The same, for a variable whose type the walk reads from the annotation the
-- type checker left on it.
rangeOfVariable :: Object SemanticAnn -> ValueAnalysisMonad (Maybe (Integer, Integer))
rangeOfVariable obj =
  maybe (return Nothing) (rangeOfType . snd) (getObjectSAnns (getAnnotation obj))

-- | Records what a variable holds from here on. Only a boolean or an integer
-- is followed; every other value, and every expression the evaluation could
-- not determine, leaves the variable unknown.
setValue ::
  Identifier
  -> Maybe (Integer, Integer)
  -> Maybe (Const SemanticAnn)
  -> ValueAnalysisMonad ()
setValue ident range mValue = modifyPath $ \p -> ValueAnalysisPath $
  case mValue >>= scalar of
    Just value -> M.insert ident (Known range (Discrete (S.singleton value))) (known p)
    Nothing -> M.delete ident (known p)

  where

    scalar value@(B _) = Just (Value value)
    scalar value@(I _ _) = Just (Value value)
    scalar _ = Nothing

forget :: Identifier -> ValueAnalysisMonad ()
forget ident = modifyPath (\p -> ValueAnalysisPath (M.delete ident (known p)))

-- | A variable whose address is handed out mutably is written behind the back
-- of this pass, so what was known of it no longer holds. An immutable
-- reference is not an escape, since the receiver cannot write through it.
noteEscapes :: Expression SemanticAnn -> ValueAnalysisMonad ()
noteEscapes = mapM_ escapesIn . expressionChildren

escapesIn :: Child SemanticAnn -> ValueAnalysisMonad ()
escapesIn (ChildReference Mutable obj) = forget (rootIdent obj)
escapesIn (ChildReference _ _) = return ()
escapesIn (ChildObject _) = return ()
escapesIn (ChildExpr expr) = noteEscapes expr
escapesIn (ChildArg expr) = noteEscapes expr
escapesIn (ChildConstExpr _) = return ()

-- | Reports a finding, unless the walk is still settling the state of a loop.
report :: Location -> Error -> ValueAnalysisMonad ()
report loc err = do
  quiet <- settling <$> getGlobal
  unless quiet (throwError $ annotateError loc err)

-- | A condition whose value this path already determines guards a path that is
-- never taken.
checkCondition :: Expression SemanticAnn -> ValueAnalysisMonad ()
checkCondition cond = do
  noteEscapes cond
  mValue <- valueOf cond
  case mValue of
    Just value@(B _) -> report (getLocation . getAnnotation $ cond) (EInvariantCondition value)
    _ -> return ()

-- | What a condition says about the variables in it inside the branch it
-- guards, in two forms: a boolean variable holds the value that took the path
-- there, and a comparison against a determined value fixes the variable at
-- that value on the side where the comparison holds. The other sides teach
-- nothing, since a refinement here only ever fixes a variable at a value.
refine :: Bool -> Expression SemanticAnn -> ValueAnalysisMonad ()
refine holds (AccessObject obj@(Variable ident _)) = do
  range <- rangeOfVariable obj
  setValue ident range (Just (B holds))
refine True (BinOp RelationalEqual left right _) = refineEquality left right
refine False (BinOp RelationalNotEqual left right _) = refineEquality left right
refine _ _ = return ()

-- | Which operand of the comparison is the variable and which the value is not
-- fixed by the syntax, so both orders are tried. A refinement only adds what
-- is known: an evaluation that fails leaves the variable as it was, instead of
-- forgetting it.
refineEquality :: Expression SemanticAnn -> Expression SemanticAnn -> ValueAnalysisMonad ()
refineEquality left right = do
  refineAgainst left right
  refineAgainst right left

  where

    refineAgainst (AccessObject obj@(Variable ident _)) other = do
      range <- rangeOfVariable obj
      valueOf other >>= maybe (return ()) (setValue ident range . Just)
    refineAgainst _ _ = return ()

-- | The body of a loop is walked twice: in silence until what is known at its
-- head stops falling, and once more from that settled state, which is the walk
-- that reports. Reporting while the state settles would blame a condition for
-- what a turn that is not the last one happens to know, as in
-- @var x = 0; for i in 0 .. 4 { if (x == 0) { … } x = 1; }@, where the first
-- turn alone sees an invariant condition.
--
-- The second walk goes through 'branch', which puts the state of the head back
-- afterwards: what the last turn assigns does not hold after a loop that may
-- run no turns at all.
walkLoopBody :: ValueAnalysisMonad () -> ValueAnalysisMonad ()
walkLoopBody body = do
  settle (fixpoint body)
  _ <- branch body
  return ()

  where

    settle m = do
      previous <- settling <$> getGlobal
      modifyGlobal (\g -> g { settling = True })
      result <- m
      modifyGlobal (\g -> g { settling = previous })
      return result

checkStatement :: Statement SemanticAnn -> ValueAnalysisMonad ()
checkStatement (Declaration ident _ ty mInitExpr _) = do
  mapM_ noteEscapes mInitExpr
  range <- rangeOfType ty
  value <- maybe (return Nothing) valueOf mInitExpr
  setValue ident range value
checkStatement (AssignmentStmt obj expr _) = do
  noteEscapes expr
  case obj of
    -- | The whole variable takes the value of the expression.
    Variable ident _ -> do
      range <- rangeOfVariable obj
      valueOf expr >>= setValue ident range
    -- | A write into a field or an element of an object, which is not a scalar
    -- and is therefore outside the lattice.
    _ -> return ()
checkStatement (SingleExpStmt expr _) = noteEscapes expr

-- | What each node means to this pass. Only the expressions that decide a path
-- are checked, which is the condition of an @if@, of each of its @else if@ and
-- the break condition of a @for@; the object a @match@ inspects is not, since
-- following it means following the variants of an enumeration and not the
-- value of a scalar.
transfer :: Transfer ValueAnalysisPath ValueAnalysisGlobal ValueAnalysisError
transfer = Transfer
  {
    onStatement = checkStatement
  , onSimpleBlock = \block -> mapM_ (mapM_ escapesIn) (simpleBlockChildren block)
  , onExpression = noteEscapes
  , onCondition = checkCondition
    -- | The variables a case binds are declared by the case, with a value that
    -- comes from the variant it matched.
  , onCaseEntry = \(MatchCase _ bvars _ _) -> mapM_ forget bvars
  , refineTrue = refine True
  , refineFalse = refine False
  , onLoopBody = walkLoopBody
  }

-- | Runs the body of a member or of a function, which knows nothing of its
-- parameters: a scalar parameter holds whatever the caller passed.
checkBody :: Block SemanticAnn -> ValueAnalysisMonad ()
checkBody body = do
  putPath (ValueAnalysisPath M.empty)
  walkForward transfer body

checkClassMember :: ClassMember SemanticAnn -> ValueAnalysisMonad ()
checkClassMember (ClassMethod _ak _ident _ps _tyret body _ann) = checkBody body
checkClassMember (ClassViewer _ident _ps _tyret body _ann) = checkBody body
checkClassMember (ClassAction _ak _ident _mp _tyret body _ann) = checkBody body
checkClassMember (ClassProcedure _ak _ident _ps body _ann) = checkBody body
checkClassMember (ClassField {}) = return ()

checkTypeDef :: TypeDef SemanticAnn -> ValueAnalysisMonad ()
checkTypeDef (Class _kind _ident members _provides _mods) = mapM_ checkClassMember members
checkTypeDef _ = return ()

checkElement :: AnnASTElement SemanticAnn -> ValueAnalysisMonad ()
checkElement (Function _ident _ps _ty body _mods _ann) = checkBody body
checkElement (TypeDefinition tyDef _ann) = checkTypeDef tyDef
checkElement (GlobalDeclaration {}) = return ()

-- | Runs the check over a whole module, with the constants it sees, returning
-- the first error.
runValueAnalysisCheck ::
  Platform
  -> M.Map Identifier (Const SemanticAnn)
  -> AnnotatedProgram SemanticAnn
  -> Maybe ValueAnalysisError
runValueAnalysisCheck plt consts = listToMaybe . mapMaybe checkOne

  where

    initialSt = DFState (ValueAnalysisPath M.empty) (ValueAnalysisGlobal consts plt False)

    checkOne = either Just (const Nothing) . fst . runDataflow initialSt . checkElement
