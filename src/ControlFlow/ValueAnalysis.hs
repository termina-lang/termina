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
-- The walk and the finding are separate. The walk records, for every condition
-- it meets, the state of the path there, and reports nothing; the diagnostic
-- then reads those records and decides.
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

import qualified Data.Map.Strict as M
import Data.List (nub)
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
    -- | The places that gave the variable what it holds, which the message
    -- points at. They are carried along and never read by the walk, so the
    -- equality below leaves them out: two states that agree on the values are
    -- the same state, and letting the origins decide would spend turns of the
    -- fixed point on information nobody iterates over.
  , valueOrigins :: S.Set Origin
  }

instance Eq Known where
  left == right =
    typeRange left == typeRange right && varValues left == varValues right

-- | What belongs to the path being walked: the local variables whose value the
-- path constrains. A variable that is not here is unknown, so joining two paths
-- keeps the entries they both constrain and drops the rest.
newtype ValueAnalysisPath = ValueAnalysisPath { known :: M.Map Identifier Known }
  deriving Eq

instance Lattice ValueAnalysisPath where
  joinPath left right =
    ValueAnalysisPath (M.mapMaybe id (M.intersectionWith agree (known left) (known right)))

    where

      agree x y = do
        values <- joinValues (typeRange x) (varValues x) (varValues y)
        return (Known (typeRange x) values
                  (S.union (valueOrigins x) (valueOrigins y)))

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
    -- | Every condition the walk has met, with the state of the path where it
    -- met it, under the position of the condition. A loop meets the same
    -- condition once per turn and each turn overwrites the one before, so what
    -- is left is what the last turn saw, which is the turn that starts from
    -- the settled state of the head.
    observed :: M.Map Location (Expression SemanticAnn, ValueAnalysisPath)
  }

type ValueAnalysisMonad = DataflowM ValueAnalysisPath ValueAnalysisGlobal ValueAnalysisError

-- | The one value a variable holds, when the path pins it to one. A set of
-- several values and an interval answer nothing, which leaves the pass where
-- it would be with the variable unknown.
singleValue :: Known -> Maybe (Const SemanticAnn)
singleValue entry = case varValues entry of
  Discrete values | [Value value] <- S.toList values -> Just value
  _ -> Nothing

-- | The value of an expression, when the constants of the module and what a
-- path knows of the local variables determine it.
--
-- The evaluation is the one of the folding, seeded with the local variables
-- pinned to a single value: such a variable answers a lookup exactly as a
-- module constant does, and one that is unknown is absent, which makes the
-- evaluation fail and the caller keep quiet. The two names cannot be confused,
-- since a local may not shadow a global (SE-081).
--
-- It takes what it reads instead of getting it from the state, since the
-- diagnostic evaluates the recorded conditions once the walk is over.
evaluate ::
  Platform
  -> M.Map Identifier (Const SemanticAnn)
  -> ValueAnalysisPath
  -> Expression SemanticAnn
  -> Maybe (Const SemanticAnn)
evaluate plt consts locals expr =
  case runConstFolding env (evalConstExpression expr) of
    Left _ -> Nothing
    Right (value, _) -> Just value

  where

    env = ConstFoldEnv
      (M.union (M.mapMaybe singleValue (known locals)) consts)
      plt

valueOf :: Expression SemanticAnn -> ValueAnalysisMonad (Maybe (Const SemanticAnn))
valueOf expr = do
  global <- getGlobal
  locals <- getPath
  return (evaluate (platform global) (moduleConsts global) locals expr)

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
  -> Origin
  -> Maybe (Const SemanticAnn)
  -> ValueAnalysisMonad ()
setValue ident range origin mValue = modifyPath $ \p -> ValueAnalysisPath $
  case mValue >>= scalar of
    Just value ->
      M.insert ident
        (Known range (Discrete (S.singleton value)) (S.singleton origin))
        (known p)
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

-- | Takes down a condition and the state of the path that reaches it, for the
-- diagnostic to read once the walk is over. It goes to the global state, which
-- a branch does not put back, so a record outlives the branch that made it.
observeCondition :: Expression SemanticAnn -> ValueAnalysisMonad ()
observeCondition cond = do
  noteEscapes cond
  locals <- getPath
  let loc = getLocation . getAnnotation $ cond
  modifyGlobal (\g -> g { observed = M.insert loc (cond, locals) (observed g) })

-- | What a condition says about the variables in it inside the branch it
-- guards, in two forms: a boolean variable holds the value that took the path
-- there, and a comparison against a determined value fixes the variable at
-- that value on the side where the comparison holds. The other sides teach
-- nothing, since a refinement here only ever fixes a variable at a value.
refine :: Bool -> Expression SemanticAnn -> ValueAnalysisMonad ()
refine holds cond@(AccessObject obj@(Variable ident _)) = do
  range <- rangeOfVariable obj
  setValue ident range (Refined (getLocation . getAnnotation $ cond)) (Just (B holds))
refine True (BinOp RelationalEqual left right ann) =
  refineEquality (getLocation ann) left right
refine False (BinOp RelationalNotEqual left right ann) =
  refineEquality (getLocation ann) left right
refine _ _ = return ()

-- | Which operand of the comparison is the variable and which the value is not
-- fixed by the syntax, so both orders are tried. A refinement only adds what
-- is known: an evaluation that fails leaves the variable as it was, instead of
-- forgetting it.
refineEquality ::
  Location -> Expression SemanticAnn -> Expression SemanticAnn -> ValueAnalysisMonad ()
refineEquality loc left right = do
  refineAgainst left right
  refineAgainst right left

  where

    refineAgainst (AccessObject obj@(Variable ident _)) other = do
      range <- rangeOfVariable obj
      valueOf other
        >>= maybe (return ()) (setValue ident range (Refined loc) . Just)
    refineAgainst _ _ = return ()

checkStatement :: Statement SemanticAnn -> ValueAnalysisMonad ()
checkStatement (Declaration ident _ ty mInitExpr ann) = do
  mapM_ noteEscapes mInitExpr
  range <- rangeOfType ty
  value <- maybe (return Nothing) valueOf mInitExpr
  setValue ident range (Assigned (getLocation ann)) value
checkStatement (AssignmentStmt obj expr ann) = do
  noteEscapes expr
  case obj of
    -- | The whole variable takes the value of the expression.
    Variable ident _ -> do
      range <- rangeOfVariable obj
      valueOf expr >>= setValue ident range (Assigned (getLocation ann))
    -- | A write into a field or an element of an object, which is not a scalar
    -- and is therefore outside the lattice.
    _ -> return ()
checkStatement (SingleExpStmt expr _) = noteEscapes expr

-- | What each node means to this pass. Only the expressions that decide a path
-- are recorded, which is the condition of an @if@, of each of its @else if@ and
-- the break condition of a @for@; the object a @match@ inspects is not, since
-- following it means following the variants of an enumeration and not the
-- value of a scalar.
transfer :: Transfer ValueAnalysisPath ValueAnalysisGlobal ValueAnalysisError
transfer = Transfer
  {
    onStatement = checkStatement
  , onSimpleBlock = mapM_ (mapM_ escapesIn) . simpleBlockChildren
  , onExpression = noteEscapes
  , onCondition = observeCondition
    -- | The variables a case binds are declared by the case, with a value that
    -- comes from the variant it matched.
  , onCaseEntry = \(MatchCase _ bvars _ _) -> mapM_ forget bvars
  , refineTrue = refine True
  , refineFalse = refine False
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

-- | The plain variables an expression reads, in the order it reads them. A
-- field or an element of an array is left out, since the pass follows neither
-- and so has nothing to say about them.
variablesIn :: Expression SemanticAnn -> [Identifier]
variablesIn = concatMap inChild . expressionChildren

  where

    inChild (ChildObject (Variable ident _)) = [ident]
    inChild (ChildObject _) = []
    inChild (ChildReference _ _) = []
    inChild (ChildExpr expr) = variablesIn expr
    inChild (ChildArg expr) = variablesIn expr
    inChild (ChildConstExpr _) = []

-- | What the message says about each name of a condition: the value it holds
-- and where it got it. A name the path pins answers with the places that gave
-- it the value; a constant of the module answers with no place, since it has
-- none inside the body.
reasonsFor ::
  ValueAnalysisGlobal -> ValueAnalysisPath -> Expression SemanticAnn -> [Reason]
reasonsFor global locals cond =
  mapMaybe reasonOf (nub (variablesIn cond))

  where

    reasonOf ident =
      case M.lookup ident (known locals) >>= withValue ident of
        Just reason -> Just reason
        Nothing ->
          (\value -> Reason ident value []) <$> M.lookup ident (moduleConsts global)

    withValue ident entry =
      (\value -> Reason ident value (S.toList (valueOrigins entry)))
        <$> singleValue entry

-- | The diagnostic: of the conditions the walk recorded, the ones whose value
-- the state at them determines, in the order the source has them, which is the
-- order of their positions.
invariantConditions :: ValueAnalysisGlobal -> [ValueAnalysisError]
invariantConditions global =
  [ annotateError loc (EInvariantCondition value (reasonsFor global locals cond))
  | (loc, (cond, locals)) <- M.toAscList (observed global)
  , Just value@(B _) <-
      [evaluate (platform global) (moduleConsts global) locals cond]
  ]

-- | Runs the check over a whole module, with the constants it sees, returning
-- the first error. Only the first, since a user resolves them one at a time.
runValueAnalysisCheck ::
  Platform
  -> M.Map Identifier (Const SemanticAnn)
  -> AnnotatedProgram SemanticAnn
  -> Maybe ValueAnalysisError
runValueAnalysisCheck plt consts = listToMaybe . mapMaybe checkOne

  where

    initialSt =
      DFState (ValueAnalysisPath M.empty) (ValueAnalysisGlobal consts plt M.empty)

    checkOne =
      listToMaybe . invariantConditions . globalState
        . snd . runDataflow initialSt . checkElement
