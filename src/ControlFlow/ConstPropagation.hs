-- | Constant propagation over the basic-block AST.
--
-- The pass follows the value of the local scalar variables of a body and
-- reports a condition whose value it already knows (CPE-001): one of the paths
-- such a condition guards is never taken, which is what MISRA-C:2023 Rule 14.3
-- forbids. It is the whole of that rule, the constants of the module included,
-- which is why it is seeded with the constant environment the folding builds
-- instead of running with the rest of the basic-block checks.
--
-- What a variable holds is a flat lattice, two levels deep: a value, with the
-- value attached, or unknown, which is the absence of an entry. Where paths
-- meet, two entries survive only if they agree, so a variable can only fall
-- from known to unknown and the walk of a loop settles after at most one fall
-- per variable, with no widening.
--
-- A condition is evaluated with the evaluator of
-- 'ControlFlow.ConstFolding', which is what keeps the arithmetic of this pass
-- and the arithmetic of the folding the same one. An evaluation that does not
-- succeed leaves the pass silent, which is the only direction an error of the
-- transpiler may be wrong in.
module ControlFlow.ConstPropagation (runConstPropagationCheck) where

import Control.Monad (unless)
import Control.Monad.Except
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe, mapMaybe)

import Configuration.Platform (Platform)
import ControlFlow.BasicBlocks.AST
import ControlFlow.BasicBlocks.Traversal
    (Child(..), expressionChildren, simpleBlockChildren, rootIdent)
import ControlFlow.ConstFolding (evalConstExpression, runConstFolding)
import ControlFlow.ConstFolding.Monad (ConstFoldEnv(..))
import ControlFlow.ConstPropagation.Errors
import ControlFlow.Dataflow
import Semantic.Types (SemanticAnn)
import Utils.Annotations

-- | A value a variable is known to hold. Equality is on the value alone: two
-- paths that reach the same number agree whether or not it was written the
-- same way, and the type a constant carries is not part of what they agree on.
newtype Value = Value { valueConst :: Const SemanticAnn }

instance Eq Value where
  Value left == Value right = sameValue left right

    where

      sameValue (B x) (B y) = x == y
      sameValue (I (TInteger x _) _) (I (TInteger y _) _) = x == y
      sameValue _ _ = False

-- | What belongs to the path being walked: the local variables whose value is
-- known at this point. A variable that is not here is unknown, so joining two
-- paths keeps the entries they agree on and drops the rest.
newtype ConstPropPath = ConstPropPath { known :: M.Map Identifier Value }
  deriving Eq

instance Lattice ConstPropPath where
  joinPath left right =
    ConstPropPath (M.mapMaybe id (M.intersectionWith agree (known left) (known right)))

    where

      agree x y = if x == y then Just x else Nothing

-- | What belongs to the program, and no branch takes back.
data ConstPropGlobal = ConstPropGlobal
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

type ConstPropMonad = DataflowM ConstPropPath ConstPropGlobal ConstPropError

-- | The value of an expression, when the constants of the module and what this
-- path knows of the local variables determine it.
--
-- The evaluation is the one of the folding, seeded with the local variables
-- whose value is known: such a variable answers a lookup exactly as a module
-- constant does, and one whose value is unknown is absent, which makes the
-- evaluation fail and the pass keep quiet. The two names cannot be confused,
-- since a local may not shadow a global (SE-081).
valueOf :: Expression SemanticAnn -> ConstPropMonad (Maybe (Const SemanticAnn))
valueOf expr = do
  global <- getGlobal
  locals <- known <$> getPath
  let env = ConstFoldEnv
        (M.union (M.map valueConst locals) (moduleConsts global))
        (platform global)
  return $ case runConstFolding env (evalConstExpression expr) of
    Left _ -> Nothing
    Right (value, _) -> Just value

-- | Records what a variable holds from here on. Only a boolean or an integer
-- is followed; every other value, and every expression the evaluation could
-- not determine, leaves the variable unknown.
setValue :: Identifier -> Maybe (Const SemanticAnn) -> ConstPropMonad ()
setValue ident mValue = modifyPath $ \p -> ConstPropPath $
  case mValue >>= scalar of
    Just value -> M.insert ident value (known p)
    Nothing -> M.delete ident (known p)

  where

    scalar value@(B _) = Just (Value value)
    scalar value@(I _ _) = Just (Value value)
    scalar _ = Nothing

forget :: Identifier -> ConstPropMonad ()
forget ident = modifyPath (\p -> ConstPropPath (M.delete ident (known p)))

-- | A variable whose address is handed out mutably is written behind the back
-- of this pass, so what was known of it no longer holds. An immutable
-- reference is not an escape, since the receiver cannot write through it.
noteEscapes :: Expression SemanticAnn -> ConstPropMonad ()
noteEscapes = mapM_ escapesIn . expressionChildren

escapesIn :: Child SemanticAnn -> ConstPropMonad ()
escapesIn (ChildReference Mutable obj) = forget (rootIdent obj)
escapesIn (ChildReference _ _) = return ()
escapesIn (ChildObject _) = return ()
escapesIn (ChildExpr expr) = noteEscapes expr
escapesIn (ChildArg expr) = noteEscapes expr
escapesIn (ChildConstExpr _) = return ()

-- | Reports a finding, unless the walk is still settling the state of a loop.
report :: Location -> Error -> ConstPropMonad ()
report loc err = do
  quiet <- settling <$> getGlobal
  unless quiet (throwError $ annotateError loc err)

-- | A condition whose value this path already determines guards a path that is
-- never taken.
checkCondition :: Expression SemanticAnn -> ConstPropMonad ()
checkCondition cond = do
  noteEscapes cond
  mValue <- valueOf cond
  case mValue of
    Just value@(B _) -> report (getLocation . getAnnotation $ cond) (EInvariantCondition value)
    _ -> return ()

-- | What a condition says about the variables in it inside the branch it
-- guards, in the two forms a flat lattice can express: a boolean variable
-- holds the value that took the path there, and a comparison against a
-- determined value fixes the variable at that value on the side where the
-- comparison holds. The other sides teach nothing, because the lattice has no
-- way to say "anything but this value".
refine :: Bool -> Expression SemanticAnn -> ConstPropMonad ()
refine holds (AccessObject (Variable ident _)) = setValue ident (Just (B holds))
refine True (BinOp RelationalEqual left right _) = refineEquality left right
refine False (BinOp RelationalNotEqual left right _) = refineEquality left right
refine _ _ = return ()

-- | Which operand of the comparison is the variable and which the value is not
-- fixed by the syntax, so both orders are tried. A refinement only adds what
-- is known: an evaluation that fails leaves the variable as it was, instead of
-- forgetting it.
refineEquality :: Expression SemanticAnn -> Expression SemanticAnn -> ConstPropMonad ()
refineEquality left right = do
  refineAgainst left right
  refineAgainst right left

  where

    refineAgainst (AccessObject (Variable ident _)) other =
      valueOf other >>= maybe (return ()) (setValue ident . Just)
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
walkLoopBody :: ConstPropMonad () -> ConstPropMonad ()
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

checkStatement :: Statement SemanticAnn -> ConstPropMonad ()
checkStatement (Declaration ident _ _ mInitExpr _) = do
  mapM_ noteEscapes mInitExpr
  value <- maybe (return Nothing) valueOf mInitExpr
  setValue ident value
checkStatement (AssignmentStmt obj expr _) = do
  noteEscapes expr
  case obj of
    -- | The whole variable takes the value of the expression.
    Variable ident _ -> valueOf expr >>= setValue ident
    -- | A write into a field or an element of an object, which is not a scalar
    -- and is therefore outside the lattice.
    _ -> return ()
checkStatement (SingleExpStmt expr _) = noteEscapes expr

-- | What each node means to this pass. Only the expressions that decide a path
-- are checked, which is the condition of an @if@, of each of its @else if@ and
-- the break condition of a @for@; the object a @match@ inspects is not, since
-- following it means following the variants of an enumeration and not the
-- value of a scalar.
transfer :: Transfer ConstPropPath ConstPropGlobal ConstPropError
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
checkBody :: Block SemanticAnn -> ConstPropMonad ()
checkBody body = do
  putPath (ConstPropPath M.empty)
  walkForward transfer body

checkClassMember :: ClassMember SemanticAnn -> ConstPropMonad ()
checkClassMember (ClassMethod _ak _ident _ps _tyret body _ann) = checkBody body
checkClassMember (ClassViewer _ident _ps _tyret body _ann) = checkBody body
checkClassMember (ClassAction _ak _ident _mp _tyret body _ann) = checkBody body
checkClassMember (ClassProcedure _ak _ident _ps body _ann) = checkBody body
checkClassMember (ClassField {}) = return ()

checkTypeDef :: TypeDef SemanticAnn -> ConstPropMonad ()
checkTypeDef (Class _kind _ident members _provides _mods) = mapM_ checkClassMember members
checkTypeDef _ = return ()

checkElement :: AnnASTElement SemanticAnn -> ConstPropMonad ()
checkElement (Function _ident _ps _ty body _mods _ann) = checkBody body
checkElement (TypeDefinition tyDef _ann) = checkTypeDef tyDef
checkElement (GlobalDeclaration {}) = return ()

-- | Runs the check over a whole module, with the constants it sees, returning
-- the first error.
runConstPropagationCheck ::
  Platform
  -> M.Map Identifier (Const SemanticAnn)
  -> AnnotatedProgram SemanticAnn
  -> Maybe ConstPropError
runConstPropagationCheck plt consts = listToMaybe . mapMaybe checkOne

  where

    initialSt = DFState (ConstPropPath M.empty) (ConstPropGlobal consts plt False)

    checkOne = either Just (const Nothing) . fst . runDataflow initialSt . checkElement
