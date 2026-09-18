{-# LANGUAGE LambdaCase #-}
-- | Scope check: a local object whose uses all fall inside one of the blocks
-- nested in the block that declares it is to be declared in that inner block
-- (VSE-001).
--
-- The inner blocks are the bodies of the branches of an @if@, of the cases of
-- a @match@ and of a @for@. The conditions of an @if@, the object a @match@
-- inspects and the bounds and break condition of a @for@ are evaluated outside
-- those bodies, so a use there is a use of the enclosing block. The check
-- records, for each declaration, the path of blocks that leads to each of its
-- uses, and the inner block it can move to is the longest prefix they share.
--
-- Moving a declaration changes when its initializer is evaluated, so the check
-- only reports it when the value would be the same: the initializer is built
-- from literals, constants and names that cannot be written, which are the
-- @let@ locals and the parameters passed by value. Moving it into the body of
-- a loop also runs the declaration once per turn instead of once, which keeps
-- the meaning of the program only if no turn reads what the previous one left
-- in it, and the check asks for that on every path of the body: the first
-- access is an assignment of the whole object, or the destination of an
-- atomic load. It relies on the definite assignment check having accepted the
-- program, and runs after it.
--
-- A reference taken to the object does not stop the check, unlike in C: a
-- reference in Termina cannot be stored nor returned, so none outlives the
-- call it is passed to.
module ControlFlow.VarScope (runVarScopeCheck) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)
import qualified Control.Monad.State as ST
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (listToMaybe, mapMaybe)

import ControlFlow.BasicBlocks.AST
import ControlFlow.BasicBlocks.Traversal
    (Child(..), ObjectVisitor(..), expressionChildren, simpleBlockChildren,
     walkObject, rootIdent, indexExpressions)
import ControlFlow.Dataflow
import ControlFlow.VarScope.Errors
import Semantic.Types (SemanticAnn)
import Utils.Annotations

-- | A block is named by the blocks that lead to it from the body, outermost
-- first, so that the blocks two uses share are the prefix their paths share.
type BlockPath = [Int]

data Decl = Decl
  {
    declIdent :: Identifier
  , declLoc :: Location
    -- | The block that declares the object.
  , declPath :: BlockPath
    -- | Whether the initializer gives the same value wherever it is evaluated.
  , declMovable :: Bool
    -- | The block of each use.
  , declUses :: [BlockPath]
  }

-- | An inner block, with the position of the construct that opens it and, if
-- it is the body of a loop, the body itself.
data Inner = Inner
  {
    innerHead :: Location
  , innerLoop :: Maybe (Block SemanticAnn)
  }

data ScopeSt = ScopeSt
  {
    nextBlock :: Int
  , here :: BlockPath
    -- | The declarations visible here, by name.
  , visible :: M.Map Identifier Int
    -- | Of the visible declarations, the @let@ ones.
  , immutables :: S.Set Identifier
    -- | Parameters whose value may change under the body: @self@ and the
    -- parameters of reference type.
  , unstable :: S.Set Identifier
  , decls :: M.Map Int Decl
  , inners :: M.Map BlockPath Inner
  }

type ScopeM = ST.State ScopeSt

useIdent :: Identifier -> ScopeM ()
useIdent ident = ST.modify $ \st ->
  case M.lookup ident (visible st) of
    Nothing -> st
    Just n -> st { decls = M.adjust (\d -> d { declUses = here st : declUses d }) n (decls st) }

useObject :: Object SemanticAnn -> ScopeM ()
useObject = walkObject ObjectVisitor
  {
    atRoot = \ident _ -> useIdent ident
  , atField = \_ _ -> return ()
  , atIndex = useExpression
  }

useExpression :: Expression SemanticAnn -> ScopeM ()
useExpression = mapM_ useChild . expressionChildren

useChild :: Child SemanticAnn -> ScopeM ()
useChild = \case
  ChildExpr e -> useExpression e
  ChildArg e -> useExpression e
  ChildObject obj -> useObject obj
  ChildReference _ obj -> useObject obj
  ChildConstExpr e -> useExpression e

-- | Whether an initializer gives the same value wherever in the scope of the
-- object it is evaluated. Calls are left out because they may have effects,
-- and references because the object they point to may change.
movableExpression :: Expression SemanticAnn -> ScopeM Bool
movableExpression = \case
  FunctionCall {} -> return False
  MemberFunctionCall {} -> return False
  DerefMemberFunctionCall {} -> return False
  ReferenceExpression {} -> return False
  ArraySliceExpression {} -> return False
  e -> and <$> mapM movableChild (expressionChildren e)

  where

    movableChild :: Child SemanticAnn -> ScopeM Bool
    movableChild = \case
      ChildExpr e -> movableExpression e
      ChildArg e -> movableExpression e
      ChildConstExpr _ -> return True
      ChildReference _ _ -> return False
      ChildObject obj -> do
        stable <- stableName (rootIdent obj)
        indices <- mapM movableExpression (indexExpressions obj)
        return (stable && and indices)

    -- | A name that is not a local of the body is a parameter, a variable a
    -- case or a loop binds, or a constant of the module.
    stableName :: Identifier -> ScopeM Bool
    stableName ident = do
      st <- ST.get
      return $ if M.member ident (visible st)
        then S.member ident (immutables st)
        else not (S.member ident (unstable st))

checkStatement :: Statement SemanticAnn -> ScopeM ()
checkStatement (Declaration ident ak _ mInit ann) = do
  mapM_ useExpression mInit
  movable <- maybe (return True) movableExpression mInit
  ST.modify $ \st ->
    let n = M.size (decls st) in
    st {
      decls = M.insert n (Decl ident (getLocation ann) (here st) movable []) (decls st)
    , visible = M.insert ident n (visible st)
    , immutables = (if ak == Immutable then S.insert else S.delete) ident (immutables st)
    }
checkStatement (AssignmentStmt obj e _) = useObject obj >> useExpression e
checkStatement (SingleExpStmt e _) = useExpression e

-- | Walks an inner block. What it declares is not visible after it.
checkInner :: Location -> Maybe (Block SemanticAnn) -> Block SemanticAnn -> ScopeM ()
checkInner headLoc loop body = do
  outer <- ST.get
  let path = here outer ++ [nextBlock outer]
  ST.put outer {
      nextBlock = nextBlock outer + 1
    , here = path
    , inners = M.insert path (Inner headLoc loop) (inners outer)
    }
  checkBlock body
  ST.modify $ \st ->
    st { here = here outer, visible = visible outer, immutables = immutables outer }

checkBlock :: Block SemanticAnn -> ScopeM ()
checkBlock = mapM_ checkBasicBlock . blockBody

checkBasicBlock :: BasicBlock SemanticAnn -> ScopeM ()
checkBasicBlock (RegularBlock stmts) = mapM_ checkStatement stmts
checkBasicBlock (IfElseBlock condIf elseIfs mElse _) = do
  useExpression (condIfCond condIf)
  mapM_ (useExpression . condElseIfCond) elseIfs
  checkInner (getLocation (condIfAnnotation condIf)) Nothing (condIfBody condIf)
  forM_ elseIfs $ \elseIf ->
    checkInner (getLocation (condElseIfAnnotation elseIf)) Nothing (condElseIfBody elseIf)
  forM_ mElse $ \els ->
    checkInner (getLocation (condElseAnnotation els)) Nothing (condElseBody els)
checkBasicBlock (MatchBlock expr cases mDefault _) = do
  useExpression expr
  forM_ cases $ \c -> checkInner (getLocation (matchAnnotation c)) Nothing (matchBody c)
  forM_ mDefault $ \(DefaultCase blk ann) -> checkInner (getLocation ann) Nothing blk
checkBasicBlock (ForLoopBlock _ _ initE endE mBreak blk ann) = do
  useExpression initE
  useExpression endE
  mapM_ useExpression mBreak
  checkInner (getLocation ann) (Just blk) blk
checkBasicBlock bb = mapM_ (mapM_ useChild) (simpleBlockChildren bb)

-- | Whether the object is assigned as a whole along the current path of a turn.
newtype Assigned = Assigned Bool deriving Eq

instance Lattice Assigned where
  joinPath (Assigned left) (Assigned right) = Assigned (left && right)

-- | Whether, on every path of the body of a loop, the first access to the
-- object assigns it as a whole, so that no turn reads what the previous one
-- left in it.
firstAccessAssigns :: Identifier -> Block SemanticAnn -> Bool
firstAccessAssigns ident body =
  either (const False) (const True) . fst $
    runDataflow (DFState (Assigned False) ()) (walkForward transfer body)

  where

    transfer :: Transfer Assigned () ()
    transfer = Transfer
      {
        onStatement = statement
      , onSimpleBlock = simpleBlock
      , onExpression = readExpr
      , onCondition = readExpr
      , onCaseEntry = \_ _ -> return ()
      , onLoopEntry = \_ _ _ _ -> return ()
      , refineTrue = const (return ())
      , refineFalse = const (return ())
      }

    statement (AssignmentStmt (Variable v _) e _)
      | v == ident = readExpr e >> putPath (Assigned True)
    statement (AssignmentStmt obj e _) = readChildren [ChildObject obj, ChildExpr e]
    statement (Declaration _ _ _ mInit _) = mapM_ readExpr mInit
    statement (SingleExpStmt e _) = readExpr e

    simpleBlock (AtomicLoad port dest _)
      | isDestination dest = readChildren [ChildObject port] >> putPath (Assigned True)
    simpleBlock (AtomicArrayLoad port index dest _)
      | isDestination dest =
          readChildren [ChildObject port, ChildExpr index] >> putPath (Assigned True)
    simpleBlock bb = mapM_ readChildren (simpleBlockChildren bb)

    isDestination (ReferenceExpression _ (Variable v _) _) = v == ident
    isDestination _ = False

    readExpr e = readChildren [ChildExpr e]

    -- | Any access other than those above reads the object, or writes a part
    -- of it, which also keeps what the rest of it held.
    readChildren children =
      when (any mentions children) $ do
        Assigned assigned <- getPath
        unless assigned (throwError ())

    mentions = \case
      ChildExpr e -> any mentions (expressionChildren e)
      ChildArg e -> any mentions (expressionChildren e)
      ChildConstExpr _ -> False
      ChildObject obj -> objectMentions obj
      ChildReference _ obj -> objectMentions obj

    objectMentions obj =
      rootIdent obj == ident || any (mentions . ChildExpr) (indexExpressions obj)

-- | The inner block a declaration can be moved to, if any.
reducedTo :: ScopeSt -> Decl -> Maybe Location
reducedTo st d
  | null (declUses d) = Nothing
  | length target <= length (declPath d) = Nothing
  | not (declMovable d) = Nothing
  | not (all (firstAccessAssigns (declIdent d)) (mapMaybe innerLoop crossed)) = Nothing
  | otherwise = innerHead <$> M.lookup target (inners st)

  where

    target = foldr1 commonPrefix (declUses d)

    crossed = mapMaybe (\n -> M.lookup (take n target) (inners st))
      [length (declPath d) + 1 .. length target]

    commonPrefix (x : xs) (y : ys) | x == y = x : commonPrefix xs ys
    commonPrefix _ _ = []

-- | Checks the body of a member or of a function, reporting the first of its
-- declarations that can be moved.
checkBody :: [Parameter SemanticAnn] -> Block SemanticAnn -> Maybe VarScopeError
checkBody ps body =
  let st = ST.execState (checkBlock body) (ScopeSt 0 [] M.empty S.empty unstableParams M.empty M.empty)
      found = mapMaybe (\d -> (,) d <$> reducedTo st d) (M.elems (decls st))
  in (\(d, target) -> annotateError (declLoc d) (EScopeCanBeReduced (declIdent d) target))
      <$> listToMaybe found

  where

    unstableParams = S.fromList ("self" : [paramIdentifier p | p <- ps, isReference p])

    isReference p = case paramType p of
      TReference {} -> True
      _ -> False

checkClassMember :: ClassMember SemanticAnn -> Maybe VarScopeError
checkClassMember (ClassMethod _ _ ps _ body _) = checkBody ps body
checkClassMember (ClassViewer _ ps _ body _) = checkBody ps body
checkClassMember (ClassAction _ _ mp _ body _) = checkBody (maybe [] (: []) mp) body
checkClassMember (ClassProcedure _ _ ps body _) = checkBody ps body
checkClassMember (ClassField {}) = Nothing

checkElement :: AnnASTElement SemanticAnn -> Maybe VarScopeError
checkElement (Function _ ps _ body _ _) = checkBody ps body
checkElement (TypeDefinition (Class _ _ members _ _) _) =
  listToMaybe (mapMaybe checkClassMember members)
checkElement _ = Nothing

-- | Runs the check over a whole module, returning the first error.
runVarScopeCheck :: AnnotatedProgram SemanticAnn -> Maybe VarScopeError
runVarScopeCheck = listToMaybe . mapMaybe checkElement
