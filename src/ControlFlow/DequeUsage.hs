{-# LANGUAGE LambdaCase #-}
-- | Deque check: a box is only put into a deque that has room for it on every
-- path that reaches the put (DQE-001 and DQE-002), and every deque is both
-- filled and taken somewhere in its class (DQE-003 and DQE-004). A deque is
-- reached through an access port, and only one element connects to it.
--
-- The class does not know how many boxes the deque holds, so the state of a
-- deque is the set of values it may hold: with room for one more box, or full.
-- A take always leaves room, and any put may fill it, so the check is a forward
-- walk of each member where paths join by union. A try_put or a replace does
-- not need room, but may fill the deque too. A deque keeps its boxes between
-- activations, so an action or a procedure starts with every deque both with
-- room and full.
--
-- Only a call to a method of the same class can change a deque behind the back
-- of its caller: a call through a port never comes back to the caller, a member
-- cannot call a procedure, and tasks and handlers have no methods. Methods come
-- before their callers in the class, so each method is summarized before any
-- call to it is met. A summary says, for each deque and each value it may hold
-- when the method starts, the values it may hold when it returns, and whether
-- the method puts into it while it may be full. No operation reads the state of
-- one deque to change another, so walking the method twice, once with every
-- deque empty and once with every deque full, gives the summary of all of them.
module ControlFlow.DequeUsage (runDequeUsageCheck) where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortOn)
import Data.Maybe (listToMaybe, fromMaybe)

import Lowering.AST
import ControlFlow.Traversal (Child'(..), expressionChildren, indexExpressions)
import ControlFlow.Dataflow
import ControlFlow.DequeUsage.Errors
import Semantic.Types (SemanticAnn)
import Utils.Annotations

data Fill = Empty | Full
  deriving (Eq, Ord, Show)

-- | The values a deque may hold at a point, with the reasons why it may be full.
type DequeState = M.Map Fill (S.Set FullBecause)

-- | The state of the deques along a path, or nothing if the path has ended.
newtype Path = Path (Maybe (M.Map Identifier DequeState))
  deriving Eq

instance Lattice Path where
  joinPath (Path Nothing) p = p
  joinPath p (Path Nothing) = p
  joinPath (Path (Just a)) (Path (Just b)) =
    Path (Just (M.unionWith (M.unionWith S.union) a b))

-- | What a method does to the deques of its class.
data Summary = Summary
  {
    -- | For each deque and each value it may hold when the method starts, the
    -- values it may hold when the method returns.
    summaryPost :: M.Map Identifier (M.Map Fill DequeState)
    -- | For each deque, where the method puts into it if it is full when the
    -- method starts.
  , summaryUnsafe :: M.Map Identifier Location
  }

data DequeGlobal = DequeGlobal
  {
    -- | The puts into a deque that may be full, by deque.
    findings :: [(Identifier, DequeUsageError)]
    -- | The state of the deques at each return, joined.
  , exits :: Path
  }

type DequeM = DataflowM Path DequeGlobal ()

dequeOf :: Object SemanticAnn -> Maybe Identifier
dequeOf (MemberAccess _ deque _) = Just deque
dequeOf (DereferenceMemberAccess _ deque _) = Just deque
dequeOf _ = Nothing

-- | The calls to a method of the class that an expression makes, in the order
-- they are evaluated.
methodCalls :: M.Map Identifier Summary -> Expression SemanticAnn -> [(Identifier, Location)]
methodCalls summaries e = concatMap inChild (expressionChildren e) ++ here

  where

    here = case e of
      MemberFunctionCall _ m _ ann | M.member m summaries -> [(m, getLocation ann)]
      DerefMemberFunctionCall _ m _ ann | M.member m summaries -> [(m, getLocation ann)]
      _ -> []

    inChild = \case
      ChildExpr x -> methodCalls summaries x
      ChildArg x -> methodCalls summaries x
      ChildObject o -> concatMap (methodCalls summaries) (indexExpressions o)
      ChildReference _ o -> concatMap (methodCalls summaries) (indexExpressions o)
      ChildConstExpr _ -> []

record :: Identifier -> DequeUsageError -> DequeM ()
record deque err = modifyGlobal (\g -> g { findings = (deque, err) : findings g })

-- | The reasons why a deque may be full in a state.
fullReasons :: DequeState -> [FullBecause]
fullReasons st = maybe [] S.toList (M.lookup Full st)

-- | Applies the summary of a method called at the given position.
applyCall :: M.Map Identifier Summary -> (Identifier, Location) -> DequeM ()
applyCall summaries (m, callLoc) = do
  Path mDeques <- getPath
  forM_ mDeques $ \deques -> do
    let summary = summaries M.! m
    forM_ (M.toList deques) $ \(deque, st) ->
      case M.lookup deque (summaryUnsafe summary) of
        Just putLoc | M.member Full st ->
          record deque (annotateError callLoc (EDequeMayBeFullInCall deque m putLoc (fullReasons st)))
        _ -> return ()
    putPath (Path (Just (M.mapWithKey (after summary) deques)))

  where

    after summary deque st =
      case M.lookup deque (summaryPost summary) of
        Nothing -> st
        Just post -> M.unionsWith S.union
          [ M.map (S.unions . map (translate (fromMaybe S.empty (M.lookup x st))) . S.toList)
              (fromMaybe M.empty (M.lookup x post))
          | x <- M.keys st ]

    -- | A reason of the callee seen from the call: the entry of the method is
    -- whatever made the deque full before the call, and a put inside it is a put
    -- made by the call.
    translate before = \case
      FullOnEntry _ -> before
      FullAfterPut putLoc -> S.singleton (FullAfterCall m callLoc putLoc)
      other -> S.singleton other

applyCallsIn :: M.Map Identifier Summary -> Expression SemanticAnn -> DequeM ()
applyCallsIn summaries = mapM_ (applyCall summaries) . methodCalls summaries

-- | Ends the path, recording its state as one the member returns with.
endPath :: Bool -> DequeM ()
endPath returns = do
  p <- getPath
  when returns $ modifyGlobal (\g -> g { exits = joinPath (exits g) p })
  putPath (Path Nothing)

dequeBlock :: BasicBlock SemanticAnn -> DequeM ()
dequeBlock = \case
  DequeTake obj _ _ -> forM_ (dequeOf obj) $ \deque ->
    modifyPath (\(Path p) -> Path (M.insert deque (M.singleton Empty S.empty) <$> p))
  DequePut _ obj _ ann -> forM_ (dequeOf obj) $ \deque -> do
    Path p <- getPath
    forM_ p $ \deques -> do
      let st = M.findWithDefault M.empty deque deques
      when (M.member Full st) $
        record deque (annotateError (getLocation ann) (EDequeMayBeFull deque (fullReasons st)))
      putPath (Path (Just (M.insert deque (M.singleton Full (S.singleton (FullAfterPut (getLocation ann)))) deques)))
  DequeExchange _ obj _ _ ann -> forM_ (dequeOf obj) $ \deque ->
    modifyPath (\(Path p) -> Path (M.adjust (M.insertWith S.union Full (S.singleton (FullAfterPut (getLocation ann)))) deque <$> p))
  ReturnBlock _ _ -> endPath True
  ContinueBlock _ _ -> endPath False
  RebootBlock _ -> endPath False
  NoReturnCall _ _ -> endPath False
  _ -> return ()

transfer :: M.Map Identifier Summary -> Transfer Path DequeGlobal ()
transfer summaries = Transfer
  {
    onStatement = \case
      Declaration _ _ _ mExpr _ -> mapM_ calls mExpr
      AssignmentStmt obj e _ -> mapM_ calls (indexExpressions obj) >> calls e
      SingleExpStmt e _ -> calls e
  , onSimpleBlock = \block -> do
      forM_ (simpleBlockChildren block) $ mapM_ $ \case
        ChildExpr e -> calls e
        ChildArg e -> calls e
        ChildObject o -> mapM_ calls (indexExpressions o)
        ChildReference _ o -> mapM_ calls (indexExpressions o)
        ChildConstExpr _ -> return ()
      dequeBlock block
  , onExpression = calls
  , onCondition = calls
  , onLoopGuard = calls
  , onCaseEntry = \_ _ -> return ()
  , onLoopEntry = \_ _ _ _ _ -> return ()
  , refineTrue = const (return ())
  , refineFalse = const (return ())
  }

  where

    calls = applyCallsIn summaries

-- | Walks a body from the given state of the deques.
walkBody :: M.Map Identifier Summary -> M.Map Identifier DequeState -> Block SemanticAnn -> DequeGlobal
walkBody summaries entry body =
  let initial = DFState (Path (Just entry)) (DequeGlobal [] (Path Nothing)) in
  globalState . snd $ runDataflow initial (walkForward (transfer summaries) body)

-- | Every deque, both empty and full on entry to the given member.
unknownEntry :: Identifier -> [Identifier] -> M.Map Identifier DequeState
unknownEntry member deques =
  M.fromList [(s, M.fromList [(Empty, S.empty), (Full, S.singleton (FullOnEntry member))]) | s <- deques]

-- | Summarizes a method from its two walks, returning the findings that do not
-- depend on its caller.
summarize :: M.Map Identifier Summary -> Identifier -> [Identifier] -> Block SemanticAnn
  -> (Summary, [(Identifier, DequeUsageError)])
summarize summaries m deques body =
  let fromEmpty = walkBody summaries (M.fromList [(s, M.singleton Empty S.empty) | s <- deques]) body
      fromFull = walkBody summaries (M.fromList [(s, M.singleton Full (S.singleton (FullOnEntry m))) | s <- deques]) body
      exitOf g s = case exits g of
        Path (Just st) -> M.findWithDefault M.empty s st
        Path Nothing -> M.empty
      post = M.fromList [(s, M.fromList [(Empty, exitOf fromEmpty s), (Full, exitOf fromFull s)]) | s <- deques]
      -- | A put that is wrong whatever the caller does is reported in the
      -- method itself. The findings are kept newest first, so the first one
      -- of each deque is the one that stays.
      own = S.fromList (map fst (findings fromEmpty))
      unsafe = M.fromList
        [ (s, loc) | (s, AnnotatedError _ loc) <- findings fromFull, not (S.member s own) ]
  in (Summary post unsafe, findings fromEmpty)

-- | The deques that the members of a class take and put.
dequeOperations :: [ClassMember SemanticAnn] -> (S.Set Identifier, S.Set Identifier)
dequeOperations members = foldr addBlock (S.empty, S.empty) (concatMap memberBlocks members)

  where

    memberBlocks = \case
      ClassMethod _ _ _ _ body _ -> allBlocks body
      ClassProcedure _ _ _ body _ -> allBlocks body
      ClassAction _ _ _ _ body _ -> allBlocks body
      _ -> []

    allBlocks (Block bbs _) = concatMap nested bbs

    nested bb = bb : case bb of
      IfElseBlock condIf elseIfs mElse _ ->
        allBlocks (condIfBody condIf) ++ concatMap (allBlocks . condElseIfBody) elseIfs
          ++ maybe [] (allBlocks . condElseBody) mElse
      ForLoopBlock _ _ _ _ _ body _ -> allBlocks body
      MatchBlock _ cases mDefault _ ->
        concatMap (allBlocks . matchBody) cases ++ maybe [] (\(DefaultCase b _) -> allBlocks b) mDefault
      _ -> []

    addBlock bb (taken, put) = case bb of
      DequeTake obj _ _ -> (maybe taken (`S.insert` taken) (dequeOf obj), put)
      DequePut _ obj _ _ -> (taken, maybe put (`S.insert` put) (dequeOf obj))
      DequeExchange _ obj _ _ _ -> (taken, maybe put (`S.insert` put) (dequeOf obj))
      _ -> (taken, put)

checkClass :: [ClassMember SemanticAnn] -> [DequeUsageError]
checkClass members
  | null dequeFields = []
  | otherwise = fieldFindings ++ go M.empty members

  where

    dequeFields = [(fid, getLocation ann) | ClassField (FieldDefinition fid (TAccessPort (TDequeAccess _)) ann) <- members]
    deques = map fst dequeFields
    (taken, put) = dequeOperations members

    fieldFindings = concat
      [ [annotateError loc (EDequeNeverTaken s) | S.member s put, not (S.member s taken)]
          ++ [annotateError loc (EDequeNeverFilled s) | not (S.member s put)]
      | (s, loc) <- dequeFields ]

    go _ [] = []
    go summaries (member : rest) = case member of
      ClassMethod _ m _ _ body _ ->
        let (summary, found) = summarize summaries m deques body in
        map snd found ++ go (M.insert m summary summaries) rest
      ClassProcedure _ p _ body _ ->
        map snd (findings (walkBody summaries (unknownEntry p deques) body)) ++ go summaries rest
      ClassAction _ a _ _ body _ ->
        map snd (findings (walkBody summaries (unknownEntry a deques) body)) ++ go summaries rest
      _ -> go summaries rest

checkElement :: AnnASTElement SemanticAnn -> [DequeUsageError]
checkElement (TypeDefinition (Class _ _ members _ _) _) = checkClass members
checkElement _ = []

-- | Runs the check over a whole module, returning the first error in the order
-- of the source.
runDequeUsageCheck :: AnnotatedProgram SemanticAnn -> Maybe DequeUsageError
runDequeUsageCheck program =
  listToMaybe (sortOn (\(AnnotatedError _ loc) -> loc) (concatMap checkElement program))
