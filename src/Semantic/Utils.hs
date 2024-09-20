module Semantic.Utils where
    
import Core.AST
import Parser.AST
import qualified Data.Map as M

-- Helper to detect invocations to 'self'
objIsSelf :: Object a -> Bool
objIsSelf (Variable ident _ann ) = ident == "self"
objIsSelf _ = False

----------------------------------------
-- Invocation Dependency in a block of code.
-- Capturing the pattern `self->f()` on objects.
-- As far as I understand it, all objects should have name, and thus, we cannot
-- (or shoudln't) concatenate invocations.

-- We do not have name shadowing, so technically there cannot be two things with
-- the same name. We do not need to insepect 'self->f()', but I am afraid of
-- breaking something else.

type SelfInvocation a = M.Map Identifier (Expression a)
type SelfDepMap a = M.Map Identifier (SelfInvocation a)

selfInv :: (Object a -> Bool) -> Expression a -> SelfInvocation a -> SelfInvocation a
selfInv isSelf expr@(MemberFunctionCall obj mident _args _ann) prevMap =
  if isSelf obj then M.insert mident expr prevMap else prevMap
selfInv isSelf expr@(DerefMemberFunctionCall obj mident _args _ann) prevMap =
  if isSelf obj then M.insert mident expr prevMap else prevMap
selfInv isSelf (BinOp _ left right _ann) prevMap =
  (selfInv isSelf right . selfInv isSelf left) prevMap
selfInv isSelf (Casting expr _ts _ann) prevMap = selfInv isSelf expr prevMap
selfInv _isSelf _ prevMap  = prevMap

selfInvStmt :: (Object a -> Bool) -> Statement a -> SelfInvocation a -> SelfInvocation a
selfInvStmt isSelf (Declaration _vident _accK _type e _ann) = selfInv isSelf e
selfInvStmt isSelf (AssignmentStmt _obj e _ann) = selfInv isSelf e
selfInvStmt isSelf (IfElseStmt eC bIf bEls bEl _ann) =
  selfInv isSelf eC .
  flip (foldr (selfInvStmt isSelf)) bIf .
  flip (foldr (selfInvElseIf isSelf)) bEls . 
  (\prevMap -> maybe prevMap (foldr (selfInvStmt isSelf) prevMap) bEl)

  where

    selfInvElseIf :: (Object a -> Bool) -> ElseIf a -> SelfInvocation a -> SelfInvocation a
    selfInvElseIf isSelf' (ElseIf cond blk _) =
      selfInv isSelf' cond . flip (foldr (selfInvStmt isSelf')) blk

selfInvStmt isSelf (ForLoopStmt _loopIdent _type _initV _endV cBreak body _ann) =
  (\prevMap -> maybe prevMap (flip (selfInv isSelf) prevMap) cBreak) .
  flip (foldr (selfInvStmt isSelf)) body
selfInvStmt isSelf (MatchStmt e mcases _ann) =
  selfInv isSelf e .
  flip (foldr (selfInvCase isSelf)) mcases

  where

    selfInvCase :: (Object a -> Bool) -> MatchCase a -> SelfInvocation a -> SelfInvocation a
    selfInvCase isSelf' (MatchCase _ _ body _) =
      flip (foldr (selfInvStmt isSelf')) body

selfInvStmt isSelf (SingleExpStmt e _ann) = selfInv isSelf e

selfInvBlock :: (Object a -> Bool) -> Block a -> SelfInvocation a -> SelfInvocation a
selfInvBlock isSelf = flip (foldr (selfInvStmt isSelf))

selfInvRetStmt :: (Object a -> Bool) -> ReturnStmt a -> SelfInvocation a -> SelfInvocation a
selfInvRetStmt isSelf (ReturnStmt ret _) prevMap =
  maybe prevMap (flip (selfInv isSelf) prevMap) ret

selfInvBlockRet :: (Object a -> Bool) -> BlockRet a -> SelfInvocation a -> SelfInvocation a
selfInvBlockRet isSelf (BlockRet body bret)
  = flip (foldr (selfInvStmt isSelf)) body . selfInvRetStmt isSelf bret

selfDepClass
  :: (Object a -> Bool)
  -> ClassMember a
  -> SelfDepMap a -> SelfDepMap a
selfDepClass _ (ClassField {}) = id
selfDepClass isSelf (ClassMethod mId _type bRet _ann) =
  M.insert mId (selfInvBlockRet isSelf bRet M.empty)
selfDepClass isSelf (ClassProcedure pId _params bRet _ann) =
  M.insert pId (selfInvBlockRet isSelf bRet M.empty)
selfDepClass isSelf (ClassViewer vId _params _type bRet _ann) =
  M.insert vId (selfInvBlockRet isSelf bRet M.empty)
selfDepClass isSelf (ClassAction aId _param _type bRet _ann) =
  M.insert aId (selfInvBlockRet isSelf bRet M.empty)
