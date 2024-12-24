{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Semantic.Utils where
    
import Core.AST
import Parser.AST
import qualified Data.Map as M
import Extras.TopSort

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

data SelfDep a = SelfDep Identifier a
  deriving Show

instance TopSortKey Identifier (SelfDep a) where
  topSortKey (SelfDep ident _) = ident

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
selfInvStmt isSelf (Declaration _vident _accK _type e _ann) prevMap = selfInv isSelf e prevMap
selfInvStmt isSelf (AssignmentStmt _obj e _ann) prevMap = selfInv isSelf e prevMap
selfInvStmt isSelf (IfElseStmt eC (Block bIfStmts _) bEls bEl _ann) prevMap =
  selfInv isSelf eC .
  flip (foldr (selfInvStmt isSelf)) bIfStmts .
  flip (foldr (selfInvElseIf isSelf)) bEls $ 
    maybe prevMap (foldr (selfInvStmt isSelf) prevMap . blockBody) bEl

  where

    selfInvElseIf :: (Object a -> Bool) -> ElseIf a -> SelfInvocation a -> SelfInvocation a
    selfInvElseIf isSelf' (ElseIf cond blk _) prevMap' =
      selfInv isSelf' cond $ foldr (selfInvStmt isSelf') prevMap' (blockBody blk)

selfInvStmt isSelf (ForLoopStmt _loopIdent _type _initV _endV cBreak (Block stmts _) _ann) prevMap =
  (\prevMap' -> maybe prevMap' (flip (selfInv isSelf) prevMap') cBreak) $
    foldr (selfInvStmt isSelf) prevMap stmts
selfInvStmt isSelf (MatchStmt e mcases _ann) prevMap =
  selfInv isSelf e $ foldr (selfInvCase isSelf) prevMap mcases

  where

    selfInvCase :: (Object a -> Bool) -> MatchCase a -> SelfInvocation a -> SelfInvocation a
    selfInvCase isSelf' (MatchCase _ _ body _) =
      flip (foldr (selfInvStmt isSelf')) . blockBody $ body

selfInvStmt isSelf (SingleExpStmt e _ann) prevMap = selfInv isSelf e prevMap
selfInvStmt isSelf (ReturnStmt ret _ann) prevMap = 
  maybe prevMap (flip (selfInv isSelf) prevMap) ret
selfInvStmt isSelf (ContinueStmt ret _ann) prevMap = 
  selfInv isSelf ret prevMap

selfInvBlock :: (Object a -> Bool) -> Block a -> SelfInvocation a -> SelfInvocation a
selfInvBlock isSelf = flip (foldr (selfInvStmt isSelf)) . blockBody

selfDepClass
  :: (Object a -> Bool)
  -> ClassMember a
  -> SelfDepMap a -> SelfDepMap a
selfDepClass _ (ClassField {}) = id
selfDepClass isSelf (ClassMethod mId _type bRet _ann) =
  M.insert mId (selfInvBlock isSelf bRet M.empty)
selfDepClass isSelf (ClassProcedure pId _params bRet _ann) =
  M.insert pId (selfInvBlock isSelf bRet M.empty)
selfDepClass isSelf (ClassViewer vId _params _type bRet _ann) =
  M.insert vId (selfInvBlock isSelf bRet M.empty)
selfDepClass isSelf (ClassAction aId _param _type bRet _ann) =
  M.insert aId (selfInvBlock isSelf bRet M.empty)
