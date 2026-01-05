{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Semantic.Utils where
    
import Core.AST
import Parser.AST
import qualified Data.Map.Strict as M
import Extras.Graph
import qualified Data.Set as S
import qualified Semantic.AST as SAST

-- Helper to detect invocations to 'self'
objIsSelf :: Object a -> Bool
objIsSelf (Variable ident _ann ) = ident == "self"
objIsSelf (Dereference (Variable ident _ann) _ann') = ident == "self"
objIsSelf _ = False

isPort :: Object a -> Maybe Identifier
isPort (MemberAccess obj mident _ann') =
  if objIsSelf obj then Just mident else Nothing
isPort (DereferenceMemberAccess obj mident _ann') =
  if objIsSelf obj then Just mident else Nothing
isPort _ = Nothing

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

instance GraphKey Identifier (SelfDep a) where
  graphKey (SelfDep ident _) = ident

type SelfDepMap a = M.Map Identifier (SelfInvocation a)

selfInv :: Expression a -> SelfInvocation a -> SelfInvocation a
selfInv expr@(MemberFunctionCall obj mident _args _ann) prevMap =
  if objIsSelf obj then M.insert mident expr prevMap else prevMap
selfInv expr@(DerefMemberFunctionCall obj mident _args _ann) prevMap =
  if objIsSelf obj then M.insert mident expr prevMap else prevMap
selfInv (BinOp _ left right _ann) prevMap =
  (selfInv right . selfInv left) prevMap
selfInv (Casting expr _ts _ann) prevMap = selfInv expr prevMap
selfInv _ prevMap  = prevMap

selfInvStmt :: Statement a -> SelfInvocation a -> SelfInvocation a
selfInvStmt (Declaration _vident _accK _type e _ann) prevMap = selfInv e prevMap
selfInvStmt (AssignmentStmt _obj e _ann) prevMap = selfInv e prevMap
selfInvStmt (IfElseStmt ifCond elifs mElse _ann) prevMap =
  selfInvIf ifCond .
  flip (foldr selfInvElseIf) elifs $ 
    maybe prevMap (\(CondElse bEl _) -> foldr selfInvStmt prevMap . blockBody $ bEl) mElse

  where

    selfInvIf :: CondIf a -> SelfInvocation a -> SelfInvocation a
    selfInvIf (CondIf eC (Block bIfStmts _) _) =
      selfInv eC .
      flip (foldr selfInvStmt) bIfStmts

    selfInvElseIf :: CondElseIf a -> SelfInvocation a -> SelfInvocation a
    selfInvElseIf (CondElseIf cond blk _) prevMap' =
      selfInv cond $ foldr selfInvStmt prevMap' (blockBody blk)

selfInvStmt (ForLoopStmt _loopIdent _type _initV _endV cBreak (Block stmts _) _ann) prevMap =
  (\prevMap' -> maybe prevMap' (`selfInv` prevMap') cBreak) $
    foldr selfInvStmt prevMap stmts
selfInvStmt (MatchStmt e mcases mDefaultCase _ann) prevMap =
  let casesSelfInv = selfInv e $ foldr selfInvCase prevMap mcases in
  case mDefaultCase of
    Just (DefaultCase blk _) -> 
      foldr selfInvStmt casesSelfInv (blockBody blk)
    Nothing -> casesSelfInv

  where

    selfInvCase :: MatchCase a -> SelfInvocation a -> SelfInvocation a
    selfInvCase (MatchCase _ _ body _) =
      flip (foldr selfInvStmt) . blockBody $ body

selfInvStmt (SingleExpStmt e _ann) prevMap = selfInv e prevMap
selfInvStmt (ReturnStmt ret _ann) prevMap = 
  maybe prevMap (`selfInv` prevMap) ret
selfInvStmt (ContinueStmt ret _ann) prevMap = 
  selfInv ret prevMap
selfInvStmt (RebootStmt _ann) prevMap = prevMap

fieldDepStmt :: Statement a -> M.Map Identifier (S.Set Identifier) -> M.Map Identifier (S.Set Identifier)
fieldDepStmt (Declaration _vident _accK _type e _ann) prevMap = fieldDepExpr e prevMap
fieldDepStmt (AssignmentStmt _obj e _ann) prevMap = fieldDepExpr e prevMap
fieldDepStmt (IfElseStmt ifCond elifs mElse _ann) prevMap =
  fieldDepIf ifCond .
  flip (foldr fieldDepElseIf) elifs $ 
    maybe prevMap (\(CondElse bEl _) -> foldr fieldDepStmt prevMap . blockBody $ bEl) mElse

  where

    fieldDepIf :: CondIf a -> M.Map Identifier (S.Set Identifier) -> M.Map Identifier (S.Set Identifier)
    fieldDepIf (CondIf eC (Block bIfStmts _) _) =
      fieldDepExpr eC .
      flip (foldr fieldDepStmt) bIfStmts

    fieldDepElseIf :: CondElseIf a -> M.Map Identifier (S.Set Identifier) -> M.Map Identifier (S.Set Identifier)
    fieldDepElseIf (CondElseIf cond blk _) prevMap' =
      fieldDepExpr cond $ foldr fieldDepStmt prevMap' (blockBody blk)

fieldDepStmt (ForLoopStmt _loopIdent _type _initV _endV cBreak (Block stmts _) _ann) prevMap =
  (\prevMap' -> maybe prevMap' (`fieldDepExpr` prevMap') cBreak) $
    foldr fieldDepStmt prevMap stmts
fieldDepStmt (MatchStmt e mcases mDefaultCase _ann) prevMap =
  let casesFieldDep = fieldDepExpr e $ foldr fieldDepCase prevMap mcases in
  case mDefaultCase of
    Just (DefaultCase blk _) -> 
      foldr fieldDepStmt casesFieldDep (blockBody blk)
    Nothing -> casesFieldDep

  where

    fieldDepCase :: MatchCase a -> M.Map Identifier (S.Set Identifier) -> M.Map Identifier (S.Set Identifier)
    fieldDepCase (MatchCase _ _ body _) =
      flip (foldr fieldDepStmt) . blockBody $ body

fieldDepStmt (SingleExpStmt e _ann) prevMap = fieldDepExpr e prevMap
fieldDepStmt (ReturnStmt ret _ann) prevMap =
  maybe prevMap (`fieldDepExpr` prevMap) ret
fieldDepStmt (ContinueStmt ret _ann) prevMap = 
  fieldDepExpr ret prevMap
fieldDepStmt (RebootStmt _ann) prevMap = prevMap

fieldDepExpr :: Expression a -> M.Map Identifier (S.Set Identifier) -> M.Map Identifier (S.Set Identifier)
fieldDepExpr (MemberFunctionCall obj mident _args _ann) prevMap =
  case isPort obj of
    Just portId -> case M.lookup portId prevMap of
      Just prevSet -> M.insert portId (S.insert mident prevSet) prevMap
      Nothing -> M.insert portId (S.singleton mident) prevMap
    Nothing -> prevMap
fieldDepExpr (DerefMemberFunctionCall obj mident _args _ann) prevMap =
  case isPort obj of
    Just portId -> case M.lookup portId prevMap of
      Just prevSet -> M.insert portId (S.insert mident prevSet) prevMap
      Nothing -> M.insert portId (S.singleton mident) prevMap
    Nothing -> prevMap
fieldDepExpr (BinOp _ left right _ann) prevMap =
  (fieldDepExpr right . fieldDepExpr left) prevMap
fieldDepExpr (Casting expr _ts _ann) prevMap = fieldDepExpr expr prevMap
fieldDepExpr _ prevMap  = prevMap

selfInvBlock :: Block a -> SelfInvocation a -> SelfInvocation a
selfInvBlock = flip (foldr selfInvStmt) . blockBody

fieldDepBlock :: Block a 
  -> M.Map Identifier (S.Set Identifier) 
  -> M.Map Identifier (S.Set Identifier)
fieldDepBlock = flip (foldr fieldDepStmt) . blockBody

selfDepClass
  :: ClassMember a
  -> SelfDepMap a -> SelfDepMap a
selfDepClass (ClassField {}) = id
selfDepClass (ClassMethod _ak mId _params _type bRet _ann) =
  M.insert mId (selfInvBlock bRet M.empty)
selfDepClass (ClassProcedure _ak pId _params bRet _ann) =
  M.insert pId (selfInvBlock bRet M.empty)
selfDepClass (ClassViewer vId _params _type bRet _ann) =
  M.insert vId (selfInvBlock bRet M.empty)
selfDepClass (ClassAction _ak aId _param _type bRet _ann) =
  M.insert aId (selfInvBlock bRet M.empty)

fieldDepClass :: ClassMember a 
  -> M.Map Identifier (S.Set Identifier) 
  -> M.Map Identifier (S.Set Identifier)
fieldDepClass (ClassField {}) = id
fieldDepClass (ClassMethod _ak _mId _params _type bRet _ann) = 
  M.unionWith S.union (fieldDepBlock bRet M.empty)
fieldDepClass (ClassProcedure _ak _pId _params bRet _ann) = 
  M.unionWith S.union (fieldDepBlock bRet M.empty)
fieldDepClass (ClassViewer _vId _params _type bRet _ann) =
  M.unionWith S.union (fieldDepBlock bRet M.empty)
fieldDepClass (ClassAction _ak _aId _param _type bRet _ann) =
  M.unionWith S.union (fieldDepBlock bRet M.empty)

(<::>) :: Identifier -> Identifier -> Identifier
(<::>) i1 i2 = i1 ++ "__" ++ i2

getMovedHash :: SAST.Object a -> Maybe Identifier -> Identifier
getMovedHash (SAST.Variable ident _ann) prev =
  maybe ident (ident <::>) prev
getMovedHash (SAST.Dereference obj _ann) prev = 
  getMovedHash obj prev
getMovedHash (SAST.DereferenceMemberAccess obj mident _ann) prev =
  getMovedHash obj $ (mident <::>) <$> prev
getMovedHash (SAST.MemberAccess obj mident _ann) prev =
  getMovedHash obj $ (mident <::>) <$> prev
getMovedHash (SAST.ArrayIndexExpression obj _expr _ann) _prev =
  getMovedHash obj Nothing
getMovedHash (SAST.Unbox obj _ann) _prev =
  getMovedHash obj Nothing
