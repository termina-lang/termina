-- | Utility AST functions

module Utils.AST where

import           AST

-- Ground Type equality
groundTyEq :: TypeSpecifier -> TypeSpecifier -> Bool
groundTyEq  UInt8  UInt8 = True
groundTyEq  UInt16  UInt16 = True
groundTyEq  UInt32  UInt32 = True
groundTyEq  UInt64  UInt64 = True
groundTyEq  Int8  Int8 = True
groundTyEq  Int16  Int16 = True
groundTyEq  Int32  Int32 = True
groundTyEq  Int64  Int64 = True
groundTyEq  Bool  Bool = True
groundTyEq  Unit Unit = True
groundTyEq  (Option _) (Option Unit) = True
groundTyEq  (Option Unit) (Option _) = True
groundTyEq  (Option tyspecl) (Option tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (Reference tyspecl) (Reference tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (DynamicSubtype tyspecl) (DynamicSubtype tyspecr) = groundTyEq tyspecl tyspecr
-- TODO: These are considered complex types and should be handled differently
groundTyEq  (Vector typespecl sizeel) (Vector typespecr sizer) = groundTyEq typespecl typespecr && constExprEq sizeel sizer
groundTyEq  (DefinedType idl) (DefinedType idr) = idl == idr
--
groundTyEq  _ _ = False

constExprEq :: ConstExpression -> ConstExpression -> Bool
constExprEq (KC ((I tyspecl intl))) (KC ((I tyspecr intr))) = groundTyEq tyspecl tyspecr && intl == intr
constExprEq (KC (B vall)) (KC (B valr)) = vall == valr
constExprEq (KC (C charl)) (KC (C charr)) = charl == charr
constExprEq _ _ = False


----------------------------------------
-- The following function defines a traversal..
-- I am not going to abstract the traverse parttern right now, but we should do
-- it next time we need it.

-- What dependencies we are interested right now
data ClassDep
  -- Variables
  = SVar Identifier
  -- Method accesses
  | MethodAccess ClassDep Identifier
  -- Field accesses
  | FieldAccess ClassDep Identifier
  -- We mark as undec objects defined through expressions.
  -- | Undec
  -- see README/Q22

depToList :: ClassDep -> (Identifier, [Identifier])
depToList (SVar i)           = (i, [])
depToList (MethodAccess a i) = (\(r,as) -> (r, i : as)) $ depToList a
depToList (FieldAccess a i)  = (\(r,as) -> (r, i : as)) $ depToList a
-- depToList Undec              = Nothing

getRoot :: ClassDep -> Identifier
getRoot = fst . depToList

-- |getDepObj| is where we compute the use of names and dependencies
-- The rest is just how we traverse the AST.
-- Here we collect two things:
-- the name of the object the argument is defining
-- and the extra dependencies we may need.
getDepObj :: Object a -> (ClassDep, [ ClassDep ])
getDepObj = getDepObj'
  where
    getDepObj' (Variable ident _ann) = (SVar ident, [])
    -----------
    -- TODO Q22
    -- getDepObj' (IdentifierExpression e _ann) = (Undec, getDepExp e)
    -----------
    getDepObj' (VectorIndexExpression obj eix _ann)
      = let (dnm, deps) = getDepObj' obj in (dnm, deps ++ getDepExp eix)
    getDepObj' (MemberAccess obj ident _ann)
      = let (dnm,deps) = getDepObj' obj
      in (FieldAccess dnm ident , deps)
    getDepObj' (Dereference obj _ann ) = getDepObj' obj
    getDepObj' (ParensObject obj _ann) = getDepObj' obj
    -- getDepObj' (MemberMethodAccess obj ident es _ann)
    --   = let (dnm, deps) = getDepObj' obj
    --   in (MethodAccess dnm ident, deps ++ concatMap getDepExp es)

getDepBlock :: Block a -> [ClassDep]
getDepBlock = concatMap getDepStmt

getDepStmt :: Statement a -> [ClassDep]
getDepStmt (Declaration _ _ e _)
  = getDepExp e
getDepStmt (AssignmentStmt _ e _)
  = getDepExp e
getDepStmt (IfElseStmt ec tB eB fB _)
  = getDepExp ec
  ++ concatMap getDepStmt tB
  ++ concatMap
      (\eC -> getDepExp (elseIfCond eC) ++ concatMap getDepStmt (elseIfBody eC) )
      eB
  ++ concatMap getDepStmt fB
getDepStmt (ForLoopStmt _ initE lastE breakE body _)
  = concatMap getDepExp [initE, lastE]
  ++ maybe [] getDepExp breakE
  ++ concatMap getDepStmt body
getDepStmt (MatchStmt e mBody _)
  = getDepExp e
  ++ concatMap (concatMap getDepStmt . matchBody) mBody
getDepStmt (SingleExpStmt e _)
  = getDepExp e


getDepExp :: Expression a -> [ClassDep]
getDepExp (AccessObject obj) =
  let (dnm, deps) = getDepObj obj in (dnm : deps)
getDepExp (Constant {}) = []
getDepExp ( ParensExpression e _ ) = getDepExp e
getDepExp ( BinOp _op le re _ann ) = getDepExp le ++ getDepExp re
getDepExp ( ReferenceExpression obj _ann ) =
  let (dnm, deps) = getDepObj obj in (dnm : deps)
getDepExp ( Casting e _ty _ann ) = getDepExp e
getDepExp ( FunctionExpression _id args _ann) = concatMap getDepExp args
getDepExp ( VectorInitExpression iE _const _ann ) = getDepExp iE
getDepExp ( FieldValuesAssignmentsExpression _id fs _ann )
  = concatMap (getDepExp . fieldAssigExpression) fs
getDepExp (EnumVariantExpression _i1 _i2 es _ann) = concatMap getDepExp es
getDepExp (OptionVariantExpression os _ann)
  =  case os of
       None   -> []
       Some e -> getDepExp e
getDepExp (MemberMethodAccess obj ident es _ann) =
  let (dnm, deps) = getDepObj obj in MethodAccess dnm ident : deps ++ concatMap getDepExp es
