{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Generator.CCCodeGen.Utils where


import Generator.CCCodeGen.Common
import Generator.LanguageC.CompCertC
import Utils.Annotations

class TypeElement a b where
          (@:) :: a -> CType -> b

instance (TypeElement Ident CObject) where
    (@:) = CVar

instance TypeElement Ident CExpression where
    (@:) ident cType =
        let cAnn = internalAnn CGenericAnn in
        CExprValOf (CVar ident cType) cType cAnn

class AccessField a b where
    (@.) :: a -> Ident -> b

data ObjField =
    ObjField CObject Ident
    deriving Show

instance AccessField CObject ObjField where
    (@.) = ObjField

instance TypeElement ObjField CObject where
    (@:) (ObjField obj field) = CField obj field

addrOf :: CObject -> CExpression
addrOf obj =
    let cAnn = internalAnn CGenericAnn
        cPtrType = CTPointer (getCObjType obj) noqual
    in
    CExprAddrOf obj cPtrType cAnn

class Cast a where
    cast :: CType -> a -> a

instance Cast CExpression where
    cast cType expr =
        let cAnn = internalAnn CGenericAnn in
        CExprCast expr cType cAnn

instance Cast CObject where
    cast cType obj =
        let cAnn = internalAnn CGenericAnn in
        CObjCast obj cType cAnn

void, void_ptr :: CType
void = CTVoid noqual
void_ptr = CTPointer (CTVoid noqual) noqual

size_t :: CType
size_t = CTSizeT noqual

typeDef :: Ident -> CType
typeDef ident = CTTypeDef ident noqual

class Pointer a where
    ptr :: a -> CType

instance Pointer CType where
    ptr cType = CTPointer cType noqual

instance Pointer Ident where
    ptr ident = CTPointer (CTTypeDef ident noqual) noqual

class ConstType a where
    _const :: a -> CType

instance ConstType CType where
    _const array@(CTArray {}) = array
    _const (CTVoid qual) = CTVoid qual{qual_const = True}
    _const (CTChar qual) = CTChar qual{qual_const = True}
    _const (CTInt size sign qual) = CTInt size sign qual{qual_const = True}
    _const (CTPointer cType qual) = CTPointer cType qual{qual_const = True}
    _const (CTStruct tag ident qual) = CTStruct tag ident qual{qual_const = True}
    _const func@(CTFunction {}) = func
    _const (CTEnum ident qual) = CTEnum ident qual{qual_const = True}
    _const (CTSizeT qual) = CTSizeT qual{qual_const = True}
    _const (CTBool qual) = CTBool qual{qual_const = True}
    _const (CTTypeDef ident qual) = CTTypeDef ident qual{qual_const = True}

instance ConstType Ident where
    _const ident = CTTypeDef ident constqual

uint8_t, uint16_t, uint32_t, uint64_t :: CType
uint8_t = CTInt IntSize8 Unsigned noqual
uint16_t = CTInt IntSize16 Unsigned noqual
uint32_t = CTInt IntSize32 Unsigned noqual
uint64_t = CTInt IntSize64 Unsigned noqual

int8_t, int16_t, int32_t, int64_t :: CType
int8_t = CTInt IntSize8 Signed noqual
int16_t = CTInt IntSize16 Signed noqual
int32_t = CTInt IntSize32 Signed noqual
int64_t = CTInt IntSize64 Signed noqual

class Assignment a b where
    (@=) :: a -> CExpression -> b

instance Assignment CObject CExpression where
    (@=) obj expr =
        let cAnn = internalAnn CGenericAnn
            cObjType = getCObjType obj in
        CExprAssign obj expr cObjType cAnn

(@@) :: CExpression -> [CExpression] -> CExpression
(@@) func params =
    let cAnn = internalAnn CGenericAnn
        funcRetType = case getCExprType func of
            CTFunction retType _ -> retType
            _ -> error "Function type expected"
    in
    CExprCall func params funcRetType cAnn

newtype Decimal = Decimal Integer
    deriving Show

dec :: Integer -> Decimal
dec = Decimal

instance TypeElement Decimal CExpression where
    (@:) (Decimal value) cType =
        let cAnn = internalAnn CGenericAnn in
        CExprConstant (CIntConst (CInteger value CDecRepr)) cType cAnn

(@!=) :: CExpression -> CExpression -> CExpression
(@!=) l r =
    CExprBinaryOp COpNe l r (CTBool noqual) (internalAnn CGenericAnn)

infix 1 @=
infix 7 @!=

class Alignment a b where
    pre_cr :: a -> b
    post_cr :: a -> b
    no_cr :: a -> b

instance Alignment CStatement CCompoundBlockItem where
    pre_cr CSSkip = CBlockStmt CSSkip
    pre_cr (CSCase expr stmt (Located (CStatementAnn _ post) loc)) =
        CBlockStmt $ CSCase expr stmt (Located (CStatementAnn True post) loc)
    pre_cr (CSDefault stmt (Located (CStatementAnn _ post) loc)) =
        CBlockStmt $ CSDefault stmt (Located (CStatementAnn True post) loc)
    pre_cr (CSDo expr (Located (CStatementAnn _ post) loc)) =
        CBlockStmt $ CSDo expr (Located (CStatementAnn True post) loc)
    pre_cr (CSCompound stmts (Located (CStatementAnn _ post) loc)) =
        CBlockStmt $ CSCompound stmts (Located (CStatementAnn True post) loc)
    pre_cr (CSIfThenElse expr stmt maybeStmt (Located (CStatementAnn _ post) loc)) =
        CBlockStmt $ CSIfThenElse expr stmt maybeStmt (Located (CStatementAnn True post) loc)
    pre_cr (CSFor expr1 expr2 expr3 stmt (Located (CStatementAnn _ post) loc)) =
        CBlockStmt $ CSFor expr1 expr2 expr3 stmt (Located (CStatementAnn True post) loc)
    pre_cr (CSReturn maybeExpr (Located (CStatementAnn _ post) loc)) =
        CBlockStmt $ CSReturn maybeExpr (Located (CStatementAnn True post) loc)
    pre_cr (CSSwitch expr stmt (Located (CStatementAnn _ post) loc)) =
        CBlockStmt $ CSSwitch expr stmt (Located (CStatementAnn True post) loc)
    pre_cr (CSBreak (Located (CStatementAnn _ post) loc)) =
        CBlockStmt $ CSBreak (Located (CStatementAnn True post) loc)
    pre_cr stmt = error $ "pre_cr: invalid annotation: " ++ show stmt

    post_cr CSSkip = CBlockStmt CSSkip
    post_cr (CSCase expr stmt (Located (CStatementAnn pre _) loc)) =
        CBlockStmt $ CSCase expr stmt (Located (CStatementAnn pre True) loc)
    post_cr (CSDefault stmt (Located (CStatementAnn pre _) loc)) =
        CBlockStmt $ CSDefault stmt (Located (CStatementAnn pre True) loc)
    post_cr (CSDo expr (Located (CStatementAnn pre _) loc)) =
        CBlockStmt $ CSDo expr (Located (CStatementAnn pre True) loc)
    post_cr (CSCompound stmts (Located (CStatementAnn pre _) loc)) =
        CBlockStmt $ CSCompound stmts (Located (CStatementAnn pre True) loc)
    post_cr (CSIfThenElse expr stmt maybeStmt (Located (CStatementAnn pre _) loc)) =
        CBlockStmt $ CSIfThenElse expr stmt maybeStmt (Located (CStatementAnn pre True) loc)
    post_cr (CSFor expr1 expr2 expr3 stmt (Located (CStatementAnn pre _) loc)) =
        CBlockStmt $ CSFor expr1 expr2 expr3 stmt (Located (CStatementAnn pre True) loc)
    post_cr (CSReturn maybeExpr (Located (CStatementAnn pre _) loc)) =
        CBlockStmt $ CSReturn maybeExpr (Located (CStatementAnn pre True) loc)
    post_cr (CSSwitch expr stmt (Located (CStatementAnn pre _) loc)) =
        CBlockStmt $ CSSwitch expr stmt (Located (CStatementAnn pre True) loc)
    post_cr (CSBreak (Located (CStatementAnn pre _) loc)) =
        CBlockStmt $ CSBreak (Located (CStatementAnn pre True) loc)
    post_cr stmt = error $ "pre_cr: invalid annotation: " ++ show stmt

    no_cr CSSkip = CBlockStmt CSSkip
    no_cr (CSCase expr stmt (Located _ loc)) =
        CBlockStmt $ CSCase expr stmt (Located (CStatementAnn False False) loc)
    no_cr (CSDefault stmt (Located _ loc)) =
        CBlockStmt $ CSDefault stmt (Located (CStatementAnn False False) loc)
    no_cr (CSDo expr (Located _ loc)) =
        CBlockStmt $ CSDo expr (Located (CStatementAnn False False) loc)
    no_cr (CSCompound stmts (Located _ loc)) =
        CBlockStmt $ CSCompound stmts (Located (CStatementAnn False False) loc)
    no_cr (CSIfThenElse expr stmt maybeStmt (Located _ loc)) =
        CBlockStmt $ CSIfThenElse expr stmt maybeStmt (Located (CStatementAnn False False) loc)
    no_cr (CSFor expr1 expr2 expr3 stmt (Located _ loc)) =
        CBlockStmt $ CSFor expr1 expr2 expr3 stmt (Located (CStatementAnn False False) loc)
    no_cr (CSReturn maybeExpr (Located _ loc)) =
        CBlockStmt $ CSReturn maybeExpr (Located (CStatementAnn False False) loc)
    no_cr (CSSwitch expr stmt (Located _ loc)) =
        CBlockStmt $ CSSwitch expr stmt (Located (CStatementAnn False False) loc)
    no_cr (CSBreak (Located _ loc)) =
        CBlockStmt $ CSBreak (Located (CStatementAnn False False) loc)

instance Alignment CExpression CCompoundBlockItem where
    pre_cr expr = CBlockStmt $ CSDo expr (internalAnn (CStatementAnn True False))
    post_cr expr = CBlockStmt $ CSDo expr (internalAnn (CStatementAnn False True))
    no_cr expr = CBlockStmt $ CSDo expr (internalAnn (CStatementAnn False False))

instance Alignment CExpression CStatement where
    pre_cr expr = CSDo expr (internalAnn (CStatementAnn True False))
    post_cr expr = CSDo expr (internalAnn (CStatementAnn False True))
    no_cr expr = CSDo expr (internalAnn (CStatementAnn False False))

_if :: CExpression -> CStatement -> CStatement
_if expr stmt1 =
    let cAnn = internalAnn (CStatementAnn False False) in
    CSIfThenElse expr stmt1 Nothing cAnn

_if_else :: CExpression -> CStatement -> CStatement -> CStatement
_if_else expr stmt1 stmt2 =
    let cAnn = internalAnn (CStatementAnn False False) in
    CSIfThenElse expr stmt1 (Just stmt2) cAnn

block :: [CCompoundBlockItem] -> CStatement
block items =
    CSCompound items (internalAnn (CCompoundAnn False False))

_break :: CStatement
_break = CSBreak (internalAnn (CStatementAnn False False))

_switch :: CExpression -> CStatement -> CStatement
_switch expr stmt =
    let cAnn = internalAnn (CStatementAnn False False) in
    CSSwitch expr stmt cAnn

_default :: CStatement -> CStatement
_default stmt =
    let cAnn = internalAnn (CStatementAnn False False) in
    CSDefault stmt cAnn

class For a where
    _for :: a -> Maybe CExpression -> Maybe CExpression -> CStatement -> CStatement

instance For (Maybe CExpression) where
    _for expr1 expr2 expr3 stmt =
        let cAnn = internalAnn (CStatementAnn False False) in
        CSFor (Left expr1) expr2 expr3 stmt cAnn

instance For Declaration where
    _for (Declaration ident ts) expr2 expr3 stmt =
        let cAnn = internalAnn (CStatementAnn False False) in
        CSFor (Right (CDecl ts (Just ident) Nothing)) expr2 expr3 stmt cAnn

instance For CDeclaration where
    _for decl expr2 expr3 stmt =
        let cAnn = internalAnn (CStatementAnn False False) in
        CSFor (Right decl) expr2 expr3 stmt cAnn

data Declaration =
    Declaration Ident CTypeSpecifier
    deriving Show

class Decl a where
    var :: Ident -> a -> Declaration

instance Decl CType where
    var ident cType = Declaration ident (CTypeSpec cType)

instance Alignment Declaration CCompoundBlockItem where
    pre_cr (Declaration ident ts) =
        let stmtAnn = internalAnn (CStatementAnn True False) in
        CBlockDecl (CDecl ts (Just ident) Nothing) stmtAnn
    post_cr (Declaration ident ts) =
        let stmtAnn = internalAnn (CStatementAnn False True) in
        CBlockDecl (CDecl ts (Just ident) Nothing) stmtAnn
    no_cr :: Declaration -> CCompoundBlockItem
    no_cr (Declaration ident ts) =
        let stmtAnn = internalAnn (CStatementAnn False False) in
        CBlockDecl (CDecl ts (Just ident) Nothing) stmtAnn
    
instance Assignment Declaration CDeclaration where
    (@=) (Declaration ident ts) expr =
        CDecl ts (Just ident) (Just expr)

instance Alignment CDeclaration CCompoundBlockItem where
    pre_cr decl = CBlockDecl decl (internalAnn (CStatementAnn True False))
    post_cr decl = CBlockDecl decl (internalAnn (CStatementAnn False True))
    no_cr decl = CBlockDecl decl (internalAnn (CStatementAnn False False))
{--


newtype ObjAddr = 
    ObjAddrVar Ident
    | ObjAddrField ObjField
    deriving Show

(@.) :: CObject -> Ident -> ObjField
(@.) = ObjField

instance GenExpression ObjField where
    (@:) (ObjField obj field) cType = 
        let cAnn = internalAnn CGenericAnn in
        CExprValOf (CField obj field cType) cType cAnn

class GenAddrOf a where
    addrOf :: a -> ObjAddr

instance GenAddrOf Ident where
    addrOf = ObjAddr

addrOf :: Ident -> ObjAddr
addrOf = ObjAddr

instance GenExpression ObjAddr where
    (@:) (ObjAddr obj) (CTPointer cType noqual) =
        let cAnn = internalAnn CGenericAnn
            cPtrType = CTPointer cType noqual
        in
        CExprAddrOf (CVar obj  cPtrType cAnn
--}