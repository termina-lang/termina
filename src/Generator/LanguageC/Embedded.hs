{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Generator.LanguageC.Embedded (
    (@:), (@$$), (@.),
    (@=), (@:=), (@@),
    (@&&), (@||), (@!=),
    (@==), (|>>), (@->),
    (@>), (@<), (@>=), (@<=),
    (@+), (@-), (@*), (@/), (@%),
    cast, addrOf, deref, ptr, str,
    void, void_ptr, size_t, typeDef, char,
    uint8_t, uint16_t, uint32_t, uint64_t,
    int8_t, int16_t, int32_t, int64_t,
    dec, indent, trail_cr, pre_cr, no_cr,
    block, var, field, struct, _const,
    function, static_function,
    global, static_global,
    _if, _if_else, _break, _switch, _case, 
    _for, _for_let, _default, _return,
    _sizeOfType, _sizeOfExpr,
    _define, _include, _ifdef, _ifndef, _endif
  ) where


import Generator.CodeGen.Common
import Generator.LanguageC.AST
import Utils.Annotations

class TypeElement a b where
    (@:) :: a -> CType -> b

instance TypeElement Ident CObject where
    (@:) = CVar

instance TypeElement Ident CExpression where
    (@:) ident cType =
        let cAnn = internalAnn CGenericAnn in
        CExprValOf (CVar ident cType) cType cAnn

instance TypeElement CObject CExpression where
    (@:) cObj cType =
        let cAnn = internalAnn CGenericAnn in
        CExprValOf cObj cType cAnn

data ObjIndex =
    ObjIndex CObject CExpression
    deriving Show

(@$$) :: CObject -> CExpression -> ObjIndex
(@$$) = ObjIndex

instance TypeElement ObjIndex CObject where
    (@:) (ObjIndex obj index) = CIndexOf obj index

data BinOp =
    BinOp CExpression CBinaryOp CExpression
    deriving Show

(@+) :: CExpression -> CExpression -> BinOp
(@+) l = BinOp l COpAdd

(@-) :: CExpression -> CExpression -> BinOp
(@-) l = BinOp l COpSub

(@*) :: CExpression -> CExpression -> BinOp
(@*) l = BinOp l COpMul

(@/) :: CExpression -> CExpression -> BinOp
(@/) l = BinOp l COpDiv

(@%) :: CExpression -> CExpression -> BinOp
(@%) l = BinOp l COpMod

instance TypeElement BinOp CExpression where
    (@:) (BinOp l op r) cType =
        let cAnn = internalAnn CGenericAnn in
        CExprBinaryOp op l r cType cAnn

data ObjField =
    ObjField CObject Ident
    deriving Show

(@.) :: CObject -> Ident -> ObjField
(@.) = ObjField

instance TypeElement ObjField CObject where
    (@:) (ObjField obj fld) = CField obj fld

instance TypeElement ObjField CExpression where
    (@:) (ObjField obj fld) cType =
        let cAnn = internalAnn CGenericAnn in
        CExprValOf (CField obj fld cType) cType cAnn

instance TypeElement CInteger CExpression where
    (@:) cInteger cType =
        let cAnn = internalAnn CGenericAnn in
        CExprConstant (CIntConst cInteger) cType cAnn

instance TypeElement Char CExpression where
    (@:) chr cType =
        let cAnn = internalAnn CGenericAnn in
        CExprConstant (CCharConst (CChar chr)) cType cAnn

addrOf :: CObject -> CExpression
addrOf obj =
    let cAnn = internalAnn CGenericAnn
        cPtrType = CTPointer (getCObjType obj) noqual
    in
    CExprAddrOf obj cPtrType cAnn

class Dereference a where
    deref :: CObject -> a

instance Dereference CObject where
    deref cObj =
        case getCObjType cObj of
            CTPointer cType _ -> CDeref cObj cType
            _ -> error "Pointer type expected"

instance Dereference CExpression where
    deref cObj =
        let cAnn = internalAnn CGenericAnn in
        case getCObjType cObj of
            CTPointer cType _ -> CExprValOf (CDeref cObj cType) cType cAnn
            _ -> error "Pointer type expected"

class Cast a where
    cast :: CType -> a -> a

instance Cast CExpression where
    cast cType expr =
        let cAnn = internalAnn CGenericAnn in
        CExprCast expr cType cAnn

instance Cast CObject where
    cast = flip CObjCast

void, void_ptr :: CType
void = CTVoid noqual
void_ptr = CTPointer (CTVoid noqual) noqual

char :: CType
char = CTChar noqual

size_t :: CType
size_t = CTSizeT noqual

typeDef :: Ident -> CType
typeDef ident = CTTypeDef ident noqual

_sizeOfType :: CType -> CExpression
_sizeOfType cType =
    let cAnn = internalAnn CGenericAnn in
    CExprSizeOfType cType size_t cAnn

_sizeOfExpr :: CExpression -> CExpression
_sizeOfExpr expr =
        let cAnn = internalAnn CGenericAnn in
        CExprSizeOfExpr expr size_t cAnn

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

(@&&) :: CExpression -> CExpression -> CExpression
(@&&) lexpr rexpr = 
    let cAnn = internalAnn CGenericAnn in
    CExprSeqAnd lexpr rexpr (CTBool noqual) cAnn

(@||) :: CExpression -> CExpression -> CExpression
(@||) lexpr rexpr = 
    let cAnn = internalAnn CGenericAnn in
    CExprSeqOr lexpr rexpr (CTBool noqual) cAnn

(@=) :: CObject -> CExpression -> CExpression
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

dec :: Integer -> CConstant
dec value = CIntConst (CInteger value CDecRepr)

str :: String -> CConstant
str = CStrConst . CString

instance TypeElement CConstant CExpression where
    (@:) constant cType =
        let cAnn = internalAnn CGenericAnn in
        CExprConstant constant cType cAnn

(@!=) :: CExpression -> CExpression -> CExpression
(@!=) l r =
    CExprBinaryOp COpNe l r (CTBool noqual) (internalAnn CGenericAnn)

(@==) :: CExpression -> CExpression -> CExpression
(@==) l r =
    CExprBinaryOp COpEq l r (CTBool noqual) (internalAnn CGenericAnn)

(@>) :: CExpression -> CExpression -> CExpression
(@>) l r =
    CExprBinaryOp COpGt l r (CTBool noqual) (internalAnn CGenericAnn)

(@<) :: CExpression -> CExpression -> CExpression
(@<) l r =
    CExprBinaryOp COpLt l r (CTBool noqual) (internalAnn CGenericAnn)

(@>=) :: CExpression -> CExpression -> CExpression
(@>=) l r =
    CExprBinaryOp COpGe l r (CTBool noqual) (internalAnn CGenericAnn)

(@<=) :: CExpression -> CExpression -> CExpression
(@<=) l r =
    CExprBinaryOp COpLe l r (CTBool noqual) (internalAnn CGenericAnn)

(|>>) :: (Located a) => a -> Location -> a
(|>>) = updateLocation

infix 1 @=
infix 1 @:=
infix 5 @+
infix 5 @-
infix 6 @*
infix 6 @/
infix 6 @%
infix 7 @!=
infix 7 @==

class Indentation a where
    indent :: a -> a

instance Indentation CCompoundBlockItem where
    indent (CBlockStmt (CSCase expr stmt (LocatedElement (CStatementAnn pre _) loc))) =
        CBlockStmt $ CSCase expr stmt (LocatedElement (CStatementAnn pre True) loc)
    indent (CBlockStmt (CSDefault stmt (LocatedElement (CStatementAnn pre _) loc))) =
        CBlockStmt $ CSDefault stmt (LocatedElement (CStatementAnn pre True) loc)
    indent (CBlockStmt (CSDo expr (LocatedElement (CStatementAnn pre _) loc))) =
        CBlockStmt $ CSDo expr (LocatedElement (CStatementAnn pre True) loc)
    indent (CBlockStmt (CSIfThenElse expr stmt maybeStmt (LocatedElement (CStatementAnn pre _) loc))) =
        CBlockStmt $ CSIfThenElse expr stmt maybeStmt (LocatedElement (CStatementAnn pre True) loc)
    indent (CBlockStmt (CSFor expr1 expr2 expr3 stmt (LocatedElement (CStatementAnn pre _) loc))) =
        CBlockStmt $ CSFor expr1 expr2 expr3 stmt (LocatedElement (CStatementAnn pre True) loc)
    indent (CBlockStmt (CSReturn maybeExpr (LocatedElement (CStatementAnn pre _) loc))) =
        CBlockStmt $ CSReturn maybeExpr (LocatedElement (CStatementAnn pre True) loc)
    indent (CBlockStmt (CSSwitch expr stmt (LocatedElement (CStatementAnn pre _) loc))) =
        CBlockStmt $ CSSwitch expr stmt (LocatedElement (CStatementAnn pre True) loc)
    indent (CBlockStmt (CSBreak (LocatedElement (CStatementAnn pre _) loc))) =
        CBlockStmt $ CSBreak (LocatedElement (CStatementAnn pre True) loc)
    indent stmt = stmt

instance Indentation CStatement where
    indent (CSCase expr stmt (LocatedElement (CStatementAnn pre _) loc)) =
        CSCase expr stmt (LocatedElement (CStatementAnn pre True) loc)
    indent (CSDefault stmt (LocatedElement (CStatementAnn pre _) loc)) =
        CSDefault stmt (LocatedElement (CStatementAnn pre True) loc)
    indent (CSDo expr (LocatedElement (CStatementAnn pre _) loc)) =
        CSDo expr (LocatedElement (CStatementAnn pre True) loc)
    indent (CSIfThenElse expr stmt maybeStmt (LocatedElement (CStatementAnn pre _) loc)) =
        CSIfThenElse expr stmt maybeStmt (LocatedElement (CStatementAnn pre True) loc)
    indent (CSFor expr1 expr2 expr3 stmt (LocatedElement (CStatementAnn pre _) loc)) =
        CSFor expr1 expr2 expr3 stmt (LocatedElement (CStatementAnn pre True) loc)
    indent (CSReturn maybeExpr (LocatedElement (CStatementAnn pre _) loc)) =
        CSReturn maybeExpr (LocatedElement (CStatementAnn pre True) loc)
    indent (CSSwitch expr stmt (LocatedElement (CStatementAnn pre _) loc)) =
        CSSwitch expr stmt (LocatedElement (CStatementAnn pre True) loc)
    indent (CSBreak (LocatedElement (CStatementAnn pre _) loc)) =
        CSBreak (LocatedElement (CStatementAnn pre True) loc)
    indent stmt = stmt

class Trailing a where
    trail_cr :: a -> a

instance Trailing CCompoundBlockItem where
    trail_cr (CBlockStmt (CSCompound stmts (LocatedElement (CCompoundAnn pre _) loc))) =
        CBlockStmt $ CSCompound stmts (LocatedElement (CCompoundAnn pre True) loc)
    trail_cr stmt = error $ "Invalid statement for trailing CR: " ++ show stmt

instance Trailing CStatement where
    trail_cr (CSCompound stmts (LocatedElement (CCompoundAnn pre _) loc)) =
        CSCompound stmts (LocatedElement (CCompoundAnn pre True) loc)
    trail_cr stmt = error $ "Invalid statement for trailing CR: " ++ show stmt

class Alignment a b where
    pre_cr :: a -> b
    no_cr :: a -> b

instance Alignment CStatement CCompoundBlockItem where
    pre_cr CSSkip = CBlockStmt CSSkip
    pre_cr (CSCase expr stmt (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSCase expr stmt (LocatedElement (CStatementAnn True ind) loc)
    pre_cr (CSDefault stmt (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSDefault stmt (LocatedElement (CStatementAnn True ind) loc)
    pre_cr (CSDo expr (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSDo expr (LocatedElement (CStatementAnn True ind) loc)
    pre_cr (CSCompound stmts (LocatedElement (CCompoundAnn _ ind) loc)) =
        CBlockStmt $ CSCompound stmts (LocatedElement (CCompoundAnn True ind) loc)
    pre_cr (CSIfThenElse expr stmt maybeStmt (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSIfThenElse expr stmt maybeStmt (LocatedElement (CStatementAnn True ind) loc)
    pre_cr (CSFor expr1 expr2 expr3 stmt (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSFor expr1 expr2 expr3 stmt (LocatedElement (CStatementAnn True ind) loc)
    pre_cr (CSReturn maybeExpr (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSReturn maybeExpr (LocatedElement (CStatementAnn True ind) loc)
    pre_cr (CSSwitch expr stmt (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSSwitch expr stmt (LocatedElement (CStatementAnn True ind) loc)
    pre_cr (CSBreak (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSBreak (LocatedElement (CStatementAnn True ind) loc)
    pre_cr stmt = error $ "pre_cr: invalid annotation: " ++ show stmt

    no_cr CSSkip = CBlockStmt CSSkip
    no_cr (CSCase expr stmt (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSCase expr stmt (LocatedElement (CStatementAnn False ind) loc)
    no_cr (CSDefault stmt (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSDefault stmt (LocatedElement (CStatementAnn False ind) loc)
    no_cr (CSDo expr (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSDo expr (LocatedElement (CStatementAnn False ind) loc)
    no_cr (CSCompound stmts (LocatedElement (CCompoundAnn _ ind) loc)) =
        CBlockStmt $ CSCompound stmts (LocatedElement (CCompoundAnn False ind) loc)
    no_cr (CSIfThenElse expr stmt maybeStmt (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSIfThenElse expr stmt maybeStmt (LocatedElement (CStatementAnn False ind) loc)
    no_cr (CSFor expr1 expr2 expr3 stmt (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSFor expr1 expr2 expr3 stmt (LocatedElement (CStatementAnn False ind) loc)
    no_cr (CSReturn maybeExpr (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSReturn maybeExpr (LocatedElement (CStatementAnn False ind) loc)
    no_cr (CSSwitch expr stmt (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSSwitch expr stmt (LocatedElement (CStatementAnn False ind) loc)
    no_cr (CSBreak (LocatedElement (CStatementAnn _ ind) loc)) =
        CBlockStmt $ CSBreak (LocatedElement (CStatementAnn False ind) loc)
    no_cr stmt = error $ "no_cr: invalid annotation: " ++ show stmt

instance Alignment CExpression CCompoundBlockItem where
    pre_cr expr = CBlockStmt $ CSDo expr (internalAnn (CStatementAnn True False))
    no_cr expr = CBlockStmt $ CSDo expr (internalAnn (CStatementAnn False False))

instance Alignment CExpression CStatement where
    pre_cr expr = CSDo expr (internalAnn (CStatementAnn True False))
    no_cr expr = CSDo expr (internalAnn (CStatementAnn False False))

instance Alignment ForLoop CStatement where
    pre_cr (ForLoop expr1 expr2 expr3 stmt) =
        CSFor expr1 expr2 expr3 stmt (internalAnn (CStatementAnn True False))
    no_cr (ForLoop expr1 expr2 expr3 stmt) =
        CSFor expr1 expr2 expr3 stmt (internalAnn (CStatementAnn False False))

instance Alignment ForLoop CCompoundBlockItem where
    pre_cr (ForLoop expr1 expr2 expr3 stmt) =
        CBlockStmt $ CSFor expr1 expr2 expr3 stmt (internalAnn (CStatementAnn True False))
    no_cr (ForLoop expr1 expr2 expr3 stmt) =
        CBlockStmt $ CSFor expr1 expr2 expr3 stmt (internalAnn (CStatementAnn False False))

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

_case :: CExpression -> CStatement -> CStatement
_case expr stmt =
    let cAnn = internalAnn (CStatementAnn False False) in
    CSCase expr stmt cAnn

_default :: CStatement -> CStatement
_default stmt =
    let cAnn = internalAnn (CStatementAnn False False) in
    CSDefault stmt cAnn

_return :: Maybe CExpression -> CStatement
_return expr =
    let cAnn = internalAnn (CStatementAnn False False) in
    CSReturn expr cAnn

data ForLoop =
    ForLoop (Either (Maybe CExpression) CDeclaration)
      (Maybe CExpression)
      (Maybe CExpression)
      CStatement
    deriving Show

_for :: Maybe CExpression -> Maybe CExpression -> Maybe CExpression -> CStatement -> ForLoop
_for expr1 = ForLoop (Left expr1)

class ForDeclaration a where
    _for_let :: a -> CExpression -> CExpression -> CStatement -> ForLoop

instance ForDeclaration Declaration where
    _for_let (Declaration ident ts) expr2 expr3 =
        ForLoop (Right (CDecl ts (Just ident) Nothing)) (Just expr2) (Just expr3) 

instance ForDeclaration CDeclaration where
    _for_let decl expr2 expr3 =
        ForLoop (Right decl) (Just expr2) (Just expr3) 

data Declaration =
    Declaration Ident CTerminaType
    deriving Show

var :: Ident -> CType -> Declaration
var ident cType = Declaration ident (CTypeSpec cType)

field :: Ident -> CType -> CDeclaration
field ident cType = CDecl (CTypeSpec cType) (Just ident) Nothing

instance Alignment Declaration CCompoundBlockItem where
    pre_cr (Declaration ident ts) =
        let declAnn = internalAnn (CDeclarationAnn True) in
        CBlockDecl (CDecl ts (Just ident) Nothing) declAnn
    no_cr (Declaration ident ts) =
        let declAnn = internalAnn (CDeclarationAnn False) in
        CBlockDecl (CDecl ts (Just ident) Nothing) declAnn
    
(@:=) :: Declaration -> CExpression -> CDeclaration
(@:=) (Declaration ident ts) expr =
    CDecl ts (Just ident) (Just expr)

instance Alignment CDeclaration CCompoundBlockItem where
    pre_cr decl = CBlockDecl decl (internalAnn (CDeclarationAnn True))
    no_cr decl = CBlockDecl decl (internalAnn (CDeclarationAnn False))

data FunctionPrototype = 
    FunctionPrototype Ident [Declaration]
    | StaticFunctionPrototype Ident [Declaration]
    deriving Show

data FunctionDeclaration = 
    FunctionDeclaration FunctionPrototype CType
    deriving Show

function, static_function :: Ident -> [Declaration] -> FunctionPrototype
function = FunctionPrototype
static_function = StaticFunctionPrototype

global, static_global :: Ident -> CType -> CFileItem
global ident cType = CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec cType) (Just ident) Nothing)) (internalAnn (CDeclarationAnn False))
static_global ident cType = CExtDecl (CEDVariable (Just CStatic) (CDecl (CTypeSpec cType) (Just ident) Nothing)) (internalAnn (CDeclarationAnn False))

data Function = Function FunctionDeclaration CStatement
    deriving Show

instance TypeElement Ident Declaration where
    (@:) ident cType = Declaration ident (CTypeSpec cType)


(@->) :: FunctionPrototype -> CType -> CStatement -> CFileItem
(@->) (FunctionPrototype ident decls) cType stmt =
    let declStmt = internalAnn (CDeclarationAnn False) in
    CFunctionDef Nothing  
        (CFunction cType ident (map (\(Declaration paramId ts) -> CDecl ts (Just paramId) Nothing) decls) stmt) declStmt
(@->) (StaticFunctionPrototype ident decls) cType stmt =
    let declStmt = internalAnn (CDeclarationAnn False) in
    CFunctionDef (Just CStatic) 
        (CFunction cType ident (map (\(Declaration paramId ts) -> CDecl ts (Just paramId) Nothing) decls) stmt) declStmt

instance Alignment CFileItem CFileItem where
    pre_cr (CFunctionDef storage (CFunction cType ident decls stmt) (LocatedElement _ loc)) = 
        CFunctionDef storage (CFunction cType ident decls stmt) (LocatedElement (CDeclarationAnn True) loc)
    pre_cr (CPPDirective (CPPInclude system path) (LocatedElement _ loc)) =
        CPPDirective (CPPInclude system path) (LocatedElement (CPPDirectiveAnn True) loc)
    pre_cr (CPPDirective (CPPDefine ident maybeValues) (LocatedElement _ loc)) =
        CPPDirective (CPPDefine ident maybeValues) (LocatedElement (CPPDirectiveAnn True) loc)
    pre_cr (CPPDirective (CPPIfDef ident) (LocatedElement _ loc)) =
        CPPDirective (CPPIfDef ident) (LocatedElement (CPPDirectiveAnn True) loc)
    pre_cr (CPPDirective (CPPIfNDef ident) (LocatedElement _ loc)) =
        CPPDirective (CPPIfNDef ident) (LocatedElement (CPPDirectiveAnn True) loc)
    pre_cr (CPPDirective CPPEndif (LocatedElement _ loc)) =
        CPPDirective CPPEndif (LocatedElement (CPPDirectiveAnn True) loc)
    pre_cr (CExtDecl (CEDVariable storage decl) (LocatedElement _ loc)) =
        CExtDecl (CEDVariable storage decl) (LocatedElement (CDeclarationAnn True) loc)
    pre_cr (CExtDecl (CEDFunction cType ident decls) (LocatedElement _ loc)) =
        CExtDecl (CEDFunction cType ident decls) (LocatedElement (CDeclarationAnn True) loc)
    pre_cr (CExtDecl (CEDEnum maybeIdent enum) (LocatedElement _ loc)) =
        CExtDecl (CEDEnum maybeIdent enum) (LocatedElement (CDeclarationAnn True) loc)
    pre_cr (CExtDecl (CEDStructUnion maybeIdent structUnion) (LocatedElement _ loc)) =
        CExtDecl (CEDStructUnion maybeIdent structUnion) (LocatedElement (CDeclarationAnn True) loc)
    pre_cr (CExtDecl (CEDTypeDef ident cType) (LocatedElement _ loc)) =
        CExtDecl (CEDTypeDef ident cType) (LocatedElement (CDeclarationAnn True) loc)

    no_cr (CFunctionDef storage (CFunction cType ident decls stmt) (LocatedElement _ loc)) = 
        CFunctionDef storage (CFunction cType ident decls stmt) (LocatedElement (CDeclarationAnn False) loc)
    no_cr (CPPDirective (CPPInclude system path) (LocatedElement _ loc)) =
        CPPDirective (CPPInclude system path) (LocatedElement (CPPDirectiveAnn False) loc)
    no_cr (CPPDirective (CPPDefine ident maybeValues) (LocatedElement _ loc)) =
        CPPDirective (CPPDefine ident maybeValues) (LocatedElement (CPPDirectiveAnn False) loc)
    no_cr (CPPDirective (CPPIfDef ident) (LocatedElement _ loc)) =
        CPPDirective (CPPIfDef ident) (LocatedElement (CPPDirectiveAnn False) loc)
    no_cr (CPPDirective (CPPIfNDef ident) (LocatedElement _ loc)) =
        CPPDirective (CPPIfNDef ident) (LocatedElement (CPPDirectiveAnn False) loc)
    no_cr (CPPDirective CPPEndif (LocatedElement _ loc)) =
        CPPDirective CPPEndif (LocatedElement (CPPDirectiveAnn False) loc)
    no_cr (CExtDecl (CEDVariable storage decl) (LocatedElement _ loc)) =
        CExtDecl (CEDVariable storage decl) (LocatedElement (CDeclarationAnn False) loc)
    no_cr (CExtDecl (CEDFunction cType ident decls) (LocatedElement _ loc)) =
        CExtDecl (CEDFunction cType ident decls) (LocatedElement (CDeclarationAnn False) loc)
    no_cr (CExtDecl (CEDEnum maybeIdent enum) (LocatedElement _ loc)) =
        CExtDecl (CEDEnum maybeIdent enum) (LocatedElement (CDeclarationAnn False) loc)
    no_cr (CExtDecl (CEDStructUnion maybeIdent structUnion) (LocatedElement _ loc)) =
        CExtDecl (CEDStructUnion maybeIdent structUnion) (LocatedElement (CDeclarationAnn False) loc)
    no_cr (CExtDecl (CEDTypeDef ident cType) (LocatedElement _ loc)) =
        CExtDecl (CEDTypeDef ident cType) (LocatedElement (CDeclarationAnn False) loc)
    
_include :: Bool -> FilePath -> CFileItem
_include system filePath =
    CPPDirective (CPPInclude system filePath) (internalAnn (CPPDirectiveAnn False))

_define :: Ident -> Maybe [String] -> CFileItem
_define ident maybeValues =
    CPPDirective (CPPDefine ident maybeValues) (internalAnn (CPPDirectiveAnn False))

_ifdef :: Ident -> CFileItem
_ifdef ident =
    CPPDirective (CPPIfDef ident) (internalAnn (CPPDirectiveAnn False))

_ifndef :: Ident -> CFileItem
_ifndef ident =
    CPPDirective (CPPIfNDef ident) (internalAnn (CPPDirectiveAnn False))

_endif :: CFileItem
_endif =
    CPPDirective CPPEndif (internalAnn (CPPDirectiveAnn False))

struct :: Ident 
    -> [CDeclaration]  -- member declarations
    -> [CAttribute] -> CFileItem
struct ident flds attrs = CExtDecl (CEDStructUnion (Just ident) (CStruct CStructTag Nothing flds attrs)) (internalAnn (CDeclarationAnn False))
