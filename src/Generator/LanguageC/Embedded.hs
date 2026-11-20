{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Generator.LanguageC.Embedded (
    (@:), (@$$), (@.),
    (@=), (@:=), (@@),
    (@&&), (@||), (@!=),
    (@==), (|>>), (@->),
    (@>), (@<), (@>=), (@<=),
    (@+), (@-), (@*), (@/), (@%), (@&), binOp,
    (@::), (@.:), (@.=),
    cast, addrOf, deref, ptr, 
    void, void_ptr, size_t, typeDef, char,
    uint8_t, uint16_t, uint32_t, uint64_t,
    int8_t, int16_t, int32_t, int64_t,
    dec, indent, trail_cr, pre_cr, no_cr,
    block, var, field, struct, documentedStruct, documentedMember, undocumentedMember,
    documentedEnumerator, undocumentedEnumerator, documented, commented, commentedStatement, _const,
    function, static_function,
    global, static_global, extern,
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

(@&) :: CExpression -> CExpression -> BinOp
(@&) l = BinOp l COpAnd

-- | A binary operation whose operator is only known at run time.
binOp :: CBinaryOp -> CExpression -> CExpression -> BinOp
binOp op l = BinOp l op

instance TypeElement BinOp CExpression where
    (@:) (BinOp l op r) cType =
        let cAnn = internalAnn CGenericAnn in
        CExprBinaryOp op l r cType cAnn

-- | An initializer list { e0, e1, ... } of the given type.
(@::) :: [CExpression] -> CType -> CExpression
(@::) elems cType =
    let cAnn = internalAnn CGenericAnn in
    CExprArrayInitializer elems cType cAnn

-- | A designated initializer list { .f0 = e0, .f1 = e1, ... } of the given
-- type.
(@.:) :: [(Ident, CExpression)] -> CType -> CExpression
(@.:) fields cType =
    let cAnn = internalAnn CGenericAnn in
    CExprDesignatedInitializer fields cType cAnn

-- | A field of a designated initializer list, .f = e.
(@.=) :: Ident -> CExpression -> (Ident, CExpression)
(@.=) = (,)

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

instance TypeElement CFloat CExpression where
    (@:) cFloat cType =
        let cAnn = internalAnn CGenericAnn in
        CExprConstant (CFloatConst cFloat) cType cAnn

instance TypeElement Char CExpression where
    (@:) chr cType =
        let cAnn = internalAnn CGenericAnn in
        CExprConstant (CCharConst (CChar chr)) cType cAnn

instance TypeElement Bool CExpression where
    (@:) b cType =
        let cAnn = internalAnn CGenericAnn in
        CExprConstant (CBoolConst b) cType cAnn

addrOf :: CObject -> CExpression
addrOf obj =
    let cAnn = internalAnn CGenericAnn
        cPtrType = CTPointer (getCObjType obj) noqual
    in
    case obj of
        -- | The address of a dereferenced pointer is the pointer itself, so
        -- &*p is emitted as p.
        CDeref ptrObj _ -> CExprValOf ptrObj cPtrType cAnn
        _ -> CExprAddrOf obj cPtrType cAnn

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
    _const (CTFunctionPointer retTy params qual) =
        CTFunctionPointer retTy params qual{qual_const = True}
    _const (CTEnum ident qual) = CTEnum ident qual{qual_const = True}
    _const (CTSizeT qual) = CTSizeT qual{qual_const = True}
    _const (CTBool qual) = CTBool qual{qual_const = True}
    _const (CTFloat sz qual) = CTFloat sz qual{qual_const = True}
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
infix 1 @.=
infix 1 @:=
infix 5 @+
infix 5 @-
infix 6 @*
infix 6 @/
infix 6 @%
infix 4 @&
infix 7 @!=
infix 7 @==

class Indentation a where
    indent :: a -> a

instance Indentation CCompoundBlockItem where
    indent (CBlockStmt stmt) = CBlockStmt (indent stmt)
    indent item = item

instance Indentation CStatement where
    indent = withStatementAnn $ \case
        CStatementAnn before _ comment -> CStatementAnn before True comment
        other -> other

class Trailing a where
    trail_cr :: a -> a

instance Trailing CCompoundBlockItem where
    trail_cr (CBlockStmt stmt@(CSCompound {})) = CBlockStmt (trail_cr stmt)
    trail_cr stmt = error $ "Invalid statement for trailing CR: " ++ show stmt

instance Trailing CStatement where
    trail_cr stmt@(CSCompound {}) = flip withStatementAnn stmt $ \case
        CCompoundAnn before _ comment -> CCompoundAnn before True comment
        other -> other
    trail_cr stmt = error $ "Invalid statement for trailing CR: " ++ show stmt

class Alignment a b where
    pre_cr :: a -> b
    no_cr :: a -> b

instance Alignment CStatement CCompoundBlockItem where
    pre_cr = CBlockStmt . statementLineBefore True
    no_cr = CBlockStmt . statementLineBefore False

instance Alignment CExpression CCompoundBlockItem where
    pre_cr expr = CBlockStmt $ CSDo expr (internalStatementAnn True)
    no_cr expr = CBlockStmt $ CSDo expr (internalStatementAnn False)

instance Alignment CExpression CStatement where
    pre_cr expr = CSDo expr (internalStatementAnn True)
    no_cr expr = CSDo expr (internalStatementAnn False)

instance Alignment ForLoop CStatement where
    pre_cr (ForLoop expr1 expr2 expr3 stmt) =
        CSFor expr1 expr2 expr3 stmt (internalStatementAnn True)
    no_cr (ForLoop expr1 expr2 expr3 stmt) =
        CSFor expr1 expr2 expr3 stmt (internalStatementAnn False)

instance Alignment ForLoop CCompoundBlockItem where
    pre_cr (ForLoop expr1 expr2 expr3 stmt) =
        CBlockStmt $ CSFor expr1 expr2 expr3 stmt (internalStatementAnn True)
    no_cr (ForLoop expr1 expr2 expr3 stmt) =
        CBlockStmt $ CSFor expr1 expr2 expr3 stmt (internalStatementAnn False)

-- | The annotation of a statement that the generator adds, without a comment,
-- given whether a blank line goes before it.
internalStatementAnn :: Bool -> CAnns
internalStatementAnn before = internalAnn (CStatementAnn before False Nothing)

-- | A statement with its annotation changed, and its location kept.
withStatementAnn :: (CItemAnn -> CItemAnn) -> CStatement -> CStatement
withStatementAnn _ CSSkip = CSSkip
withStatementAnn f stmt = updateAnnotation stmt (ann { element = f (element ann) })

    where

        ann = getAnnotation stmt

-- | A statement with a blank line before it or without one.
statementLineBefore :: Bool -> CStatement -> CStatement
statementLineBefore before = withStatementAnn $ \case
    CStatementAnn _ ind comment -> CStatementAnn before ind comment
    CCompoundAnn _ trailing comment -> CCompoundAnn before trailing comment
    other -> error $ "Invalid annotation of a statement: " ++ show other

-- | An item of a body with the comment of the statement it starts.
commented :: Maybe CComment -> CCompoundBlockItem -> CCompoundBlockItem
commented Nothing item = item
commented comment (CBlockStmt stmt) = CBlockStmt (commentedStatement comment stmt)
commented comment (CBlockDecl decl (LocatedElement (CLocalDeclarationAnn before inner) loc)) =
    CBlockDecl decl (LocatedElement (CLocalDeclarationAnn before (joinComments comment inner)) loc)
commented _ item = error $ "Invalid annotation of an item of a body: " ++ show item

-- | A statement with a comment before it. The comment of a block goes before
-- its first item, and on its own line when the block is empty.
commentedStatement :: Maybe CComment -> CStatement -> CStatement
commentedStatement Nothing stmt = stmt
commentedStatement comment (CSCompound (item : items) ann) = CSCompound (commented comment item : items) ann
commentedStatement comment stmt = flip withStatementAnn stmt $ \case
    CStatementAnn before ind inner -> CStatementAnn before ind (joinComments comment inner)
    CCompoundAnn before trailing inner -> CCompoundAnn before trailing (joinComments comment inner)
    other -> error $ "Invalid annotation of a statement: " ++ show other

-- | Two comments that fall on the same item, the outer one first: the one of
-- a match and the one of its only case, which has no block of its own.
joinComments :: Maybe CComment -> Maybe CComment -> Maybe CComment
joinComments (Just (CComment outer)) (Just (CComment inner)) = Just (CComment (outer ++ [mempty] ++ inner))
joinComments outer Nothing = outer
joinComments Nothing inner = inner

_if :: CExpression -> CStatement -> CStatement
_if expr stmt1 = CSIfThenElse expr stmt1 Nothing (internalStatementAnn False)

_if_else :: CExpression -> CStatement -> CStatement -> CStatement
_if_else expr stmt1 stmt2 = CSIfThenElse expr stmt1 (Just stmt2) (internalStatementAnn False)

block :: [CCompoundBlockItem] -> CStatement
block items =
    CSCompound items (internalAnn (CCompoundAnn False False Nothing))

_break :: CStatement
_break = CSBreak (internalStatementAnn False)

_switch :: CExpression -> CStatement -> CStatement
_switch expr stmt = CSSwitch expr stmt (internalStatementAnn False)

_case :: CExpression -> CStatement -> CStatement
_case expr stmt = CSCase expr stmt (internalStatementAnn False)

_default :: CStatement -> CStatement
_default stmt = CSDefault stmt (internalStatementAnn False)

_return :: Maybe CExpression -> CStatement
_return expr = CSReturn expr (internalStatementAnn False)

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
        CBlockDecl (CDecl ts (Just ident) Nothing) (internalAnn (CLocalDeclarationAnn True Nothing))
    no_cr (Declaration ident ts) =
        CBlockDecl (CDecl ts (Just ident) Nothing) (internalAnn (CLocalDeclarationAnn False Nothing))
    
(@:=) :: Declaration -> CExpression -> CDeclaration
(@:=) (Declaration ident ts) expr =
    CDecl ts (Just ident) (Just expr)

instance Alignment CDeclaration CCompoundBlockItem where
    pre_cr decl = CBlockDecl decl (internalAnn (CLocalDeclarationAnn True Nothing))
    no_cr decl = CBlockDecl decl (internalAnn (CLocalDeclarationAnn False Nothing))

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

class GlobalDecl a where
    global :: a -> CFileItem
    static_global :: a -> CFileItem
    extern :: a -> CFileItem

instance GlobalDecl Declaration where
    global (Declaration ident ts) =
        let declAnn = internalAnn (CDeclarationAnn False Nothing) in
        CExtDecl (CEDVariable Nothing (CDecl ts (Just ident) Nothing)) declAnn
    static_global (Declaration ident ts) =
        let declAnn = internalAnn (CDeclarationAnn False Nothing) in
        CExtDecl (CEDVariable (Just CStatic) (CDecl ts (Just ident) Nothing)) declAnn
    extern (Declaration ident ts) =
        let declAnn = internalAnn (CDeclarationAnn False Nothing) in
        CExtDecl (CEDVariable (Just CExtern) (CDecl ts (Just ident) Nothing)) declAnn

instance GlobalDecl CDeclaration where
    global :: CDeclaration -> CFileItem
    global decl =
        let declAnn = internalAnn (CDeclarationAnn False Nothing) in
        CExtDecl (CEDVariable Nothing decl) declAnn
    static_global decl =
        let declAnn = internalAnn (CDeclarationAnn False Nothing) in
        CExtDecl (CEDVariable (Just CStatic) decl) declAnn
    extern decl =
        let declAnn = internalAnn (CDeclarationAnn False Nothing) in
        CExtDecl (CEDVariable (Just CExtern) decl) declAnn

data Function = Function FunctionDeclaration CStatement
    deriving Show

instance TypeElement Ident Declaration where
    (@:) ident cType = Declaration ident (CTypeSpec cType)


(@->) :: FunctionPrototype -> CType -> CStatement -> CFileItem
(@->) (FunctionPrototype ident decls) cType stmt =
    let declStmt = internalAnn (CDeclarationAnn False Nothing) in
    CFunctionDef Nothing  
        (CFunction cType ident (map (\(Declaration paramId ts) -> CDecl ts (Just paramId) Nothing) decls) stmt) declStmt
(@->) (StaticFunctionPrototype ident decls) cType stmt =
    let declStmt = internalAnn (CDeclarationAnn False Nothing) in
    CFunctionDef (Just CStatic) 
        (CFunction cType ident (map (\(Declaration paramId ts) -> CDecl ts (Just paramId) Nothing) decls) stmt) declStmt

instance Alignment CFileItem CFileItem where
    pre_cr = newLineBefore True
    no_cr = newLineBefore False

-- | An item of a file with or without a blank line before it. The rest of
-- its annotation, its documentation comment included, is kept.
newLineBefore :: Bool -> CFileItem -> CFileItem
newLineBefore before item = withItemAnn newLine item

    where

        newLine ann = case item of
            CPPDirective {} -> CPPDirectiveAnn before (itemDoc ann)
            _ -> CDeclarationAnn before (itemDoc ann)
        itemDoc (CDeclarationAnn _ doc) = doc
        itemDoc (CPPDirectiveAnn _ doc) = doc
        itemDoc _ = Nothing

-- | An item of a file with a documentation comment in its annotation.
documented :: Maybe CDoc -> CFileItem -> CFileItem
documented doc = withItemAnn $ \case
    CDeclarationAnn before _ -> CDeclarationAnn before doc
    CPPDirectiveAnn before _ -> CPPDirectiveAnn before doc
    other -> other

-- | An item of a file with its annotation changed, and its location kept.
withItemAnn :: (CItemAnn -> CItemAnn) -> CFileItem -> CFileItem
withItemAnn f item = case item of
    CFunctionDef storage func (LocatedElement ann loc) -> CFunctionDef storage func (LocatedElement (f ann) loc)
    CExtDecl decl (LocatedElement ann loc) -> CExtDecl decl (LocatedElement (f ann) loc)
    CPPDirective directive (LocatedElement ann loc) -> CPPDirective directive (LocatedElement (f ann) loc)

_include :: Bool -> FilePath -> CFileItem
_include system filePath =
    CPPDirective (CPPInclude system filePath) (internalAnn (CPPDirectiveAnn False Nothing))

_define :: Ident -> Maybe [String] -> CFileItem
_define ident maybeValues =
    CPPDirective (CPPDefine ident maybeValues) (internalAnn (CPPDirectiveAnn False Nothing))

_ifdef :: Ident -> CFileItem
_ifdef ident =
    CPPDirective (CPPIfDef ident) (internalAnn (CPPDirectiveAnn False Nothing))

_ifndef :: Ident -> CFileItem
_ifndef ident =
    CPPDirective (CPPIfNDef ident) (internalAnn (CPPDirectiveAnn False Nothing))

_endif :: CFileItem
_endif =
    CPPDirective CPPEndif (internalAnn (CPPDirectiveAnn False Nothing))

struct :: Ident 
    -> [CDeclaration]  -- member declarations
    -> [CAttribute] -> CFileItem
struct ident flds = documentedStruct ident (map undocumentedMember flds)

-- | A struct whose members carry their own documentation comments.
documentedStruct :: Ident -> [CMember] -> [CAttribute] -> CFileItem
documentedStruct ident members attrs = CExtDecl (CEDStructUnion (Just ident) (CStruct CStructTag Nothing members attrs)) (internalAnn (CDeclarationAnn False Nothing))

-- | A member of a struct or a union with a documentation comment.
documentedMember :: Maybe CDoc -> CDeclaration -> CMember
documentedMember doc decl = CMember decl (internalAnn (CMemberAnn doc))

-- | A member of a struct or a union without a documentation comment.
undocumentedMember :: CDeclaration -> CMember
undocumentedMember = documentedMember Nothing

-- | An enumerator without a value and with a documentation comment.
documentedEnumerator :: Maybe CDoc -> Ident -> CEnumerator
documentedEnumerator doc ident = CEnumerator ident Nothing (internalAnn (CMemberAnn doc))

-- | An enumerator without a value and without a documentation comment.
undocumentedEnumerator :: Ident -> CEnumerator
undocumentedEnumerator = documentedEnumerator Nothing
