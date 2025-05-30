{-# LANGUAGE FlexibleInstances #-}
module Generator.LanguageC.AST where

import Prettyprinter
import Numeric
import Data.Char
import Utils.Annotations

type Ident = String

data CFile' a
    = CSourceFile FilePath [CFileItem' a]
    | CHeaderFile FilePath [CFileItem' a]

data CPreprocessorDirective
    = CPPInclude Bool FilePath
    | CPPDefine Ident (Maybe [String])
    | CPPIfDef Ident
    | CPPIfNDef Ident
    | CPPEndif

data CFileItem' a
    = CExtDecl (CExternalDeclaration' a) a
    | CFunctionDef (Maybe CStorageSpecifier) (CFunction' a) a
    | CPPDirective CPreprocessorDirective a

data CAttribute' a = CAttr Ident [CExpression' a]
    deriving Show

data CStructureUnion' a
  = CStruct
    CStructTag
    (Maybe Ident)      -- struct/union name (optional)
    [CDeclaration' a]  -- member declarations
    [CAttribute' a]    -- __attribute__s
    deriving Show

data CStructTag = CStructTag
                | CUnionTag
    deriving Show

instance Pretty CStructTag where
  pretty CStructTag = pretty "struct"
  pretty CUnionTag = pretty "union"

data CEnum' a = 
    CEnum 
    (Maybe Ident) -- ^ enum name (optional)
    [(Ident, Maybe (CExpression' a))] -- ^ enum constants
    [CAttribute' a] -- ^ __attribute__s
    deriving Show

data CTerminaType' a = 
    -- | Basic type specifiers
    CTypeSpec CType
    -- | Annonymous struct/union
    | CTSStructUnion (CStructureUnion' a)
    -- | Annonymous enum
    | CTSEnum (CEnum' a)
    deriving Show

data CDeclaration' a =
    CDecl (CTerminaType' a) (Maybe Ident) (Maybe (CExpression' a))
    deriving Show

data CExternalDeclaration' a
    = CEDVariable (Maybe CStorageSpecifier) (CDeclaration' a)
    | CEDFunction CType Ident [CDeclaration' a]
    | CEDEnum (Maybe Ident) (CEnum' a)
    | CEDStructUnion (Maybe Ident) (CStructureUnion' a)
    | CEDTypeDef Ident CType

data CFunction' a =
    CFunction CType Ident [CDeclaration' a] (CStatement' a)

data CQualifier = CQualifier {
    qual_volatile :: Bool,
    qual_const :: Bool,
    qual_atomic :: Bool
} deriving Show

noqual :: CQualifier
noqual = CQualifier False False False

constqual :: CQualifier
constqual = CQualifier False True False

volatile :: CQualifier
volatile = CQualifier True False False

atomic :: CQualifier
atomic = CQualifier False False True

data CSignedness =
    Signed
    | Unsigned
    deriving Show

data CIntSize =
    IntSize8
    | IntSize16
    | IntSize32
    | IntSize64
    | IntSize128
    deriving Show

data CType = 
    -- | The void type
    CTVoid CQualifier
    | CTChar CQualifier
    -- | Integer types
    | CTInt CIntSize CSignedness CQualifier
    -- | Pointer types
    | CTPointer CType CQualifier
    -- | TArray types
    | CTArray CType CExpression
    -- | Struct types
    | CTStruct CStructTag Ident CQualifier
    -- | Function type
    | CTFunction CType [CType]
    -- | Enumeration types
    | CTEnum Ident CQualifier
    -- | size_t type 
    | CTSizeT CQualifier
    -- | _Bool type  
    | CTBool CQualifier
    -- | typedef name
    | CTTypeDef Ident CQualifier
    deriving Show

data CBinaryOp = 
    COpAdd                   -- ^ addition (+)
    | COpSub                 -- ^ subtraction (-)
    | COpMul                 -- ^ multiplication (*)
    | COpDiv                 -- ^ division (/)
    | COpMod                 -- ^ remainder of division (%)
    | COpAnd                 -- ^ bitwise and (&)
    | COpXor                 -- ^ exclusive bitwise or (^)
    | COpOr                  -- ^ inclusive bitwise or (|)
    | COpShl                 -- ^ shift left
    | COpShr                 -- ^ shift right
    | COpEq                  -- ^ equal (==)
    | COpNe                  -- ^ not equal (!=)
    | COpLt                  -- ^ less (<)
    | COpGt                  -- ^ greater (>)
    | COpLe                  -- ^ less than or equal (<=)
    | COpGe                  -- ^ greater or equal (>=)
    deriving Show

data CUnaryOp = 
    CMinOp                   -- ^ prefix minus
    | CCompOp                -- ^ one's complement
    | CNegOp                 -- ^ logical negation
    deriving Show

data CStorageSpecifier
  = CAuto        -- ^ auto
  | CRegister    -- ^ register
  | CStatic      -- ^ static
  | CExtern      -- ^ extern
  | CThread      -- ^ C11/GNUC thread local storage

instance Pretty CStorageSpecifier where
    pretty CAuto = pretty "auto"
    pretty CRegister = pretty "register"
    pretty CStatic = pretty "static"
    pretty CExtern = pretty "extern"
    pretty CThread = pretty "_Thread_local"

-- | C char constant
newtype CChar = CChar Char
    deriving Show

instance Pretty CChar where
  pretty (CChar c) = pretty $ show c

instance Pretty CString where
  pretty (CString s) = pretty $ show s

newtype CString = CString String
    deriving Show

data CIntRepr = CDecRepr | CHexRepr | COctalRepr
    deriving Show

data CInteger = 
    CInteger !Integer !CIntRepr
    deriving Show

instance Pretty CInteger where
  pretty (CInteger i CDecRepr) = pretty i
  pretty (CInteger i CHexRepr) = pretty "0x" <> pretty (toUpper <$> showHex i "")
  pretty (CInteger i COctalRepr) = pretty "0" <> pretty (showOct i "")

data CConstant =
  CIntConst   CInteger
  | CCharConst  CChar
  | CStrConst   CString
    deriving Show

data CObject' a = 
    CVar Ident CType
    | CField (CObject' a) Ident CType
    | CDeref (CObject' a) CType -- ^ pointer dereference (unary *)
    | CIndexOf (CObject' a) (CExpression' a) CType -- ^ array indexing
    | CObjCast (CObject' a) CType
    deriving Show

data CExpression' a =
    CExprConstant CConstant CType a         -- ^ integer, character, floating point and string constants
    | CExprValOf (CObject' a) CType a       -- ^ l-value used as a r-value  
    | CExprAddrOf (CObject' a) CType a      -- ^ address-of operator (&)
    | CExprUnaryOp CUnaryOp (CExpression' a) CType a
    | CExprBinaryOp CBinaryOp (CExpression' a) (CExpression' a) CType a
    | CExprCast (CExpression' a) CType a
    | CExprSeqAnd (CExpression' a) (CExpression' a) CType a -- ^ sequential "and" r1 && r2
    | CExprSeqOr (CExpression' a) (CExpression' a) CType a -- ^ sequential "or" r1 || r2 
    | CExprSizeOfType CType CType a
    | CExprSizeOfExpr (CExpression' a) CType a
    | CExprAlignOfType CType CType a
    | CExprAssign (CObject' a) (CExpression' a) CType a
    | CExprComma (CExpression' a) (CExpression' a) CType a -- ^ sequence expression r1, r2
    | CExprCall (CExpression' a) [CExpression' a] CType a
    deriving Show

instance Annotated CExpression' where
  getAnnotation (CExprConstant _ _ a) = a
  getAnnotation (CExprValOf _ _ a) = a
  getAnnotation (CExprAddrOf _ _ a) = a
  getAnnotation (CExprUnaryOp _ _ _ a) = a
  getAnnotation (CExprBinaryOp _ _ _ _ a) = a
  getAnnotation (CExprCast _ _ a) = a
  getAnnotation (CExprSeqAnd _ _ _ a) = a
  getAnnotation (CExprSeqOr _ _ _ a) = a
  getAnnotation (CExprSizeOfType _ _ a) = a
  getAnnotation (CExprSizeOfExpr _ _ a) = a
  getAnnotation (CExprAlignOfType _ _ a) = a
  getAnnotation (CExprAssign _ _ _ a) = a
  getAnnotation (CExprComma _ _ _ a) = a
  getAnnotation (CExprCall _ _ _ a) = a

  updateAnnotation (CExprConstant c t _) = CExprConstant c t
  updateAnnotation (CExprValOf o t _) = CExprValOf o t
  updateAnnotation (CExprAddrOf o t _) = CExprAddrOf o t
  updateAnnotation (CExprUnaryOp op e t _) = CExprUnaryOp op e t
  updateAnnotation (CExprBinaryOp op e1 e2 t _) = CExprBinaryOp op e1 e2 t
  updateAnnotation (CExprCast e t _) = CExprCast e t
  updateAnnotation (CExprSeqAnd e1 e2 t _) = CExprSeqAnd e1 e2 t
  updateAnnotation (CExprSeqOr e1 e2 t _) = CExprSeqOr e1 e2 t
  updateAnnotation (CExprSizeOfType t1 t2 _) = CExprSizeOfType t1 t2
  updateAnnotation (CExprSizeOfExpr e t _) = CExprSizeOfExpr e t
  updateAnnotation (CExprAlignOfType t1 t2 _) = CExprAlignOfType t1 t2
  updateAnnotation (CExprAssign o e t _) = CExprAssign o e t
  updateAnnotation (CExprComma e1 e2 t _) = CExprComma e1 e2 t
  updateAnnotation (CExprCall e es t _) = CExprCall e es t

getCExprType :: CExpression' a -> CType
getCExprType (CExprConstant _ t _) = t
getCExprType (CExprValOf _ t _) = t
getCExprType (CExprAddrOf _ t _) = t
getCExprType (CExprUnaryOp _ _ t _) = t
getCExprType (CExprBinaryOp _ _ _ t _) = t
getCExprType (CExprCast _ t _) = t
getCExprType (CExprSeqAnd _ _ t _) = t
getCExprType (CExprSeqOr _ _ t _) = t
getCExprType (CExprSizeOfType _ t _) = t
getCExprType (CExprSizeOfExpr _ t _) = t
getCExprType (CExprAlignOfType _ t _) = t
getCExprType (CExprAssign _ _ t _) = t
getCExprType (CExprComma _ _ t _) = t
getCExprType (CExprCall _ _ t _) = t

getCObjType :: CObject' a -> CType
getCObjType (CVar _ t) = t
getCObjType (CField _ _ t) = t
getCObjType (CDeref _ t) = t
getCObjType (CIndexOf _ _ t) = t
getCObjType (CObjCast _ t) = t

data CCompoundBlockItem' a
  = CBlockStmt (CStatement' a)     -- ^ A statement
  | CBlockDecl (CDeclaration' a) a -- ^ A local declaration
    deriving Show

data CStatement' a =
   CSSkip  -- ^ do nothing
   | CSCase (CExpression' a) (CStatement' a) a -- ^ Case (labeled statement)
   | CSDefault (CStatement' a) a -- ^ Default (labeled statement)
   | CSDo (CExpression' a) a -- ^ evaluate expression for side effects
   | CSCompound [CCompoundBlockItem' a] a -- ^ compound statement 
   | CSIfThenElse (CExpression' a) (CStatement' a) (Maybe (CStatement' a)) a -- ^ conditional
   | CSFor (Either (Maybe (CExpression' a)) (CDeclaration' a))
      (Maybe (CExpression' a))
      (Maybe (CExpression' a))
      (CStatement' a) a -- ^ for loop
   | CSReturn (Maybe (CExpression' a)) a -- ^ return statement
   | CSSwitch (CExpression' a) (CStatement' a) a -- ^ switch statement
   | CSBreak a -- ^ break statement
    deriving Show

instance Annotated CStatement' where 
  getAnnotation CSSkip = error "CSSkip has no annotation"
  getAnnotation (CSCase _ _ a) = a
  getAnnotation (CSDefault _ a) = a
  getAnnotation (CSDo _ a) = a
  getAnnotation (CSCompound _ a) = a
  getAnnotation (CSIfThenElse _ _ _ a) = a
  getAnnotation (CSFor _ _ _ _ a) = a
  getAnnotation (CSReturn _ a) = a
  getAnnotation (CSSwitch _ _ a) = a
  getAnnotation (CSBreak a) = a

  updateAnnotation CSSkip = const CSSkip
  updateAnnotation (CSCase e s _) = CSCase e s
  updateAnnotation (CSDefault s _) = CSDefault s
  updateAnnotation (CSDo e _) = CSDo e
  updateAnnotation (CSCompound ss _) = CSCompound ss
  updateAnnotation (CSIfThenElse e s1 s2 _) = CSIfThenElse e s1 s2
  updateAnnotation (CSFor e1 e2 e3 s _) = CSFor e1 e2 e3 s
  updateAnnotation (CSReturn e _) = CSReturn e
  updateAnnotation (CSSwitch e s _) = CSSwitch e s
  updateAnnotation (CSBreak _) = CSBreak

instance Pretty CConstant where
  pretty (CIntConst i) = pretty i
  pretty (CCharConst c) = pretty c
  pretty (CStrConst s) = pretty s

instance Pretty CBinaryOp where
  pretty op = pretty $ case op of
    COpMul -> "*"
    COpDiv -> "/"
    COpMod -> "%"
    COpAdd -> "+"
    COpSub -> "-"
    COpShl -> "<<"
    COpShr -> ">>"
    COpLt  -> "<"
    COpGt  -> ">"
    COpLe -> "<="
    COpGe -> ">="
    COpEq  -> "=="
    COpNe -> "!="
    COpAnd -> "&"
    COpXor -> "^"
    COpOr  -> "|"

instance Pretty CUnaryOp where
  pretty op = pretty $ case op of
    CMinOp     -> "-"
    CCompOp    -> "~"
    CNegOp     -> "!"

data CItemAnn = 
    -- | Generic annotation
    CGenericAnn
    -- | C Statement annotation
    | CStatementAnn 
      Bool -- ^ Add new line before statement
      Bool -- ^ Indent statement
    | CCompoundAnn
      Bool -- ^ Add new line before the compound block
      Bool -- ^ Add trailing line inside the compound block
    | CDeclarationAnn
      Bool -- ^ Add new line before declaration 
    | CPPDirectiveAnn
      Bool -- ^ Add new line before directive
    deriving Show

type CAnns = LocatedElement CItemAnn

itemAnnotation :: CAnns -> CItemAnn
itemAnnotation = element

type CFile = CFile' CAnns
type CFileItem = CFileItem' CAnns
type CExternalDeclaration = CExternalDeclaration' CAnns
type CFunction = CFunction' CAnns
type CStatement = CStatement' CAnns
type CCompoundBlockItem = CCompoundBlockItem' CAnns
type CObject = CObject' CAnns
type CExpression = CExpression' CAnns
type CDeclaration = CDeclaration' CAnns
type CTerminaType = CTerminaType' CAnns
type CAttribute = CAttribute' CAnns
type CStructureUnion = CStructureUnion' CAnns
type CEnum = CEnum' CAnns

instance Located (CExpression' CAnns) where
  getLocation = getLocation . getAnnotation 
  updateLocation e loc = updateAnnotation e (updateLocation (getAnnotation e) loc)

instance Located (CStatement' CAnns) where
  getLocation = getLocation . getAnnotation
  updateLocation e loc = updateAnnotation e (updateLocation (getAnnotation e) loc)

instance Located (CCompoundBlockItem' CAnns) where
  getLocation (CBlockStmt stmt) = getLocation stmt -- ^ A statement
  getLocation (CBlockDecl _ ann) = getLocation ann
  updateLocation (CBlockStmt stmt) loc = CBlockStmt (updateLocation stmt loc)
  updateLocation (CBlockDecl decl ann) loc = CBlockDecl decl (updateLocation ann loc)

instance Located (CFileItem' CAnns) where
  getLocation (CExtDecl _ ann) = getLocation ann
  getLocation (CFunctionDef _ _ ann) = getLocation ann
  getLocation (CPPDirective _ ann) = getLocation ann
  updateLocation (CExtDecl decl ann) loc = CExtDecl decl (updateLocation ann loc)
  updateLocation (CFunctionDef spec f ann) loc = CFunctionDef spec f (updateLocation ann loc)
  updateLocation (CPPDirective dir ann) loc = CPPDirective dir (updateLocation ann loc)
