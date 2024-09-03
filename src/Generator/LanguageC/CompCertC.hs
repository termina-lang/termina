module Generator.LanguageC.CompCertC where

import Prettyprinter
import Numeric
import Data.Char
import Utils.Annotations

type Ident = String

data CFile' a
    = CSourceFile FilePath [CFileItem' a]
    | CHeaderFile FilePath [CFileItem' a]

data CPreprocessorDirective' a
    = CPPInclude Bool FilePath a
    | CPPDefine Ident (Maybe [String]) a
    | CPPIfDef Ident a
    | CPPIfNDef Ident a
    | CPPEndif a

data CFileItem' a
    = CExtDecl (CExternalDeclaration' a)
    | CPPDirective (CPreprocessorDirective' a)

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

data CTypeSpecifier' a = 
    -- | Basic type specifiers
    CTypeSpec CType
    -- | Annonymous struct/union
    | CTSStructUnion (CStructureUnion' a)
    -- | Annonymous enum
    | CTSEnum (CEnum' a)
    deriving Show

data CDeclaration' a =
    CDecl (CTypeSpecifier' a) Ident
    deriving Show

data CExternalDeclaration' a
    = CEDVariable CStorageSpecifier (CDeclaration' a) a
    | CEDFunction (CFunction' a) a
    | CEDEnum (CEnum' a) a
    | CEDStructUnion (CStructureUnion' a) a
    | CEDTypeDef Ident CType a

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
    CTVoid
    | CTChar CQualifier
    -- | Integer types
    | CTInt CIntSize CSignedness CQualifier
    -- | Pointer types
    | CTPointer CType CQualifier
    -- | Array types
    | CTArray CType CArraySize
    -- | Struct types
    | CTStruct CStructTag Ident CQualifier
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

data CArraySize =
    CArraySizeK CInteger
    | CArraySizeV Ident
    deriving Show

data CObject' a = 
    CVar Ident CType
    | CField (CExpression' a) Ident CType
    | CDeref (CExpression' a) CType -- ^ pointer dereference (unary *)
    | CIndexOf (CObject' a) (CExpression' a) CType -- ^ array indexing
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
    | CExprAlignOfType CType CType a
    | CExprAssign (CObject' a) (CExpression' a) CType a
    | CExprComma (CExpression' a) (CExpression' a) CType a -- ^ sequence expression r1, r2
    | CExprCall (CExpression' a) [CExpression' a] CType a
    deriving Show

getType :: CExpression' a -> CType
getType (CExprConstant _ t _) = t
getType (CExprValOf _ t _) = t
getType (CExprAddrOf _ t _) = t
getType (CExprUnaryOp _ _ t _) = t
getType (CExprBinaryOp _ _ _ t _) = t
getType (CExprCast _ t _) = t
getType (CExprSeqAnd _ _ t _) = t
getType (CExprSeqOr _ _ t _) = t
getType (CExprSizeOfType _ t _) = t
getType (CExprAlignOfType _ t _) = t
getType (CExprAssign _ _ t _) = t
getType (CExprComma _ _ t _) = t
getType (CExprCall _ _ t _) = t

data CCompoundBlockItem' a
  = CBlockStmt    (CStatement' a)    -- ^ A statement
  | CBlockDecl    (CDeclaration' a)  -- ^ A local declaration
    deriving Show

data CStatement' a =
   CSSkip  -- ^ do nothing
   | CSCase (CExpression' a) (CStatement' a) a -- ^ Case (labeled statement)
   | CSDefault (CStatement' a) a -- ^ Default (labeled statement)
   | CSDo (CExpression' a) a -- ^ evaluate expression for side effects
   | CSCompound [CCompoundBlockItem' a] a -- ^ compound statement 
   | CSIfThenElse (CExpression' a) (CStatement' a) (CStatement' a) a -- ^ conditional
   | CSFor (CStatement' a) (CExpression' a) (CStatement' a) (CStatement' a) a -- ^ for loop
   | CSReturn (Maybe (CExpression' a)) a -- ^ return statement
   | CSSwitch (CExpression' a) (CStatement' a) a -- ^ switch statement
    deriving Show

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

type CAnns = Located CItemAnn

itemAnnotation :: CAnns -> CItemAnn
itemAnnotation = element

type CFile = CFile' CAnns
type CPreprocessorDirective = CPreprocessorDirective' CAnns
type CFileItem = CFileItem' CAnns
type CExternalDeclaration = CExternalDeclaration' CAnns
type CFunction = CFunction' CAnns
type CStatement = CStatement' CAnns
type CCompoundBlockItem = CCompoundBlockItem' CAnns
type CObject = CObject' CAnns
type CExpression = CExpression' CAnns
type CDeclaration = CDeclaration' CAnns
type CTypeSpecifier = CTypeSpecifier' CAnns
type CAttribute = CAttribute' CAnns
type CStructureUnion = CStructureUnion' CAnns
type CEnum = CEnum' CAnns