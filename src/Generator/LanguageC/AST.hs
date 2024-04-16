{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Generator.LanguageC.AST where
import Prettyprinter
import Semantic.Monad
import Numeric

type Ident = String

data CFile' a
    = CSourceFile FilePath [CFileItem' a]
    | CHeaderFile FilePath [CFileItem' a]
    deriving (Show)

data CPreprocessorDirective' a
    = CPPInclude Bool FilePath a
    | CPPDefine Ident (Maybe [String]) a
    | CPPIfDef Ident a
    | CPPIfNDef Ident a
    | CPPEndif a
    deriving (Show)

data CFileItem' a
    = CExtDecl (CExternalDeclaration' a)
    | CPPDirective (CPreprocessorDirective' a)
    deriving (Show)

data CExternalDeclaration' a
    = CDeclExt (CDeclaration' a)
    | CFDefExt (CFunctionDef' a)
    deriving (Show)

data CFunctionDef' a
  = CFunDef
    [CDeclarationSpecifier' a] -- type specifier and qualifier
    (CDeclarator' a)           -- declarator
    (CStatement' a)            -- compound statement
    a
    deriving (Show)

data CDeclaration' a
  = CDeclaration
    [CDeclarationSpecifier' a] -- type specifier and qualifier, __attribute__
    [(Maybe (CDeclarator' a),  -- declarator (may be omitted)
      Maybe (CInitializer' a), -- optional initialize
      Maybe (CExpression' a))] -- optional size (const expr)
    a
    deriving (Show)

data CDeclarator' a
    = CDeclarator (Maybe Ident) [CDerivedDeclarator' a] [CAttribute' a] a
    deriving (Show)

data CDerivedDeclarator' a
  = CPtrDeclr [CTypeQualifier' a] a
  -- ^ Pointer declarator @CPtrDeclr tyquals declr@
  | CArrDeclr [CTypeQualifier' a] (CArraySize' a) a
  -- ^ Array declarator @CArrDeclr declr tyquals size-expr?@
  | CFunDeclr [CDeclaration' a] [CAttribute' a] a
    -- ^ Function declarator @CFunDeclr declr (old-style-params | new-style-params) c-attrs@
    deriving (Show)

data CArraySize' a
  = CNoArrSize Bool               -- ^ @CUnknownSize isCompleteType@
  | CArrSize Bool (CExpression' a) -- ^ @CArrSize isStatic expr@
    deriving (Show)

data CStatement' a
  -- | A statement of the form @case expr : stmt@
  = CCase (CExpression' a) (CStatement' a) a
  -- | The default case @default : stmt@
  | CDefault (CStatement' a) a
  -- | A simple statement, that is in C: evaluating an expression with
  --   side-effects and discarding the result.
  | CExpr (Maybe (CExpression' a)) a
  -- | compound statement @CCompound blockItems at@
  | CCompound [CCompoundBlockItem' a] a
  -- | conditional statement @CIf ifExpr thenStmt maybeElseStmt at@
  | CIf (CExpression' a) (CStatement' a) (Maybe (CStatement' a)) a
  -- | switch statement @CSwitch selectorExpr switchStmt@, where
  -- @switchStmt@ usually includes /case/, /break/ and /default/
  -- statements
  | CSwitch (CExpression' a) (CStatement' a) a
  -- | for statement @CFor init expr-2 expr-3 stmt@, where @init@ is
  -- either a declaration or initializing expression
  | CFor (Either (Maybe (CExpression' a)) (CDeclaration' a))
    (Maybe (CExpression' a))
    (Maybe (CExpression' a))
    (CStatement' a)
    a
  -- | continue statement
  | CCont a
  -- | break statement
  | CBreak a
  -- | return statement @CReturn returnExpr@
  | CReturn (Maybe (CExpression' a)) a
    deriving (Show)

data CCompoundBlockItem' a
  = CBlockStmt    (CStatement' a)    -- ^ A statement
  | CBlockDecl    (CDeclaration' a)  -- ^ A local declaration
    deriving (Show)

data CDeclarationSpecifier' a
  = CStorageSpec CStorageSpecifier     -- ^ storage-class specifier or typedef
  | CTypeSpec    (CTypeSpecifier' a)   -- ^ type name
  | CTypeQual    (CTypeQualifier' a)   -- ^ type qualifier
  | CFunSpec     CFunctionSpecifier -- ^ function specifier
    deriving (Show)

data CStorageSpecifier
  = CAuto        -- ^ auto
  | CRegister    -- ^ register
  | CStatic      -- ^ static
  | CExtern      -- ^ extern
  | CTypedef     -- ^ typedef
  | CThread      -- ^ C11/GNUC thread local storage
    deriving (Show)

instance Pretty CStorageSpecifier where
    pretty CAuto = pretty "auto"
    pretty CRegister = pretty "register"
    pretty CStatic = pretty "static"
    pretty CExtern = pretty "extern"
    pretty CTypedef = pretty "typedef"
    pretty CThread = pretty "_Thread_local"

data CTypeSpecifier' a
    = CVoidType
    | CCharType
    | CUInt8Type
    | CInt8Type 
    | CUInt16Type
    | CInt16Type
    | CUInt32Type
    | CInt32Type
    | CUInt64Type
    | CInt64Type
    | CUInt128Type
    | CInt128Type
    | CBoolType
    | CSizeTType
    | CSUType      (CStructureUnion' a) -- ^ Struct or Union specifier
    | CEnumType    (CEnumeration' a)    -- ^ Enumeration specifier
    | CTypeDef     Ident                -- ^ Typedef name
    | CAtomicType  (CDeclaration' a)    -- ^ @_Atomic(type)@
    deriving (Show)

data CTypeQualifier' a
  = CConstQual
  | CVolatQual
  | CRestrQual
  | CAtomicQual
  | CAttrQual  (CAttribute' a)
    deriving (Show)

data CFunctionSpecifier
  = CInlineQual
  | CNoreturnQual
    deriving (Show)

instance Pretty CFunctionSpecifier where
    pretty CInlineQual = pretty "inline"
    pretty CNoreturnQual = pretty "_Noreturn"

data CStructureUnion' a
  = CStruct
    CStructTag
    (Maybe Ident)
    (Maybe [CDeclaration' a])  -- member declarations
    [CAttribute' a]            -- __attribute__s
    deriving (Show)

data CStructTag = CStructTag
                | CUnionTag
                deriving (Show)

instance Pretty CStructTag where
  pretty CStructTag = pretty "struct"
  pretty CUnionTag = pretty "union"

data CEnumeration' a
  = CEnum
    (Maybe Ident)
    (Maybe [(Ident,                    -- variant name
             Maybe (CExpression' a))]) -- explicit variant value
    [CAttribute' a]                    -- __attribute__s
    deriving (Show)

data CInitializer' a
  -- | assignment expression
  = CInitExpr (CExpression' a) a
  -- | initialization list (see 'CInitList')
  | CInitList (CInitializerList' a) a
    deriving (Show)

type CInitializerList' a = [([CPartDesignator' a], CInitializer' a)]

data CPartDesignator' a
  -- | array position designator
  = CArrDesig     (CExpression' a) a
  -- | member designator
  | CMemberDesig  Ident a
    deriving (Show)

data CAttribute' a = CAttr Ident [CExpression' a]
                    deriving (Show)

data CBinaryOp = CMulOp
               | CDivOp
               | CRmdOp                 -- ^ remainder of division
               | CAddOp
               | CSubOp
               | CShlOp                 -- ^ shift left
               | CShrOp                 -- ^ shift right
               | CLeOp                  -- ^ less
               | CGrOp                  -- ^ greater
               | CLeqOp                 -- ^ less or equal
               | CGeqOp                 -- ^ greater or equal
               | CEqOp                  -- ^ equal
               | CNeqOp                 -- ^ not equal
               | CAndOp                 -- ^ bitwise and
               | CXorOp                 -- ^ exclusive bitwise or
               | COrOp                  -- ^ inclusive bitwise or
               | CLndOp                 -- ^ logical and
               | CLorOp                 -- ^ logical or
               deriving (Eq, Ord, Show)

data CUnaryOp = CAdrOp                  -- ^ address operator
              | CIndOp                  -- ^ indirection operator
              | CMinOp                  -- ^ prefix minus
              | CCompOp                 -- ^ one's complement
              | CNegOp                  -- ^ logical negation
              deriving (Eq, Ord, Show)

data CExpression' a
  = CComma       [CExpression' a]         -- comma expression list, n >= 2
                 a
  | CAssignment  (CExpression' a)         -- l-value
                 (CExpression' a)         -- r-value
                 a
  | CBinary      CBinaryOp                -- binary operator
                 (CExpression' a)         -- lhs
                 (CExpression' a)         -- rhs
                 a
  | CCast        (CDeclaration' a)        -- type name
                 (CExpression' a)
                 a
  | CUnary       CUnaryOp                 -- unary operator
                 (CExpression' a)
                 a
  | CSizeofExpr  (CExpression' a)
                 a
  | CSizeofType  (CDeclaration' a)        -- type name
                 a
  | CAlignofExpr (CExpression' a)
                 a
  | CAlignofType (CDeclaration' a)        -- type name
                 a
  | CIndex       (CExpression' a)         -- array
                 (CExpression' a)         -- index
                 a
  | CCall        (CExpression' a)         -- function
                 [CExpression' a]         -- arguments
                 a
  | CMember      (CExpression' a)         -- structure
                 Ident                   -- member name
                 Bool                    -- deref structure? (True for `->')
                 a
  | CVar         Ident                   -- identifier (incl. enumeration const)
                 a
  | CConst       CConstant  a            -- ^ integer, character, floating point and string constants
    deriving (Show)

data CIntFlag = FlagUnsigned | FlagLong | FlagLongLong | FlagImag
  deriving (Eq,Ord,Enum,Bounded)
instance Show CIntFlag where
    show FlagUnsigned = "u"
    show FlagLong = "L"
    show FlagLongLong = "LL"
    show FlagImag = "i"

data CIntRepr = CDecRepr | CHexRepr | COctalRepr
  deriving (Show, Eq,Ord)

data CInteger = CInteger
                 !Integer
                 !CIntRepr
                 deriving (Show, Eq,Ord)

instance Pretty CInteger where
  pretty (CInteger i CDecRepr) = pretty i
  pretty (CInteger i CHexRepr) = pretty "0x" <> pretty (showHex i "")
  pretty (CInteger i COctalRepr) = pretty "0" <> pretty (showOct i "")

-- | C char constants (abstract)
data CChar = CChar
              !Char
           | CChars
              [Char] -- multi-character character constant
           deriving (Show, Eq,Ord)

instance Pretty CChar where
  pretty (CChar c) = pretty $ show c
  pretty (CChars cs) = pretty $ show cs

newtype CString = CString
                String    -- characters
                deriving (Show, Eq,Ord)

instance Pretty CString where
  pretty (CString s) = pretty $ show s

data CConstant
  = CIntConst   CInteger
  | CCharConst  CChar
  | CStrConst   CString
    deriving (Show)

instance Pretty CConstant where
  pretty (CIntConst i) = pretty i
  pretty (CCharConst c) = pretty c
  pretty (CStrConst s) = pretty s

instance Pretty CBinaryOp where
  pretty op = pretty $ case op of
    CMulOp -> "*"
    CDivOp -> "/"
    CRmdOp -> "%"
    CAddOp -> "+"
    CSubOp -> "-"
    CShlOp -> "<<"
    CShrOp -> ">>"
    CLeOp  -> "<"
    CGrOp  -> ">"
    CLeqOp -> "<="
    CGeqOp -> ">="
    CEqOp  -> "=="
    CNeqOp -> "!="
    CAndOp -> "&"
    CXorOp -> "^"
    COrOp  -> "|"
    CLndOp -> "&&"
    CLorOp -> "||"

instance Pretty CUnaryOp where
  pretty op = pretty $ case op of
    CAdrOp     -> "&"
    CIndOp     -> "*"
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
  deriving (Show)

data CAnns = CAnnotations
  {
    location :: Locations,
    itemAnnotation :: CItemAnn
  }
  deriving (Show)

type CFile = CFile' CAnns
type CPreprocessorDirective = CPreprocessorDirective' CAnns
type CFileItem = CFileItem' CAnns
type CExternalDeclaration = CExternalDeclaration' CAnns
type CFunctionDef = CFunctionDef' CAnns
type CStatement = CStatement' CAnns
type CExpression = CExpression' CAnns
type CDeclaration = CDeclaration' CAnns
type CDeclarationSpecifier = CDeclarationSpecifier' CAnns
type CTypeSpecifier = CTypeSpecifier' CAnns
type CTypeQualifier = CTypeQualifier' CAnns
type CDeclarator = CDeclarator' CAnns
type CDerivedDeclarator = CDerivedDeclarator' CAnns
type CArraySize = CArraySize' CAnns
type CInitializer = CInitializer' CAnns
type CInitializerList = CInitializerList' CAnns
type CAttribute = CAttribute' CAnns
type CStructureUnion = CStructureUnion' CAnns
type CPartDesignator = CPartDesignator' CAnns
type CEnumeration = CEnumeration' CAnns
type CCompoundBlockItem = CCompoundBlockItem' CAnns