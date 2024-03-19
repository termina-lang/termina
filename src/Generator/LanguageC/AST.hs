module Generator.LanguageC.AST where
import Prettyprinter
import Semantic.Monad

type Ident = String

data CFile' a
    = CSourceFile FilePath [CFileItem' a]
    | CHeaderFile FilePath [CFileItem' a]
    deriving (Show)

data CPreprocessorDirective' a
    = CPPInclude FilePath Bool a
    | CPPDefine Ident (Maybe [String]) a
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
    [CDeclaration' a]          -- optional declaration list
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
{--
instance Functor CStatement' where
        fmap _f (CCase a1 a2 a3) = CCase (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CDefault a1 a2) = CDefault (fmap _f a1) (_f a2)
        fmap _f (CExpr a1 a2) = CExpr (fmap (fmap _f) a1) (_f a2)
        fmap _f (CCompound a1 a2)
          = CCompound (fmap (fmap _f) a1) (_f a2)
        fmap _f (CIf a1 a2 a3 a4)
          = CIf (fmap _f a1) (fmap _f a2) (fmap (fmap _f) a3) (_f a4)
        fmap _f (CSwitch a1 a2 a3)
          = CSwitch (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CFor a1 a2 a3 a4 a5)
          = CFor (mapEither (fmap (fmap _f)) (fmap _f) a1)
                 (fmap (fmap _f) a2) (fmap (fmap _f) a3) (fmap _f a4)
                 (_f a5)
          where mapEither f1 f2 = either (Left . f1) (Right . f2)
        fmap _f (CCont a1) = CCont (_f a1)
        fmap _f (CBreak a1) = CBreak (_f a1)
        fmap _f (CReturn a1 a2) = CReturn (fmap (fmap _f) a1) (_f a2)
--}
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
  | CCompoundLit (CDeclaration' a)
                 (CInitializerList' a)   -- type name & initialiser list
                 a                       -- ^ C99 compound literal
    deriving (Show)

{--
-- deriving Functor does not work (type synonyms)
instance Functor CExpression' where
        fmap _f (CComma a1 a2) = CComma (fmap (fmap _f) a1) (_f a2)
        fmap _f (CAssignment a1 a2 a3)
          = CAssignment (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CBinary a1 a2 a3 a4)
          = CBinary a1 (fmap _f a2) (fmap _f a3) (_f a4)
        fmap _f (CCast a1 a2 a3) = CCast (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CUnary a1 a2 a3) = CUnary a1 (fmap _f a2) (_f a3)
        fmap _f (CSizeofExpr a1 a2) = CSizeofExpr (fmap _f a1) (_f a2)
        fmap _f (CSizeofType a1 a2) = CSizeofType (fmap _f a1) (_f a2)
        fmap _f (CAlignofExpr a1 a2) = CAlignofExpr (fmap _f a1) (_f a2)
        fmap _f (CAlignofType a1 a2) = CAlignofType (fmap _f a1) (_f a2)
        fmap _f (CIndex a1 a2 a3)
          = CIndex (fmap _f a1) (fmap _f a2) (_f a3)
        fmap _f (CCall a1 a2 a3)
          = CCall (fmap _f a1) (fmap (fmap _f) a2) (_f a3)
        fmap _f (CMember a1 a2 a3 a4) = CMember (fmap _f a1) a2 a3 (_f a4)
        fmap _f (CVar a1 a2) = CVar a1 (_f a2)
        fmap _f (CConst a1) = CConst a1
        fmap _f (CCompoundLit a1 a2 a3)
          = CCompoundLit (fmap _f a1) (fmapInitList _f a2) (_f a3)


fmapInitList :: (a->b) -> CInitializerList' a -> CInitializerList' b
fmapInitList _f = map (\(desigs, initializer) -> (fmap (fmap _f) desigs, fmap _f initializer))
--}

data CIntFlag = FlagUnsigned | FlagLong | FlagLongLong | FlagImag
  deriving (Eq,Ord,Enum,Bounded)
instance Show CIntFlag where
    show FlagUnsigned = "u"
    show FlagLong = "L"
    show FlagLongLong = "LL"
    show FlagImag = "i"

data CIntRepr = DecRepr | HexRepr | OctalRepr
  deriving (Show, Eq,Ord)

data CInteger = CInteger
                 !Integer
                 !CIntRepr
                 deriving (Show, Eq,Ord)

instance Pretty CInteger where
  pretty (CInteger i DecRepr) = pretty i
  pretty (CInteger i HexRepr) = pretty "0x" <> pretty i
  pretty (CInteger i OctalRepr) = pretty "0" <> pretty i

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

type CFile = CFile' SemanticAnns
type CPreprocessorDirective = CPreprocessorDirective' SemanticAnns
type CFileItem = CFileItem' SemanticAnns
type CExternalDeclaration = CExternalDeclaration' SemanticAnns
type CFunctionDef = CFunctionDef' SemanticAnns
type CStatement = CStatement' SemanticAnns
type CExpression = CExpression' SemanticAnns
type CDeclaration = CDeclaration' SemanticAnns
type CDeclarationSpecifier = CDeclarationSpecifier' SemanticAnns
type CTypeSpecifier = CTypeSpecifier' SemanticAnns
type CTypeQualifier = CTypeQualifier' SemanticAnns
type CDeclarator = CDeclarator' SemanticAnns
type CDerivedDeclarator = CDerivedDeclarator' SemanticAnns
type CArraySize = CArraySize' SemanticAnns
type CInitializer = CInitializer' SemanticAnns
type CInitializerList = CInitializerList' SemanticAnns
type CAttribute = CAttribute' SemanticAnns
type CStructureUnion = CStructureUnion' SemanticAnns
type CPartDesignator = CPartDesignator' SemanticAnns
type CEnumeration = CEnumeration' SemanticAnns
type CCompoundBlockItem = CCompoundBlockItem' SemanticAnns