module PPrinter.Common where

import AST

import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.Maybe
import Semantic.Monad

type DocStyle = Doc AnsiStyle

getExpType :: Expression SemanticAnns -> TypeSpecifier
getExpType (Variable _ ann) = ty_ann ann
getExpType (Constant _ ann) = ty_ann ann
getExpType (OptionVariantExpression _ ann) = ty_ann ann
getExpType (BinOp _ _ _ ann) = ty_ann ann
getExpType (ReferenceExpression _ ann) = ty_ann ann
getExpType (DereferenceExpression _ ann) = ty_ann ann
getExpType (Casting _ _ ann) = ty_ann ann
getExpType (FunctionExpression _ _ ann) = ty_ann ann
getExpType (FieldValuesAssignmentsExpression _ _ ann) = ty_ann ann
getExpType (EnumVariantExpression _ _ _ ann) = ty_ann ann
getExpType (VectorIndexExpression _ _ ann) = ty_ann ann
getExpType (VectorInitExpression _ _ ann) = ty_ann ann
getExpType (ParensExpression expr _) = getExpType expr

getLocation :: Expression SemanticAnns -> Locations
getLocation (Variable _ ann) = parse ann
getLocation (Constant _ ann) = parse ann
getLocation (OptionVariantExpression _ ann) = parse ann
getLocation (BinOp _ _ _ ann) = parse ann
getLocation (ReferenceExpression _ ann) = parse ann
getLocation (DereferenceExpression _ ann) = parse ann
getLocation (Casting _ _ ann) = parse ann
getLocation (FunctionExpression _ _ ann) = parse ann
getLocation (FieldValuesAssignmentsExpression _ _ ann) = parse ann
getLocation (EnumVariantExpression _ _ _ ann) = parse ann
getLocation (VectorIndexExpression _ _ ann) = parse ann
getLocation (VectorInitExpression _ _ ann) = parse ann
getLocation (ParensExpression expr _) = getLocation expr

-- | Type of the pretty printers
type Printer a b =
  (b -> DocStyle)
  -- ^ Function that pretty prints an annotation BEFORE printing the construct
  -> (b -> DocStyle)
  -- ^ Function that pretty prints an annotation AFTER printing the construct
  -> a b
  -- ^ The annotated element to pretty print
  -> DocStyle

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

-- | This function is used to create the names of temporal variables
-- and symbols.
namefy :: Identifier -> Identifier
namefy = ("__" ++)

--------------------------------------------------------------------------------
-- C pretty keywords
-- Creturn, C_typedef, C_enum, C_struct, C_union :: DocStyle
returnC, typedefC, enumC, structC, unionC, voidC :: DocStyle
returnC = pretty "return"
typedefC = pretty "typedef"
enumC = pretty "enum"
structC = pretty "struct"
unionC = pretty "union"
voidC = pretty "void";

-- C pretty unsigned integer types
uint8C, uint16C, uint32C, uint64C :: DocStyle
uint8C = pretty "uint8_t"
uint16C = pretty "uint16_t"
uint32C = pretty "uint32_t"
uint64C = pretty "uint64_t"

-- C pretty signed integer types
int8C, int16C, int32C, int64C :: DocStyle
int8C = pretty "int8_t"
int16C = pretty "int16_t"
int32C = pretty "int32_t"
int64C = pretty "int64_t"

-- C pretty char
charC :: DocStyle
charC = pretty "char"

-- C attribute pragma
attribute :: DocStyle
attribute = pretty "__attribute__"

-- | Termina's pretty builtin types
pool, msgQueue, mutex :: DocStyle
pool = pretty "__termina_pool_t"
msgQueue = pretty "__termina_msg_queue_id_t"
mutex = pretty "__termina_mutex_id_t"

enumIdentifier :: Identifier -> DocStyle
enumIdentifier identifier = pretty "__enum_" <> pretty identifier

enumVariantsField :: DocStyle
enumVariantsField = pretty "__variant"

ppRootType :: TypeSpecifier -> DocStyle
ppRootType UInt8 = uint8C
ppRootType UInt16 = uint16C
ppRootType UInt32 = uint32C
ppRootType UInt64 = uint64C
ppRootType Int8 = int8C
ppRootType Int16 = int16C
ppRootType Int32 = int32C
ppRootType Int64 = int64C
ppRootType Bool = uint8C
ppRootType Char = charC
ppRootType (DefinedType typeIdentifier) = pretty typeIdentifier
ppRootType (Vector ts _) = ppRootType ts
ppRootType (Option ts) = case ts of
  Option ts' -> ppRootType ts'
  Vector ts' _ -> ppRootType ts'
  Reference ts' -> ppRootType ts'
  DynamicSubtype ts' -> ppRootType ts'
  Unit -> error "unsupported type"
  _ -> ppRootType ts <+> pretty "*"
ppRootType (Pool _ _) = pool
ppRootType (Reference ts) = case ts of
  Option ts' -> ppRootType ts'
  Vector ts' _ -> ppRootType ts'
  Reference ts' -> ppRootType ts'
  DynamicSubtype ts' -> ppRootType ts'
  Unit -> error "unsupported type"
  _ -> ppRootType ts <+> pretty "*"
ppRootType (MsgQueue _ _) = msgQueue
ppRootType (DynamicSubtype ts) = case ts of
  Option ts' -> ppRootType ts'
  Vector ts' _ -> ppRootType ts'
  Reference ts' -> ppRootType ts'
  DynamicSubtype ts' -> ppRootType ts'
  Unit -> error "unsupported type"
  _ -> ppRootType ts <+> pretty "*"
ppRootType Unit = error "unsupported type"

ppDimension :: TypeSpecifier -> DocStyle
ppDimension (Vector ts (KC size)) = ppDimension ts <> brackets (ppConst size)
ppDimension _ = emptyDoc

ppDeclaration :: Identifier -> TypeSpecifier -> DocStyle
ppDeclaration identifier ts = ppRootType ts <+> pretty identifier <> ppDimension ts

ppParameter :: Parameter -> DocStyle
ppParameter (Parameter identifier ts) = ppDeclaration identifier ts

-- | Pretty print a C function declaration
ppCFunctionDeclaration ::
    DocStyle -> -- ^ function identifier (name)
    [DocStyle] -> -- ^ list of parameters (possibly empty)
    Maybe DocStyle -> -- ^ type of the return value (optional)
    DocStyle
ppCFunctionDeclaration identifier parameters rTS =
  fromMaybe voidC rTS <+> identifier <>
      parens (align (fillSep (punctuate comma parameters)))

-- | Pretty print a C function call
ppCFunctionCall :: DocStyle -> [DocStyle] -> DocStyle
ppCFunctionCall identifier parameters = identifier <>
      parens (align (fillSep (punctuate comma parameters)))

ppConst :: Const -> DocStyle
ppConst (B b) = if b then pretty "1" else pretty "0"
ppConst (I _ integer) = pretty integer
ppConst (C char) = pretty "'" <> pretty char <> pretty "'"

ppModifier :: Modifier -> DocStyle
ppModifier (Modifier identifier (Just (KC c))) = pretty identifier <> parens (ppConst c)
ppModifier (Modifier identifier Nothing) = pretty identifier

typeDefEqFunctionName :: Identifier -> DocStyle
typeDefEqFunctionName identifier = pretty ("__" ++ identifier ++ "__eq")

-- | Pretty print a reference expression
ppCReferenceExpression :: DocStyle -> DocStyle
ppCReferenceExpression expr = pretty "&" <> expr

-- | Pretty print a dereference expression
ppCDereferenceExpression :: DocStyle -> DocStyle
ppCDereferenceExpression expr = pretty "*" <> expr
