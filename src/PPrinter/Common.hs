module PPrinter.Common where

import AST

import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.Maybe
import Semantic.Monad


type DocStyle = Doc AnsiStyle

getType :: Expression SemanticAnns -> TypeSpecifier
getType (Variable _ (SemAnn _ (ETy ts))) = ts
getType (Constant _ (SemAnn _ (ETy ts))) = ts 
getType (OptionVariantExpression _ (SemAnn _ (ETy ts))) = ts
getType (BinOp _ _ _ (SemAnn _ (ETy ts))) = ts
getType (ReferenceExpression _ (SemAnn _ (ETy ts))) = ts
getType (DereferenceExpression _ (SemAnn _ (ETy ts))) = ts
getType (Casting _ _ (SemAnn _ (ETy ts))) = ts
getType (FunctionExpression _ _ (SemAnn _ (ETy ts))) = ts
getType (FieldValuesAssignmentsExpression _ _ (SemAnn _ (ETy ts))) = ts
getType (EnumVariantExpression _ _ _ (SemAnn _ (ETy ts))) = ts
getType (VectorIndexExpression _ _ (SemAnn _ (ETy ts))) = ts
getType (VectorInitExpression _ _ (SemAnn _ (ETy ts))) = ts
getType (ParensExpression expr _) = getType expr
getType _ = error "invalid annotation"

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
pool, msgQueue, mutex, optionDyn :: DocStyle
pool = pretty "__termina_pool_t"
msgQueue = pretty "__termina_msg_queue_id_t"
mutex = pretty "__termina_mutex_id_t"
optionDyn = pretty "__Option_dyn_t"

enumIdentifier :: Identifier -> DocStyle
enumIdentifier identifier = pretty (namefy ("enum_" ++ identifier))

-- | Pretty prints the name of the field that will store the variant
-- inside the struct corresponding to the enum.
enumVariantsField :: DocStyle
enumVariantsField = pretty (namefy "variant")

-- | Pretty prints the corresponding C type of a primitive type
-- This function is used to pretty print the type of a variable
ppPrimitiveType :: TypeSpecifier -> DocStyle
-- | Unsigned integer types
ppPrimitiveType UInt8 = uint8C
ppPrimitiveType UInt16 = uint16C
ppPrimitiveType UInt32 = uint32C
ppPrimitiveType UInt64 = uint64C
-- | Signed integer types
ppPrimitiveType Int8 = int8C
ppPrimitiveType Int16 = int16C
ppPrimitiveType Int32 = int32C
ppPrimitiveType Int64 = int64C
ppPrimitiveType Bool = uint8C
ppPrimitiveType Char = charC
ppPrimitiveType (DefinedType typeIdentifier) = pretty typeIdentifier
-- | Vector type
-- The type of the vector is the type of the elements
ppPrimitiveType (Vector ts _) = ppPrimitiveType ts
-- | Option type
ppPrimitiveType (Option (DynamicSubtype _)) = optionDyn
-- Non-primitive types:
ppPrimitiveType _ = error "unsupported type"

ppDimension :: TypeSpecifier -> DocStyle
ppDimension (Vector ts (KC size)) = brackets (ppConst size) <> ppDimension ts
ppDimension _ = emptyDoc

ppReturnType :: TypeSpecifier -> DocStyle
ppReturnType (Vector ts _) = ppPrimitiveType ts <+> pretty "*"
ppReturnType ts = ppPrimitiveType ts

ppParameterDeclaration :: Parameter -> DocStyle
ppParameterDeclaration (Parameter identifier (Reference ts)) =
  case ts of
    (Vector _ _) -> ppPrimitiveType ts <+> pretty ("__ref__" ++ identifier) <> ppDimension ts
    _ -> ppPrimitiveType ts <+> pretty "*" <+> pretty identifier
ppParameterDeclaration (Parameter identifier ts) = ppPrimitiveType ts <+> pretty identifier <> ppDimension ts

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

methodName :: Identifier -> Identifier -> DocStyle
methodName identifier method = pretty("__" ++ identifier ++ "_" ++ method)

typeDefEqFunctionName :: Identifier -> DocStyle
typeDefEqFunctionName identifier = methodName identifier "_eq"

structAssignAnonymFunctionName :: Identifier -> DocStyle
structAssignAnonymFunctionName identifier = methodName identifier "_assign"

enumAssignAnonymFunctionName :: Identifier -> Identifier -> DocStyle
enumAssignAnonymFunctionName identifier variant = methodName identifier (variant ++ "__assign")

poolMethodName :: Identifier -> DocStyle
poolMethodName = methodName "pool"

msgQueueMethodName :: Identifier -> DocStyle
msgQueueMethodName = methodName "msg_queue"

-- | Pretty print a reference expression
ppCReferenceExpression :: DocStyle -> DocStyle
ppCReferenceExpression expr = pretty "&" <> expr

-- | Pretty print a dereference expression
ppCDereferenceExpression :: DocStyle -> DocStyle
ppCDereferenceExpression expr = pretty "*" <> parens expr

ppCForLoopInitExpression :: DocStyle -> DocStyle -> DocStyle -> DocStyle
ppCForLoopInitExpression ts identifier initValue = ts <+> identifier <+> pretty "=" <+> initValue

ppCForLoopIncrExpression :: DocStyle -> DocStyle -> DocStyle
ppCForLoopIncrExpression identifier increment = identifier <+> pretty "=" <+> identifier <+> pretty "+" <+> increment

ppCForLoop :: DocStyle -> DocStyle -> DocStyle -> DocStyle -> DocStyle
ppCForLoop initializer cond incr body =
    pretty "for" <+> parens
      (initializer <> semi <+> cond <> semi <+> incr) <+> braces' (
        (indentTab . align) body)

-- | This function recursively finds all the field values assignments expressions from
-- one of them
findFieldValuesAssignmentsExpressions :: Identifier -> Expression SemanticAnns -> [(Identifier, Expression SemanticAnns)]
findFieldValuesAssignmentsExpressions base expr =
    case expr of
        (FieldValuesAssignmentsExpression _ vas _) ->
            (base, expr) : concatMap (\(FieldValueAssignment identifier expr') ->
              findFieldValuesAssignmentsExpressions (base ++ "_" ++ identifier) expr') vas
        (EnumVariantExpression {}) -> error "unsupported"
        (VectorInitExpression expr' _ _) -> findFieldValuesAssignmentsExpressions base expr'
        _ -> []
