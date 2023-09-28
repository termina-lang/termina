module PPrinter.Common where

import           Data.Maybe
import           Prettyprinter
import           Prettyprinter.Render.Terminal
import           SemanAST
import           Semantic.Monad

type DocStyle = Doc AnsiStyle

getObjectType :: Object SemanticAnns -> TypeSpecifier
getObjectType (Variable _ (SemAnn _ (ETy (SimpleType ts))))                  = ts
getObjectType (VectorIndexExpression _ _ (SemAnn _ (ETy (SimpleType ts))))   = ts
getObjectType (MemberAccess _ _ (SemAnn _ (ETy (SimpleType ts))))            = ts
getObjectType (Dereference _ (SemAnn _ (ETy (SimpleType ts))))               = ts
getObjectType (VectorSliceExpression _ _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getObjectType (Undyn _ (SemAnn _ (ETy (SimpleType ts))))                     = ts
getObjectType (ParensObject _ (SemAnn _ (ETy (SimpleType ts))))              = ts
getObjectType obj = error $ "invalid object annotation: " ++ show obj

getType :: Expression SemanticAnns -> TypeSpecifier
getType (AccessObject obj) = getObjectType obj
getType (Constant _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (OptionVariantExpression _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (BinOp _ _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (ReferenceExpression _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (Casting _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (FunctionExpression _ _ (SemAnn _ (ETy (AppType _ ts)))) = ts
getType (MemberMethodAccess _ _ _ (SemAnn _ (ETy (AppType _ ts)))) = ts
getType (FieldValuesAssignmentsExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (EnumVariantExpression _ _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (VectorInitExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (ParensExpression expr _) = getType expr
getType _ = error "invalid expression annotation"

getParameters :: Expression SemanticAnns -> [Parameter]
getParameters (FunctionExpression _ _ (SemAnn _ (ETy (AppType params _)))) = params
getParameters ann = error $ "invalid expression annotation: " ++ show ann

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

-- |  This function is used to create the names of temporal variables
--  and symbols.
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
voidC = pretty "void"

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

-- C pretty bool
boolC :: DocStyle
boolC = pretty "_Bool"

-- C pretty extern
externC :: DocStyle
externC = pretty "extern"

-- C attribute pragma
attribute :: DocStyle
attribute = pretty "__attribute__"

-- | Termina's pretty builtin types
pool, msgQueue, mutex, optionDyn, dynamicStruct :: DocStyle
pool = pretty $ namefy "termina_pool_t"
msgQueue = pretty $ namefy "termina_msg_queue_id_t"
mutex = pretty $ namefy "termina_mutex_id_t"
optionDyn = pretty $ namefy "termina_option_dyn_t"
dynamicStruct = pretty $ namefy "termina_dyn_t"

enumIdentifier :: Identifier -> DocStyle
enumIdentifier identifier = pretty (namefy ("enum_" ++ identifier))

-- |  Pretty prints the name of the field that will store the variant
--  inside the struct corresponding to the enum.
enumVariantsField :: DocStyle
enumVariantsField = pretty (namefy "variant")

optionSomeVariant, optionNoneVariant :: DocStyle
optionSomeVariant = pretty "Some"
optionNoneVariant = pretty "None"

optionSomeField :: DocStyle
optionSomeField = pretty (namefy "Some") <> pretty ".__0"

-- | Pretty prints the corresponding C type of a primitive type
-- This function is used to pretty print the type of a variable
ppTypeSpecifier :: TypeSpecifier -> DocStyle
-- |  Unsigned integer types
ppTypeSpecifier UInt8                        = uint8C
ppTypeSpecifier UInt16                       = uint16C
ppTypeSpecifier UInt32                       = uint32C
ppTypeSpecifier UInt64                       = uint64C
-- | Signed integer types
ppTypeSpecifier Int8                         = int8C
ppTypeSpecifier Int16                        = int16C
ppTypeSpecifier Int32                        = int32C
ppTypeSpecifier Int64                        = int64C
ppTypeSpecifier Bool                         = boolC
ppTypeSpecifier Char                         = charC
ppTypeSpecifier (DefinedType typeIdentifier) = pretty typeIdentifier
-- | Vector type
-- The type of the vector is the type of the elements
ppTypeSpecifier (Vector ts _)                = ppTypeSpecifier ts
-- | Option type
ppTypeSpecifier (Option (DynamicSubtype _))  = optionDyn
-- Non-primitive types:
-- | Dynamic subtype
ppTypeSpecifier (DynamicSubtype _)           = dynamicStruct
-- | Pool type
ppTypeSpecifier (Pool _ _)                   = pool
ppTypeSpecifier t                            = error $ "unsupported type: " ++ show t

ppDimension :: TypeSpecifier -> DocStyle
ppDimension (Vector ts (KC size)) = brackets (ppConst size) <> ppDimension ts
ppDimension _                     = emptyDoc

ppReturnVectorValueStructure :: DocStyle -> DocStyle
ppReturnVectorValueStructure identifier =
  pretty "__ret_" <> identifier <> pretty "_t"

ppReturnVectorValueStructureDecl :: DocStyle -> TypeSpecifier -> DocStyle
ppReturnVectorValueStructureDecl identifier ts =
  typedefC <+> structC <+> braces' (
          indentTab . align $ ppTypeSpecifier ts <+> pretty "array" <> ppDimension ts <> semi)
      <+> ppReturnVectorValueStructure identifier <> semi

ppParameterVectorValueStructure :: DocStyle -> DocStyle -> DocStyle
ppParameterVectorValueStructure prefix identifier =
  pretty "__param_" <> prefix <> pretty "_" <> identifier <> pretty "_t"

ppParameterVectorValueStructureDecl :: DocStyle -> DocStyle -> TypeSpecifier -> DocStyle
ppParameterVectorValueStructureDecl prefix identifier ts =
  typedefC <+> structC <+> braces' (
          indentTab . align $ ppTypeSpecifier ts <+> pretty "array" <> ppDimension ts <> semi)
      <+> ppParameterVectorValueStructure prefix identifier <> semi

ppReturnType :: DocStyle -> TypeSpecifier -> DocStyle
ppReturnType identifier (Vector _ _) = ppReturnVectorValueStructure identifier
ppReturnType _ ts                    = ppTypeSpecifier ts

ppParameterDeclaration :: DocStyle -> Parameter -> DocStyle
ppParameterDeclaration _ (Parameter identifier (Reference ts)) =
  case ts of
    (Vector _ _) -> ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts
    _            -> ppTypeSpecifier ts <+> pretty "*" <+> pretty identifier
-- | If a vector is passed as parameter, the a structure must  be declared and used instead
-- This way, we guarantee that the array is copied by value when passed to the function
ppParameterDeclaration prefix (Parameter identifier (Vector _ _)) =
  ppParameterVectorValueStructure prefix (pretty identifier) <+> pretty identifier
ppParameterDeclaration _ (Parameter identifier ts) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts

-- | Pretty print a C function prototype
ppCFunctionPrototype ::
  -- | function identifier (name)
  DocStyle ->
  -- | list of parameters (possibly empty)
  [DocStyle] ->
  -- | type of the return value (optional)
  Maybe DocStyle ->
  DocStyle
ppCFunctionPrototype identifier parameters rTS =
  fromMaybe voidC rTS
    <+> identifier
      <> parens (align (fillSep (punctuate comma parameters)))

-- | Pretty print a C function call
ppCFunctionCall :: DocStyle -> [DocStyle] -> DocStyle
ppCFunctionCall identifier parameters =
  identifier
    <> parens (align (fillSep (punctuate comma parameters)))

ppConst :: Const -> DocStyle
ppConst (B b)         = if b then pretty "1" else pretty "0"
ppConst (I _ integer) = pretty integer
ppConst (C char)      = pretty "'" <> pretty char <> pretty "'"

ppModifier :: Modifier -> DocStyle
ppModifier (Modifier identifier (Just (KC c))) = pretty identifier <> parens (ppConst c)
ppModifier (Modifier identifier Nothing) = pretty identifier
-- TODO: Support modifiers with non-integer values
ppModifier m = error $ "unsupported modifier: " ++ show m

methodAccessOp :: AccessOp -> DocStyle
methodAccessOp (UserDef ident) = pretty ident
methodAccessOp Alloc = pretty "alloc"
methodAccessOp Send = pretty "send"
methodAccessOp Receive = pretty "receive"

methodNameAOp :: Identifier -> AccessOp -> DocStyle
methodNameAOp identifier = (pretty ("__" ++ identifier ++ "_") <>) . methodAccessOp

methodName :: Identifier -> Identifier -> DocStyle
methodName ident = (pretty ("__" ++ ident ++ "_") <>) . pretty

poolMethodNameAOp :: AccessOp -> DocStyle
poolMethodNameAOp = methodNameAOp "termina_pool"

poolMethodName :: String -> DocStyle
poolMethodName = methodName "termina_pool"

msgQueueMethodNameAOp :: AccessOp -> DocStyle
msgQueueMethodNameAOp = methodNameAOp "termina_msg_queue"

msgQueueMethodName :: String -> DocStyle
msgQueueMethodName = methodName "termina_msg_queue"

-- | Prints the name of the function that frees an objet to the pool
poolFree :: DocStyle
poolFree = poolMethodName "free"

-- | Pretty prints a C reference expression
ppCReferenceExpression :: DocStyle -> DocStyle
ppCReferenceExpression expr = pretty "&" <> expr

-- | Pretty print a dereference expression
-- TODO: Remove parens and let the upper level decide if they are necessary
ppCDereferenceExpression :: DocStyle -> DocStyle
ppCDereferenceExpression expr = pretty "*" <> parens expr

ppCForLoopInitExpression :: DocStyle -> DocStyle -> DocStyle -> DocStyle
ppCForLoopInitExpression ts identifier initValue = ts <+> identifier <+> pretty "=" <+> initValue

ppCForLoopIncrExpression :: DocStyle -> DocStyle -> DocStyle
ppCForLoopIncrExpression identifier increment = identifier <+> pretty "=" <+> identifier <+> pretty "+" <+> increment

ppCForLoop :: DocStyle -> DocStyle -> DocStyle -> DocStyle -> DocStyle
ppCForLoop initializer cond incr body =
  pretty "for"
    <+> parens
      (initializer <> semi <+> cond <> semi <+> incr)
    <+> braces'
      ((indentTab . align) body)

ppCIfBlock ::
  -- | Conditional expression
  DocStyle
  -- | Body
  -> DocStyle ->
  DocStyle
ppCIfBlock cond body =
  pretty "if" <+> parens cond <+> braces' (line <> (indentTab . align) body)

ppCElseIfBlock ::
  -- | Conditional expression
  DocStyle
  -- | Body
  -> DocStyle ->
  DocStyle
ppCElseIfBlock cond body =
  pretty "else" <+> pretty "if" <+> parens cond <+> braces' (line <> (indentTab . align) body)

ppCElseBlock ::
  -- | Body
  DocStyle -> DocStyle
ppCElseBlock body =
  pretty "else" <+> braces' (line <> (indentTab . align) body)
