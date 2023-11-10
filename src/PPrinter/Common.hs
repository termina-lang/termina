module PPrinter.Common where

import           Data.Maybe
import           Prettyprinter
import           Prettyprinter.Render.Terminal
import           AST.Seman

import           Semantic.Monad

type DocStyle = Doc AnsiStyle

getObjectType :: Object SemanticAnns -> TypeSpecifier
getObjectType (Variable _ (SemAnn _ (ETy (ObjectType _ ts))))                  = ts
getObjectType (VectorIndexExpression _ _ (SemAnn _ (ETy (ObjectType _ ts))))   = ts
getObjectType (MemberAccess _ _ (SemAnn _ (ETy (ObjectType _ ts))))            = ts
getObjectType (Dereference _ (SemAnn _ (ETy (ObjectType _ ts))))               = ts
getObjectType (VectorSliceExpression _ _ _ (SemAnn _ (ETy (ObjectType _ ts)))) = ts
getObjectType (Undyn _ (SemAnn _ (ETy (ObjectType _ ts))))                     = ts
getObjectType (DereferenceMemberAccess _ _ (SemAnn _ (ETy (ObjectType _ ts)))) = ts
getObjectType ann = error $ "invalid object annotation: " ++ show ann

getType :: Expression SemanticAnns -> TypeSpecifier
getType (AccessObject obj) = getObjectType obj
getType (Constant _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (OptionVariantExpression _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (BinOp _ _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (ReferenceExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (Casting _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (FunctionExpression _ _ (SemAnn _ (ETy (AppType _ ts)))) = ts
getType (MemberFunctionAccess _ _ _ (SemAnn _ (ETy (AppType _ ts)))) = ts
getType (FieldAssignmentsExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (EnumVariantExpression _ _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType (VectorInitExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = ts
getType _ = error "invalid expression annotation"

getBinOpPrecedence :: Op -> Int
getBinOpPrecedence Multiplication = 3
getBinOpPrecedence Division = 3
getBinOpPrecedence Modulo = 3
getBinOpPrecedence Addition = 4
getBinOpPrecedence Subtraction = 4
getBinOpPrecedence BitwiseAnd = 8
getBinOpPrecedence BitwiseOr = 10
getBinOpPrecedence BitwiseXor = 9
getBinOpPrecedence LogicalAnd = 11
getBinOpPrecedence LogicalOr = 12
getBinOpPrecedence RelationalEqual = 7
getBinOpPrecedence RelationalNotEqual = 7
getBinOpPrecedence RelationalLT = 6
getBinOpPrecedence RelationalGT = 6
getBinOpPrecedence RelationalLTE = 6
getBinOpPrecedence RelationalGTE = 6
getBinOpPrecedence BitwiseLeftShift = 5
getBinOpPrecedence BitwiseRightShift = 5

getObjPrecedence :: Object SemanticAnns -> Int
getObjPrecedence (Variable _ _) = 0
getObjPrecedence (VectorIndexExpression {}) = 1
getObjPrecedence (MemberAccess {}) = 1
getObjPrecedence (DereferenceMemberAccess {}) = 1
getObjPrecedence obj@(Dereference _ _) =
  case getObjectType obj of
    (Vector _ _) -> 1
    _            -> 2
getObjPrecedence (VectorSliceExpression {}) = 2
getObjPrecedence (Undyn obj _) = getObjPrecedence obj

getExpPrecedence :: Expression SemanticAnns -> Int
getExpPrecedence (AccessObject obj) = getObjPrecedence obj
getExpPrecedence (Constant _ _) = 0
getExpPrecedence (OptionVariantExpression _ _) = 0
getExpPrecedence (BinOp op _ _ _) = getBinOpPrecedence op
getExpPrecedence (ReferenceExpression {}) = 2
getExpPrecedence (Casting {}) = 2
getExpPrecedence (FunctionExpression {}) = 0
getExpPrecedence (MemberFunctionAccess {}) = 1
getExpPrecedence (DerefMemberFunctionAccess {}) = 1
getExpPrecedence (FieldAssignmentsExpression {}) = 0
getExpPrecedence (EnumVariantExpression {}) = 0
getExpPrecedence (VectorInitExpression {}) = 0

getParameters :: Expression SemanticAnns -> [Parameter]
getParameters (FunctionExpression _ _ (SemAnn _ (ETy (AppType params _)))) = params
getParameters ann = error $ "invalid expression annotation: " ++ show ann

braces' :: DocStyle -> DocStyle
braces' b = braces (line <> b <> line)

indentTab :: DocStyle -> DocStyle
indentTab = indent 4

-- |  This function is used to create the names of temporal variables
--  and symbols.
namefy :: DocStyle -> DocStyle
namefy = (pretty "__" <>)

(<::>) :: DocStyle -> DocStyle -> DocStyle
(<::>) id0 id1 = id0 <> pretty "__" <> id1

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

sizeofC :: DocStyle -> DocStyle
sizeofC ts = pretty "sizeof" <> parens ts

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

sizeTC :: DocStyle
sizeTC = pretty "size_t"

-- C pretty extern
externC :: DocStyle
externC = pretty "extern"

-- C pretty static
staticC :: DocStyle
staticC = pretty "static"

-- C pretty volatile
volatileC :: DocStyle
volatileC = pretty "volatile"

-- C attribute pragma
attribute :: DocStyle
attribute = pretty "__attribute__"

-- | Termina's pretty builtin types
pool, msgQueue, optionDyn, dynamicStruct, taskID, resourceID, handlerID :: DocStyle
pool = namefy $ pretty "termina_pool_t"
msgQueue = namefy $ pretty "termina_msg_queue_t"
optionDyn = namefy $ pretty "termina_option_dyn_t"
dynamicStruct = namefy $ pretty "termina_dyn_t"
taskID = namefy $ pretty "termina_task_t"
resourceID = namefy $ pretty "termina_resource_t"
handlerID = namefy $ pretty "termina_handler_t"

-- | Pretty prints the ID field of the resource, task and handler classes
ppResourceClassIDField, ppTaskClassIDField, ppHandlerClassIDField :: DocStyle
ppResourceClassIDField = pretty "__resource_id"
ppTaskClassIDField = pretty "__task_id"
ppHandlerClassIDField = pretty "__handler_id"

-- | Pretty prints the name of the enum that defines the variants
-- of the enumeration
enumIdentifier :: DocStyle -> DocStyle
enumIdentifier identifier = namefy (pretty "enum_" <> identifier <> pretty "_t")

-- |  Pretty prints the name of the field that will store the variant
--  inside the struct corresponding to the enum.
enumVariantsField :: DocStyle
enumVariantsField = namefy $ pretty "variant"

optionSomeVariant, optionNoneVariant :: DocStyle
optionSomeVariant = pretty "Some"
optionNoneVariant = pretty "None"

optionSomeField :: DocStyle
optionSomeField = optionSomeVariant <> pretty ".__0"

poolMemoryArea :: DocStyle -> DocStyle
poolMemoryArea identifier = namefy $ pretty "pool_" <> identifier <> pretty "_memory"

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
ppTypeSpecifier USize                        = sizeTC
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
ppTypeSpecifier (MsgQueue _ _)               = msgQueue
ppTypeSpecifier (Location ts)                = volatileC <+> ppTypeSpecifier ts <+> pretty "*"
ppTypeSpecifier (Port ts)                    = ppTypeSpecifier ts <+> pretty "*"
ppTypeSpecifier t                            = error $ "unsupported type: " ++ show t

ppDimension :: TypeSpecifier -> DocStyle
ppDimension (Vector ts (K size)) = brackets (pretty size) <> ppDimension ts
ppDimension _                     = emptyDoc

ppReturnVectorValueStructure :: DocStyle -> DocStyle
ppReturnVectorValueStructure identifier =
  namefy $ pretty "ret_" <> identifier <> pretty "_t"

ppReturnVectorValueStructureDecl :: DocStyle -> TypeSpecifier -> DocStyle
ppReturnVectorValueStructureDecl identifier ts =
  typedefC <+> structC <+> braces' (
          indentTab . align $ ppTypeSpecifier ts <+> pretty "array" <> ppDimension ts <> semi)
      <+> ppReturnVectorValueStructure identifier <> semi

ppParameterVectorValueStructure :: DocStyle -> DocStyle -> DocStyle
ppParameterVectorValueStructure prefix identifier =
  namefy $ pretty "param_" <> prefix <> pretty "_" <> identifier <> pretty "_t"

-- | Pretty print the declaration of a structure that will be used to pass a vector as parameter
ppParameterVectorValueStructureDecl ::
  -- | prefix used as part of the function's name
  DocStyle
  -- | parameter identifier
  -> DocStyle
  -- | type specifier of the parameter
  -> TypeSpecifier -> DocStyle
ppParameterVectorValueStructureDecl prefix identifier ts =
  typedefC <+> structC <+> braces' (
          indentTab . align $ ppTypeSpecifier ts <+> pretty "array" <> ppDimension ts <> semi)
      <+> ppParameterVectorValueStructure prefix identifier <> semi

ppReturnType :: DocStyle -> TypeSpecifier -> DocStyle
ppReturnType identifier (Vector _ _) = ppReturnVectorValueStructure identifier
ppReturnType _ ts                    = ppTypeSpecifier ts

ppParameterDeclaration :: DocStyle -> Parameter -> DocStyle
ppParameterDeclaration _ (Parameter identifier (Reference _ ts)) =
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

classFunctionName :: DocStyle -> DocStyle -> DocStyle
classFunctionName = (<::>)

taskRunMethodName :: Identifier -> DocStyle
taskRunMethodName identifier = classFunctionName (pretty identifier) (pretty "run")

handlerHandleMethodName :: Identifier -> DocStyle
handlerHandleMethodName identifier = classFunctionName (pretty identifier) (pretty "handle")

poolMethodName :: Identifier -> DocStyle
poolMethodName mName = classFunctionName (namefy $ pretty "termina") (pretty "pool_" <> pretty mName)

msgQueueMethodName :: Identifier -> DocStyle
msgQueueMethodName mName = classFunctionName (namefy $ pretty "termina") (pretty "msg_queue_" <> pretty mName)

resourceLock :: DocStyle
resourceLock = classFunctionName (namefy $ pretty "termina") (pretty "resource_lock")

resourceUnlock :: DocStyle
resourceUnlock = classFunctionName (namefy $ pretty "termina") (pretty "resource_unlock")

-- | Prints the name of the function that frees an objet to the pool
poolFree :: DocStyle
poolFree = poolMethodName "free"

-- | Pretty prints a C reference expression
ppCReferenceExpression :: DocStyle -> DocStyle
ppCReferenceExpression expr = pretty "&" <> expr

-- | Pretty print a dereference expression
-- TODO: Remove parens and let the upper level decide if they are necessary
ppCDereferenceExpression :: DocStyle -> DocStyle
ppCDereferenceExpression expr = pretty "*" <> expr

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

ppCInfiniteLoop :: DocStyle -> DocStyle
ppCInfiniteLoop body =
  pretty "for"
    <+> parens (semi <> semi)
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
