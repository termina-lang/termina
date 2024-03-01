module PPrinter.Common where

import           Data.Maybe
import           Prettyprinter
import           Prettyprinter.Render.Terminal
import           AST.Seman

import           Semantic.Monad

type DocStyle = Doc AnsiStyle

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
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
getExpPrecedence (IsEnumVariantExpression {}) = 7
getExpPrecedence (IsOptionVariantExpression {}) = 7

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
pool, msgQueue, optionDyn, dynamicStruct, taskID, resourceID, periodicTimer, sinkPort, inPort, outPort :: DocStyle
pool = namefy $ pretty "termina" <::> pretty "pool_t"
msgQueue = namefy $ pretty "termina" <::> pretty "msg_queue_t"
optionDyn = namefy $ pretty "option" <::> pretty "dyn_t"
dynamicStruct = namefy $ pretty "termina" <::> pretty "dyn_t"
taskID = namefy $ pretty "termina" <::> pretty "task_t"
resourceID = namefy $ pretty "termina" <::> pretty "resource_t"
periodicTimer = pretty "PeriodicTimer"
sinkPort = namefy $ pretty "termina" <::> pretty "sink_port_t"
inPort = namefy $ pretty "termina" <::> pretty "in_port_t"
outPort = namefy $ pretty "termina" <::> pretty "out_port_t"

ppDimensionOptionTS :: TypeSpecifier -> DocStyle
ppDimensionOptionTS (Vector ts (K size)) = pretty "__" <> pretty size <> ppDimensionOptionTS ts
ppDimensionOptionTS _ = emptyDoc

ppOptionSomeParameterStructName :: TypeSpecifier -> DocStyle
ppOptionSomeParameterStructName Bool = namefy $ pretty "option" <::> pretty "bool_params_t"
ppOptionSomeParameterStructName Char = namefy $ pretty "option" <::> pretty "char_params_t"
ppOptionSomeParameterStructName UInt8 = namefy $ pretty "option" <::> pretty "uint8_params_t"
ppOptionSomeParameterStructName UInt16 = namefy $ pretty "option" <::> pretty "uint16_params_t"
ppOptionSomeParameterStructName UInt32 = namefy $ pretty "option" <::> pretty "uint32_params_t"
ppOptionSomeParameterStructName UInt64 = namefy $ pretty "option" <::> pretty "uint64_params_t"
ppOptionSomeParameterStructName Int8 = namefy $ pretty "option" <::> pretty "int8_params_t"
ppOptionSomeParameterStructName Int16 = namefy $ pretty "option" <::> pretty "int16_params_t"
ppOptionSomeParameterStructName Int32 = namefy $ pretty "option" <::> pretty "int32_params_t"
ppOptionSomeParameterStructName Int64 = namefy $ pretty "option" <::> pretty "int64_params_t"
ppOptionSomeParameterStructName ts@(Option _) = error $ "invalid recursive option type: " ++ show ts
ppOptionSomeParameterStructName ts = namefy $ pretty "option" <::> ppTypeSpecifier' ts <> ppDimensionOptionTS ts <> pretty "_params_t"
  where
    ppTypeSpecifier' UInt8 = pretty "uint8"
    ppTypeSpecifier' UInt16 = pretty "uint16"
    ppTypeSpecifier' UInt32 = pretty "uint32"
    ppTypeSpecifier' UInt64 = pretty "uint64"
    ppTypeSpecifier' Int8 = pretty "int8"
    ppTypeSpecifier' Int16 = pretty "int16"
    ppTypeSpecifier' Int32 = pretty "int32"
    ppTypeSpecifier' Int64 = pretty "int64"
    ppTypeSpecifier' (Vector ts' _) = ppTypeSpecifier' ts'
    ppTypeSpecifier' (DefinedType typeIdentifier) = pretty typeIdentifier
    ppTypeSpecifier' ts' = error $ "invalid option type specifier :" ++ show ts'

ppOptionSomeParameterStruct :: TypeSpecifier -> DocStyle
ppOptionSomeParameterStruct ts =
  typedefC <+> structC <+> braces' 
    (indentTab . align $ 
      (ppTypeSpecifier ts <+> namefy (pretty (show (0 :: Integer))) <> ppDimension ts <> semi))
    <+> ppOptionSomeParameterStructName ts <> semi

ppOptionStruct :: TypeSpecifier -> DocStyle
ppOptionStruct ts = 
    typedefC <+> structC <+> braces' (line <> 
      (indentTab . align $ vsep [
          ppOptionSomeParameterStructName ts <+> optionSomeVariant <> semi,
          emptyDoc,
          enumIdentifier (pretty "option") <+> enumVariantsField <> semi,
          emptyDoc
      ])) <+> ppOptionStructName ts <> semi

-- | Pretty prints the name of the option type
ppOptionStructName :: TypeSpecifier -> DocStyle
ppOptionStructName Bool = namefy $ pretty "option" <::> pretty "bool_t"
ppOptionStructName Char = namefy $ pretty "option" <::> pretty "char_t"
ppOptionStructName UInt8 = namefy $ pretty "option" <::> ppTypeSpecifier UInt8
ppOptionStructName UInt16 = namefy $ pretty "option" <::> ppTypeSpecifier UInt16
ppOptionStructName UInt32 = namefy $ pretty "option" <::> ppTypeSpecifier UInt32
ppOptionStructName UInt64 = namefy $ pretty "option" <::> ppTypeSpecifier UInt64
ppOptionStructName Int8 = namefy $ pretty "option" <::> ppTypeSpecifier Int8
ppOptionStructName Int16 = namefy $ pretty "option" <::> ppTypeSpecifier Int16
ppOptionStructName Int32 = namefy $ pretty "option" <::> ppTypeSpecifier Int32
ppOptionStructName Int64 = namefy $ pretty "option" <::> ppTypeSpecifier Int64
ppOptionStructName ts@(Option _) = error $ "invalid recursive option type: " ++ show ts
ppOptionStructName ts = namefy $ pretty "option" <::> ppTypeSpecifier' ts <> ppDimensionOptionTS ts <> pretty "_t"
  where
    ppTypeSpecifier' UInt8 = pretty "uint8"
    ppTypeSpecifier' UInt16 = pretty "uint16"
    ppTypeSpecifier' UInt32 = pretty "uint32"
    ppTypeSpecifier' UInt64 = pretty "uint64"
    ppTypeSpecifier' Int8 = pretty "int8"
    ppTypeSpecifier' Int16 = pretty "int16"
    ppTypeSpecifier' Int32 = pretty "int32"
    ppTypeSpecifier' Int64 = pretty "int64"
    ppTypeSpecifier' (Vector ts' _) = ppTypeSpecifier' ts'
    ppTypeSpecifier' (DefinedType typeIdentifier) = pretty typeIdentifier
    ppTypeSpecifier' ts' = error $ "invalid option type specifier :" ++ show ts'

-- | Pretty prints the ID field of the resource, task and handler classes
ppResourceClassIDField, ppTaskClassIDField :: DocStyle
ppResourceClassIDField = pretty "__resource"
ppTaskClassIDField = pretty "__task"

ppInterfaceThatField :: DocStyle
ppInterfaceThatField = pretty "__that"

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
ppTypeSpecifier (Option ts)                  = ppOptionStructName ts
-- Non-primitive types:
-- | Dynamic subtype
ppTypeSpecifier (DynamicSubtype _)           = dynamicStruct
-- | Pool type
ppTypeSpecifier (Pool _ _)                   = pool
ppTypeSpecifier (MsgQueue _ _)               = msgQueue
ppTypeSpecifier (Location ts)                = volatileC <+> ppTypeSpecifier ts <+> pretty "*"
ppTypeSpecifier (AccessPort ts)              = ppTypeSpecifier ts
ppTypeSpecifier (Allocator _)                = pool <+> pretty "*"
-- | Type of the ports
ppTypeSpecifier (SinkPort {})                = sinkPort
ppTypeSpecifier (OutPort {})                 = outPort
ppTypeSpecifier (InPort {})                  = inPort
ppTypeSpecifier t                            = error $ "unsupported type: " ++ show t

ppDimension :: TypeSpecifier -> DocStyle
ppDimension (Vector ts (K size)) = brackets (pretty size) <> ppDimension ts
ppDimension _                    = emptyDoc

ppSizeOf :: TypeSpecifier -> DocStyle
ppSizeOf (Vector ts (K size)) = ppSizeOf ts <+> pretty "*" <+> pretty size
ppSizeOf ts = sizeofC (ppTypeSpecifier ts)

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

poolMethodName :: Identifier -> DocStyle
poolMethodName mName = classFunctionName (namefy $ pretty "termina" <::> pretty "pool") (pretty mName)

msgQueueMethodName :: Identifier -> DocStyle
msgQueueMethodName mName = classFunctionName (namefy $ pretty "termina" <::> pretty "msg_queue") (pretty mName)

resourceLock :: DocStyle
resourceLock = classFunctionName (namefy $ pretty "termina" <::> pretty "resource") (pretty "lock")

resourceUnlock :: DocStyle
resourceUnlock = classFunctionName (namefy $ pretty "termina" <::> pretty "resource") (pretty "unlock")

ppCFunctionPointer :: DocStyle -> DocStyle -> [DocStyle] -> DocStyle
ppCFunctionPointer ts identifier parameters = 
  ts <+> parens (pretty "*" <> identifier) <> parens (align (fillSep (punctuate comma parameters)))

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

ppCSwitchCase :: DocStyle -> DocStyle -> DocStyle
ppCSwitchCase expr body = pretty "case" <+> expr <> colon <> (line <> (indentTab . align) body)

ppCDefaultSwitchCase :: DocStyle -> DocStyle
ppCDefaultSwitchCase body = pretty "default" <> colon <> (line <> (indentTab . align) body)

ppCSwitchBlock ::
  -- | Switch expression
  DocStyle
  -- | Cases
  -> [DocStyle] ->
  DocStyle
ppCSwitchBlock expr cases =
  pretty "switch" <+> parens expr <+> braces' (line <> (indentTab . align) (vsep cases))