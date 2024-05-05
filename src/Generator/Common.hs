{-# LANGUAGE FlexibleContexts #-}

module Generator.Common where

import AST.Seman
import Semantic.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map
import Data.Set
import Generator.LanguageC.AST
import Data.Char
import Numeric

newtype CGeneratorError = InternalError String
    deriving (Show)

type Substitutions = Map Identifier CExpression
type OptionTypes = Map TypeSpecifier (Set TypeSpecifier)

type CSourceGenerator = ReaderT Substitutions (Either CGeneratorError)
type CHeaderGenerator = ReaderT OptionTypes (Either CGeneratorError)

-- |  This function is used to create the names of temporal variables
--  and symbols.
namefy :: Identifier -> Identifier
namefy = ("__" <>)

(<::>) :: Identifier -> Identifier -> Identifier
(<::>) id0 id1 = id0 <> "__" <> id1

-- | Termina's pretty builtin types
pool, msgQueue, optionDyn, dynamicStruct, taskID, resourceID, sinkPort, inPort, outPort :: Identifier
pool = namefy "termina_pool_t"
msgQueue = namefy "termina_msg_queue_t"
optionDyn = namefy "option_dyn_t"
dynamicStruct = namefy "termina_dyn_t"
taskID = namefy "termina_task_t"
resourceID = namefy "termina_resource_t"
sinkPort = namefy "termina_sink_port_t"
inPort = namefy "termina_in_port_t"
outPort = namefy "termina_out_port_t"

poolMethodName :: Identifier -> Identifier
poolMethodName mName = namefy "termina_pool" <::> mName

poolMemoryArea :: Identifier -> Identifier
poolMemoryArea identifier = namefy $ "pool_" <> identifier <> "_memory"

msgQueueMethodName :: Identifier -> Identifier
msgQueueMethodName mName = namefy "termina_msg_queue" <::> mName

resourceLock, resourceUnlock :: Identifier
resourceLock = namefy "termina_resource" <::> "lock"
resourceUnlock = namefy "termina_resource" <::> "unlock"

thatField, thisParam, selfParam :: Identifier
thatField = "__that"
thisParam = "__this"
selfParam = "self"

resourceClassIDField, taskClassIDField :: Identifier
resourceClassIDField = "__resource"
taskClassIDField = "__task"

genEnumStructName :: (MonadError CGeneratorError m) => Identifier -> m Identifier
genEnumStructName identifier = return $ namefy $ "enum_" <> identifier <> "_t"

genEnumVariantName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genEnumVariantName enumId variant = return $ enumId <::> variant

genEnumParameterStructName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genEnumParameterStructName enumId variant = return $ namefy $ "enum_" <> enumId <::> variant <> "_params_t"

genClassFunctionName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genClassFunctionName className functionName = return $ className <::> functionName

-- | This function returns the name of the struct that represents the parameters
-- of an option type. 
genOptionParameterStructName :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
genOptionParameterStructName Bool = return $ namefy "option_bool_params_t"
genOptionParameterStructName Char = return $ namefy "option_char_params_t"
genOptionParameterStructName UInt8 = return $ namefy "option_uint8_params_t"
genOptionParameterStructName UInt16 = return $ namefy "option_uint16_params_t"
genOptionParameterStructName UInt32 = return $ namefy "option_uint32_params_t"
genOptionParameterStructName UInt64 = return $ namefy "option_uint64_params_t"
genOptionParameterStructName Int8 = return $ namefy "option_int8_params_t"
genOptionParameterStructName Int16 = return $ namefy "option_int16_params_t"
genOptionParameterStructName Int32 = return $ namefy "option_int32_params_t"
genOptionParameterStructName Int64 = return $ namefy "option_int64_params_t"
genOptionParameterStructName ts@(Option _) = throwError $ InternalError $ "invalid recursive option type: " ++ show ts
genOptionParameterStructName (DynamicSubtype _) = return $ namefy "option_dyn_params_t"
genOptionParameterStructName ts = 
    case ts of
        Array {} -> do
            tsName <- genTypeSpecName ts
            tsDimension <- genDimensionOptionTS ts
            return $ namefy $ "option_" <> tsName <> "_" <> tsDimension <> "_params_t"
        _ -> do
            tsName <- genTypeSpecName ts
            return $ namefy $ "option_" <> tsName <> "_params_t"

    where
        genTypeSpecName :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
        genTypeSpecName UInt8 = return "uint8"
        genTypeSpecName UInt16 = return "uint16"
        genTypeSpecName UInt32 = return "uint32"
        genTypeSpecName UInt64 = return "uint64"
        genTypeSpecName Int8 = return "int8"
        genTypeSpecName Int16 = return "int16"
        genTypeSpecName Int32 = return "int32"
        genTypeSpecName Int64 = return "int64"
        genTypeSpecName (Array ts' _) = genTypeSpecName ts'
        genTypeSpecName (DefinedType typeIdentifier) = return typeIdentifier
        genTypeSpecName ts' = throwError $ InternalError $ "invalid option type specifier: " ++ show ts'

        genDimensionOptionTS :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
        genDimensionOptionTS (Array ts' (K (TInteger s _))) = (("_" <> show s) <>) <$> genDimensionOptionTS ts'
        genDimensionOptionTS _ = return ""

enumVariantsField :: Identifier
enumVariantsField = namefy "variant"

optionSomeVariant, optionNoneVariant :: Identifier
optionSomeVariant = "Some"
optionNoneVariant = "None"

optionSomeField :: Identifier
optionSomeField = "__0"

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
getObjectType :: (MonadError CGeneratorError m) => Object SemanticAnns -> m TypeSpecifier
getObjectType (Variable _ (SemAnn _ (ETy (ObjectType _ ts))))                  = return ts
getObjectType (ArrayIndexExpression _ _ (SemAnn _ (ETy (ObjectType _ ts))))    = return ts
getObjectType (MemberAccess _ _ (SemAnn _ (ETy (ObjectType _ ts))))            = return ts
getObjectType (Dereference _ (SemAnn _ (ETy (ObjectType _ ts))))               = return ts
getObjectType (ArraySlice _ _ _ (SemAnn _ (ETy (ObjectType _ ts))))            = return ts
getObjectType (Undyn _ (SemAnn _ (ETy (ObjectType _ ts))))                     = return ts
getObjectType (DereferenceMemberAccess _ _ (SemAnn _ (ETy (ObjectType _ ts)))) = return ts
getObjectType ann = throwError $ InternalError $ "invalid object annotation: " ++ show ann

getConstParameters :: (MonadError CGeneratorError m) => Expression SemanticAnns -> m [ConstParameter]
getConstParameters (FunctionExpression _ _ _ (SemAnn _ (ETy (AppType constParams _ _)))) = return constParams
getConstParameters ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

getParameters :: (MonadError CGeneratorError m) => Expression SemanticAnns -> m [Parameter]
getParameters (FunctionExpression _ _ _ (SemAnn _ (ETy (AppType _ params _)))) = return params
getParameters ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

getExprType :: (MonadError CGeneratorError m) => Expression SemanticAnns -> m TypeSpecifier
getExprType (AccessObject obj) = getObjectType obj
getExprType (Constant _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (OptionVariantExpression _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (BinOp _ _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (ReferenceExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (Casting _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (FunctionExpression _ _ _ (SemAnn _ (ETy (AppType _ _ ts)))) = return $ ts
getExprType (MemberFunctionAccess _ _ _ _ (SemAnn _ (ETy (AppType _ _ ts)))) = return $ ts
getExprType (FieldAssignmentsExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (EnumVariantExpression _ _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (ArrayInitExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

-- | Generates the name of the option struct type
genOptionStructName :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
genOptionStructName Bool = return $ namefy "option_bool_t"
genOptionStructName Char = return $ namefy "option_char_t"
genOptionStructName UInt8 = return $ namefy "option_uint8_t"
genOptionStructName UInt16 = return $ namefy "option_uint16_t"
genOptionStructName UInt32 = return $ namefy "option_uint32_t"
genOptionStructName UInt64 = return $ namefy "option_uint64_t"
genOptionStructName Int8 = return $ namefy "option_int8_t"
genOptionStructName Int16 = return $ namefy "option_int16_t"
genOptionStructName Int32 = return $ namefy "option_int32_t"
genOptionStructName Int64 = return $ namefy "option_int64_t"
genOptionStructName ts@(Option _) = throwError $ InternalError $ "invalid recursive option type: " ++ show ts
genOptionStructName (DynamicSubtype _) = return optionDyn
genOptionStructName ts =
    case ts of
        Array {} -> do
            tsName <- genTypeSpecName ts
            tsDimension <- genDimensionOptionTS ts
            return $ namefy $ "option_" <> tsName <> "_" <> tsDimension <> "_t"
        _ -> do
            tsName <- genTypeSpecName ts
            return $ namefy $ "option_" <> tsName <> "_t"

    where
        genTypeSpecName :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
        genTypeSpecName UInt8 = return "uint8"
        genTypeSpecName UInt16 = return "uint16"
        genTypeSpecName UInt32 = return "uint32"
        genTypeSpecName UInt64 = return "uint64"
        genTypeSpecName Int8 = return "int8"
        genTypeSpecName Int16 = return "int16"
        genTypeSpecName Int32 = return "int32"
        genTypeSpecName Int64 = return "int64"
        genTypeSpecName (Array ts' _) = genTypeSpecName ts'
        genTypeSpecName (DefinedType typeIdentifier) = return typeIdentifier
        genTypeSpecName ts' = throwError $ InternalError $ "invalid option type specifier: " ++ show ts'

        genDimensionOptionTS :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
        genDimensionOptionTS (Array ts' (K (TInteger s _))) = (("_" <> show s) <>) <$> genDimensionOptionTS ts'
        genDimensionOptionTS _ = return ""

-- | Obtains the corresponding C type of a primitive type
genDeclSpecifiers :: (MonadError CGeneratorError m) => TypeSpecifier -> m [CDeclarationSpecifier]
-- |  Unsigned integer types
genDeclSpecifiers UInt8  = return [CTypeSpec CUInt8Type]
genDeclSpecifiers UInt16 = return [CTypeSpec CUInt16Type]
genDeclSpecifiers UInt32 = return [CTypeSpec CUInt32Type]
genDeclSpecifiers UInt64 = return [CTypeSpec CUInt64Type]
-- | Signed integer types
genDeclSpecifiers Int8   = return [CTypeSpec CInt8Type]
genDeclSpecifiers Int16  = return [CTypeSpec CInt16Type]
genDeclSpecifiers Int32  = return [CTypeSpec CInt32Type]
genDeclSpecifiers Int64  = return [CTypeSpec CInt64Type]
-- | Other primitive types
genDeclSpecifiers USize  = return [CTypeSpec CSizeTType]
genDeclSpecifiers Bool   = return [CTypeSpec CBoolType]
genDeclSpecifiers Char   = return [CTypeSpec CCharType]
-- | Primitive type
genDeclSpecifiers (DefinedType typeIdentifier) = return [CTypeSpec $ CTypeDef typeIdentifier]
-- | Array type
-- The type of the vector is the type of the elements
genDeclSpecifiers (Array ts _) = genDeclSpecifiers ts
-- | Option type
genDeclSpecifiers (Option (DynamicSubtype _))  = return [CTypeSpec $ CTypeDef optionDyn]
genDeclSpecifiers (Option ts)                  = do
    optName <- genOptionStructName ts
    return [CTypeSpec $ CTypeDef optName]
-- Non-primitive types:
-- | Dynamic subtype
genDeclSpecifiers (DynamicSubtype _)           = return [CTypeSpec $ CTypeDef dynamicStruct]
-- | Pool type
genDeclSpecifiers (Pool _ _)                   = return [CTypeSpec $ CTypeDef pool]
genDeclSpecifiers (MsgQueue _ _)               = return [CTypeSpec $ CTypeDef msgQueue]
genDeclSpecifiers (Location ts)                = genDeclSpecifiers ts
genDeclSpecifiers (AccessPort ts)              = genDeclSpecifiers ts
genDeclSpecifiers (Allocator _)                = return [CTypeSpec $ CTypeDef pool]
-- | Type of the ports
genDeclSpecifiers (SinkPort {})                = return [CTypeSpec $ CTypeDef sinkPort]
genDeclSpecifiers (OutPort {})                 = return [CTypeSpec $ CTypeDef outPort]
genDeclSpecifiers (InPort {})                  = return [CTypeSpec $ CTypeDef inPort]
genDeclSpecifiers (Reference Immutable ts)     = do
    tsDecl <- genDeclSpecifiers ts
    return $ CTypeQual CConstQual : tsDecl
genDeclSpecifiers (Reference _ ts)             = genDeclSpecifiers ts
genDeclSpecifiers t                            = throwError $ InternalError $ "Unsupported type: " ++ show t

genInteger :: TInteger -> CInteger
genInteger (TInteger i DecRepr) = CInteger i CDecRepr
genInteger (TInteger i HexRepr) = CInteger i CHexRepr
genInteger (TInteger i OctalRepr) = CInteger i COctalRepr

genArraySize :: (MonadError CGeneratorError m) => Size -> SemanticAnns -> m CExpression
genArraySize (K s) ann = return $ CConst (CIntConst (genInteger s)) (buildGenericAnn ann)
genArraySize (V v) ann = return $ CVar v (buildGenericAnn ann)

genArraySizeDeclarator :: (MonadError CGeneratorError m) => TypeSpecifier -> SemanticAnns -> m [CDerivedDeclarator]
genArraySizeDeclarator (Array ts arraySize) ann = do
    let cAnn = buildGenericAnn ann
    cSize <- genArraySize arraySize ann
    rest <- genArraySizeDeclarator ts ann
    return $ CArrDeclr [] (CArrSize False cSize) cAnn : rest
genArraySizeDeclarator (Reference _ (Array ts arraySize)) ann = do
    let cAnn = buildGenericAnn ann 
    cSize <- genArraySize arraySize ann
    rest <- genArraySizeDeclarator ts ann
    return $ CArrDeclr [] (CArrSize False cSize) cAnn : rest
genArraySizeDeclarator _ _ = return []

genParameterDeclaration :: (MonadError CGeneratorError m) => SemanticAnns -> Parameter -> m CDeclaration
genParameterDeclaration ann (Parameter identifier (Reference accKind ts)) = do
    declSpec <- genDeclSpecifiers ts
    arrayDecl <- genArraySizeDeclarator ts ann
    let exprCAnn = buildGenericAnn ann
        decl = case accKind of
            Immutable -> CTypeQual CConstQual : declSpec
            _ -> declSpec
    case ts of
        Array {} -> do
            return $ CDeclaration decl [(Just (CDeclarator (Just identifier) arrayDecl [] exprCAnn), Nothing, Nothing)]
                (buildDeclarationAnn ann False)
        _ -> return $ CDeclaration decl [(Just (CDeclarator (Just identifier) [CPtrDeclr [] exprCAnn] [] exprCAnn), Nothing, Nothing)]
            (buildDeclarationAnn ann False)
genParameterDeclaration ann (Parameter identifier ts) = do
    let exprCAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return $ CDeclaration decl [(Just (CDeclarator (Just identifier) [] [] exprCAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann False)

genCastDeclaration :: (MonadError CGeneratorError m) => TypeSpecifier -> SemanticAnns -> m CDeclaration
genCastDeclaration (DynamicSubtype ts@(Array _ _)) ann = do
    -- We must obtain the declaration specifier of the vector
    specs <- genDeclSpecifiers ts
    decls <- genPtrArrayDeclarator ts ann
    return $ CDeclaration specs [(Just decls, Nothing, Nothing)] declAnn

    where

        cAnn, declAnn :: CAnns
        cAnn = buildGenericAnn ann
        declAnn = buildDeclarationAnn ann False

        genPtrArrayDeclarator :: (MonadError CGeneratorError m) => TypeSpecifier -> SemanticAnns -> m CDeclarator
        genPtrArrayDeclarator (Array ts' _) ann' = do
            arrayDecl <- genArraySizeDeclarator ts' ann'
            return $ CDeclarator Nothing (CPtrDeclr [] cAnn : arrayDecl) [] cAnn
        genPtrArrayDeclarator ts' _ = throwError $ InternalError $ "Invalid type specifier, not an array: " ++ show ts'

genCastDeclaration (DynamicSubtype ts) ann = do
    let cAnn = buildGenericAnn ann
        declAnn = buildDeclarationAnn ann False
    specs <- genDeclSpecifiers ts
    return $ CDeclaration specs [(Just (CDeclarator Nothing [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] declAnn
genCastDeclaration (Location ts) ann = do
    let cAnn = buildGenericAnn ann
        declAnn = buildDeclarationAnn ann False
    specs <- genDeclSpecifiers ts
    return $ CDeclaration (CTypeQual CVolatQual : specs) [(Just (CDeclarator Nothing [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] declAnn
genCastDeclaration ts ann = do
    let declAnn = buildDeclarationAnn ann False
    specs <- genDeclSpecifiers ts
    return $ CDeclaration specs [] declAnn

buildGenericAnn :: SemanticAnns -> CAnns
buildGenericAnn ann = CAnnotations (Semantic.Monad.location ann) CGenericAnn

buildStatementAnn :: SemanticAnns -> Bool -> CAnns
buildStatementAnn ann before = CAnnotations (Semantic.Monad.location ann) (CStatementAnn before False)

buildDeclarationAnn :: SemanticAnns -> Bool -> CAnns
buildDeclarationAnn ann before = CAnnotations (Semantic.Monad.location ann) (CDeclarationAnn before)

buildCompoundAnn :: SemanticAnns -> Bool -> Bool -> CAnns
buildCompoundAnn ann before trailing = CAnnotations (Semantic.Monad.location ann) (CCompoundAnn before trailing)

buildCPPDirectiveAnn :: SemanticAnns -> Bool -> CAnns
buildCPPDirectiveAnn ann before = CAnnotations (Semantic.Monad.location ann) (CPPDirectiveAnn before)

printIntegerLiteral :: TInteger -> String
printIntegerLiteral (TInteger i DecRepr) = show i
printIntegerLiteral (TInteger i HexRepr) = "0x" <> (toUpper <$> showHex i "")
printIntegerLiteral (TInteger i OctalRepr) = "0" <> showOct i ""

printTypedInteger :: TypeSpecifier -> TInteger -> String
printTypedInteger ts ti =
    case ts of
        UInt8 -> "UINT8_C(" <> printIntegerLiteral ti <> ")"
        UInt16 -> "UINT16_C(" <> printIntegerLiteral ti <> ")"
        UInt32 -> "UINT32_C(" <> printIntegerLiteral ti <> ")"
        UInt64 -> "UINT64_C(" <> printIntegerLiteral ti <> ")"
        Int8 -> "INT8_C(" <> printIntegerLiteral ti <> ")"
        Int16 -> "INT16_C(" <> printIntegerLiteral ti <> ")"
        Int32 -> "INT32_C(" <> printIntegerLiteral ti <> ")"
        Int64 -> "INT64_C(" <> printIntegerLiteral ti <> ")"
        _ -> error "Invalid type specifier: " <> show ts


printLiteral :: Const -> String
printLiteral (I ti (Just ts)) = printTypedInteger ts ti
printLiteral (I ti Nothing) = printIntegerLiteral ti
printLiteral (B True) = "1"
printLiteral (B False) = "0"
printLiteral (C c) = "'" <> [c] <> "'"
