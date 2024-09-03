{-# LANGUAGE FlexibleContexts #-}

module Generator.CCCodeGen.Common where

import AST.Seman
import Semantic.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map
import Data.Set
import Generator.LanguageC.CompCertC
import Data.Char
import Numeric
import Utils.Annotations
    ( Located(Located, location), Location(Internal) )

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
pool, msgQueue, optionBox, boxStruct, taskID, resourceID, sinkPort, inPort, outPort :: Identifier
pool = namefy "termina_pool_t"
msgQueue = namefy "termina_msg_queue_t"
optionBox = namefy "option_box_t"
boxStruct = namefy "termina_box_t"
taskID = namefy "termina_task_t"
resourceID = namefy "termina_resource_t"
sinkPort = namefy "termina_sink_port_t"
inPort = namefy "termina_in_port_t"
outPort = namefy "termina_out_port_t"

poolMethodName :: Identifier -> Identifier
poolMethodName mName = namefy "termina_pool" <::> mName

atomicMethodName :: Identifier -> Identifier
atomicMethodName mName = "atomic_" <> mName

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
genOptionParameterStructName (BoxSubtype _) = return $ namefy "option_box_params_t"
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
getObjType :: (MonadError CGeneratorError m) => Object SemanticAnn -> m TypeSpecifier
getObjType (Variable _ (Located (ETy (ObjectType _ ts)) _))                  = return ts
getObjType (ArrayIndexExpression _ _ (Located (ETy (ObjectType _ ts)) _))    = return ts
getObjType (MemberAccess _ _ (Located (ETy (ObjectType _ ts)) _))            = return ts
getObjType (Dereference _ (Located (ETy (ObjectType _ ts)) _))               = return ts
getObjType (ArraySlice _ _ _ (Located (ETy (ObjectType _ ts)) _))            = return ts
getObjType (Unbox _ (Located (ETy (ObjectType _ ts)) _))                     = return ts
getObjType (DereferenceMemberAccess _ _ (Located (ETy (ObjectType _ ts)) _)) = return ts
getObjType ann = throwError $ InternalError $ "invalid object annotation: " ++ show ann

getParameters :: (MonadError CGeneratorError m) => Expression SemanticAnn -> m [Parameter]
getParameters (FunctionCall _ _ (Located (ETy (AppType params _)) _)) = return params
getParameters ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

getExprType :: (MonadError CGeneratorError m) => Expression SemanticAnn -> m TypeSpecifier
getExprType (AccessObject obj) = getObjType obj
getExprType (Constant _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (OptionVariantInitializer _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (BinOp _ _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (ReferenceExpression _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (Casting _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (FunctionCall _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (MemberFunctionCall _ _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (DerefMemberFunctionCall _ _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (StructInitializer _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (EnumVariantInitializer _ _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayInitializer _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayExprListInitializer _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

getConstExprType :: (MonadError CGeneratorError m) => ConstExpression SemanticAnn -> m TypeSpecifier
getConstExprType (KC _ (Located (ETy (SimpleType ts)) _)) = return ts
getConstExprType (KV _ (Located (ETy (SimpleType ts)) _)) = return ts
getConstExprType ann = throwError $ InternalError $ "invalid constant expression annotation: " ++ show ann

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
genOptionStructName (BoxSubtype _) = return optionBox
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
    

getCInteger :: TInteger -> CInteger
getCInteger (TInteger i DecRepr) = CInteger i CDecRepr
getCInteger (TInteger i HexRepr) = CInteger i CHexRepr
getCInteger (TInteger i OctalRepr) = CInteger i COctalRepr

getArraySize :: Size -> CArraySize
getArraySize (K tint) = CArraySizeK (getCInteger tint)
getArraySize (V ident) = CArraySizeV ident

-- | Translate type annotation to C type
genType :: (MonadError CGeneratorError m) => CQualifier -> TypeSpecifier -> m CType
-- |  Unsigned integer types
genType qual UInt8 = return (CTInt IntSize8 Unsigned qual)
genType qual UInt16 = return (CTInt IntSize16 Unsigned qual)
genType qual UInt32 = return (CTInt IntSize32 Unsigned qual)
genType qual UInt64 = return (CTInt IntSize64 Unsigned qual)
-- | Signed integer types
genType qual Int8 = return (CTInt IntSize8 Signed qual)
genType qual Int16 = return (CTInt IntSize16 Signed qual)
genType qual Int32 = return (CTInt IntSize32 Signed qual)
genType qual Int64 = return (CTInt IntSize64 Signed qual)
-- | Other primitive typess
genType qual USize = return (CTSizeT qual)
genType qual Bool = return (CTBool qual)
genType qual Char = return (CTChar qual)
-- | Primitive type
genType qual (DefinedType typeIdentifier) = return (CTTypeDef typeIdentifier qual)
-- | Array type
genType qual (Array ts' s) = do
    ts <- genType qual ts'
    return (CTArray ts (getArraySize s))
-- | Option types
genType _qual (Option (BoxSubtype _)) = return (CTTypeDef optionBox noqual)
genType _qual (Option ts) = do
    optName <- genOptionStructName ts
    return (CTTypeDef optName noqual)
-- Non-primitive types:
-- | Box subtype
genType _qual (BoxSubtype _) = return (CTTypeDef boxStruct noqual)
-- | Pool type
genType _qual (Pool _ _) = return (CTTypeDef pool noqual)
genType _qual (MsgQueue _ _) = return (CTTypeDef msgQueue noqual)
genType qual (Location ts) = do
    ts' <- genType volatile ts
    return (CTPointer ts' qual)
genType _qual (AccessPort ts) = do
    ts' <- genType noqual ts
    return (CTPointer ts' noqual)
genType _qual (Allocator _) = return (CTTypeDef pool noqual)
genType _qual (Atomic ts) = genType atomic ts
genType _qual (AtomicArray ts _) = genType atomic ts
genType _qual (AtomicAccess ts) = do
    ts' <- genType atomic ts
    return (CTPointer ts' noqual)
genType _qual (AtomicArrayAccess ts _) = do
    ts' <- genType atomic ts
    return (CTPointer ts' noqual)
-- | Type of the ports
genType _qual (SinkPort {}) = return (CTTypeDef sinkPort noqual)
genType _qual (OutPort {}) = return (CTTypeDef outPort noqual)
genType _qual (InPort {}) = return (CTTypeDef inPort noqual)
genType qual (Reference Immutable ts) = do
    ts' <- genType qual{qual_const = True} ts
    return (CTPointer ts' constqual)
genType _qual (Reference _ ts) = do
    ts' <- genType noqual ts
    return (CTPointer ts' constqual)
genType qual (Slice ts) = do
    ts' <- genType qual{qual_const = True} ts
    return (CTPointer ts' constqual)
genType _noqual Unit = return CTVoid

{--
genParameterDeclaration :: (MonadError CGeneratorError m) => SemanticAnn -> Parameter -> m CDeclaration
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

genCastDeclaration :: (MonadError CGeneratorError m) => TypeSpecifier -> SemanticAnn -> m CDeclaration
genCastDeclaration (BoxSubtype ts@(Array _ _)) ann = do
    -- We must obtain the declaration specifier of the vector
    specs <- genDeclSpecifiers ts
    decls <- genPtrArrayDeclarator ts ann
    return $ CDeclaration specs [(Just decls, Nothing, Nothing)] declAnn

    where

        cAnn, declAnn :: CAnns
        cAnn = buildGenericAnn ann
        declAnn = buildDeclarationAnn ann False

        genPtrArrayDeclarator :: (MonadError CGeneratorError m) => TypeSpecifier -> SemanticAnn -> m CDeclarator
        genPtrArrayDeclarator (Array ts' _) ann' = do
            arrayDecl <- genArraySizeDeclarator ts' ann'
            return $ CDeclarator Nothing (CPtrDeclr [] cAnn : arrayDecl) [] cAnn
        genPtrArrayDeclarator ts' _ = throwError $ InternalError $ "Invalid type specifier, not an array: " ++ show ts'

genCastDeclaration (BoxSubtype ts) ann = do
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
--}
internalAnn :: CItemAnn -> CAnns
internalAnn = flip Located Internal

buildGenericAnn :: SemanticAnn -> CAnns
buildGenericAnn ann = Located CGenericAnn (location ann) 

buildStatementAnn :: SemanticAnn -> Bool -> CAnns
buildStatementAnn ann before = Located (CStatementAnn before False) (location ann) 

buildDeclarationAnn :: SemanticAnn -> Bool -> CAnns
buildDeclarationAnn ann before = Located (CDeclarationAnn before) (location ann) 

buildCompoundAnn :: SemanticAnn -> Bool -> Bool -> CAnns
buildCompoundAnn ann before trailing = Located (CCompoundAnn before trailing) (location ann) 

buildCPPDirectiveAnn :: SemanticAnn -> Bool -> CAnns
buildCPPDirectiveAnn ann before = Located (CPPDirectiveAnn before) (location ann) 

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
        -- | No correct way to do this for the time being
        USize -> printIntegerLiteral ti
        _ -> error "Invalid type specifier: " <> show ts


printLiteral :: Const -> String
printLiteral (I ti (Just ts)) = printTypedInteger ts ti
printLiteral (I ti Nothing) = printIntegerLiteral ti
printLiteral (B True) = "1"
printLiteral (B False) = "0"
printLiteral (C c) = "'" <> [c] <> "'"
