{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Common where

import ControlFlow.BasicBlocks.AST
import Semantic.Types
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map
import Data.Set
import Generator.LanguageC.AST
import Data.Char
import Numeric
import Utils.Annotations

newtype CGeneratorError = InternalError String
    deriving (Show)

type Substitutions = Map Identifier CObject
type OptionTypes = Map TerminaType (Set TerminaType)

type CSourceGenerator = ExceptT CGeneratorError (Reader Substitutions)
type CHeaderGenerator = ExceptT CGeneratorError (Reader OptionTypes)

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

msgQueueSendMethodName :: Identifier
msgQueueSendMethodName = namefy "termina_msg_queue" <::> "send"

resourceLock, resourceUnlock :: Identifier
resourceLock = namefy "termina_resource" <::> "lock"
resourceUnlock = namefy "termina_resource" <::> "unlock"

cResourceIDType, cResourceLockFuncType, cResourceUnlockFuncType :: CType
cResourceIDType = CTTypeDef resourceID noqual
cResourceLockFuncType = CTFunction (CTVoid noqual) [CTPointer (CTTypeDef resourceID noqual) noqual]
cResourceUnlockFuncType = CTFunction (CTVoid noqual) [CTPointer (CTTypeDef resourceID noqual) noqual]


thatField, thisParam, selfParam :: Identifier
thatField = "__that"
thisParam = "__this"
selfParam = "self"

resourceClassIDField, taskClassIDField, timerField :: Identifier
resourceClassIDField = "__resource"
taskClassIDField = "__task"
timerField = "__timer"

genEnumStructName :: (MonadError CGeneratorError m) => Identifier -> m Identifier
genEnumStructName identifier = return $ namefy $ "enum_" <> identifier <> "_t"

genEnumVariantName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genEnumVariantName enumId this_variant = return $ enumId <::> this_variant

genEnumParameterStructName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genEnumParameterStructName enumId this_variant = return $ namefy $ "enum_" <> enumId <::> this_variant <> "_params_t"

genClassFunctionName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genClassFunctionName className functionName = return $ className <::> functionName

-- | This function returns the name of the struct that represents the parameters
-- of an option type. 
genOptionParameterStructName :: (MonadError CGeneratorError m) => TerminaType -> m Identifier
genOptionParameterStructName TBool = return $ namefy "option_bool_params_t"
genOptionParameterStructName TChar = return $ namefy "option_char_params_t"
genOptionParameterStructName TUInt8 = return $ namefy "option_uint8_params_t"
genOptionParameterStructName TUInt16 = return $ namefy "option_uint16_params_t"
genOptionParameterStructName TUInt32 = return $ namefy "option_uint32_params_t"
genOptionParameterStructName TUInt64 = return $ namefy "option_uint64_params_t"
genOptionParameterStructName TInt8 = return $ namefy "option_int8_params_t"
genOptionParameterStructName TInt16 = return $ namefy "option_int16_params_t"
genOptionParameterStructName TInt32 = return $ namefy "option_int32_params_t"
genOptionParameterStructName TInt64 = return $ namefy "option_int64_params_t"
genOptionParameterStructName ts@(TOption _) = throwError $ InternalError $ "invalid recursive option type: " ++ show ts
genOptionParameterStructName (TBoxSubtype _) = return $ namefy "option_box_params_t"
genOptionParameterStructName ts =
    case ts of
        TArray {} -> do
            tsName <- genTypeSpecName ts
            tsDimension <- genDimensionOptionTS ts
            return $ namefy $ "option_" <> tsName <> "_" <> tsDimension <> "_params_t"
        _ -> do
            tsName <- genTypeSpecName ts
            return $ namefy $ "option_" <> tsName <> "_params_t"

    where

        genTypeSpecName :: (MonadError CGeneratorError m) => TerminaType -> m Identifier
        genTypeSpecName TUInt8 = return "uint8"
        genTypeSpecName TUInt16 = return "uint16"
        genTypeSpecName TUInt32 = return "uint32"
        genTypeSpecName TUInt64 = return "uint64"
        genTypeSpecName TInt8 = return "int8"
        genTypeSpecName TInt16 = return "int16"
        genTypeSpecName TInt32 = return "int32"
        genTypeSpecName TInt64 = return "int64"
        genTypeSpecName (TArray ts' _) = genTypeSpecName ts'
        genTypeSpecName (TStruct name) = return name
        genTypeSpecName (TEnum name) = return name
        genTypeSpecName ts' = throwError $ InternalError $ "invalid option type specifier: " ++ show ts'

        genDimensionOptionTS :: (MonadError CGeneratorError m) => TerminaType -> m Identifier
        genDimensionOptionTS (TArray ts' (K (TInteger s _))) = (("_" <> show s) <>) <$> genDimensionOptionTS ts'
        genDimensionOptionTS _ = return ""

variant :: Identifier
variant = namefy "variant"

optionSomeVariant, optionNoneVariant :: Identifier
optionSomeVariant = "Some"
optionNoneVariant = "None"

optionSomeField :: Identifier
optionSomeField = "__0"

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
getObjType :: (MonadError CGeneratorError m) => Object SemanticAnn -> m TerminaType
getObjType (Variable _ (Located (ETy (ObjectType _ ts)) _))                  = return ts
getObjType (ArrayIndexExpression _ _ (Located (ETy (ObjectType _ ts)) _))    = return ts
getObjType (MemberAccess _ _ (Located (ETy (ObjectType _ ts)) _))            = return ts
getObjType (Dereference _ (Located (ETy (ObjectType _ ts)) _))               = return ts
getObjType (Unbox _ (Located (ETy (ObjectType _ ts)) _))                     = return ts
getObjType (DereferenceMemberAccess _ _ (Located (ETy (ObjectType _ ts)) _)) = return ts
getObjType ann = throwError $ InternalError $ "invalid object annotation: " ++ show ann

getParameterTypes :: (MonadError CGeneratorError m) => Expression SemanticAnn -> m [TerminaType]
getParameterTypes (FunctionCall _ _ (Located (ETy (AppType params _)) _)) = return params
getParameterTypes ann = throwError $ InternalError $ "invalid parameter annotation: " ++ show ann

getExprType :: (MonadError CGeneratorError m) => Expression SemanticAnn -> m TerminaType
getExprType (AccessObject obj) = getObjType obj
getExprType (Constant _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (OptionVariantInitializer _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (BinOp _ _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (ReferenceExpression _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (Casting _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (FunctionCall _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (MemberFunctionCall _ _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (DerefMemberFunctionCall _ _ _ (Located (ETy (AppType _ ts)) _)) = return ts
getExprType (StructInitializer _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (EnumVariantInitializer _ _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayInitializer _ _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayExprListInitializer _ (Located (ETy (SimpleType ts)) _)) = return ts
getExprType ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

unboxObject :: (MonadError CGeneratorError m) => CExpression -> m CObject
unboxObject (CExprValOf obj _ _) = return obj
unboxObject e = throwError $ InternalError ("invalid unbox object: " ++ show e)

-- | Generates the name of the option struct type
genOptionStructName :: (MonadError CGeneratorError m) => TerminaType -> m Identifier
genOptionStructName TBool = return $ namefy "option_bool_t"
genOptionStructName TChar = return $ namefy "option_char_t"
genOptionStructName TUInt8 = return $ namefy "option_uint8_t"
genOptionStructName TUInt16 = return $ namefy "option_uint16_t"
genOptionStructName TUInt32 = return $ namefy "option_uint32_t"
genOptionStructName TUInt64 = return $ namefy "option_uint64_t"
genOptionStructName TInt8 = return $ namefy "option_int8_t"
genOptionStructName TInt16 = return $ namefy "option_int16_t"
genOptionStructName TInt32 = return $ namefy "option_int32_t"
genOptionStructName TInt64 = return $ namefy "option_int64_t"
genOptionStructName ts@(TOption _) = throwError $ InternalError $ "invalid recursive option type: " ++ show ts
genOptionStructName (TBoxSubtype _) = return optionBox
genOptionStructName ts =
    case ts of
        TArray {} -> do
            tsName <- genTypeSpecName ts
            tsDimension <- genDimensionOptionTS ts
            return $ namefy $ "option_" <> tsName <> "_" <> tsDimension <> "_t"
        _ -> do
            tsName <- genTypeSpecName ts
            return $ namefy $ "option_" <> tsName <> "_t"

    where
        genTypeSpecName :: (MonadError CGeneratorError m) => TerminaType -> m Identifier
        genTypeSpecName TUInt8 = return "uint8"
        genTypeSpecName TUInt16 = return "uint16"
        genTypeSpecName TUInt32 = return "uint32"
        genTypeSpecName TUInt64 = return "uint64"
        genTypeSpecName TInt8 = return "int8"
        genTypeSpecName TInt16 = return "int16"
        genTypeSpecName TInt32 = return "int32"
        genTypeSpecName TInt64 = return "int64"
        genTypeSpecName (TArray ts' _) = genTypeSpecName ts'
        genTypeSpecName (TGlobal _ clsIdentifier) = return clsIdentifier
        genTypeSpecName (TStruct name) = return name
        genTypeSpecName (TEnum name) = return name
        genTypeSpecName ts' = throwError $ InternalError $ "invalid option type specifier: " ++ show ts'

        genDimensionOptionTS :: (MonadError CGeneratorError m) => TerminaType -> m Identifier
        genDimensionOptionTS (TArray ts' (K (TInteger s _))) = (("_" <> show s) <>) <$> genDimensionOptionTS ts'
        genDimensionOptionTS _ = return ""


getCInteger :: TInteger -> CInteger
getCInteger (TInteger i DecRepr) = CInteger i CDecRepr
getCInteger (TInteger i HexRepr) = CInteger i CHexRepr
getCInteger (TInteger i OctalRepr) = CInteger i COctalRepr

getArraySize :: Size -> CExpression
getArraySize (K tint) = CExprConstant (CIntConst (getCInteger tint)) (CTSizeT noqual) (Located CGenericAnn Internal)
getArraySize (V ident) = CExprValOf (CVar ident (CTSizeT noqual)) (CTSizeT noqual) (Located CGenericAnn Internal)

-- | Translate type annotation to C type
genType :: (MonadError CGeneratorError m) => CQualifier -> TerminaType -> m CType
-- |  Unsigned integer types
genType qual TUInt8 = return (CTInt IntSize8 Unsigned qual)
genType qual TUInt16 = return (CTInt IntSize16 Unsigned qual)
genType qual TUInt32 = return (CTInt IntSize32 Unsigned qual)
genType qual TUInt64 = return (CTInt IntSize64 Unsigned qual)
-- | Signed integer types
genType qual TInt8 = return (CTInt IntSize8 Signed qual)
genType qual TInt16 = return (CTInt IntSize16 Signed qual)
genType qual TInt32 = return (CTInt IntSize32 Signed qual)
genType qual TInt64 = return (CTInt IntSize64 Signed qual)
-- | Other primitive typess
genType qual TUSize = return (CTSizeT qual)
genType qual TBool = return (CTBool qual)
genType qual TChar = return (CTChar qual)
-- | Primitive type
genType qual (TGlobal _ clsIdentifier) = return (CTTypeDef clsIdentifier qual)
-- | TArray type
genType qual (TArray ts' s) = do
    ts <- genType qual ts'
    return (CTArray ts (getArraySize s))
-- | Option types
genType _qual (TOption (TBoxSubtype _)) = return (CTTypeDef optionBox noqual)
genType _qual (TOption ts) = do
    optName <- genOptionStructName ts
    return (CTTypeDef optName noqual)
-- Non-primitive types:
-- | Box subtype
genType _qual (TBoxSubtype _) = return (CTTypeDef boxStruct noqual)
-- | TPool type
genType _qual (TPool _ _) = return (CTTypeDef pool noqual)
genType _qual (TMsgQueue _ _) = return (CTTypeDef msgQueue noqual)
genType qual (TFixedLocation ts) = do
    ts' <- genType volatile ts
    return (CTPointer ts' qual)
genType _qual (TAccessPort ts) =  genType noqual ts
genType _qual (TAllocator _) = return (CTPointer (CTTypeDef pool noqual) noqual)
genType _qual (TAtomic ts) = genType atomic ts
genType _qual (TAtomicArray ts s) = do
    ts' <- genType atomic ts
    return (CTArray ts' (getArraySize s))
genType _qual (TAtomicAccess ts) = do
    ts' <- genType atomic ts
    return (CTPointer ts' noqual)
genType _qual (TAtomicArrayAccess ts _) = do
    ts' <- genType atomic ts
    return (CTPointer ts' noqual)
-- | Type of the ports
genType _qual (TSinkPort {}) = return (CTTypeDef sinkPort noqual)
genType _qual (TOutPort {}) = return (CTTypeDef outPort noqual)
genType _qual (TInPort {}) = return (CTTypeDef inPort noqual)
genType qual (TReference Immutable ts) = do
    case ts of
        TArray {} -> genType constqual ts
        _ -> do
            ts' <- genType qual{qual_const = True} ts
            return (CTPointer ts' constqual)
genType _qual (TReference _ ts) = do
    case ts of
        TArray {} -> genType noqual ts
        _ -> do
            ts' <- genType noqual ts
            return (CTPointer ts' constqual)
genType _noqual TUnit = return (CTVoid noqual)
genType qual (TEnum name) = return (CTTypeDef name qual)
genType qual (TStruct name) = return (CTTypeDef name qual)
genType qual (TInterface name) = return (CTTypeDef name qual)

genFunctionType :: (MonadError CGeneratorError m) => TerminaType -> [TerminaType] -> m CType
genFunctionType ts tsParams = do
    ts' <- genType noqual ts
    tsParams' <- traverse (genType noqual) tsParams
    return (CTFunction ts' tsParams')

genIndexOf :: (MonadError CGeneratorError m) => CObject -> CExpression -> m CObject
genIndexOf obj index = 
    let cObjType = getCObjType obj in
    case cObjType of
        CTArray ty _ -> return $ CIndexOf obj index ty
        CTPointer ty _ -> return $ CIndexOf obj index ty
        _ -> throwError $ InternalError $ "invalid object type. Not indexable: " ++ show cObjType

genAddrOf :: (MonadError CGeneratorError m) => CObject -> CQualifier -> CAnns -> m CExpression
genAddrOf obj qual cAnn =
    let cObjType = getCObjType obj in
    case cObjType of
        CTArray {} -> return $ CExprValOf obj cObjType cAnn
        ty -> return $ CExprAddrOf obj (CTPointer ty qual) cAnn

genPoolMethodCallExpr :: (MonadError CGeneratorError m) => Identifier -> CExpression -> CExpression -> CAnns ->  m CExpression
genPoolMethodCallExpr mName cObj cArg cAnn =
    case mName of
        "alloc" -> do
            let cFuncType = CTFunction (CTVoid noqual) [getCExprType cObj, getCExprType cArg]
            return $ CExprCall (CExprValOf (CVar (poolMethodName mName) cFuncType) cFuncType cAnn) [cObj, cArg] (CTVoid noqual) cAnn
        "free" -> do
            let cFuncType = CTFunction (CTVoid noqual) [getCExprType cObj]
            return $ CExprCall (CExprValOf (CVar (poolMethodName mName) cFuncType) cFuncType cAnn) [cObj, cArg] (CTVoid noqual) cAnn
        _ -> throwError $ InternalError $ "invalid pool method name: " ++ mName

genMsgQueueSendCall :: (MonadError CGeneratorError m) => CObject -> CExpression -> CAnns -> m CExpression
genMsgQueueSendCall cObj cArg cAnn = do
    let cArgType = getCExprType cArg
    let cFuncType = CTFunction (CTVoid noqual) [CTPointer (CTVoid noqual) noqual]
    let cObjExpr = CExprValOf cObj (getCObjType cObj) cAnn
    -- | If it is a send, the first parameter is the object to be sent. The
    -- function is expecting to receive a reference to that object.
    case cArgType of
        CTArray {} -> do
            let cDataArg = CExprCast cArg (CTPointer (CTVoid noqual) noqual) cAnn
            return $
                CExprCall (CExprValOf (CVar msgQueueSendMethodName cFuncType) cFuncType cAnn) [cObjExpr, cDataArg] (CTVoid noqual) cAnn
        _ -> do
            cArgObj <- unboxObject cArg
            let cDataArg = CExprCast (CExprAddrOf cArgObj (CTPointer cArgType noqual) cAnn) (CTPointer (CTVoid noqual) noqual) cAnn
            return $
                CExprCall (CExprValOf (CVar msgQueueSendMethodName cFuncType) cFuncType cAnn) [cObjExpr, cDataArg] (CTVoid noqual) cAnn

genAtomicMethodCall :: (MonadError CGeneratorError m) => Identifier -> CExpression -> [CExpression] -> CAnns ->  m CExpression
genAtomicMethodCall mName cObj cArgs cAnn =
    case mName of
        "load" -> do
            let cFuncType = CTFunction (CTVoid noqual) [getCExprType cObj]
            return $ CExprCall (CExprValOf (CVar (atomicMethodName mName) cFuncType) cFuncType cAnn) [cObj] (CTVoid noqual) cAnn
        "store" -> do
            let cFuncType = CTFunction (CTVoid noqual) (getCExprType cObj : fmap getCExprType cArgs)
            return $ CExprCall (CExprValOf (CVar (atomicMethodName mName) cFuncType) cFuncType cAnn) (cObj : cArgs) (CTVoid noqual) cAnn
        _ -> throwError $ InternalError $ "invalid atomic method name: " ++ mName

genParameterDeclaration :: (MonadError CGeneratorError m) => Parameter -> m CDeclaration
genParameterDeclaration (Parameter identifier ts) = do
    cParamType <- genType noqual ts
    return $ CDecl (CTypeSpec cParamType) (Just identifier) Nothing

genInteger :: TInteger -> CInteger
genInteger (TInteger i DecRepr) = CInteger i CDecRepr
genInteger (TInteger i HexRepr) = CInteger i CHexRepr
genInteger (TInteger i OctalRepr) = CInteger i COctalRepr

getCArrayItemType :: (MonadError CGeneratorError m) => CType -> m CType
getCArrayItemType (CTArray ty _) = return ty
getCArrayItemType ty = throwError $ InternalError $ "invalid array type: " ++ show ty

genArraySizeExpr :: (MonadError CGeneratorError m) => Size -> SemanticAnn -> m CExpression
genArraySizeExpr (K s) ann = return $ CExprConstant (CIntConst (genInteger s)) (CTSizeT noqual) (buildGenericAnn ann)
genArraySizeExpr (V v) ann = return $ CExprValOf (CVar v (CTSizeT noqual)) (CTSizeT noqual) (buildGenericAnn ann)

genArraySize :: (MonadError CGeneratorError m) => Size -> m CExpression
genArraySize s = return $ getArraySize s

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

{--
printTypedInteger :: TerminaType -> TInteger -> String
printTypedInteger ts ti =
    case ts of
        TUInt8 -> "UINT8_C(" <> printIntegerLiteral ti <> ")"
        TUInt16 -> "UINT16_C(" <> printIntegerLiteral ti <> ")"
        TUInt32 -> "UINT32_C(" <> printIntegerLiteral ti <> ")"
        TUInt64 -> "UINT64_C(" <> printIntegerLiteral ti <> ")"
        TInt8 -> "INT8_C(" <> printIntegerLiteral ti <> ")"
        TInt16 -> "INT16_C(" <> printIntegerLiteral ti <> ")"
        TInt32 -> "INT32_C(" <> printIntegerLiteral ti <> ")"
        TInt64 -> "INT64_C(" <> printIntegerLiteral ti <> ")"
        -- | No correct way to do this for the time being
        TUSize -> printIntegerLiteral ti
        _ -> error "Invalid type specifier: " <> show ts
--}

printLiteral :: Const -> String
printLiteral (I ti _) = printIntegerLiteral ti
printLiteral (B True) = "1"
printLiteral (B False) = "0"
printLiteral (C c) = "'" <> [c] <> "'"

-- | TODO: The size of the enum field has been hardcoded, it should be
-- platform dependent
enumFieldType :: CType
enumFieldType = CTInt IntSize32 Unsigned noqual