{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Common where

import ControlFlow.BasicBlocks.AST
import Semantic.Types
import Control.Monad.Except
import Data.Map
import Generator.LanguageC.AST
import Utils.Annotations
import Configuration.Configuration
import Generator.Monadic
import qualified Control.Monad.State as ST
import qualified Data.Set as S

newtype CGeneratorError = InternalError String
    deriving (Show)

data CGeneratorEnv = CGeneratorEnv { 
    currentModule :: QualifiedName,
    extraImports :: S.Set QualifiedName,
    monadicTypes :: MonadicTypes,
    configParams :: TerminaConfig,
    interruptsMap :: Map Identifier Integer
  }

type CGenerator = ExceptT CGeneratorError (ST.State CGeneratorEnv)

-- |  This function is used to create the names of temporal variables
--  and symbols.
namefy :: Identifier -> Identifier
namefy = ("__" <>)

(<::>) :: Identifier -> Identifier -> Identifier
(<::>) id0 id1 = id0 <> "__" <> id1

(<:>) :: Identifier -> Identifier -> Identifier
(<:>) id0 id1 = id0 <> "_" <> id1

-- | Termina's pretty builtin types
optionBox, boxStruct, sinkPort, inPort, outPort :: Identifier
optionBox = namefy "option_box_t"
boxStruct = namefy "termina_box_t"
sinkPort = namefy "termina_id_t"
inPort = namefy "termina_id_t"
outPort = namefy "termina_out_port_t"

terminaID :: Identifier
terminaID = namefy "termina_id_t"

pool, allocator, msgQueue, periodicTimer :: Identifier
pool = namefy "termina_pool_t"
allocator = namefy "termina_allocator_t"
msgQueue = namefy "termina_msg_queue_t"
periodicTimer = namefy "termina_periodic_timer_t"

atomicMethodName :: Identifier -> Identifier
atomicMethodName mName = "atomic" <:> mName

poolMemoryArea :: Identifier -> Identifier
poolMemoryArea identifier = namefy $ "pool" <:> identifier <:> "memory"

msgQueueSendMethodName :: Identifier
msgQueueSendMethodName = namefy "termina_out_port" <::> "send"

thatField, thisParam, eventParam, selfParam, lockVar :: Identifier
thatField = namefy "that"
thisParam = namefy "this"
eventParam = namefy "ev"
selfParam = "self"
lockVar = namefy "lock"

resourceLockTypeField, taskMsgQueueIDField,
    timerField, taskIDField, handlerIDField :: Identifier
resourceLockTypeField = namefy $ "lock" <:> "type"
taskMsgQueueIDField = namefy $ "task" <:> "msg_queue" <:> "id"
timerField = namefy $ "timer" <:> "id"
taskIDField = namefy $ "task" <:> "id"
handlerIDField = namefy $ "handler" <:> "id"

genEnumStructName :: (MonadError CGeneratorError m) => Identifier -> m Identifier
genEnumStructName identifier = return $ namefy $ "enum" <:> identifier <:> "t"

genEnumVariantName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genEnumVariantName enumId this_variant = return $ enumId <::> this_variant

genEnumParameterStructName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genEnumParameterStructName enumId this_variant = return $ namefy $ "enum" <:> enumId <::> this_variant <:> "params" <:> "t"

genClassFunctionName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genClassFunctionName className functionName = return $ className <::> functionName

genTypeSpecName :: (MonadError CGeneratorError m) => TerminaType SemanticAnn -> m Identifier
genTypeSpecName TBool = return "bool"
genTypeSpecName TChar = return "char"
genTypeSpecName TUInt8 = return "uint8"
genTypeSpecName TUInt16 = return "uint16"
genTypeSpecName TUInt32 = return "uint32"
genTypeSpecName TUInt64 = return "uint64"
genTypeSpecName TInt8 = return "int8"
genTypeSpecName TInt16 = return "int16"
genTypeSpecName TInt32 = return "int32"
genTypeSpecName TInt64 = return "int64"
genTypeSpecName TUSize = return "size"
genTypeSpecName (TStruct ident) = return ident
genTypeSpecName (TEnum ident) = return ident
genTypeSpecName ts' = throwError $ InternalError $ "invalid option type specifier: " ++ show ts'

-- | This function returns the name of the struct that represents the parameters
-- of an option type. 
genOptionParameterStructName :: (MonadError CGeneratorError m) => TerminaType SemanticAnn -> m Identifier
genOptionParameterStructName (TBoxSubtype _) = return $ namefy "option" <:> "box" <:> "params" <:> "t"
genOptionParameterStructName ty = do
    tyName <- genTypeSpecName ty
    return $ namefy "option" <:> tyName <::> "Some" <:> "params" <:> "t"

genStatusParameterStructName :: (MonadError CGeneratorError m) => TerminaType SemanticAnn -> m Identifier
genStatusParameterStructName ty = do
    tyName <- genTypeSpecName ty
    return $ namefy "status" <:> tyName <::> "Failure" <:> "params" <:> "t"

genResultParameterStructName :: (MonadError CGeneratorError m) => 
    TerminaType SemanticAnn 
    -> TerminaType SemanticAnn
    -> Identifier
    -> m Identifier
genResultParameterStructName okTy errorTy this_variant = do
    okTyName <- genTypeSpecName okTy
    errorTyName <- genTypeSpecName errorTy
    return $ namefy "result" <:> okTyName <:> errorTyName <::> this_variant <:> "params" <:> "t"

variant :: Identifier
variant = namefy "variant"

optionSomeVariant, optionNoneVariant :: Identifier
optionSomeVariant = "Some"
optionNoneVariant = "None"

resultOkVariant, resultErrorVariant :: Identifier
resultOkVariant = "Ok"
resultErrorVariant = "Error"

statusSuccessVariant, statusFailureVariant :: Identifier
statusSuccessVariant = "Success"
statusFailureVariant = "Failure"

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
getObjType :: (MonadError CGeneratorError m) => Object SemanticAnn -> m (TerminaType SemanticAnn)
getObjType (Variable _ (SemanticAnn (ETy (ObjectType _ ts)) _))                  = return ts
getObjType (ArrayIndexExpression _ _ (SemanticAnn (ETy (ObjectType _ ts)) _))    = return ts
getObjType (MemberAccess _ _ (SemanticAnn (ETy (ObjectType _ ts)) _))            = return ts
getObjType (MemberAccess _ _ (SemanticAnn (ETy (AccessPortObjType _ _ ts)) _))     = return ts 
getObjType (Dereference _ (SemanticAnn (ETy (ObjectType _ ts)) _))               = return ts
getObjType (Unbox _ (SemanticAnn (ETy (ObjectType _ ts)) _))                     = return ts
getObjType (DereferenceMemberAccess _ _ (SemanticAnn (ETy (ObjectType _ ts)) _)) = return ts
getObjType (DereferenceMemberAccess _ _ (SemanticAnn (ETy (AccessPortObjType _ _ ts)) _)) = return ts
getObjType ann = throwError $ InternalError $ "invalid object annotation: " ++ show ann

getExprType :: (MonadError CGeneratorError m) => Expression SemanticAnn -> m (TerminaType SemanticAnn)
getExprType (AccessObject obj) = getObjType obj
getExprType (Constant _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (MonadicVariantInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (BinOp _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ReferenceExpression _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (Casting _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (FunctionCall _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (MemberFunctionCall _ _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (DerefMemberFunctionCall _ _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (StructInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (EnumVariantInitializer _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayInitializer _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayExprListInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (StringInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArraySliceExpression _ _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (IsEnumVariantExpression _ _ _  (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (IsMonadicVariantExpression _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

-- | Extracts the 'CObject' from a 'CExpression'.
-- This function is typically used when an expression is known to be a direct
-- representation of an object, i.e., an 'CExprValOf'.
-- It throws an 'InternalError' if the expression is not a 'CExprValOf'.
getCObject :: (MonadError CGeneratorError m) => CExpression -> m CObject
getCObject (CExprValOf obj _ _) = return obj
getCObject e = throwError $ InternalError ("getCObject: Expected CExprValOf, but received: " ++ show e)

-- | Generates the name of the option struct type.
-- This function is used to generate the name of the struct that represents the
-- option type. The function assumes that the option type is well-typed and that
-- the semantic annotation is correct. If the option type is not well-typed, the
-- function will throw an internal error.
genOptionStructName :: (MonadError CGeneratorError m) => TerminaType SemanticAnn -> m Identifier
genOptionStructName (TBoxSubtype _) = return optionBox
genOptionStructName ty = do
    tyName <- genTypeSpecName ty
    return $ namefy "option" <:> tyName <:> "t"

genStatusStructName :: (MonadError CGeneratorError m) => TerminaType SemanticAnn -> m Identifier
genStatusStructName ty = do
    tyName <- genTypeSpecName ty
    return $ namefy "status" <:> tyName <:> "t"


genResultStructName :: (MonadError CGeneratorError m) => 
    TerminaType SemanticAnn 
    -> TerminaType SemanticAnn -> m Identifier
genResultStructName tyOk tyError = do
    tyOkName <- genTypeSpecName tyOk
    tyErrorName <- genTypeSpecName tyError
    return $ namefy "result" <:> tyOkName <::> tyErrorName <:> "t"

getCInteger :: TInteger -> CInteger
getCInteger (TInteger i DecRepr) = CInteger i CDecRepr
getCInteger (TInteger i HexRepr) = CInteger i CHexRepr
getCInteger (TInteger i OctalRepr) = CInteger i COctalRepr

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

genPoolMethodCallExpr :: (MonadError CGeneratorError m) => Identifier -> CObject -> CExpression -> CAnns ->  m CExpression
genPoolMethodCallExpr mName cObj cArg cAnn =
    let cEventArgType = CTPointer (CTTypeDef "__termina_event_t" noqual) noqual
        cEventArg = CExprValOf (CVar eventParam cEventArgType) cEventArgType cAnn 
    in
    case mName of
        "alloc" -> do
            let cObjExpr = CExprValOf (CField cObj thatField (CTPointer (getCObjType cObj) noqual)) (CTPointer (getCObjType cObj) noqual) cAnn
                cFuncType = CTFunction (CTVoid noqual) [cEventArgType, getCExprType cObjExpr, getCExprType cArg]
                cFunctionCall = CField cObj "alloc" cFuncType
            return $ CExprCall (CExprValOf cFunctionCall cFuncType cAnn) [cEventArg, cObjExpr, cArg] (CTVoid noqual) cAnn
        "free" -> do
            let cObjExpr = CExprValOf (CField cObj thatField (CTPointer (getCObjType cObj) noqual)) (CTPointer (getCObjType cObj) noqual) cAnn
                cFuncType = CTFunction (CTVoid noqual) [cEventArgType, getCExprType cObjExpr]
                cFunctionCall = CField cObj "free" cFuncType
            return $ CExprCall (CExprValOf cFunctionCall cFuncType cAnn) [cEventArg, cObjExpr, cArg] (CTVoid noqual) cAnn
        _ -> throwError $ InternalError $ "invalid pool method name: " ++ mName

genMsgQueueSendNULLExpr :: (MonadError CGeneratorError m) => CObject -> CAnns -> m CExpression
genMsgQueueSendNULLExpr cObj cAnn = do
    let cEventArgType = CTPointer (CTTypeDef "__termina_event_t" noqual) noqual
        cEventArg = CExprValOf (CVar eventParam cEventArgType) cEventArgType cAnn 
        cFuncType = CTFunction (CTVoid noqual) [cEventArgType, getCObjType cObj, CTPointer (CTVoid noqual) noqual]
        cObjExpr = CExprValOf cObj (getCObjType cObj) cAnn
        cDataArg = CExprValOf (CVar "NULL" (CTPointer (CTVoid noqual) noqual)) (CTPointer (CTVoid noqual) noqual) cAnn
    return $
        CExprCall (CExprValOf (CVar msgQueueSendMethodName cFuncType) cFuncType cAnn) [cEventArg, cObjExpr, cDataArg] (CTVoid noqual) cAnn

genMsgQueueSendCall :: (MonadError CGeneratorError m) => CObject -> CExpression -> CAnns -> m CExpression
genMsgQueueSendCall cObj cArg cAnn = do
    let cEventArgType = CTPointer (CTTypeDef "__termina_event_t" noqual) noqual
        cEventArg = CExprValOf (CVar eventParam cEventArgType) cEventArgType cAnn 
        cArgType = getCExprType cArg
        cFuncType = CTFunction (CTVoid noqual) [cEventArgType, cArgType, CTPointer (CTVoid noqual) noqual]
        cObjExpr = CExprValOf cObj (getCObjType cObj) cAnn
    -- | If it is a send, the first parameter is the object to be sent. The
    -- function is expecting to receive a reference to that object.
    cArgObj <- getCObject cArg
    let cDataArg = CExprCast (CExprAddrOf cArgObj (CTPointer cArgType noqual) cAnn) (CTPointer (CTVoid noqual) noqual) cAnn
    return $
        CExprCall (CExprValOf (CVar msgQueueSendMethodName cFuncType) cFuncType cAnn) [cEventArg, cObjExpr, cDataArg] (CTVoid noqual) cAnn

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

genInteger :: TInteger -> CInteger
genInteger (TInteger i DecRepr) = CInteger i CDecRepr
genInteger (TInteger i HexRepr) = CInteger i CHexRepr
genInteger (TInteger i OctalRepr) = CInteger i COctalRepr

getCArrayItemType :: (MonadError CGeneratorError m) => CType -> m CType
getCArrayItemType (CTArray ty _) = return ty
getCArrayItemType ty = throwError $ InternalError $ "invalid array type: " ++ show ty

internalAnn :: CItemAnn -> CAnns
internalAnn = flip LocatedElement Internal

buildGenericAnn :: SemanticAnn -> CAnns
buildGenericAnn (SemanticAnn _ loc) = LocatedElement CGenericAnn loc

buildStatementAnn :: SemanticAnn -> Bool -> CAnns
buildStatementAnn (SemanticAnn _ loc) before = LocatedElement (CStatementAnn before False) loc

buildDeclarationAnn :: SemanticAnn -> Bool -> CAnns
buildDeclarationAnn (SemanticAnn _ loc) before = LocatedElement (CDeclarationAnn before) loc

buildCompoundAnn :: SemanticAnn -> Bool -> Bool -> CAnns
buildCompoundAnn (SemanticAnn _ loc) before trailing = LocatedElement (CCompoundAnn before trailing) loc

buildCPPDirectiveAnn :: SemanticAnn -> Bool -> CAnns
buildCPPDirectiveAnn (SemanticAnn _ loc) before = LocatedElement (CPPDirectiveAnn before) loc

-- | TODO: The size of the enum field has been hardcoded, it should be
-- platform dependent
enumFieldType :: CType
enumFieldType = CTInt IntSize32 Unsigned noqual

taskFunctionName :: (MonadError CGeneratorError m) => Identifier -> m Identifier
taskFunctionName classId = return $ namefy classId <::> "termina" <:> "task"