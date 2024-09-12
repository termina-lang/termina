{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Platform.RTEMS5NoelSpike where

import Generator.LanguageC.CompCertC
import System.FilePath
import Semantic.Monad
import AST.Seman
import Modules.Modules
import Generator.CCCodeGen.Common
import qualified AST.Seman as SAST
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List (find)
import Semantic.Types (GEntry (GGlob), SemGlobal (SEmitter))
import Control.Monad.Except
import Generator.LanguageC.CompCertCPrinter
import Control.Monad.Reader
import Data.Text (unpack)
import Utils.Annotations

data RTEMSPort =
    RTEMSEventPort
        Identifier -- ^ port identifier
        Identifier -- ^ event emitter identifier
        TypeSpecifier -- ^ data type specifier
        Identifier -- ^ action to be executed
    | RTEMSAccessPort
        Identifier -- ^ port identifier
        Identifier -- ^ resource identifier
    | RTEMSInputPort
        Identifier -- ^ port identifier
        Identifier -- ^ channel identifier
        TypeSpecifier -- ^ data type specifier
        Identifier -- ^ action to be executed
    | RTEMSOutputPort
        Identifier -- ^ port identifier
        Identifier -- ^ channel identifier
    deriving Show

data RTEMSGlobal =
    -- | RTEMS Task
    RTEMSTask
      Identifier -- ^ task identifier
      Identifier -- ^ task class identifier
      (TypeDef SemanticAnn) -- ^ task class definition
      TInteger -- ^ task priority
      TInteger -- ^ task stack size
      [RTEMSPort] -- ^ task ports
    -- | RTEMS Handler
    | RTEMSHandler
      Identifier -- ^ handler identifier
      Identifier -- ^ handler class identifier
      (TypeDef SemanticAnn)  -- ^ handler class definition
      RTEMSPort -- ^ event port
      [RTEMSPort] -- ^ resource access ports
    -- | RTEMS Resource
    | RTEMSResource
      Identifier -- ^ resource identifier
      Identifier -- ^ resource class identifier
      [RTEMSPort] -- ^ resource access ports
    | RTEMSPool
      Identifier -- ^ pool identifier
      TypeSpecifier -- ^ type of the elements of the pool
      Size -- ^ pool size
    | RTEMSAtomic
      Identifier -- ^ atomic identifier
      TypeSpecifier -- ^ type of the atomic
    | RTEMSAtomicArray
      Identifier -- ^ atomic array identifier
      TypeSpecifier -- ^ type of the elements of the atomic array
      Size -- ^ atomic array size
    deriving Show

data RTEMSEmitter =
    RTEMSInterruptEmitter
      Identifier -- ^ interrupt identifier
      RTEMSGlobal -- ^ target of the interrupt (task or handler)
    | RTEMSPeriodicTimerEmitter
      Identifier -- ^ periodic timer identifier
      RTEMSGlobal -- ^ target of the timer (task or handler)
    | RTEMSSystemInitEmitter
      Identifier -- ^ initial event identifier
      RTEMSGlobal -- ^ target of the initial event (task or handler)
    deriving Show

data RTEMSMsgQueue =
    RTEMSTaskMsgQueue
      Identifier -- ^ message queue identifier
      TInteger -- ^ message queue size
    | RTEMSChannelMsgQueue
      Identifier -- ^ name of the channel
      TypeSpecifier -- ^ type of the elements of the message queue
      TInteger -- ^ message queue size
      RTEMSGlobal -- ^ task that will receive the messages
    | RTEMSSinkPortMsgQueue
      Identifier -- ^ identifier of the receiving task
      Identifier -- ^ identifier of the port that will receive the messages
      TypeSpecifier -- ^ type of the elements of the message queue
      TInteger -- ^ message queue size
    deriving Show

data RTEMSResourceLock =
    RTEMSResourceLockNone |
    RTEMSResourceLockIrq |
    RTEMSResourceLockMutex TInteger
    deriving Show

-- | Eq instance for RTEMSGlobal
instance Eq RTEMSGlobal where
    (RTEMSTask id1 _ _ _ _ _) == (RTEMSTask id2 _ _ _ _ _) = id1 == id2
    (RTEMSHandler id1 _ _ _ _) == (RTEMSHandler id2 _ _ _ _) = id1 == id2
    (RTEMSResource id1 _ _) == (RTEMSResource id2 _ _) = id1 == id2
    _ == _ = False

-- | Ord instance for RTEMSGlobal
instance Ord RTEMSGlobal where
    compare (RTEMSTask id1 _ _ _ _ _) (RTEMSTask id2 _ _ _ _ _) = compare id1 id2
    compare (RTEMSHandler id1 _ _ _ _) (RTEMSHandler id2 _ _ _ _) = compare id1 id2
    compare (RTEMSResource id1 _ _) (RTEMSResource id2 _ _) = compare id1 id2
    compare (RTEMSPool id1 _ _) (RTEMSPool id2 _ _) = compare id1 id2
    compare (RTEMSTask {}) _ = LT
    compare (RTEMSHandler {}) _ = LT
    compare (RTEMSResource {}) _ = LT
    compare (RTEMSPool {}) _ = LT
    compare (RTEMSAtomic {}) _ = LT
    compare (RTEMSAtomicArray {}) _ = LT

-- | Generic RTEMS types
cRTEMSStatusCodeType, cRTEMSIdType, cRTEMSMessageQueueReceiveType, cRTEMSMessageQueueSendType :: CType
cRTEMSStatusCodeType = CTTypeDef "rtems_status_code" noqual
cRTEMSIdType = CTTypeDef "rtems_id" noqual
cRTEMSMessageQueueReceiveType = 
    CTFunction cRTEMSStatusCodeType 
        [
            -- | rtems_id id
            CTTypeDef "rtems_id" noqual, 
            -- | void * buffer
            CTPointer (CTVoid noqual) noqual, 
            -- | size_t * size
            CTPointer (CTSizeT noqual) noqual,
            -- | rtems_option option_set
            CTTypeDef "rtems_option" noqual,
            -- | rtems_interval timeout
            CTTypeDef "rtems_interval" noqual
        ]
cRTEMSMessageQueueSendType =
    CTFunction cRTEMSStatusCodeType 
        [
            -- | rtems_id id
            CTTypeDef "rtems_id" noqual, 
            -- | void * buffer
            CTPointer (CTVoid noqual) noqual, 
            -- | size_t size
            CTSizeT noqual,
            -- | rtems_option option_set
            CTTypeDef "rtems_option" noqual
        ]

-- void __termina__add_timeval(TimeVal * const lhs, const TimeVal * const rhs);
cTerminaAddTimeValFunctionType :: CType
cTerminaAddTimeValFunctionType = 
    CTFunction (CTVoid noqual) 
        [
            -- | TimeVal * const lhs
            CTPointer (CTTypeDef "TimeVal" constqual) constqual,
            -- | const TimeVal * const rhs
            CTPointer (CTTypeDef "TimeVal" constqual) constqual
        ]
-- rtems_status_code __rtems__timer_delay_at(rtems_id id,
--                                          const TimeVal * next_time,
--                                          rtems_timer_service_routine_entry routine);
cRTEMSTimerDelayAtFunctionType :: CType
cRTEMSTimerDelayAtFunctionType = 
    CTFunction cRTEMSStatusCodeType 
        [
            -- | rtems_id id
            CTTypeDef "rtems_id" noqual,
            -- | const TimeVal * next_time
            CTPointer (CTTypeDef "TimeVal" constqual) constqual,
            -- | rtems_timer_service_routine_entry routine
            CTTypeDef "rtems_timer_service_routine_entry" noqual
        ]

-- Result classId__handle(classId * const self,
--                                  uint32_t _vector);
cIrqHandlerActionFunctionType :: Identifier -> CType
cIrqHandlerActionFunctionType classId = 
    CTFunction (CTTypeDef "Result" noqual) 
        [
            -- | CRISCVUARTHandler * const self
            CTPointer (CTTypeDef classId constqual) constqual,
            -- | uint32_t _vector
            CTInt IntSize32 Unsigned noqual
        ]

-- rtems_shutdown_executive(1);
cRTEMSShutdownExecutiveCall :: CAnns -> CExpression
cRTEMSShutdownExecutiveCall cAnn =
    let cRTEMSShutdownExecutiveType = CTFunction (CTVoid noqual) [CTInt IntSize32 Unsigned noqual] in
    CExprCall (CExprValOf (CVar "rtems_shutdown_executive" cRTEMSShutdownExecutiveType) cRTEMSShutdownExecutiveType cAnn)
        [CExprConstant (CIntConst (CInteger 1 CDecRepr)) (CTInt IntSize32 Unsigned noqual) cAnn] (CTVoid noqual) cAnn

-- | Returns the value of the "priority" modifier, if present in the list of modifiers.
-- If not, it returns 255, which is the default value for the priority (the lowest).
getPriority :: [Modifier] -> TInteger
getPriority [] = TInteger 255 DecRepr
getPriority ((Modifier "priority" (Just (I priority _))) : _) = priority
getPriority (_ : modifiers) = getPriority modifiers

-- | Returns the value of the "stack_size" modifier, if present in the list of modifiers.
-- If not, it returns 4096, which is the default value for the stack size (RTEMS_MINIUMUM_STACK_SIZE)
getStackSize :: [Modifier] -> TInteger
getStackSize [] = TInteger 4096 DecRepr
getStackSize ((Modifier "stack_size" (Just (I stackSize _))) : _) = stackSize
getStackSize (_ : modifiers) = getStackSize modifiers

addDependency :: RTEMSGlobal -> Maybe (S.Set RTEMSGlobal) -> Maybe (S.Set RTEMSGlobal)
addDependency newGlb Nothing = Just (S.singleton newGlb)
addDependency newGlb (Just prevGlbs) = Just (S.insert newGlb prevGlbs)

-- Finds the assignment that connects a given port
findPortConnection :: Identifier -> [FieldAssignment a] -> Maybe (FieldAssignment a)
findPortConnection _ [] = Nothing
findPortConnection identifier (assignment : assignments) =
    case assignment of
        FieldPortConnection _ port _ _ | port == identifier -> Just assignment
        _ -> findPortConnection identifier assignments

buildRTEMSGlobal :: Global SemanticAnn -> M.Map Identifier (TypeDef SemanticAnn) -> RTEMSGlobal
buildRTEMSGlobal (Task identifier (DefinedType ty) (Just (StructInitializer assignments _ _)) modifiers _) classMap =
    RTEMSTask identifier clsIdentifier clsTypeDefinition (getPriority modifiers) (getStackSize modifiers) ports
    where
        -- Task class
        (clsTypeDefinition, clsIdentifier, clsMembers) = case fromJust (M.lookup ty classMap) of
            cls@(Class TaskClass clsId members _ _) -> (cls, clsId, members)
            cls -> error $ "invalid task class: " ++ show cls

        ports =
            concatMap (\case
                ClassField (FieldDefinition portIdentifier (AccessPort {})) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection AccessPortConnection _ resourceIdentifier _) ->
                            [RTEMSAccessPort portIdentifier resourceIdentifier]
                        _ -> error $ "Invalid port connections: " ++ show portIdentifier;
                ClassField (FieldDefinition portIdentifier (SinkPort dts action)) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection InboundPortConnection _ eventEmitter _) ->
                            [RTEMSEventPort portIdentifier eventEmitter dts action]
                        _ -> error $ "Invalid port connections: " ++ show portIdentifier;
                ClassField (FieldDefinition portIdentifier (InPort dts action)) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection InboundPortConnection _ channelIdentifier _) ->
                            [RTEMSInputPort portIdentifier channelIdentifier dts action]
                        _ -> error $ "Invalid port connections: " ++ show portIdentifier;
                ClassField (FieldDefinition portIdentifier (OutPort _)) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection OutboundPortConnection _ channelIdentifier _) ->
                            [RTEMSOutputPort portIdentifier channelIdentifier]
                        _ -> error $ "Invalid port connections: " ++ show portIdentifier;
                _ -> []) clsMembers
buildRTEMSGlobal (Handler identifier (DefinedType ty) (Just (StructInitializer assignments _ _)) _ _) classMap =
    RTEMSHandler identifier clsIdentifier clsTypeDefinition eventPort ports
    where
        -- Handler class
        (clsTypeDefinition, clsIdentifier, clsMembers) = case fromJust (M.lookup ty classMap) of
            cls@(Class HandlerClass clsId members _ _) -> (cls, clsId, members)
            cls -> error $ "invalid task class: " ++ show cls

        buildEventPort :: [ClassMember' a b c] -> RTEMSPort
        buildEventPort [] = error $ "handler does not have an event port: " ++ show clsIdentifier
        buildEventPort (ClassField (FieldDefinition portIdentifier (SinkPort dts action)) _ : _) =
            case findPortConnection portIdentifier assignments of
                Just (FieldPortConnection InboundPortConnection _ emitterIdentifier _) ->
                    RTEMSEventPort portIdentifier emitterIdentifier dts action
                conn -> error $ "Invalid port connection: " ++ show conn
        buildEventPort (_ : members) = buildEventPort members

        eventPort = buildEventPort clsMembers

        ports =
            concatMap (\case
                ClassField (FieldDefinition portIdentifier (AccessPort {})) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection AccessPortConnection _ resourceIdentifier _) ->
                            [RTEMSAccessPort portIdentifier resourceIdentifier]
                        _ -> error $ "Invalid port connection: " ++ show portIdentifier;
                ClassField (FieldDefinition portIdentifier (OutPort {})) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection OutboundPortConnection _ channelIdentifier _) ->
                            [RTEMSOutputPort portIdentifier channelIdentifier]
                        _ -> error $ "Invalid port connection: " ++ show portIdentifier;
                _ -> []) clsMembers
buildRTEMSGlobal (Resource identifier (DefinedType ty) (Just (StructInitializer assignments _ _)) _ _) classMap =
    RTEMSResource identifier clsIdentifier ports
    where
        -- Resource class
        (clsIdentifier, clsMembers) = case fromJust (M.lookup ty classMap) of
            (Class ResourceClass clsId members _ _) -> (clsId, members)
            cls -> error $ "invalid task class: " ++ show cls

        ports =
            concatMap (\case
                ClassField (FieldDefinition portIdentifier (AccessPort {})) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection AccessPortConnection _ resourceIdentifier _) ->
                            [RTEMSAccessPort portIdentifier resourceIdentifier]
                        _ -> error $ "Invalid port connection: " ++ show portIdentifier;
                _ -> []) clsMembers
buildRTEMSGlobal (Resource identifier (Pool ty size) _ _ _) _ = RTEMSPool identifier ty size
buildRTEMSGlobal (Resource identifier (Atomic ty) _ _ _) _ = RTEMSAtomic identifier ty
buildRTEMSGlobal (Resource identifier (AtomicArray ty size) _ _ _) _ = RTEMSAtomicArray identifier ty size
buildRTEMSGlobal obj _ = error $ "Invalid global object: " ++ show obj

buildRTEMSEmitter :: Global SemanticAnn -> M.Map Identifier RTEMSGlobal -> Maybe RTEMSEmitter
buildRTEMSEmitter (Emitter identifier (DefinedType "Interrupt") _ _ _) connectionsMap =
    case M.lookup identifier connectionsMap of
        Just glb -> Just (RTEMSInterruptEmitter identifier glb)
        Nothing -> Nothing -- Not connected
buildRTEMSEmitter (Emitter identifier (DefinedType "PeriodicTimer") _ _ _) connectionsMap =
    case M.lookup identifier connectionsMap of
        Just glb -> Just (RTEMSPeriodicTimerEmitter identifier glb)
        Nothing -> Nothing -- Not connected
buildRTEMSEmitter (Emitter identifier (DefinedType "SystemInit") _ _ _) connectionsMap =
    case M.lookup identifier connectionsMap of
        Just glb -> Just (RTEMSSystemInitEmitter identifier glb)
        Nothing -> Nothing -- Not connected
buildRTEMSEmitter emitter@(Emitter {}) _ = error $ "Unsupported emitter" ++ show emitter
buildRTEMSEmitter _ _ = Nothing

genVariantForPort ::
    -- | Name of the task class
    Identifier
    -- | Name of the port
    -> Identifier -> CSourceGenerator Identifier
genVariantForPort taskCls port = return $ namefy $ taskCls <::> port

genVariantsForTaskPorts :: TypeDef SemanticAnn -> CSourceGenerator [CFileItem]
genVariantsForTaskPorts (Class _ classId members _ _) =
    genDefineVariantsForPorts ports
    where

        ports = foldr (\field acc ->
                        case field of
                            ClassField (FieldDefinition prt (SinkPort {})) _ -> prt : acc
                            ClassField (FieldDefinition prt (InPort {})) _ -> prt : acc
                            _ -> acc ) [] members

        genDefineVariantsForPorts :: [Identifier] -> CSourceGenerator [CFileItem]
        genDefineVariantsForPorts [] = return []
        genDefineVariantsForPorts (port : xs) = do
            variant <- genVariantForPort classId port
            rest <- genDefineVariantsForPorts' xs 1
            return $ CPPDirective (CPPDefine variant (Just [show (0 :: Integer)]) (internalAnn (CPPDirectiveAnn True))) : rest

        genDefineVariantsForPorts' :: [Identifier] -> Integer -> CSourceGenerator [CFileItem]
        genDefineVariantsForPorts' [] _ = return []
        genDefineVariantsForPorts' (port : xs) value = do
            rest <- genDefineVariantsForPorts' xs (value + 1)
            variant <- genVariantForPort classId port
            return $ CPPDirective (CPPDefine variant (Just [show value]) (internalAnn (CPPDirectiveAnn False))) : rest

genVariantsForTaskPorts def = throwError $ InternalError $ "Definition not a class: " ++ show def

genPoolMemoryArea :: Bool -> RTEMSGlobal -> CSourceGenerator CFileItem
genPoolMemoryArea before (RTEMSPool identifier ts size) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cSize <- genArraySize size
    cType <- genType noqual ts
    return $ CExtDecl $ CEDVariable (Just CStatic) (CDecl (CTypeSpec (CTArray cType cSize)) (Just $ poolMemoryArea identifier) Nothing) declStmt
genPoolMemoryArea _ obj = error $ "Invalid global object (not a pool): " ++ show obj

genPoolMemoryAreas :: [RTEMSGlobal] -> CSourceGenerator [CFileItem]
genPoolMemoryAreas [] = return []
genPoolMemoryAreas (obj : objs) = do
    memArea <- genPoolMemoryArea True obj
    rest <- mapM (genPoolMemoryArea False) objs
    return $ memArea : rest

genAtomicDeclaration :: Bool -> RTEMSGlobal -> CSourceGenerator CFileItem
genAtomicDeclaration before (RTEMSAtomic identifier ts) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cType <- genType atomic ts
    return $ CExtDecl $ CEDVariable Nothing (CDecl (CTypeSpec cType) (Just identifier) Nothing) declStmt
genAtomicDeclaration _ obj = error $ "Invalid global object (not an atomic): " ++ show obj

genAtomicDeclarations :: [RTEMSGlobal] -> CSourceGenerator [CFileItem]
genAtomicDeclarations [] = return []
genAtomicDeclarations (obj : objs) = do
    decl <- genAtomicDeclaration True obj
    rest <- mapM (genAtomicDeclaration False) objs
    return $ decl : rest

genAtomicArrayDeclaration :: Bool -> RTEMSGlobal -> CSourceGenerator CFileItem
genAtomicArrayDeclaration before (RTEMSAtomicArray identifier ts size) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cSize <- genArraySize size
    cType <- genType atomic ts
    return $ CExtDecl $ CEDVariable Nothing (CDecl (CTypeSpec (CTArray cType cSize)) (Just identifier) Nothing) declStmt
genAtomicArrayDeclaration _ obj = error $ "Invalid global object (not an atomic array): " ++ show obj

genAtomicArrayDeclarations :: [RTEMSGlobal] -> CSourceGenerator [CFileItem]
genAtomicArrayDeclarations [] = return []
genAtomicArrayDeclarations (obj : objs) = do
    decl <- genAtomicArrayDeclaration True obj
    rest <- mapM (genAtomicArrayDeclaration False) objs
    return $ decl : rest

genInterruptEmitterDeclaration :: Bool -> RTEMSEmitter -> CSourceGenerator CFileItem
genInterruptEmitterDeclaration before (RTEMSInterruptEmitter identifier (RTEMSTask {})) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cType <- genType noqual (DefinedType ("rtems" <::> "interrupt_emitter_t"))
    return $ CExtDecl $ CEDVariable (Just CStatic) (CDecl (CTypeSpec cType) (Just identifier) Nothing) declStmt
genInterruptEmitterDeclaration _ obj = error $ "Invalid global object (not an interrupt emitter): " ++ show obj

genInterruptEmitterDeclarations :: [RTEMSEmitter] -> CSourceGenerator [CFileItem]
genInterruptEmitterDeclarations [] = return []
genInterruptEmitterDeclarations (obj : objs) = do
    decl <- genInterruptEmitterDeclaration True obj
    rest <- mapM (genInterruptEmitterDeclaration False) objs
    return $ decl : rest

genTaskClassCode :: TypeDef SemanticAnn -> CSourceGenerator CFileItem
genTaskClassCode (Class TaskClass classId members _ _) = do
    let cRetType = CTTypeDef "rtems_task" noqual
        declStmt = internalAnn (CDeclarationAnn True)
        cReturn = [CBlockStmt $ CSReturn Nothing (internalAnn (CStatementAnn True False))]
        cParamDecls = [CDecl (CTypeSpec $ CTTypeDef "rtems_task_argument" noqual) (Just "arg") Nothing]
    cBody <- genBody
    return $ CFunctionDef (Just CStatic) (CFunction cRetType
        (namefy "rtems_task" <::> classId) cParamDecls
        (CSCompound (cBody ++ cReturn) (internalAnn (CCompoundAnn False True)))
        declStmt)

    where

        actions :: [(Identifier, TypeSpecifier, Identifier)]
        actions = foldl (\acc member ->
            case member of
                ClassField (FieldDefinition identifier (SinkPort dts action)) _ -> (identifier, dts, action) : acc
                ClassField (FieldDefinition identifier (InPort dts action)) _ -> (identifier, dts, action) : acc
                _ -> acc
            ) [] members

        -- TOOD: The current implementation does not work with vectors
        getMsgDataVariable :: Bool -> Identifier -> TypeSpecifier -> CSourceGenerator CCompoundBlockItem
        getMsgDataVariable before action dts = do
            let declStmt = internalAnn (CDeclarationAnn before)
            cDataType <- genType noqual dts
            return $ CBlockDecl (CDecl (CTypeSpec cDataType) (Just $ action <::> "msg_data") Nothing) declStmt

        getMsgDataVariables :: [(Identifier, TypeSpecifier, Identifier)] -> CSourceGenerator [CCompoundBlockItem]
        getMsgDataVariables [] = return []
        getMsgDataVariables ((_identifier, dts, action) : xs) = do
            decl <- getMsgDataVariable True action dts
            rest <- mapM (uncurry (getMsgDataVariable False) . (\(_, dts', action') -> (action', dts'))) xs
            return $ decl : rest

        genCase :: (Identifier, TypeSpecifier, Identifier) -> CSourceGenerator [CCompoundBlockItem]
        genCase (port, dts, action) = do
            let cAnn = internalAnn CGenericAnn
                stmt before expand = internalAnn (CStatementAnn before expand)
            variant <- genVariantForPort classId port
            classFunctionName <- genClassFunctionName classId action
            classStructType <- genType noqual (DefinedType classId)
            cDataType <- genType noqual dts
            let cSelfObj = CVar "self" (CTPointer classStructType constqual)
                classFunctionType = CTFunction (CTTypeDef "Result" noqual) 
                    [CTPointer classStructType constqual, cDataType]
            return
                [
                    -- case variant:
                    CBlockStmt $ CSCase (CExprValOf (CVar variant enumFieldType) enumFieldType cAnn)
                    -- status = rtems_message_queue_receive(self->port, &action_msg_data, 
                    --                                      &size, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
                        (CSDo (CExprAssign (CVar "status" (CTTypeDef "rtems_status_code" noqual))
                            (CExprCall (CExprValOf (CVar "rtems_message_queue_receive" cRTEMSMessageQueueReceiveType) cRTEMSMessageQueueReceiveType cAnn)
                                [
                                    CExprValOf (CField cSelfObj port cRTEMSIdType) cRTEMSIdType cAnn,
                                    CExprAddrOf (CVar (action <::> "msg_data") cDataType) (CTPointer (CTVoid constqual) noqual) cAnn,
                                    CExprAddrOf (CVar "size" (CTSizeT noqual)) (CTSizeT noqual) cAnn,
                                    CExprValOf (CVar "RTEMS_NO_WAIT" (CTTypeDef "rtems_option" noqual)) (CTTypeDef "rtems_option" noqual) cAnn,
                                    CExprValOf (CVar "RTEMS_NO_TIMEOUT" (CTTypeDef "rtems_interval" noqual)) (CTTypeDef "rtems_interval" noqual) cAnn
                                ] cRTEMSStatusCodeType cAnn) cRTEMSStatusCodeType cAnn) (stmt True True))
                        (stmt True False),
                    -- if (RTEMS_SUCCESSFUL != status)
                    CBlockStmt $ CSIfThenElse (
                            CExprBinaryOp COpNe 
                                (CExprValOf (CVar "RTEMS_SUCCESSFUL" cRTEMSStatusCodeType) cRTEMSStatusCodeType cAnn)
                                (CExprValOf (CVar "status" cRTEMSStatusCodeType) cRTEMSStatusCodeType cAnn) (CTBool noqual) cAnn)
                        (CSCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CSDo (cRTEMSShutdownExecutiveCall cAnn) (stmt False False)
                        ] (internalAnn (CCompoundAnn False False))) Nothing (stmt True True),
                    -- result = classFunctionName(self, action_msg_data);
                    CBlockStmt $ CSDo (CExprAssign (CVar "result" (CTTypeDef "Result" noqual))
                        (CExprCall (CExprValOf (CVar classFunctionName classFunctionType) classFunctionType cAnn)
                            [
                                CExprValOf (CVar "self" classStructType) classStructType cAnn, 
                                CExprValOf (CVar (action <::> "msg_data") cDataType) cDataType cAnn
                            ] (CTTypeDef "Result" noqual) cAnn) (CTTypeDef "Result" noqual) cAnn) (stmt True True),
                    -- if (result.__variant != Result__Ok)
                    CBlockStmt $ CSIfThenElse (
                            CExprBinaryOp COpNe 
                                (CExprValOf (CField (CVar "result" (CTTypeDef "Result" noqual)) enumVariantsField enumFieldType) enumFieldType cAnn)
                                (CExprValOf (CVar ("Result" <::> "Ok") enumFieldType) enumFieldType cAnn) (CTBool noqual) cAnn)
                        (CSCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CSDo (cRTEMSShutdownExecutiveCall cAnn) (stmt False False)
                        ] (internalAnn (CCompoundAnn False False))) Nothing (stmt True True),
                    -- break;
                    CBlockStmt $ CSBreak (stmt True True)

                ]

        genLoop :: CSourceGenerator CStatement
        genLoop = do
            let cAnn = internalAnn CGenericAnn
                stmt before = internalAnn (CStatementAnn before False)
                compoundAnn = internalAnn (CCompoundAnn False True)
            classStructType <- genType noqual (DefinedType classId)
            let cSelfObj = CVar "self" (CTPointer classStructType constqual)
            cases <- concat <$> mapM genCase actions
            return $ CSCompound [
                    CBlockStmt $ CSDo (CExprAssign (CVar "status" (CTTypeDef "rtems_status_code" noqual))
                        (CExprCall (CExprValOf (CVar "rtems_message_queue_receive" cRTEMSMessageQueueReceiveType) cRTEMSMessageQueueReceiveType cAnn)
                            [
                                CExprValOf (CField (CField cSelfObj "__task" (CTTypeDef taskID noqual)) "msgq_id" cRTEMSIdType) cRTEMSIdType cAnn,
                                CExprAddrOf (CVar "next_msg" (CTInt IntSize32 Unsigned noqual)) (CTPointer (CTVoid constqual) noqual) cAnn,
                                CExprAddrOf (CVar "size" (CTSizeT noqual)) (CTSizeT noqual) cAnn,
                                CExprValOf (CVar "RTEMS_NO_WAIT" (CTTypeDef "rtems_option" noqual)) (CTTypeDef "rtems_option" noqual) cAnn,
                                CExprValOf (CVar "RTEMS_NO_TIMEOUT" (CTTypeDef "rtems_interval" noqual)) (CTTypeDef "rtems_interval" noqual) cAnn
                            ] cRTEMSStatusCodeType cAnn) cRTEMSStatusCodeType cAnn) (stmt True),
                    -- if (RTEMS_SUCCESSFUL != status)
                    CBlockStmt $ CSIfThenElse (
                            CExprBinaryOp COpNe 
                                (CExprValOf (CVar "RTEMS_SUCCESSFUL" cRTEMSStatusCodeType) cRTEMSStatusCodeType cAnn)
                                (CExprValOf (CVar "status" cRTEMSStatusCodeType) cRTEMSStatusCodeType cAnn) (CTBool noqual) cAnn)
                        (CSCompound [
                            -- break;
                            CBlockStmt $ CSBreak (stmt False)
                        ] (internalAnn (CCompoundAnn False False))) Nothing (stmt True),
                    CBlockStmt $ CSSwitch (CExprValOf (CVar "next_msg" (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) cAnn)
                        (CSCompound (cases ++
                            [
                                -- default:
                                CBlockStmt $ CSDefault
                                    -- rtems_shutdown_executive(1);
                                    (CSDo (cRTEMSShutdownExecutiveCall cAnn) (stmt True)) (stmt True),
                                    -- break;
                                CBlockStmt $ CSBreak (internalAnn (CStatementAnn True True))
                            ])
                        compoundAnn) (stmt True)
                ] compoundAnn

        genBody :: CSourceGenerator [CCompoundBlockItem]
        genBody = do
            let declStmt before = internalAnn (CDeclarationAnn before)
                stmt before = internalAnn (CStatementAnn before False)
                cAnn = internalAnn CGenericAnn
            msgDataVars <- getMsgDataVariables actions
            loop <- genLoop
            return $ [
                    -- ClassIdentifier self = (ClassIdentifier *)&arg;
                    CBlockDecl (CDecl (CTypeSpec (CTTypeDef classId noqual)) (Just "self")
                        (Just (CExprCast (CExprAddrOf 
                                (CVar "arg" (CTTypeDef "rtems_task_argument" noqual)) 
                                (CTPointer (CTTypeDef "rtems_task_argument" noqual) constqual) cAnn) (CTPointer (CTTypeDef classId noqual) noqual) cAnn))) (declStmt False),
                    
                    -- rtems_status_code status = RTEMS_SUCCESSFUL;
                    CBlockDecl (CDecl (CTypeSpec (CTTypeDef "rtems_status_code" noqual)) (Just "status")
                        (Just (CExprValOf (CVar "RTEMS_SUCCESSFUL" cRTEMSStatusCodeType) cRTEMSStatusCodeType cAnn))) (declStmt False),
                    -- uint32_t next_msg = 0;
                    CBlockDecl (CDecl (CTypeSpec (CTInt IntSize32 Unsigned noqual)) (Just "next_msg")
                        (Just (CExprConstant (CIntConst (CInteger 0 CDecRepr)) (CTInt IntSize32 Unsigned noqual) cAnn))) (declStmt False),
                    -- size_t size = 0;
                    CBlockDecl (CDecl (CTypeSpec (CTSizeT noqual)) (Just "size")
                        (Just (CExprConstant (CIntConst (CInteger 0 CDecRepr)) (CTSizeT noqual) cAnn))) (declStmt False),
                    -- Result result;
                    CBlockDecl (CDecl (CTypeSpec (CTTypeDef "Result" noqual)) (Just "result") Nothing) (declStmt False),
                    -- result.__variant = Result__Ok;
                    CBlockStmt (CSDo (CExprAssign (CField (CVar "result" (CTTypeDef "Result" noqual)) enumVariantsField enumFieldType) 
                        (CExprValOf (CVar ("Result" <::> "Ok") enumFieldType) enumFieldType cAnn) enumFieldType cAnn) (stmt False))

                ] ++ msgDataVars ++
                [
                    CBlockStmt $ CSFor (Left Nothing) Nothing Nothing loop (stmt True),
                    CBlockStmt $ CSDo (cRTEMSShutdownExecutiveCall cAnn) (stmt True)
                ]
genTaskClassCode obj = throwError $ InternalError $ "Invalid global object (not a task): " ++ show obj

emitterToArrayMap :: M.Map Identifier Integer
emitterToArrayMap = M.fromList [("irq_0", 0), ("irq_1", 1), ("irq_2", 2), ("irq_3", 3), ("irq_4", 4)]

genArmTimer :: CObject -> Identifier -> CSourceGenerator [CCompoundBlockItem]
genArmTimer cObj identifier = do
    let cAnn = internalAnn CGenericAnn
        stmtAnn = internalAnn (CStatementAnn True False)
    return [
            -- __termina__add_timeval(&timer.__timer.current, timer.period);
            CBlockStmt $ CSDo (CExprCall (CExprValOf (CVar (namefy $ "termina" <::> "add_timeval") cTerminaAddTimeValFunctionType) cTerminaAddTimeValFunctionType cAnn)
                [
                    CExprAddrOf (CField 
                            (CField cObj (namefy "timer") (CTTypeDef "__termina_timer_t" noqual)) "current" (CTPointer (CTTypeDef "TimeVal" noqual) noqual)
                        ) (CTPointer cRTEMSIdType noqual) cAnn,
                    CExprAddrOf (CField cObj "period" (CTTypeDef "TimeVal" noqual)) (CTPointer (CTTypeDef "TimeVal" noqual) noqual) cAnn
                ] (CTVoid noqual) cAnn) stmtAnn,
            -- status = rtems_timer_fire_after(timer.__timer.timer_id, &timer.__timer.current, timer.period);
            CBlockStmt $ CSDo (CExprAssign (CVar "status" cRTEMSStatusCodeType)
                    (CExprCall (CExprValOf (CVar (namefy $ "rtems" <::> "timer_delay_at") cRTEMSTimerDelayAtFunctionType) cRTEMSTimerDelayAtFunctionType cAnn)
                        [
                            CExprAddrOf (CField 
                                    (CField cObj (namefy "timer") (CTTypeDef "__termina_timer_t" noqual)) "timer_id" cRTEMSIdType
                                ) (CTPointer cRTEMSIdType noqual) cAnn,
                            CExprAddrOf (CField 
                                    (CField cObj (namefy "timer") (CTTypeDef "__termina_timer_t" noqual)) "current" (CTPointer (CTTypeDef "TimeVal" noqual) noqual)
                                ) (CTPointer cRTEMSIdType noqual) cAnn,
                            CExprValOf (CVar (namefy "rtems_periodic_timer" <::> identifier) (CTTypeDef "rtems_timer_service_routine_entry" noqual)) (CTTypeDef "rtems_timer_service_routine_entry" noqual) cAnn
                        ] cRTEMSStatusCodeType cAnn) cRTEMSStatusCodeType cAnn) stmtAnn
        ]

genEmitter :: RTEMSEmitter -> CSourceGenerator CFileItem
genEmitter (RTEMSInterruptEmitter interrupt (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) = do
    let cAnn = internalAnn CGenericAnn
        declStmt = internalAnn (CDeclarationAnn True)
        irqArray = emitterToArrayMap M.! interrupt
    classFunctionName <- genClassFunctionName classId action
    return $  CFunctionDef Nothing $ 
        -- void * rtems_isr_interrupt(void * ignored) {
        CFunction (CTVoid noqual) (namefy "rtems_isr" <::> interrupt)
            [CDecl (CTypeSpec (CTVoid noqual)) (Just "_ignored") Nothing]
            (CSCompound [
                -- classId * self = &identifier;
                CBlockDecl (CDecl (CTypeSpec (CTPointer (CTTypeDef classId noqual) noqual)) (Just "self")
                    (Just $ CExprAddrOf (CVar identifier (CTTypeDef classId noqual)) (CTTypeDef classId noqual) cAnn)) declStmt,
                -- Result result;
                CBlockDecl (CDecl (CTypeSpec (CTTypeDef "Result" noqual)) (Just "result") Nothing) declStmt,
                -- result.__variant = Result__Ok;
                CBlockStmt $ CSDo (CExprAssign
                    (CField (CVar "result" (CTTypeDef "Result" noqual)) enumVariantsField enumFieldType)
                    (CExprValOf (CVar ("Result" <::> "Ok") enumFieldType) enumFieldType cAnn) enumFieldType cAnn) (internalAnn (CStatementAnn True False)),
                -- result = classFunctionName(self, interrupt);
                CBlockStmt $ CSDo (CExprAssign
                    (CVar "result" (CTTypeDef "Result" noqual))
                    (CExprCall (CExprValOf (CVar classFunctionName (cIrqHandlerActionFunctionType classId)) (cIrqHandlerActionFunctionType classId) cAnn)
                        [
                            CExprValOf (CVar "self" (CTPointer (CTTypeDef classId noqual) noqual)) (CTPointer (CTTypeDef classId noqual) noqual) cAnn, 
                            CExprConstant (CIntConst (CInteger irqArray CDecRepr)) (CTInt IntSize32 Unsigned noqual) cAnn
                        ] (CTTypeDef "Result" noqual) cAnn) (CTTypeDef "Result" noqual) cAnn) (internalAnn (CStatementAnn True False)),
                -- if (result.__variant != Result__Ok)
                CBlockStmt $ CSIfThenElse (
                        CExprBinaryOp COpNe 
                            (CExprValOf (CField (CVar "result" (CTTypeDef "Result" noqual)) enumVariantsField enumFieldType) enumFieldType cAnn)
                            (CExprValOf (CVar ("Result" <::> "Ok") enumFieldType) enumFieldType cAnn) (CTBool noqual) cAnn)
                    (CSCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CSDo (cRTEMSShutdownExecutiveCall cAnn) (internalAnn (CStatementAnn False False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True True)),
                CBlockStmt $ CSReturn Nothing (internalAnn (CStatementAnn True False))
            ] (internalAnn (CCompoundAnn False True))) declStmt
genEmitter (RTEMSInterruptEmitter interrupt (RTEMSTask {})) = do
    let cAnn = internalAnn CGenericAnn
        declStmt = internalAnn (CDeclarationAnn True)
        irqArray = emitterToArrayMap M.! interrupt
    return $ CFunctionDef Nothing $ 
        -- void * rtems_isr_interrupt(void * ignored) {
        CFunction (CTVoid noqual) (namefy "rtems_isr" <::> interrupt) 
            [CDecl (CTypeSpec (CTVoid noqual)) (Just "_ignored") Nothing]
            (CSCompound [
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl (CDecl (CTypeSpec cRTEMSStatusCodeType) (Just "status")
                    (Just (CExprValOf (CVar "RTEMS_SUCCESSFUL" cRTEMSStatusCodeType) cRTEMSStatusCodeType cAnn))) declStmt,
                -- uint32_t vector = interrupt;
                CBlockDecl (CDecl (CTypeSpec (CTInt IntSize32 Unsigned noqual)) (Just "vector")
                    (Just (CExprConstant (CIntConst (CInteger irqArray CDecRepr)) (CTInt IntSize32 Unsigned noqual) cAnn))) declStmt,
                -- status = rtems_message_queue_send(interrupt.sink_msgq_id, &interrupt.task_port, sizeof(uint32_t));
                CBlockStmt $ CSDo (CExprAssign
                    (CVar "status" cRTEMSStatusCodeType)
                    (CExprCall (CExprValOf (CVar "rtems_message_queue_send" cRTEMSMessageQueueSendType) cRTEMSMessageQueueSendType cAnn)
                        [
                            CExprValOf (CField (CVar interrupt (CTTypeDef ("rtems" <::> "interrupt_emitter_t") noqual)) "sink_msgq_id" cRTEMSIdType) cRTEMSIdType cAnn,
                            CExprAddrOf (CVar "vector" (CTInt IntSize32 Unsigned noqual)) (CTPointer (CTInt IntSize32 Unsigned noqual) noqual) cAnn,
                            CExprSizeOfType (CTInt IntSize32 Unsigned noqual) (CTSizeT noqual) cAnn
                        ] cRTEMSStatusCodeType cAnn) cRTEMSStatusCodeType cAnn) (internalAnn (CStatementAnn True False)),
                -- if (RTEMS_SUCCESSFUL == status)
                CBlockStmt $ CSIfThenElse (
                        CExprBinaryOp COpEq
                            (CExprValOf (CVar "RTEMS_SUCCESSFUL" cRTEMSStatusCodeType) cRTEMSStatusCodeType cAnn)
                            (CExprValOf (CVar "status" cRTEMSStatusCodeType) cRTEMSStatusCodeType cAnn) (CTBool noqual) cAnn)
                    (CSCompound [
                        -- status = rtems_message_queue_send(interrupt.task_msgq_id, &vector, sizeof(uint32_t));
                        CBlockStmt $ CSDo (CExprAssign
                            (CVar "status" cRTEMSStatusCodeType)
                            (CExprCall (CExprValOf (CVar "rtems_message_queue_send" cRTEMSMessageQueueSendType) cRTEMSMessageQueueSendType cAnn)
                                [
                                    CExprValOf (CField (CVar interrupt (CTTypeDef ("rtems" <::> "interrupt_emitter_t") noqual)) "task_msgq_id" cRTEMSIdType) cRTEMSIdType cAnn,
                                    CExprAddrOf (CField (CVar "vector" (CTInt IntSize32 Unsigned noqual)) "task_port" enumFieldType) (CTPointer (CTInt IntSize32 Unsigned noqual) noqual) cAnn,
                                    CExprSizeOfType (CTInt IntSize32 Unsigned noqual) (CTSizeT noqual) cAnn
                                ] cRTEMSStatusCodeType cAnn) cRTEMSStatusCodeType cAnn) (internalAnn (CStatementAnn True False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True False)),
                -- if (RTEMS_SUCCESSFUL != status)
                CBlockStmt $ CSIfThenElse (
                        CExprBinaryOp COpNe 
                            (CExprValOf (CVar "RTEMS_SUCCESSFUL" cRTEMSStatusCodeType) cRTEMSStatusCodeType cAnn)
                            (CExprValOf (CVar "status" cRTEMSStatusCodeType) cRTEMSStatusCodeType cAnn) (CTBool noqual) cAnn)
                    (CSCompound [
                        -- break;
                        CBlockStmt $ CSBreak (internalAnn (CStatementAnn False False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True False)),
                -- return;
                CBlockStmt $ CSReturn Nothing (internalAnn (CStatementAnn True False))
            ] (internalAnn (CCompoundAnn False True))) declStmt
genEmitter (RTEMSInterruptEmitter _ glb) = throwError $ InternalError $ "Invalid connection for interrupt: " ++ show glb
genEmitter (RTEMSPeriodicTimerEmitter timer (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) = do
    let cAnn = internalAnn CGenericAnn
        declStmt = internalAnn (CDeclarationAnn True)
    classFunctionName <- genClassFunctionName classId action
    armTimer <- genArmTimer (CVar timer (CTTypeDef "PeriodicTimer" noqual)) timer
    return $ CFunctionDef Nothing $ 
        -- void * rtems_periodic_timer(void * _timer_id, void * _argument) {
        CFunction (CTPointer (CTVoid noqual) noqual) (namefy "rtems_periodic_timer" <::> timer) 
            [
                CDecl (CTypeSpec cRTEMSIdType) (Just "_timer_id") Nothing,
                CDecl (CTypeSpec (CTPointer (CTVoid noqual) noqual)) (Just "_ignored") Nothing
            ]
            (CSCompound ([
                -- classId * self = &identifier;
                CBlockDecl (CDecl (CTypeSpec (CTPointer (CTTypeDef classId noqual) noqual)) (Just "self")
                    (Just $ CExprAddrOf (CVar identifier (CTTypeDef classId noqual)) (CTTypeDef classId noqual) cAnn)) declStmt,
                -- Result result;
                CBlockDecl (CDecl (CTypeSpec (CTTypeDef "Result" noqual)) (Just "result") Nothing) declStmt,
                -- Result result;
                CBlockDecl (CDecl (CTypeSpec (CTTypeDef "Result" noqual)) (Just "result") Nothing) declStmt,
                -- result.__variant = Result__Ok;
                CBlockStmt $ CSDo (CExprAssign
                    (CField (CVar "result" (CTTypeDef "Result" noqual)) enumVariantsField enumFieldType)
                    (CExprValOf (CVar ("Result" <::> "Ok") enumFieldType) enumFieldType cAnn) enumFieldType cAnn) (internalAnn (CStatementAnn True False)),
                -- result = classFunctionName(self, timer.current);
                CBlockStmt $ CSDo (CExprAssign
                    (CVar "result" (CTTypeDef "Result" noqual))
                    (CExprCall (CExprValOf (CVar classFunctionName (cIrqHandlerActionFunctionType classId)) (cIrqHandlerActionFunctionType classId) cAnn)
                        [
                            CExprValOf (CVar "self" (CTPointer (CTTypeDef classId noqual) noqual)) (CTPointer (CTTypeDef classId noqual) noqual) cAnn, 
                            CExprValOf (CField (CVar timer (CTTypeDef "PeriodicTimer" noqual)) "current" (CTTypeDef "TimeVal" noqual)) (CTTypeDef "TimeVal" noqual) cAnn
                        ] (CTTypeDef "Result" noqual) cAnn) (CTTypeDef "Result" noqual) cAnn) (internalAnn (CStatementAnn True False)),
                -- if (result.__variant != Result__Ok)
                CBlockStmt $ CSIfThenElse (
                        CExprBinaryOp COpNe 
                            (CExprValOf (CField (CVar "result" (CTTypeDef "Result" noqual)) enumVariantsField enumFieldType) enumFieldType cAnn)
                            (CExprValOf (CVar ("Result" <::> "Ok") enumFieldType) enumFieldType cAnn) (CTBool noqual) cAnn)
                    (CSCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CSDo (cRTEMSShutdownExecutiveCall cAnn) (internalAnn (CStatementAnn False False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True True))
            ] ++ armTimer ++ [
                -- if (result.__variant != Result__Ok)
                CBlockStmt $ CSIfThenElse (
                        CExprBinaryOp COpNe 
                            (CExprValOf (CField (CVar "result" (CTTypeDef "Result" noqual)) enumVariantsField enumFieldType) enumFieldType cAnn)
                            (CExprValOf (CVar ("Result" <::> "Ok") enumFieldType) enumFieldType cAnn) (CTBool noqual) cAnn)
                    (CSCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CSDo (cRTEMSShutdownExecutiveCall cAnn) (internalAnn (CStatementAnn False False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True True)),
                CBlockStmt $ CSReturn Nothing (internalAnn (CStatementAnn True False))
            ] ++ armTimer ++ [
            ]) (internalAnn (CCompoundAnn False True))) declStmt
{--
genEmitter (RTEMSPeriodicTimerEmitter timer (RTEMSTask {})) = do
    let cAnn = internalAnn CGenericAnn
        declStmt = internalAnn (CDeclarationAnn True)
    armTimer <- genArmTimer (CVar timer (CTTypeDef "PeriodicTimer" noqual)) timer
    return $ CFunctionDef Nothing $ 
        -- void * rtems_periodic_timer(void * _timer_id, void * _argument) {
        CFunDef (CTPointer (CTVoid noqual) noqual) (namefy "rtems_periodic_timer" <::> timer)
            [
                CDecl (CTypeSpec cRTEMSIdType) (Just "_timer_id") Nothing,
                CDecl (CTypeSpec (CTPointer (CTVoid noqual) noqual)) (Just "_ignored") Nothing
            ]
            (CSCompound ([
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl $ CDecl [CTypeSpec $ CTypeDef "rtems_status_code"]
                    [(Just $ CDeclarator (Just "status") [] [] cAnn, Just $ CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn, Nothing)] declStmt,
                -- status = rtems_message_queue_send(interrupt.sink_msgq_id, &interrupt.task_port, sizeof(uint32_t));
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "status" cAnn)
                    (CCall (CVar "rtems_message_queue_send" cAnn)
                        [CMember (CMember (CVar timer cAnn) (namefy "timer") False cAnn) "sink_msgq_id" False cAnn,
                        CUnary CAdrOp (CMember (CMember (CVar timer cAnn) (namefy "timer") False cAnn) "current" False cAnn) cAnn,
                        CSizeofType (CDecl [CTypeSpec $ CTypeDef "TimeVal"] [] (internalAnn (CDeclarationAnn False))) cAnn] cAnn) cAnn) (internalAnn (CStatementAnn True False)),
                -- if (RTEMS_SUCCESSFUL == status)
                CBlockStmt $ CIf (CBinary CEqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- status = rtems_message_queue_send(interrupt.task_msgq_id, &vector, sizeof(uint32_t));
                        CBlockStmt $ CExpr (Just $ CAssignment
                            (CVar "status" cAnn)
                            (CCall (CVar "rtems_message_queue_send" cAnn)
                                [CMember (CMember (CVar timer cAnn) (namefy "timer") False cAnn) "task_msgq_id" False cAnn,
                                CUnary CAdrOp (CMember (CMember (CVar timer cAnn) (namefy "timer") False cAnn) "task_port" False cAnn) cAnn,
                                CSizeofType (CDecl [CTypeSpec CUInt32Type] [] (internalAnn (CDeclarationAnn False))) cAnn] cAnn) cAnn) (internalAnn (CStatementAnn True False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True False)),
                -- if (RTEMS_SUCCESSFUL != status)
                CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True False))
            ] ++ armTimer ++ [
                -- if (RTEMS_SUCCESSFUL != status)
                CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True False)),
                CBlockStmt $ CReturn Nothing (internalAnn (CStatementAnn True False))
            ]) (internalAnn (CCompoundAnn False True))) declStmt
genEmitter (RTEMSPeriodicTimerEmitter _ glb) = throwError $ InternalError $ "Invalid connection for timer: " ++ show glb
genEmitter (RTEMSSystemInitEmitter _ (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) = do
    let cAnn = internalAnn CGenericAnn
        declStmt = internalAnn (CDeclarationAnn True)
    classFunctionName <- genClassFunctionName classId action
    return $ CExtDecl $ CFDefExt $
        -- void * rtems_periodic_timer(void * _timer_id, void * _argument) {
        CFunDef [CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "inital_event") [CFunDeclr
                [CDecl
                    [CTypeSpec $ CTypeDef "TimeVal"] [(Just (CDeclarator (Just "current") [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] (internalAnn (CDeclarationAnn False))] [] cAnn] [] cAnn)
            (CCompound [
                -- classId * self = &identifier;
                CBlockDecl $ CDecl [CTypeSpec $ CTypeDef classId]
                    [(Just $ CDeclarator (Just "self") [CPtrDeclr [] cAnn] [] cAnn, Just $ CInitExpr (CUnary CAdrOp (CVar identifier cAnn) cAnn) cAnn, Nothing)] declStmt,
                -- Result result;
                CBlockDecl $ CDecl [CTypeSpec $ CTypeDef "Result"]
                    [(Just $ CDeclarator (Just "result") [] [] cAnn, Nothing, Nothing)] declStmt,
                -- result.__variant = Result__Ok;
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn) (internalAnn (CStatementAnn True False)),
                -- result = classFunctionName(self, *current);
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "result" cAnn)
                    (CCall (CVar classFunctionName cAnn)
                        [CVar "self" cAnn, CUnary CIndOp (CVar "current" cAnn) cAnn] cAnn) cAnn) (internalAnn (CStatementAnn True False)),
                -- if (result.__variant != Result__Ok)
                CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True False)),
                CBlockStmt $ CReturn Nothing (internalAnn (CStatementAnn True False))
            ] (internalAnn (CCompoundAnn False True))) declStmt
genEmitter (RTEMSSystemInitEmitter event (RTEMSTask identifier classId _ _ _ ports)) = do
    let cAnn = internalAnn CGenericAnn
        declStmt = internalAnn (CDeclarationAnn True)
    action <- case find (\case { RTEMSEventPort _ emitter _ _ -> event == emitter; _ -> False }) ports of
        Just (RTEMSEventPort _ _ _ actionId) -> return actionId
        _ -> throwError $ InternalError $ "Invalid port connection for interrupt: " ++ show event
    classFunctionName <- genClassFunctionName classId action
    return $ CExtDecl $ CFDefExt $
        -- void * rtems_periodic_timer(void * _timer_id, void * _argument) {
        CFunDef [CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "inital_event") [CFunDeclr
                [CDecl
                    [CTypeSpec $ CTypeDef "TimeVal"] [(Just (CDeclarator (Just "current") [] [] cAnn), Nothing, Nothing)] (internalAnn (CDeclarationAnn False))] [] cAnn] [] cAnn)
            (CCompound [
                -- classId * self = &identifier;
                CBlockDecl $ CDecl [CTypeSpec $ CTypeDef classId]
                    [(Just $ CDeclarator (Just "self") [CPtrDeclr [] cAnn] [] cAnn, Just $ CInitExpr (CUnary CAdrOp (CVar identifier cAnn) cAnn) cAnn, Nothing)] declStmt,
                -- Result result;
                CBlockDecl $ CDecl [CTypeSpec $ CTypeDef "Result"]
                    [(Just $ CDeclarator (Just "result") [] [] cAnn, Nothing, Nothing)] declStmt,
                -- result.__variant = Result__Ok;
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn) (internalAnn (CStatementAnn True False)),
                -- result = classFunctionName(self, timer.current);
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "result" cAnn)
                    (CCall (CVar classFunctionName cAnn)
                        [CVar "self" cAnn, CVar "current" cAnn] cAnn) cAnn) (internalAnn (CStatementAnn True False)),
                -- if (result.__variant != Result__Ok)
                CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True False)),
                CBlockStmt $ CReturn Nothing (internalAnn (CStatementAnn True False))
            ] (internalAnn (CCompoundAnn False True))) declStmt
genEmitter _ = error "Invalid emitter"

-- | Function __rtems_app__enable_protection. This function is called from the Init task.
-- It enables the protection of the shared resources when needed. In case the resource uses a mutex,
-- it also initializes the mutex. The function is called AFTER the initialization of the tasks and handlers.
genEnableProtection :: M.Map Identifier RTEMSResourceLock -> CSourceGenerator CFileItem
genEnableProtection resLockingMap = do
    let cAnn = internalAnn CGenericAnn
        declStmt = internalAnn (CDeclarationAnn True)
    initResourcesProt <- concat <$> mapM genInitResourceProt (M.toList resLockingMap)
    return $ CExtDecl $ CFDefExt $
        -- void * rtems_periodic_timer(void * _timer_id, void * _argument) {
        CFunDef [CStorageSpec CStatic, CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "enable_protection") [CFunDeclr [] [] cAnn] [] cAnn)
            (CCompound ([
                -- Result result;
                CBlockDecl $ CDecl [CTypeSpec $ CTypeDef "Result"]
                    [(Just $ CDeclarator (Just "result") [] [] cAnn, Nothing, Nothing)] declStmt,
                -- result.__variant = Result__Ok;
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn) (internalAnn (CStatementAnn True False))
            ] ++ initResourcesProt) (internalAnn (CCompoundAnn False True))) declStmt
    where

        genInitResourceProt :: (Identifier, RTEMSResourceLock) -> CSourceGenerator [CCompoundBlockItem]
        genInitResourceProt (identifier, RTEMSResourceLockNone) = do
            let cAnn = internalAnn CGenericAnn
                stmt before expand = internalAnn (CStatementAnn before expand)
            return [
                CBlockStmt $ CExpr (Just $
                    CAssignment (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "lock" False cAnn)
                        (CVar (namefy "RTEMSResourceLock__None") cAnn) cAnn) (stmt True False)
                ]
        genInitResourceProt (identifier, RTEMSResourceLockIrq) = do
            let cAnn = internalAnn CGenericAnn
                stmt before expand = internalAnn (CStatementAnn before expand)
            return [
                CBlockStmt $ CExpr (Just $
                    CAssignment (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "lock" False cAnn)
                        (CVar (namefy "RTEMSResourceLock__Irq") cAnn) cAnn) (stmt True False)
                ]
        genInitResourceProt (identifier, RTEMSResourceLockMutex ceilPrio) = do
            let cAnn = internalAnn CGenericAnn
                stmt before expand = internalAnn (CStatementAnn before expand)
                cCeilPrio = genInteger ceilPrio
            return [
                CBlockStmt $ CExpr (Just $
                    CAssignment (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "lock" False cAnn)
                        (CVar (namefy "RTEMSResourceLock__Mutex") cAnn) cAnn) (stmt True False),
                CBlockStmt $ CExpr (Just $
                    CAssignment (CMember (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "mutex" False cAnn) "policy" False cAnn)
                        (CVar (namefy "RTEMSMutexPolicy__Ceiling") cAnn) cAnn) (stmt False False),
                CBlockStmt $ CExpr (Just $
                    CAssignment (CMember (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "mutex" False cAnn) "prio_ceiling" False cAnn)
                        (CConst (CIntConst cCeilPrio) cAnn) cAnn) (stmt False False),
               -- result = classFunctionName(self, timer.current);
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "result" cAnn)
                    (CCall (CVar (namefy $ "termina_resource" <::> "init") cAnn)
                        [CUnary CAdrOp (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) cAnn] cAnn) cAnn) (internalAnn (CStatementAnn True False)),
                -- if (result.__variant != Result__Ok)
                CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing (internalAnn (CStatementAnn True False))
                ]

-- | Function __rtems_app__init_globals. This function is called from the Init task.
-- The function is called BEFORE the initialization of the tasks and handlers. The function disables
-- the protection of the global resources, since it is not needed when running in the Init task. It also
-- executes the init() method of the resources if defined.
genInitGlobals ::
    -- | Resources 
    [RTEMSGlobal]
    -- | Pools
    -> [RTEMSGlobal]
    -- | Task Message Queues
    -> [RTEMSMsgQueue]
    -- | Channel Message Queues  
    -> [RTEMSMsgQueue]
    -- | Interrupt emitters connected to tasks
    -> [RTEMSEmitter]
    -- | Timers connected to tasks
    -> [RTEMSEmitter]
    -- | Complete list of tasks
    -> [RTEMSGlobal]
    -- | Complete list of timers
    -> [RTEMSEmitter]
    -> CSourceGenerator CFileItem
genInitGlobals resources pools tasksMessageQueues channelMessageQueues interruptEmittersToTasks timersToTasks tasks timers = do
    let cAnn = internalAnn CGenericAnn
        declStmt before = internalAnn (CDeclarationAnn before)
    initResources <- mapM genInitResource resources
    initPools <- concat <$> mapM genInitPool pools
    cTaskMessageQueues <- concat <$> mapM genRTEMSCreateMsgQueue tasksMessageQueues
    cChannelMessageQueues <- concat <$> mapM genRTEMSCreateMsgQueue channelMessageQueues
    cInterruptEmittersToTasks <- concat <$> mapM genInitInterruptEmitterToTask interruptEmittersToTasks
    cTimersToTasks <- concat <$> mapM genInitTimerToTask timersToTasks
    cTaskInitialization <- concat <$> mapM genTaskInitialization tasks
    cCreateTimers <- concat <$> mapM genRTEMSCreateTimer timers
    return $ CExtDecl $ CFDefExt $
        -- static void __rtems_app__init_globals() {
        CFunDef [CStorageSpec CStatic, CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "init_globals") [CFunDeclr [] [] cAnn] [] cAnn)
            (CCompound ([
                -- Result result;
                CBlockDecl $ CDecl [CTypeSpec $ CTypeDef "Result"]
                    [(Just $ CDeclarator (Just "result") [] [] cAnn, Nothing, Nothing)] (declStmt True),
                -- result.__variant = Result__Ok;
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn) (internalAnn (CStatementAnn True False))
            ] ++ initResources ++ initPools
            ++ (if not (null tasksMessageQueues) || not (null channelMessageQueues) || not (null timers) then
                [
                    -- rtems_status_code status = RTEMS_SUCCESSFUL;
                    CBlockDecl $ CDecl [CTypeSpec $ CTypeDef "rtems_status_code"]
                        [(Just $ CDeclarator (Just "status") [] [] cAnn,
                          Just (CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn), Nothing)] (declStmt True)
                ] else []) ++ cTaskMessageQueues ++ cChannelMessageQueues
                ++ cInterruptEmittersToTasks ++ cTimersToTasks ++ cTaskInitialization ++ cCreateTimers
            ) (internalAnn (CCompoundAnn False True))) (declStmt True)

    where

        genInitResource :: RTEMSGlobal -> CSourceGenerator CCompoundBlockItem
        genInitResource (RTEMSResource identifier _ _) = do
            let cAnn = internalAnn CGenericAnn
            return $ CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "lock" False cAnn)
                    (CVar "__RTEMSResourceLock__None" cAnn) cAnn) (internalAnn (CStatementAnn True False))
        genInitResource obj = throwError $ InternalError $ "Invalid global object (not a resource): " ++ show obj

        genInitPool :: RTEMSGlobal -> CSourceGenerator [CCompoundBlockItem]
        genInitPool (RTEMSPool identifier ts _) = do
            let cAnn = internalAnn CGenericAnn
                cStmtAnn = internalAnn (CStatementAnn True False)
            declSpec <- genDeclSpecifiers ts
            return
                [
                    -- identifier.resource.lock = __RTEMSResourceLock__None;
                    CBlockStmt $ CExpr (Just $ CAssignment
                        (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "lock" False cAnn)
                        (CVar "__RTEMSResourceLock__None" cAnn) cAnn) cStmtAnn,
                    -- result = __termina_pool__init(&identifier, (void *)memory_area_identifier)
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "result" cAnn)
                            (CCall (CVar (namefy "termina_pool" <::> "init") cAnn)
                                [
                                    CUnary CAdrOp (CVar identifier cAnn) cAnn,
                                    CCast (CDecl [CTypeSpec CVoidType]
                                        [(Just $ CDeclarator Nothing [CPtrDeclr [] cAnn] [] cAnn, Nothing, Nothing)] (internalAnn (CDeclarationAnn False)))
                                        (CVar (poolMemoryArea identifier) cAnn) cAnn,
                                    CSizeofExpr (CVar (poolMemoryArea identifier) cAnn) cAnn,
                                    CSizeofType (CDecl declSpec [] (internalAnn (CDeclarationAnn False))) cAnn
                                    ] cAnn) cAnn) cStmtAnn,
                    -- if (result.__variant != Result__Ok)
                    CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                        (CVar ("Result" <::> "Ok") cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                        ] (internalAnn (CCompoundAnn False False))) Nothing cStmtAnn
                ]
        genInitPool obj = throwError $ InternalError $ "Invalid global object (not a pool): " ++ show obj

        -- | Prints the code to initialize a message queue. The function is called to generate the code for the
        -- message queues corresponding to the channels declared by the user plus the ones that belong to each
        -- of the tasks that is used to notify the inclusion of a given message on a specific queue.
        genRTEMSCreateMsgQueue :: RTEMSMsgQueue -> CSourceGenerator [CCompoundBlockItem]
        genRTEMSCreateMsgQueue (RTEMSChannelMsgQueue identifier ts size (RTEMSTask taskId classId _ _ _ ports)) = do
            let cAnn = internalAnn CGenericAnn
                cStmtAnn before = internalAnn (CStatementAnn before False)
                cSize = genInteger size
            declSpec <- genDeclSpecifiers ts
            variantForPort <- genVariantForPort classId port
            return
                [
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "task_msgq_id" False cAnn)
                            (CMember (CMember (CVar taskId cAnn) taskClassIDField False cAnn) "msgq_id" False cAnn) cAnn) (cStmtAnn True),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "task_port" False cAnn)
                            (CVar variantForPort cAnn) cAnn) (cStmtAnn False),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "message_size" False cAnn)
                            (CSizeofType (CDecl declSpec [] (internalAnn (CDeclarationAnn False))) cAnn) cAnn) (cStmtAnn False),
                    -- statuss = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                            (CCall (CVar (namefy "rtems" <::> "create_msg_queue") cAnn)
                                [
                                    CConst (CIntConst cSize) cAnn,
                                    CSizeofType (CDecl declSpec [] (internalAnn (CDeclarationAnn False))) cAnn,
                                    CUnary CAdrOp (CMember (CVar identifier cAnn) "msgq_id" False cAnn) cAnn
                                ] cAnn) cAnn) (cStmtAnn True),
                    CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                        ] (internalAnn (CCompoundAnn False False))) Nothing (cStmtAnn True)
                ]
            where

                port = case find (\case {
                    RTEMSInputPort _ chid _ _ -> chid == identifier;
                    _ -> False }) ports of
                        Just (RTEMSInputPort prt _ _ _) -> prt
                        _ -> error $ "Invalid port connection for channel: " ++ show identifier

        genRTEMSCreateMsgQueue obj@(RTEMSChannelMsgQueue {}) = throwError $ InternalError $ "Invalid channel objet: " ++ show obj
        genRTEMSCreateMsgQueue (RTEMSTaskMsgQueue identifier size) = do
            let cAnn = internalAnn CGenericAnn
                cStmtAnn = internalAnn (CStatementAnn True False)
                cSize = genInteger size
            return
                [
                    -- statuss = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                            (CCall (CVar (namefy "rtems" <::> "create_msg_queue") cAnn)
                                [
                                    CConst (CIntConst cSize) cAnn,
                                    CSizeofType (CDecl [CTypeSpec CUInt32Type] [] (internalAnn (CDeclarationAnn False))) cAnn,
                                    CUnary CAdrOp (CMember (CMember (CVar identifier cAnn) taskClassIDField False cAnn) "msgq_id" False cAnn) cAnn
                                ] cAnn) cAnn) cStmtAnn,
                    CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                        ] (internalAnn (CCompoundAnn False False))) Nothing cStmtAnn
                ]
        genRTEMSCreateMsgQueue (RTEMSSinkPortMsgQueue taskId portId ts size) = do
            let cAnn = internalAnn CGenericAnn
                cStmtAnn = internalAnn (CStatementAnn True False)
                cSize = genInteger size
            declSpec <- genDeclSpecifiers ts
            return
                [
                    -- status = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                            (CCall (CVar (namefy "rtems" <::> "create_msg_queue") cAnn)
                                [
                                    CConst (CIntConst cSize) cAnn,
                                    CSizeofType (CDecl declSpec [] (internalAnn (CDeclarationAnn False))) cAnn,
                                    CUnary CAdrOp (CMember (CVar taskId cAnn) portId False cAnn) cAnn
                                ] cAnn) cAnn) cStmtAnn,
                    CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                        ] (internalAnn (CCompoundAnn False False))) Nothing cStmtAnn
                ]

        genInitInterruptEmitterToTask :: RTEMSEmitter -> CSourceGenerator [CCompoundBlockItem]
        genInitInterruptEmitterToTask (RTEMSInterruptEmitter identifier (RTEMSTask taskId classId _ _ _ ports)) = do
            let cAnn = internalAnn CGenericAnn
                cStmtAnn before = internalAnn (CStatementAnn before False)
            variantForPort <- genVariantForPort classId port
            return
                [
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "task_msgq_id" False cAnn)
                            (CMember (CMember (CVar taskId cAnn) taskClassIDField False cAnn) "msgq_id" False cAnn) cAnn) (cStmtAnn True),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "sink_msgq_id" False cAnn)
                            (CMember (CVar taskId cAnn) port False cAnn) cAnn) (cStmtAnn False),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "task_port" False cAnn)
                            (CVar variantForPort cAnn) cAnn) (cStmtAnn False)
                ]

            where
                port = case find (\case {
                    RTEMSEventPort _ chid _ _ -> chid == identifier;
                    _ -> False }) ports of
                        Just (RTEMSEventPort prt _ _ _) -> prt
                        _ -> error $ "Invalid port connection for channel: " ++ show identifier
        genInitInterruptEmitterToTask obj = throwError $ InternalError $ "Invalid global object (not an interrupt emitter connected to a task): " ++ show obj

        genInitTimerToTask :: RTEMSEmitter -> CSourceGenerator [CCompoundBlockItem]
        genInitTimerToTask (RTEMSPeriodicTimerEmitter identifier (RTEMSTask taskId classId _ _ _ ports)) = do
            let cAnn = internalAnn CGenericAnn
                cStmtAnn before = internalAnn (CStatementAnn before False)
            variantForPort <- genVariantForPort classId port
            return
                [
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CMember (CVar identifier cAnn) "__timer" False cAnn) "task_msgq_id" False cAnn)
                            (CMember (CMember (CVar taskId cAnn) taskClassIDField False cAnn) "msgq_id" False cAnn) cAnn) (cStmtAnn True),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CMember (CVar identifier cAnn) "__timer" False cAnn) "sink_msgq_id" False cAnn)
                            (CMember (CVar taskId cAnn) port False cAnn) cAnn) (cStmtAnn False),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CMember (CVar identifier cAnn) "__timer" False cAnn) "task_port" False cAnn)
                            (CVar variantForPort cAnn) cAnn) (cStmtAnn False)
                ]

            where

                port = case find (\case {
                    RTEMSEventPort _ chid _ _ -> chid == identifier;
                    _ -> False }) ports of
                        Just (RTEMSEventPort prt _ _ _) -> prt
                        _ -> error $ "Invalid port connection for channel: " ++ show identifier
        genInitTimerToTask obj = throwError $ InternalError $ "Invalid global object (not a timer connected to a task): " ++ show obj

        genTaskInitialization :: RTEMSGlobal -> CSourceGenerator [CCompoundBlockItem]
        genTaskInitialization (RTEMSTask identifier _ _ _ _ ports) = do
            mapM genInputPortInitialization inputPorts

            where

                inputPorts = filter (\case {
                    RTEMSInputPort {} -> True;
                    _ -> False }) ports

                genInputPortInitialization :: RTEMSPort -> CSourceGenerator CCompoundBlockItem
                genInputPortInitialization (RTEMSInputPort portId channelId _ _) = do
                    let cAnn = internalAnn CGenericAnn
                        cStmtAnn = internalAnn (CStatementAnn True False)
                    return $ CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) portId False cAnn)
                            (CMember (CVar channelId cAnn) "msgq_id" False cAnn) cAnn) cStmtAnn
                genInputPortInitialization obj = throwError $ InternalError $ "Invalid port object: " ++ show obj
        genTaskInitialization obj = throwError $ InternalError $ "Invalid global object (not a task): " ++ show obj

        genRTEMSCreateTimer :: RTEMSEmitter -> CSourceGenerator [CCompoundBlockItem]
        genRTEMSCreateTimer (RTEMSPeriodicTimerEmitter identifier _) = do
            let cAnn = internalAnn CGenericAnn
                cStmtAnn = internalAnn (CStatementAnn True False)
            return
                [
                    -- statuss = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                            (CCall (CVar (namefy "rtems" <::> "create_timer") cAnn)
                                [
                                    CUnary CAdrOp (CMember (CMember (CVar identifier cAnn) "__timer" False cAnn) "timer_id" False cAnn) cAnn
                                ] cAnn) cAnn) cStmtAnn,
                    CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                        ] (internalAnn (CCompoundAnn False False))) Nothing cStmtAnn
                ]
        genRTEMSCreateTimer obj = throwError $ InternalError $ "Invalid global object (not a timer): " ++ show obj

-- | Function __rtems_app__install_emitters. This function is called from the Init task.
-- The function installs the ISRs and the periodic timers. The function is called AFTER the initialization
-- of the tasks and handlers.
genInstallEmitters :: [RTEMSEmitter] -> CSourceGenerator CFileItem
genInstallEmitters emitters = do
    let cAnn = internalAnn CGenericAnn
        declStmt before = internalAnn (CDeclarationAnn before)
    installEmitters <- mapM genRTEMSInstallEmitter $ filter (\case { RTEMSSystemInitEmitter {} -> False; _ -> True }) emitters
    return $ CExtDecl $ CFDefExt $
        -- static void __rtems_app__init_globals() {
        CFunDef [CStorageSpec CStatic, CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "install_emitters") [CFunDeclr
                [CDecl
                    [CTypeSpec $ CTypeDef "TimeVal"] [(Just (CDeclarator (Just "current") [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] (internalAnn (CDeclarationAnn False))]
                [] cAnn] [] cAnn)
            (CCompound (
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl (CDecl [CTypeSpec $ CTypeDef "rtems_status_code"]
                    [(Just $ CDeclarator (Just "status") [] [] cAnn, Just $ CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn, Nothing)] (declStmt True))
             : installEmitters) (internalAnn (CCompoundAnn False True))) (declStmt True)

    where

        genRTEMSInstallEmitter :: RTEMSEmitter -> CSourceGenerator CCompoundBlockItem
        genRTEMSInstallEmitter (RTEMSInterruptEmitter interrupt _) = do
            let cAnn = internalAnn CGenericAnn
                stmtAnn = internalAnn (CStatementAnn True False)
            return $
                CBlockStmt $ CIf (CBinary CEqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                                (CCall (CVar (namefy "rtems" <::> "install_isr") cAnn)
                                    [CVar (show $ emitterToArrayMap M.! interrupt) cAnn,
                                    CVar (namefy $ "rtems_isr" <::> interrupt) cAnn] cAnn) cAnn)
                            stmtAnn
                    ] (internalAnn (CCompoundAnn False True))) Nothing stmtAnn
        genRTEMSInstallEmitter (RTEMSPeriodicTimerEmitter timer _) = do
            let cAnn = internalAnn CGenericAnn
                stmtAnn = internalAnn (CStatementAnn True False)
                cExpr = CVar timer cAnn
            armTimer <- genArmTimer cExpr timer
            return $
                CBlockStmt $ CIf (CBinary CEqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound
                        (CBlockStmt (CExpr (Just $ CAssignment
                            (CMember (CMember cExpr (namefy "timer") False cAnn) "current" False cAnn)
                            (CUnary CIndOp (CVar "current" cAnn) cAnn) cAnn) stmtAnn) :
                        armTimer)
                    (internalAnn (CCompoundAnn False False))) Nothing stmtAnn
        genRTEMSInstallEmitter (RTEMSSystemInitEmitter {}) = throwError $ InternalError $ "Initial event does not have to be installed"

genCreateTasks :: [RTEMSGlobal] -> CSourceGenerator CFileItem
genCreateTasks tasks = do
    let cAnn = internalAnn CGenericAnn
        declStmt before = internalAnn (CDeclarationAnn before)
    createTasks <- concat <$> mapM genRTEMSCreateTask tasks
    return $ CExtDecl $ CFDefExt $
        -- static void __rtems_app__create_tasks() {
        CFunDef [CStorageSpec CStatic, CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "create_tasks") [CFunDeclr
                [] [] cAnn] [] cAnn)
            (CCompound (
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl (CDecl [CTypeSpec $ CTypeDef "rtems_status_code"]
                    [(Just $ CDeclarator (Just "status") [] [] cAnn, Just $ CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn, Nothing)] (declStmt True))
             : createTasks) (internalAnn (CCompoundAnn False True))) (declStmt True)
    where

        genRTEMSCreateTask :: RTEMSGlobal -> CSourceGenerator [CCompoundBlockItem]
        genRTEMSCreateTask (RTEMSTask identifier classId _ priority stackSize _) = do
            let cAnn = internalAnn CGenericAnn
                stmtAnn = internalAnn (CStatementAnn True False)
                cPriority = genInteger priority
                cStackSize = genInteger stackSize
            return [
                CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                    (CCall (CVar (namefy "rtems" <::> "create_task") cAnn)
                        [CConst (CIntConst cPriority) cAnn,
                        CConst (CIntConst cStackSize) cAnn,
                        CVar (namefy $ "rtems_task" <::> classId) cAnn,
                        CCast
                            (CDecl [CTypeSpec $ CTypeDef "rtems_task_argument"]
                                [] (internalAnn (CDeclarationAnn False)))
                                (CUnary CAdrOp (CVar identifier cAnn) cAnn) cAnn,
                        CUnary CAdrOp
                            (CMember (CMember (CVar identifier cAnn) "__task" False cAnn) "task_id" False cAnn) cAnn
                        ] cAnn) cAnn)
                    stmtAnn,
                CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (internalAnn (CStatementAnn False False))
                    ] (internalAnn (CCompoundAnn False False))) Nothing stmtAnn
                ]
        genRTEMSCreateTask obj = throwError $ InternalError $ "Invalid global object (not a task): " ++ show obj

genInitTask :: [RTEMSEmitter] -> CSourceGenerator CFileItem
genInitTask emitters = do
    let cAnn = internalAnn CGenericAnn
        declStmt before = internalAnn (CDeclarationAnn before)
        cStmtAnn = internalAnn (CStatementAnn True False)
    return $ CExtDecl $ CFDefExt $
        CFunDef [CTypeSpec $ CTypeDef "rtems_task"]
            (CDeclarator (Just $ "Init") [CFunDeclr
                [CDecl
                    [CTypeSpec $ CTypeDef "rtems_task_argument"] [(Just (CDeclarator (Just "_ignored") [] [] cAnn), Nothing, Nothing)] (internalAnn (CDeclarationAnn False))]
                [] cAnn] [] cAnn)
            (CCompound ([
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl (CDecl [CTypeSpec $ CTypeDef "TimeVal"]
                    [(Just $ CDeclarator (Just "current") [] [] cAnn, Nothing, Nothing)] (declStmt True)),
                CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "termina" <::> "clock_get_uptime") cAnn)
                                [CUnary CAdrOp (CVar "current" cAnn) cAnn] cAnn) cStmtAnn,
                CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "termina_app" <::> "init_globals") cAnn) [] cAnn) cStmtAnn,
                CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "rtems_app" <::> "init_globals") cAnn) [] cAnn) cStmtAnn
             ] ++
                (case find (\case { RTEMSSystemInitEmitter {} -> True; _ -> False }) emitters of
                    Just (RTEMSSystemInitEmitter {}) -> [
                            CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "rtems_app" <::> "inital_event") cAnn)
                                [CUnary CAdrOp (CVar "current" cAnn) cAnn] cAnn) cStmtAnn
                        ]
                    _ -> []) ++
                [
                    CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "rtems_app" <::> "enable_protection") cAnn) [] cAnn) cStmtAnn,
                    CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "rtems_app" <::> "install_emitters") cAnn)
                        [CUnary CAdrOp (CVar "current" cAnn) cAnn] cAnn) cStmtAnn,
                    CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "rtems_app" <::> "create_tasks") cAnn) [] cAnn) cStmtAnn,
                    CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_task_delete" cAnn) [CVar "RTEMS_SELF" cAnn] cAnn) cStmtAnn
                ]
             ) (internalAnn (CCompoundAnn False True))) (declStmt True)

genAppConfig ::
    [RTEMSGlobal]
    -> [RTEMSMsgQueue]
    -> [RTEMSEmitter]
    -> [RTEMSResourceLock]
    -> CSourceGenerator [CFileItem]
genAppConfig tasks msgQueues timers mutexes = do
    let cppAnn before = internalAnn (CPPDirectiveAnn before)
    messageBufferMemory <- genMessageBufferMemory msgQueues
    return $ [
            -- #define CONFIGURE_MAXIMUM_TASKS
            CPPDirective $ CPPDefine "CONFIGURE_MAXIMUM_TASKS" (Just [show (length tasks + 1)]) (cppAnn True),
            -- #define CONFIGURE_MAXIMUM_MESSAGE_QUEUES
            CPPDirective $ CPPDefine "CONFIGURE_MAXIMUM_MESSAGE_QUEUES" (Just [show (length msgQueues)]) (cppAnn False),
            -- #define CONFIGURE_MAXIMUM_TIMERS
            CPPDirective $ CPPDefine "CONFIGURE_MAXIMUM_TIMERS" (Just [show (length timers)]) (cppAnn False),
            -- #define CONFIGURE_MAXIMUM_SEMAPHORES
            CPPDirective $ CPPDefine "CONFIGURE_MAXIMUM_SEMAPHORES" (Just [show (length mutexes)]) (cppAnn False)
        ] ++ messageBufferMemory ++
        [
            CPPDirective $ CPPDefine "CONFIGURE_APPLICATION_DOES_NOT_NEED_CONSOLE_DRIVER" Nothing (cppAnn True),
            CPPDirective $ CPPDefine "CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER" Nothing (cppAnn False),
            CPPDirective $ CPPDefine "CONFIGURE_MICROSECONDS_PER_TICK" (Just [show (10000 :: Integer)]) (cppAnn False),
            CPPDirective $ CPPDefine "CONFIGURE_RTEMS_INIT_TASKS_TABLE" Nothing (cppAnn True),
            CPPDirective $ CPPDefine "CONFIGURE_INIT" Nothing (cppAnn True),
            CPPDirective $ CPPInclude True "rtems/confdefs.h" (cppAnn True)
        ]

    where

        genSizeOf :: TypeSpecifier -> CSourceGenerator CExpression
        genSizeOf ts = do
            let cAnn = internalAnn CGenericAnn
            declSpec <- genDeclSpecifiers ts
            return $ CSizeofType (CDecl declSpec [] (internalAnn (CDeclarationAnn False))) cAnn


        genMessagesForQueue :: RTEMSMsgQueue -> CSourceGenerator [String]
        genMessagesForQueue (RTEMSTaskMsgQueue _ (TInteger size _)) = do
            cSizeOf <- genSizeOf UInt32
            let ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> show size <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        genMessagesForQueue (RTEMSChannelMsgQueue _ ts (TInteger size _) _) = do
            cSizeOf <- genSizeOf ts
            let ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> show size <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        genMessagesForQueue (RTEMSSinkPortMsgQueue _ _ ts (TInteger size _)) = do
            cSizeOf <- genSizeOf ts
            let ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> show size <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]

        genMessagesForQueues :: [RTEMSMsgQueue] -> CSourceGenerator [String]
        genMessagesForQueues [msgq] = genMessagesForQueue msgq
        genMessagesForQueues (msgq : xs) = do
            msgsForQueue <- genMessagesForQueue msgq
            msgsForQueues <- genMessagesForQueues xs
            return $ msgsForQueue ++ ["+ "] ++ msgsForQueues
        genMessagesForQueues [] = throwError $ InternalError "Invalid message queue list: empty list"

        genMessageBufferMemory :: [RTEMSMsgQueue] -> CSourceGenerator [CFileItem]
        genMessageBufferMemory [] = return []
        genMessageBufferMemory msgq = do
            messagesForQueue <- genMessagesForQueues msgq
            return [
                    CPPDirective $ CPPDefine "CONFIGURE_MESSAGE_BUFFER_MEMORY"
                        (Just $
                            "( " : messagesForQueue ++ [")"]
                        ) (internalAnn (CPPDirectiveAnn True))
                ]


genMainFile :: QualifiedName ->  [(QualifiedName, SAST.AnnotatedProgram SemanticAnn)] -> CSourceGenerator CFile
genMainFile mName prjprogs = do
    let cAnn = internalAnn CGenericAnn
        includeRTEMS = CPPDirective $ CPPInclude True "rtems.h" (internalAnn (CPPDirectiveAnn True))
        includeTermina = CPPDirective $ CPPInclude True "termina.h" (internalAnn (CPPDirectiveAnn True))
        externInitGlobals = CExtDecl $ CDeclExt $ CDecl
            [CStorageSpec CExtern, CTypeSpec CVoidType]
            [(Just $ CDeclarator (Just $ namefy $ "termina_app" <::> "init_globals") [CFunDeclr [] [] cAnn] [] cAnn, Nothing, Nothing)]
            (internalAnn (CDeclarationAnn True))
    cVariantsForTaskPorts <- concat <$> mapM genVariantsForTaskPorts (M.elems taskClss)
    cPoolMemoryAreas <- genPoolMemoryAreas pools
    cAtomicDeclarations <- genAtomicDeclarations atomics
    cAtomicArrayDeclarations <- genAtomicArrayDeclarations atomicArrays
    cInterruptEmitterDeclarations <- genInterruptEmitterDeclarations interruptEmittersToTasks
    cTaskClassesCode <- mapM genTaskClassCode (M.elems taskClss)
    cEmitters <- mapM genEmitter emitters
    enableProtection <- genEnableProtection resLockingMap
    initGlobals <- genInitGlobals (M.elems resources) pools tasksMessageQueues channelMessageQueues interruptEmittersToTasks timersToTasks tasks timers
    installEmitters <- genInstallEmitters emitters
    createTasks <- genCreateTasks tasks
    initTask <- genInitTask emitters
    appConfig <- genAppConfig tasks msgQueues timers mutexes
    return $ CSourceFile mName $ [
            -- #include <rtems.h>
            includeRTEMS,
            -- #include <termina.h>
            includeTermina
        ] ++ includes
        ++ [
            externInitGlobals
        ] ++ cVariantsForTaskPorts ++ cAtomicDeclarations ++ cAtomicArrayDeclarations 
        ++ cPoolMemoryAreas ++ cInterruptEmitterDeclarations
        ++ cTaskClassesCode ++ cEmitters ++ [enableProtection, initGlobals, installEmitters, createTasks, initTask]
        ++ appConfig

    where
        -- | Original program list filtered to only include the global declaration
        globals = map (\(mn, elems) -> (mn, [g | (SAST.GlobalDeclaration g) <- elems])) prjprogs
        -- | Map between the class identifiers and the class definitions
        classMap = foldr
                (\(_, objs) accMap ->
                    foldr (\obj currMap ->
                        case obj of
                            SAST.TypeDefinition cls@(Class _ classId _ _ _) _ -> M.insert classId cls currMap
                            _ -> currMap
                        ) accMap objs
                ) M.empty prjprogs
        -- | List of modules that actually contain the global declarations and are the only ones that must
        -- be included
        glbs = filter (\(_, objs) -> not (null objs)) globals
        -- | List of modules that must be included
        incs = map fst glbs
        -- | List of include directives
        includes = map (\nm -> CPPDirective $ CPPInclude False (nm <.> "h") (internalAnn (CPPDirectiveAnn True))) incs
        -- List of RTEMS global declarations (tasks, handlers, resources and channels)
        rtemsGlbs = concatMap (\(_, objs) ->
            map (`buildRTEMSGlobal` classMap)
                (filter (\case { Task {} -> True; Resource {} -> True; Handler {} -> True; _ -> False}) objs)
            ) glbs

        -- List of used task classes
        taskClss = foldr (\glb acc -> case glb of
                RTEMSTask _ _ cls@(Class _ classId _ _ _) _ _ _ -> M.insert classId cls acc
                _ -> acc
            ) M.empty rtemsGlbs

        tasks = [t | t@(RTEMSTask {}) <- rtemsGlbs]
        pools = [p | p@(RTEMSPool {}) <- rtemsGlbs]
        resources = M.fromList [(ident, r) | r@(RTEMSResource ident _ _) <- rtemsGlbs]
        atomics = [a | a@(RTEMSAtomic {}) <- rtemsGlbs]
        atomicArrays = [a | a@(RTEMSAtomicArray {}) <- rtemsGlbs]

        targetChannelConnections = foldr
                (\glb accMap ->
                    case glb of
                        RTEMSTask _ _ _ _ _ ports  ->
                            foldr (\port currMap ->
                                case port of
                                    RTEMSInputPort _ channelIdentifier _ _ -> M.insert channelIdentifier glb currMap
                                    _ -> currMap
                            ) accMap ports
                        _ -> accMap
                ) M.empty rtemsGlbs

        tasksMessageQueues = foldr (\glb acc ->
                case glb of
                    RTEMSTask identifier _ _ _ _ ports -> RTEMSTaskMsgQueue identifier (TInteger 1 DecRepr) :
                        foldr (\port acc' ->
                            case port of
                                RTEMSEventPort portId _ ts _ -> RTEMSSinkPortMsgQueue identifier portId ts (TInteger 1 DecRepr) : acc'
                                _ -> acc'
                        ) acc ports
                    _ -> acc
            ) [] tasks

        channelMessageQueues = concatMap (\(_, objs) ->
            map (\case {
                    (Channel identifier (MsgQueue ts (K size)) _ _ _) ->
                        case M.lookup identifier targetChannelConnections of
                            Just task@(RTEMSTask {}) ->
                                RTEMSChannelMsgQueue identifier ts size task
                            _ -> error $ "channel not connected: " ++ show identifier
                        ;
                    _ -> error "Invalid global object (not a channel)"})
                (filter (\case { Channel {} -> True; _ -> False}) objs)
            ) glbs

        msgQueues = tasksMessageQueues ++ channelMessageQueues

        -- Map between the resources and the task and handlers that access them
        dependenciesMap = foldr
                (\glb accMap ->
                    case glb of
                    -- TODO: We are not considering the case of a resource being accessed by another resources
                    -- We need to change the way we are building the dependencies map to consider this case
                    RTEMSResource {} -> accMap
                    RTEMSTask _ _ _ _ _ ports ->
                        foldr (\port currMap ->
                                case port of
                                    RTEMSAccessPort _ identifier -> M.alter (addDependency glb) identifier currMap
                                    _ -> currMap
                            ) accMap ports
                    RTEMSHandler _ _ _ _ ports ->
                        foldr (\port currMap ->
                                case port of
                                    RTEMSAccessPort _ identifier -> M.alter (addDependency glb) identifier currMap
                                    _ -> currMap
                            ) accMap ports
                    _ -> accMap
                ) M.empty rtemsGlbs

        emitterConnectionsMap = foldr
                (\glb accMap ->
                    case glb of
                        RTEMSTask _ _ _ _ _ ports  ->
                            foldr (\port currMap ->
                                case port of
                                    RTEMSEventPort _ identifier _ _ -> M.insert identifier glb currMap
                                    _ -> currMap
                            ) accMap ports
                        RTEMSHandler _ _ _ eventPort _ ->
                            case eventPort of
                                RTEMSEventPort _ identifier _ _ -> M.insert identifier glb accMap
                                _ -> error $ "invalid event port for handler: " ++ show glb
                        _ -> accMap
                ) M.empty rtemsGlbs

        emitters = catMaybes $ concatMap (\(_, objs) ->
                map (`buildRTEMSEmitter` emitterConnectionsMap) objs) glbs ++
                map (`buildRTEMSEmitter` emitterConnectionsMap) [
                        Emitter "irq_1" (DefinedType "Interrupt") Nothing [] (Located (GTy (GGlob (SEmitter (DefinedType "Interrupt")))) Internal),
                        Emitter "irq_2" (DefinedType "Interrupt") Nothing [] (Located (GTy (GGlob (SEmitter (DefinedType "Interrupt")))) Internal),
                        Emitter "irq_3" (DefinedType "Interrupt") Nothing [] (Located (GTy (GGlob (SEmitter (DefinedType "Interrupt")))) Internal),
                        Emitter "irq_4" (DefinedType "Interrupt") Nothing [] (Located (GTy (GGlob (SEmitter (DefinedType "Interrupt")))) Internal),
                        Emitter "system_init" (DefinedType "SystemInit") Nothing [] (Located (GTy (GGlob (SEmitter (DefinedType "SystemInit")))) Internal)
                    ]

        timers = [t | t <- emitters, case t of { RTEMSPeriodicTimerEmitter {} -> True; _ -> False }]
        interruptEmittersToTasks = [e | e <- emitters, case e of { RTEMSInterruptEmitter _ (RTEMSTask{}) -> True; _ -> False }]
        timersToTasks = [e | e <- emitters, case e of { RTEMSPeriodicTimerEmitter _ (RTEMSTask{}) -> True; _ -> False }]

        -- | Map between the resources and the locking mechanism that must be used
        resLockingMap = getResLocking . S.elems <$> M.filterWithKey (\k _ -> M.member k resources) dependenciesMap
        -- | Obtains the locking mechanism that must be used for a resource
        getResLocking :: [RTEMSGlobal] -> RTEMSResourceLock
        getResLocking [] = RTEMSResourceLockNone
        getResLocking [_] = RTEMSResourceLockNone
        getResLocking ((RTEMSHandler {}) : _) = RTEMSResourceLockIrq
        getResLocking ((RTEMSTask _ _ _ priority _ _) : gs) = getResLocking' priority gs
            where
                getResLocking' :: TInteger -> [RTEMSGlobal] -> RTEMSResourceLock
                -- | If we have reach the end of the list, it means that there are at least two different tasks that
                -- access the resource. We are going to force the use of the priority ceiling algorithm. In the
                -- (hopefully near) future, we will support algorithm selection via the configuration file.
                getResLocking' ceilPrio [] = RTEMSResourceLockMutex ceilPrio
                getResLocking' _ ((RTEMSHandler {}) : _) = RTEMSResourceLockIrq
                getResLocking' ceilPrio ((RTEMSTask _ _ _ prio _ _) : gs') = getResLocking' (min prio ceilPrio) gs'
                getResLocking' _ _ = error "Internal error when obtaining the resource dependencies."
        getResLocking _ = error "Internal error when obtaining the resource dependencies."

        mutexes = [m | m <- M.elems resLockingMap, (\case{ RTEMSResourceLockMutex {} -> True; _ -> False }) m]

runGenMainFile :: QualifiedName -> [(QualifiedName, SAST.AnnotatedProgram SemanticAnn)] -> Either CGeneratorError CFile
runGenMainFile mainFilePath prjprogs = runReaderT (genMainFile mainFilePath prjprogs) M.empty
--}