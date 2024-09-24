{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Platform.RTEMS5NoelSpike where

import Generator.LanguageC.AST
import ControlFlow.AST
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List (find)
import Generator.CodeGen.Utils
import Generator.CodeGen.Application.OS.RTEMS.Types
import Generator.CodeGen.Application.Types
import Generator.CodeGen.Common
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.Reader (runReader, ReaderT (runReaderT))
import Data.Text (unpack)
import Generator.LanguageC.Printer
import Modules.Modules (QualifiedName)
import Utils.Annotations
import Semantic.Types
import System.FilePath

data RTEMSPort =
    RTEMSEventPort
        Identifier -- ^ port identifier
        Identifier -- ^ event emitter identifier
        TerminaType -- ^ data type specifier
        Identifier -- ^ action to be executed
    | RTEMSAccessPort
        Identifier -- ^ port identifier
        Identifier -- ^ resource identifier
    | RTEMSInputPort
        Identifier -- ^ port identifier
        Identifier -- ^ channel identifier
        TerminaType -- ^ data type specifier
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
      TerminaType -- ^ type of the elements of the pool
      Size -- ^ pool size
    | RTEMSAtomic
      Identifier -- ^ atomic identifier
      TerminaType -- ^ type of the atomic
    | RTEMSAtomicArray
      Identifier -- ^ atomic array identifier
      TerminaType -- ^ type of the elements of the atomic array
      Size -- ^ atomic array size
    deriving Show

data RTEMSEmitter =
    RTEMSInterruptEmitter
      Identifier -- ^ interrupt identifier
      Identifier -- ^ identifier of the interrupt emitter class
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
      Identifier -- ^ task identifier
      Identifier -- ^ task class identifier
      TInteger -- ^ message queue size
    | RTEMSChannelMsgQueue
      Identifier -- ^ name of the channel
      TerminaType -- ^ type of the elements of the message queue
      TInteger -- ^ message queue size
      RTEMSGlobal -- ^ task that will receive the messages
    | RTEMSSinkPortMsgQueue
      Identifier -- ^ identifier of the receiving task
      Identifier -- ^ identifier of the class of the receiving task
      Identifier -- ^ identifier of the port that will receive the messages
      TerminaType -- ^ type of the elements of the message queue
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

        buildEventPort :: [ClassMember' blk a] -> RTEMSPort
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
        Just glb -> Just (RTEMSInterruptEmitter identifier "Interrupt" glb)
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
            this_variant <- genVariantForPort classId port
            rest <- genDefineVariantsForPorts' xs 1
            return $ pre_cr (_define this_variant (Just [show (0 :: Integer)])) : rest

        genDefineVariantsForPorts' :: [Identifier] -> Integer -> CSourceGenerator [CFileItem]
        genDefineVariantsForPorts' [] _ = return []
        genDefineVariantsForPorts' (port : xs) value = do
            rest <- genDefineVariantsForPorts' xs (value + 1)
            this_variant <- genVariantForPort classId port
            return $ _define this_variant (Just [show value]) : rest

genVariantsForTaskPorts def = throwError $ InternalError $ "Definition not a class: " ++ show def

genPoolMemoryArea :: Bool -> RTEMSGlobal -> CSourceGenerator CFileItem
genPoolMemoryArea before (RTEMSPool identifier ts size) = do
    cSize <- genArraySize size
    cType <- genType noqual ts
    let poolSize = __termina_pool__size @@ [_sizeOfType cType, cSize]
    if before then 
        return $ pre_cr $ static_global (poolMemoryArea identifier) (CTArray uint8_t poolSize)
    else
        return $ static_global (poolMemoryArea identifier) (CTArray uint8_t poolSize)
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
    return $ CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec cType) (Just identifier) Nothing)) declStmt
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
    return $ CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec (CTArray cType cSize)) (Just identifier) Nothing)) declStmt
genAtomicArrayDeclaration _ obj = error $ "Invalid global object (not an atomic array): " ++ show obj

genAtomicArrayDeclarations :: [RTEMSGlobal] -> CSourceGenerator [CFileItem]
genAtomicArrayDeclarations [] = return []
genAtomicArrayDeclarations (obj : objs) = do
    decl <- genAtomicArrayDeclaration True obj
    rest <- mapM (genAtomicArrayDeclaration False) objs
    return $ decl : rest

genInterruptEmitterDeclaration :: Bool -> RTEMSEmitter -> CSourceGenerator CFileItem
genInterruptEmitterDeclaration before (RTEMSInterruptEmitter identifier _ (RTEMSTask {})) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cType <- genType noqual (DefinedType (namefy ("rtems" <::> "interrupt_emitter_t")))
    return $ CExtDecl (CEDVariable (Just CStatic) (CDecl (CTypeSpec cType) (Just identifier) Nothing)) declStmt
genInterruptEmitterDeclaration _ obj = error $ "Invalid global object (not an interrupt emitter): " ++ show obj

genInterruptEmitterDeclarations :: [RTEMSEmitter] -> CSourceGenerator [CFileItem]
genInterruptEmitterDeclarations [] = return []
genInterruptEmitterDeclarations (obj : objs) = do
    decl <- genInterruptEmitterDeclaration True obj
    rest <- mapM (genInterruptEmitterDeclaration False) objs
    return $ decl : rest

genTaskClassCode :: TypeDef SemanticAnn -> CSourceGenerator CFileItem
genTaskClassCode (Class TaskClass classId members _ _) = do
    cBody <- genBody
    return $ pre_cr $ static_function (namefy "rtems_task" <::> classId) [
            "arg" @: rtems_task_argument
        ] @-> rtems_task $ trail_cr . block $
            cBody ++ [pre_cr (_return Nothing)]

    where

        actions :: [(Identifier, TerminaType, Identifier)]
        actions = foldl (\acc member ->
            case member of
                ClassField (FieldDefinition identifier (SinkPort dts action)) _ -> (identifier, dts, action) : acc
                ClassField (FieldDefinition identifier (InPort dts action)) _ -> (identifier, dts, action) : acc
                _ -> acc
            ) [] members

        -- TOOD: The current implementation does not work with vectors
        getMsgDataVariable :: Bool -> Identifier -> TerminaType -> CSourceGenerator CCompoundBlockItem
        getMsgDataVariable before action dts = do
            cDataType <- genType noqual dts
            if before then
                return $ pre_cr $ var (action <::> "msg_data") cDataType
            else
                return $ no_cr $ var (action <::> "msg_data") cDataType

        getMsgDataVariables :: [(Identifier, TerminaType, Identifier)] -> CSourceGenerator [CCompoundBlockItem]
        getMsgDataVariables [] = return []
        getMsgDataVariables ((_identifier, dts, action) : xs) = do
            decl <- getMsgDataVariable True action dts
            rest <- mapM (uncurry (getMsgDataVariable False) . (\(_, dts', action') -> (action', dts'))) xs
            return $ decl : rest

        genCase :: (Identifier, TerminaType, Identifier) -> CSourceGenerator [CCompoundBlockItem]
        genCase (port, dts, action) = do
            this_variant <- genVariantForPort classId port
            classFunctionName <- genClassFunctionName classId action
            classStructType <- genType noqual (DefinedType classId)
            cDataType <- genType noqual dts
            let classFunctionType = CTFunction _Result
                    [_const . ptr $ classStructType, cDataType]
            return
                [
                    -- case variant:
                    pre_cr $ _case (this_variant @: enumFieldType) $
                    -- status = rtems_message_queue_receive(self->port, &action_msg_data, 
                    --                                      &size, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
                    indent . pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_receive @@
                            [
                                ("self" @: ptr classStructType) @. port @: rtems_id,
                                cast void_ptr (addrOf ((action <::> "msg_data") @: cDataType)),
                                addrOf ("size" @: size_t),
                                "RTEMS_NO_WAIT" @: rtems_option,
                                "RTEMS_NO_TIMEOUT" @: rtems_interval
                            ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    indent . pre_cr $ _if (
                            "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ],
                    -- result = classFunctionName(self, action_msg_data);
                    indent . pre_cr $ "result" @: CTTypeDef "Result" noqual @=
                        (classFunctionName @: classFunctionType) @@ ["self" @: classStructType, (action <::> "msg_data") @: cDataType],
                    -- if (result.__variant != Result__Ok)
                    indent . pre_cr $ _if (
                            (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ],
                    indent . pre_cr $ _break
                ]

        genLoop :: CSourceGenerator CStatement
        genLoop = do
            classStructType <- genType noqual (DefinedType classId)
            cases <- concat <$> mapM genCase actions
            return $ trail_cr . block $ [
                    -- | status = rtems_message_queue_receive(
                    -- |                self->__task.msgq_id, &next_msg, &size, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
                    pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_receive @@
                            [
                                (("self" @: ptr classStructType) @. taskClassIDField @: __termina_task_t) @. "msgq_id" @: rtems_id,
                                addrOf ("next_msg" @: uint32_t),
                                addrOf ("size" @: size_t),
                                "RTEMS_WAIT" @: rtems_option,
                                "RTEMS_NO_TIMEOUT" @: rtems_interval
                            ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if
                            ("RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                        $ block [
                            -- break;
                            no_cr _break
                        ],
                    pre_cr $ _switch ("next_msg" @: uint32_t) $
                        trail_cr . block $ (cases ++
                            [
                                -- default:
                                pre_cr $ _default $
                                    -- rtems_shutdown_executive(1);
                                    indent . pre_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t],
                                    -- break;
                                    indent . pre_cr $ _break
                            ])
                ]

        genBody :: CSourceGenerator [CCompoundBlockItem]
        genBody = do
            msgDataVars <- getMsgDataVariables actions
            loop <- genLoop
            return $ [
                    -- ClassIdentifier * self = (ClassIdentifier *)&arg;
                    pre_cr $ var "self" (ptr classId) @:= cast (ptr classId) ("arg" @: rtems_task_argument),
                    -- rtems_status_code status = RTEMS_SUCCESSFUL;
                    no_cr $ var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code,
                    -- uint32_t next_msg = 0;
                    no_cr $ var "next_msg" uint32_t @:= dec 0 @: uint32_t,
                    -- size_t size = 0;
                    no_cr $ var "size" size_t @:= dec 0 @: size_t,
                    -- Result result;
                    pre_cr $ var "result" (typeDef "Result"),
                    -- result.__variant = Result__Ok;
                    no_cr $ ("result" @: typeDef "Result") @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType
                ] ++ msgDataVars ++
                [
                    pre_cr $ _for Nothing Nothing Nothing loop,
                    pre_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                ]
genTaskClassCode obj = throwError $ InternalError $ "Invalid global object (not a task): " ++ show obj

emitterToArrayMap :: M.Map Identifier Integer
emitterToArrayMap = M.fromList [("irq_0", 0), ("irq_1", 1), ("irq_2", 2), ("irq_3", 3), ("irq_4", 4)]

genArmTimer :: CObject -> Identifier -> CSourceGenerator [CCompoundBlockItem]
genArmTimer cObj identifier = do
    return [
            -- __termina__add_timeval(&timer.__timer.current, timer.period);
            pre_cr $ __termina__add_timeval @@
                [
                    addrOf ((cObj @. "__timer" @: __termina_timer_t) @. "current" @: _TimeVal),
                    addrOf (cObj @. "period" @: _TimeVal)
                ],
            -- status = __rtems__timer_delay_at(timer.__timer.timer_id, 
            --                                  &timer.__timer.current,
            --                                  __rtems_periodic_timer__identifier);
            pre_cr $ "status" @: rtems_status_code @=  __rtems__timer_delay_at @@
                [
                    (cObj @. "__timer" @: __termina_timer_t) @. "timer_id" @: rtems_id,
                    addrOf ((cObj @. "__timer" @: __termina_timer_t) @. "current" @: _TimeVal),
                    (namefy "rtems_periodic_timer" <::> identifier) @: typeDef "rtems_timer_service_routine_entry"
                ]
        ]

genEmitter :: RTEMSEmitter -> CSourceGenerator CFileItem
genEmitter (RTEMSInterruptEmitter interrupt _ (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) = do
    let irqArray = emitterToArrayMap M.! interrupt
        classIdType = typeDef classId
    return $ pre_cr $ function (namefy "rtems_isr" <::> interrupt) ["_ignored" @: void] @-> void $
            trail_cr . block $ [
                -- classId * self = &identifier;
                pre_cr $ var "self" (ptr classIdType) @:= addrOf (identifier @: classIdType),
                -- Result result; 
                pre_cr $ var "result" _Result,
                -- result.__variant = Result__Ok;
                no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType,
                -- result = classFunctionName(self, interrupt);
                pre_cr $ "result" @: _Result @=
                    irq_handler classId action @@
                        [
                            "self" @: ptr classIdType,
                            dec irqArray @: uint32_t
                        ],
                -- if (result.__variant != Result__Ok)
                pre_cr $ _if (
                        (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ],
                pre_cr $ _return Nothing
            ]
genEmitter (RTEMSInterruptEmitter interrupt _ (RTEMSTask {})) = do
    let irqArray = emitterToArrayMap M.! interrupt
    return $ pre_cr $ function (namefy "rtems_isr" <::> interrupt) ["_ignored" @: void] @-> void $
            block [
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                pre_cr $ var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code,
                -- uint32_t vector = interrupt;
                pre_cr $ var "vector" uint32_t @:= dec irqArray @: uint32_t,
                -- status = rtems_message_queue_send(interrupt.sink_msgq_id, &interrupt.task_port, sizeof(uint32_t));
                pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_send @@
                    [
                        (interrupt @: __rtems_interrupt_emitter_t) @. "sink_msgq_id" @: rtems_id,
                        addrOf ((interrupt @: __rtems_interrupt_emitter_t) @. "vector" @: uint32_t),
                        _sizeOfType uint32_t
                    ],
                -- if (RTEMS_SUCCESSFUL == status)
                pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @== "status" @: rtems_status_code)
                    $ block [
                        -- status = rtems_message_queue_send(interrupt.task_msgq_id, &vector, sizeof(uint32_t));
                        pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_send @@
                            [
                                (interrupt @: __rtems_interrupt_emitter_t) @. "task_msgq_id" @: rtems_id,
                                (interrupt @: __rtems_interrupt_emitter_t) @. "task_port" @: uint32_t,
                                _sizeOfType uint32_t
                            ],
                        -- if (RTEMS_SUCCESSFUL != status)
                        pre_cr $ _if (
                                "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                            $ block [
                                -- rtems_shutdown_executive(1);
                                no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                            ]
                    ],
                -- return;
                pre_cr $ _return Nothing
            ]
genEmitter (RTEMSInterruptEmitter _ _ glb) = throwError $ InternalError $ "Invalid connection for interrupt: " ++ show glb
genEmitter (RTEMSPeriodicTimerEmitter timer (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) = do
    armTimer <- genArmTimer (CVar timer (CTTypeDef "PeriodicTimer" noqual)) timer
    return $ pre_cr $ function (namefy "rtems_periodic_timer" <::> timer)
            [
                "_timer_id" @: rtems_id,
                "_ignored" @: void_ptr
            ] @-> void $
            trail_cr . block $ [
                -- classId * self = &identifier;
                pre_cr $ var "self" (ptr classId) @:= addrOf (identifier @: typeDef classId),
                -- Result result;
                pre_cr $ var "result" _Result,
                -- result.__variant = Result__Ok;
                no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType,
                -- result = classFunctionName(self, timer.current);
                pre_cr $ "result" @: _Result @= irq_handler classId action @@
                    [
                        "self" @: ptr classId,
                        ((timer @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "current" @: _TimeVal
                    ],
                -- if (result.__variant != Result__Ok)
                pre_cr $ _if (
                        (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
            ] ++ armTimer ++ [
                -- if (RTEMS_SUCCESSFUL != status)
                pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ],
                pre_cr $ _return Nothing
            ]
genEmitter (RTEMSPeriodicTimerEmitter timer (RTEMSTask {})) = do
    armTimer <- genArmTimer (CVar timer (CTTypeDef "PeriodicTimer" noqual)) timer
    return $ pre_cr $ function (namefy "rtems_periodic_timer" <::> timer)
            [
                "_timer_id" @: rtems_id,
                "_ignored" @: void_ptr
            ] @-> void $
            trail_cr . block $ [
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                pre_cr $ var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code,
                -- status = rtems_message_queue_send(timer.timer.sink_msg_id, &timer.current, sizeof(TimeVal));
                pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_send @@
                    [
                        ((timer @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "sink_msgq_id" @: rtems_id,
                        addrOf (((timer @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "current" @: _TimeVal),
                        _sizeOfType _TimeVal
                    ],
                -- if (RTEMS_SUCCESSFUL == status)
                pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @== "status" @: rtems_status_code)
                    $ block [
                        -- status = rtems_message_queue_send(timer.__timer.task_msgq_id, &timer.__timer.task_port, sizeof(uint32_t));
                        pre_cr $ "status" @: rtems_status_code @= rtems_message_queue_send @@
                            [
                                ((timer @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "task_msgq_id" @: rtems_id,
                                addrOf (((timer @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "task_port" @: uint32_t),
                                _sizeOfType uint32_t
                            ]
                    ],
                -- if (RTEMS_SUCCESSFUL != status)
                pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
            ] ++ armTimer ++ [
                -- if (RTEMS_SUCCESSFUL != status)
                pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ],
                pre_cr $ _return Nothing
            ]
genEmitter (RTEMSPeriodicTimerEmitter _ glb) = throwError $ InternalError $ "Invalid connection for timer: " ++ show glb
genEmitter (RTEMSSystemInitEmitter _ (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) = do
    let classIdType = typeDef classId
    return $ pre_cr $ function (namefy "rtems_app" <::> "initial_event") [
            "current" @: ptr (typeDef "TimeVal")
        ] @-> void $
            trail_cr . block $ [
                -- classId * self = &identifier;
                pre_cr $ var "self" (ptr classIdType) @:= addrOf (identifier @: classIdType),
                -- Result result;
                pre_cr $ var "result" _Result,
                -- result.__variant = Result__Ok;
                no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType,
                -- result = classFunctionName(self, current);
                pre_cr $ "result" @: _Result @=
                    timer_handler classId action @@
                        [
                            "self" @: ptr classIdType,
                            deref ("current" @: ptr _TimeVal)
                        ],
                -- if (result.__variant != Result__Ok)
                pre_cr $ _if (
                        (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ],
                pre_cr $ _return Nothing
            ]
genEmitter (RTEMSSystemInitEmitter event (RTEMSTask identifier classId _ _ _ ports)) = do
    let classIdType = typeDef classId
    action <- case find (\case { RTEMSEventPort _ emitter _ _ -> event == emitter; _ -> False }) ports of
        Just (RTEMSEventPort _ _ _ actionId) -> return actionId
        _ -> throwError $ InternalError $ "Invalid port connection for interrupt: " ++ show event
    return $ pre_cr $ function (namefy "rtems_app" <::> "inital_event") [
            "current" @: ptr (typeDef "TimeVal")
        ] @-> void $
            block [
                -- classId * self = &identifier;
                pre_cr $ var "self" (ptr classIdType) @:= addrOf (identifier @: classIdType),
                -- Result result;
                pre_cr $ var "result" _Result,
                -- result.__variant = Result__Ok;
                no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType,
                -- result = classFunctionName(self, current);
                pre_cr $ "result" @: _Result @=
                    timer_handler classId action @@
                        [
                            "self" @: ptr classIdType,
                            deref ("current" @: ptr _TimeVal)
                        ],
                -- if (result.__variant != Result__Ok)
                pre_cr $ _if (
                        (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ],
                pre_cr $ _return Nothing
            ]
genEmitter _ = error "Invalid emitter"


-- | Function __rtems_app__enable_protection. This function is called from the Init task.
-- It enables the protection of the shared resources when needed. In case the resource uses a mutex,
-- it also initializes the mutex. The function is called AFTER the initialization of the tasks and handlers.
genEnableProtection :: M.Map Identifier (RTEMSGlobal, RTEMSResourceLock) -> CSourceGenerator CFileItem
genEnableProtection resLockingMap = do
    initResourcesProt <- concat <$> mapM genInitResourceProt (M.toList resLockingMap)
    return $ pre_cr $ static_function (namefy "rtems_app" <::> "enable_protection") [] @-> void $
        trail_cr . block $ [
                -- Result result;
                pre_cr $ var "result" _Result,
                -- result.__variant = Result__Ok;
                no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType
            ] ++ initResourcesProt
    where

        genInitResourceProt :: (Identifier, (RTEMSGlobal, RTEMSResourceLock)) -> CSourceGenerator [CCompoundBlockItem]
        genInitResourceProt (identifier, (RTEMSResource _ resourceClass _,  RTEMSResourceLockNone)) = do
            return [
                pre_cr $ ((identifier @: typeDef resourceClass) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                    @= namefy "RTEMSResourceLock" <::> "None" @: __rtems_runtime_resource_lock_t
                ]
        genInitResourceProt (identifier, (RTEMSResource _ resourceClass _, RTEMSResourceLockIrq)) = do
            return [
                pre_cr $ ((identifier @: typeDef resourceClass) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                    @= namefy "RTEMSResourceLock" <::> "Irq" @: __rtems_runtime_resource_lock_t
                ]
        genInitResourceProt (identifier, (RTEMSResource _ resourceClass _, RTEMSResourceLockMutex ceilPrio)) = do
            let cCeilPrio = genInteger ceilPrio
            return [
                    -- | identifier.__resource.lock = RTEMSResourceLock__Mutex;
                    pre_cr $ ((identifier @: typeDef resourceClass) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                        @= namefy "RTEMSResourceLock" <::> "Mutex" @: __rtems_runtime_resource_lock_t,
                    no_cr $
                        (((identifier @: typeDef resourceClass) @. resourceClassIDField @: __termina_resource_t) @. "mutex" @: __rtems_runtime_resource_lock_mutex_params_t) @. "policy" @: __rtems_runtime_mutex_policy
                        @= namefy "RTEMSMutexPolicy" <::> "Ceiling" @: __rtems_runtime_mutex_policy,
                    no_cr $
                        (((identifier @: typeDef resourceClass) @. resourceClassIDField @: __termina_resource_t) @. "mutex" @: __rtems_runtime_resource_lock_mutex_params_t) @. "prio_ceiling" @: rtems_task_priority
                        @= cCeilPrio @: rtems_task_priority,
                    pre_cr $ "result" @: _Result @= __termina_resource__init @@
                        [
                            addrOf ((identifier @: typeDef resourceClass) @. resourceClassIDField @: __termina_resource_t)
                        ],
                    -- if (result.__variant != Result__Ok)
                    pre_cr $ _if (
                            (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ]
                ]
        genInitResourceProt (identifier, (RTEMSPool {}, RTEMSResourceLockNone)) = do
            return [
                pre_cr $ ((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                    @= namefy "RTEMSResourceLock" <::> "None" @: __rtems_runtime_resource_lock_t
                ]
        genInitResourceProt (identifier, (RTEMSPool {}, RTEMSResourceLockIrq)) = do
            return [
                pre_cr $ ((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                    @= namefy "RTEMSResourceLock" <::> "Irq" @: __rtems_runtime_resource_lock_t
                ]
        genInitResourceProt (identifier, (RTEMSPool {}, RTEMSResourceLockMutex ceilPrio)) = do
            let cCeilPrio = genInteger ceilPrio
            return [
                    -- | identifier.__resource.lock = RTEMSResourceLock__Mutex;
                    pre_cr $ ((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                        @= namefy "RTEMSResourceLock" <::> "Mutex" @: __rtems_runtime_resource_lock_t,
                    no_cr $
                        (((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "mutex" @: __rtems_runtime_resource_lock_mutex_params_t) @. "policy" @: __rtems_runtime_mutex_policy
                        @= namefy "RTEMSMutexPolicy" <::> "Ceiling" @: __rtems_runtime_mutex_policy,
                    no_cr $
                        (((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "mutex" @: __rtems_runtime_resource_lock_mutex_params_t) @. "prio_ceiling" @: rtems_task_priority
                        @= cCeilPrio @: rtems_task_priority,
                    pre_cr $ "result" @: _Result @= __termina_resource__init @@
                        [
                            addrOf ((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t)
                        ],
                    -- if (result.__variant != Result__Ok)
                    pre_cr $ _if (
                            (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ]
                ]
        genInitResourceProt res = throwError $ InternalError ("Invalid resource: " ++ show res)

-- | Function __rtems_app__init_globals. This function is called from the Init task.
-- The function is called BEFORE the initialization of the tasks and handlers. The function disables
-- the protection of the global resources, since it is not needed when running in the Init task. 
genInitGlobals ::
    -- | Resources 
    [RTEMSGlobal]
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
genInitGlobals resources tasksMessageQueues channelMessageQueues interruptEmittersToTasks timersToTasks tasks timers = do
    initResources <- concat <$> mapM genInitResource resources
    cTaskMessageQueues <- concat <$> mapM genRTEMSCreateMsgQueue tasksMessageQueues
    cChannelMessageQueues <- concat <$> mapM genRTEMSCreateMsgQueue channelMessageQueues
    cInterruptEmittersToTasks <- concat <$> mapM genInitInterruptEmitterToTask interruptEmittersToTasks
    cTimersToTasks <- concat <$> mapM genInitTimerToTask timersToTasks
    cTaskInitialization <- concat <$> mapM genTaskInitialization tasks
    cCreateTimers <- concat <$> mapM genRTEMSCreateTimer timers
    return $ pre_cr $ static_function (namefy "rtems_app" <::> "init_globals") [] @-> void $
            trail_cr . block $ [
                -- Result result;
                pre_cr $ var "result" _Result,
                -- result.__variant = Result__Ok;
                no_cr $ ("result" @: _Result) @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType
            ] ++ initResources 
            ++ (if not (null tasksMessageQueues) || not (null channelMessageQueues) || not (null timers) then
                [
                    pre_cr $ var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code
                ] else []) ++ cTaskMessageQueues ++ cChannelMessageQueues
                ++ cInterruptEmittersToTasks ++ cTimersToTasks ++ cTaskInitialization ++ cCreateTimers

    where

        genInitResource :: RTEMSGlobal -> CSourceGenerator [CCompoundBlockItem]
        genInitResource (RTEMSResource identifier classId _) = do
            -- | resource.__resource.lock = RTEMSResourceLock__None;
            return $ [pre_cr $ ((identifier @: typeDef classId) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                    @= namefy "RTEMSResourceLock" <::> "None" @: __rtems_runtime_resource_lock_t]
        genInitResource (RTEMSPool identifier ts _) = do
            cTs <- genType noqual ts
            return
                [
                    -- identifier.__resource.lock = __RTEMSResourceLock__None;
                    pre_cr $ ((identifier @: __termina_pool_t) @. resourceClassIDField @: __termina_resource_t) @. "lock" @: __rtems_runtime_resource_lock_t
                        @= (namefy "RTEMSResourceLock" <::> "None") @: __rtems_runtime_resource_lock_t,
                    -- result = __termina_pool__init(&identifier, (void *)memory_area_identifier)
                    pre_cr $ "result" @: _Result @= __termina_pool__init @@
                                [
                                    addrOf (identifier @: ptr __termina_pool_t),
                                    cast (ptr void) (poolMemoryArea identifier @: ptr uint8_t),
                                    _sizeOfExpr (poolMemoryArea identifier @: ptr uint8_t),
                                    _sizeOfType cTs
                                ],
                    -- if (result.__variant != Result__Ok)
                    pre_cr $ _if (
                            (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                        ]
                ]
        genInitResource obj = throwError $ InternalError $ "Invalid global object (not a pool): " ++ show obj

        -- | Prints the code to initialize a message queue. The function is called to generate the code for the
        -- message queues corresponding to the channels declared by the user plus the ones that belong to each
        -- of the tasks that is used to notify the inclusion of a given message on a specific queue.
        genRTEMSCreateMsgQueue :: RTEMSMsgQueue -> CSourceGenerator [CCompoundBlockItem]
        genRTEMSCreateMsgQueue (RTEMSChannelMsgQueue identifier ts size (RTEMSTask taskId classId _ _ _ ports)) = do
            let cSize = genInteger size
            cTs <- genType noqual ts
            variantForPort <- genVariantForPort classId port
            let classIdType = typeDef classId
            return
                [
                    pre_cr $ (identifier @: __termina_msg_queue_t) @. "task_msgq_id" @: rtems_id @=
                        ((taskId @: classIdType) @. taskClassIDField @: __termina_task_t) @. "msgq_id" @: rtems_id,
                    no_cr $ (identifier @: __termina_msg_queue_t) @. "task_port" @: uint32_t @=
                        variantForPort @: uint32_t,
                    no_cr $ (identifier @: __termina_msg_queue_t) @. "message_size" @: size_t @=
                            _sizeOfType cTs,
                    -- status = __rtems__create_msg_queue(count, msg_size, &msg_queue_id)
                    pre_cr $ "status" @: rtems_status_code @= __rtems__create_msg_queue @@
                        [
                            cSize @: uint32_t,
                            _sizeOfType cTs,
                            addrOf ((identifier @: __termina_msg_queue_t) @. "msgq_id" @: rtems_id)
                        ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
                ]
            where

                port = case find (\case {
                    RTEMSInputPort _ chid _ _ -> chid == identifier;
                    _ -> False }) ports of
                        Just (RTEMSInputPort prt _ _ _) -> prt
                        _ -> error $ "Invalid port connection for channel: " ++ show identifier

        genRTEMSCreateMsgQueue obj@(RTEMSChannelMsgQueue {}) = throwError $ InternalError $ "Invalid channel objet: " ++ show obj
        genRTEMSCreateMsgQueue (RTEMSTaskMsgQueue taskId classId size) = do
            let cSize = genInteger size
                classIdType = typeDef classId
            return
                [
                    -- status = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    pre_cr $ "status" @: rtems_status_code @= __rtems__create_msg_queue @@
                        [
                            cSize @: uint32_t,
                            _sizeOfType uint32_t,
                            addrOf (((taskId @: classIdType) @. taskClassIDField @: __termina_task_t) @. "msgq_id" @: rtems_id)
                        ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
                ]
        genRTEMSCreateMsgQueue (RTEMSSinkPortMsgQueue taskId classId portId ts size) = do
            let cSize = genInteger size
            cTs <- genType noqual ts
            return
                [
                    -- status = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    pre_cr $ "status" @: rtems_status_code @= __rtems__create_msg_queue @@
                        [
                            cSize @: size_t,
                            _sizeOfType cTs,
                            addrOf ((taskId @: typeDef classId) @. portId @: rtems_id)
                        ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
                ]

        genInitInterruptEmitterToTask :: RTEMSEmitter -> CSourceGenerator [CCompoundBlockItem]
        genInitInterruptEmitterToTask (RTEMSInterruptEmitter identifier emitterClassId (RTEMSTask taskId classId _ _ _ ports)) = do
            variantForPort <- genVariantForPort classId port
            return
                [
                    pre_cr $ (identifier @: typeDef emitterClassId) @. "task_msgq_id" @: rtems_id @=
                            ((taskId @: typeDef classId) @. taskClassIDField @: __termina_task_t) @. "msgq_id" @: rtems_id,
                    no_cr $ (identifier @: typeDef emitterClassId) @. "sink_msgq_id" @: rtems_id @=
                            (taskId @: typeDef classId) @. port @: rtems_id,
                    no_cr $ (identifier @: typeDef emitterClassId) @. "task_port" @: uint32_t @=
                            variantForPort @: uint32_t
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
            variantForPort <- genVariantForPort classId port
            return
                [
                    pre_cr $ ((identifier @: _PeriodicTimer) @. "__timer" @: __termina_timer_t) @. "task_msgq_id" @: rtems_id @=
                        ((taskId @: typeDef classId) @. taskClassIDField @: __termina_task_t) @. "msgq_id" @: rtems_id,
                    no_cr $ ((identifier @: _PeriodicTimer) @. "__timer" @: __termina_timer_t) @. "sink_msgq_id" @: rtems_id @=
                        (taskId @: typeDef classId) @. port @: rtems_id,
                    no_cr $ ((identifier @: _PeriodicTimer) @. "__timer" @: __termina_timer_t) @. "task_port" @: uint32_t @=
                        variantForPort @: uint32_t
                ]

            where

                port = case find (\case {
                    RTEMSEventPort _ chid _ _ -> chid == identifier;
                    _ -> False }) ports of
                        Just (RTEMSEventPort prt _ _ _) -> prt
                        _ -> error $ "Invalid port connection for channel: " ++ show identifier
        genInitTimerToTask obj = throwError $ InternalError $ "Invalid global object (not a timer connected to a task): " ++ show obj

        genTaskInitialization :: RTEMSGlobal -> CSourceGenerator [CCompoundBlockItem]
        genTaskInitialization (RTEMSTask identifier classId _ _ _ ports) = do
            mapM genInputPortInitialization inputPorts

            where

                inputPorts = filter (\case {
                    RTEMSInputPort {} -> True;
                    _ -> False }) ports

                genInputPortInitialization :: RTEMSPort -> CSourceGenerator CCompoundBlockItem
                genInputPortInitialization (RTEMSInputPort portId channelId _ _) = do
                    return $ pre_cr $
                        identifier @: typeDef classId @. portId @: rtems_id @=
                            (channelId @: __termina_msg_queue_t) @. "msgq_id" @: rtems_id
                genInputPortInitialization obj = throwError $ InternalError $ "Invalid port object: " ++ show obj
        genTaskInitialization obj = throwError $ InternalError $ "Invalid global object (not a task): " ++ show obj

        genRTEMSCreateTimer :: RTEMSEmitter -> CSourceGenerator [CCompoundBlockItem]
        genRTEMSCreateTimer (RTEMSPeriodicTimerEmitter identifier _) = do
            return
                [
                    -- status = __rtems__create_timer(&timer_id)
                    pre_cr $ "status" @: rtems_status_code @= __rtems__create_timer @@
                            [
                                addrOf (((identifier @: _PeriodicTimer) @. timerField @: __termina_timer_t) @. "timer_id" @: rtems_id)
                            ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
                ]
        genRTEMSCreateTimer obj = throwError $ InternalError $ "Invalid global object (not a timer): " ++ show obj

-- | Function __rtems_app__install_emitters. This function is called from the Init task.
-- The function installs the ISRs and the periodic timers. The function is called AFTER the initialization
-- of the tasks and handlers.
genInstallEmitters :: [RTEMSEmitter] -> CSourceGenerator CFileItem
genInstallEmitters emitters = do
    installEmitters <- mapM genRTEMSInstallEmitter $ filter (\case { RTEMSSystemInitEmitter {} -> False; _ -> True }) emitters
    return $ pre_cr $ static_function (namefy "rtems_app" <::> "install_emitters")
            [
                "current" @: ptr (typeDef "TimeVal")
            ] @-> void $
            trail_cr . block $ 
                    -- rtems_status_code status = RTEMS_SUCCESSFUL;
                pre_cr (var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code) : installEmitters

    where

        genRTEMSInstallEmitter :: RTEMSEmitter -> CSourceGenerator CCompoundBlockItem
        genRTEMSInstallEmitter (RTEMSInterruptEmitter interrupt _ _) = do
            return $
                pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @== "status" @: rtems_status_code)
                    $ trail_cr . block $ [
                        pre_cr $ "status" @: rtems_status_code @= __rtems__install_isr @@
                            [
                                dec (emitterToArrayMap M.! interrupt) @: uint32_t,
                                namefy ("rtems_isr" <::> interrupt) @: rtems_interrupt_handler
                            ]
                    ]
        genRTEMSInstallEmitter (RTEMSPeriodicTimerEmitter timer _) = do
            let cExpr = CVar timer (CTTypeDef "PeriodicTimer" noqual)
            armTimer <- genArmTimer cExpr timer
            return $
                pre_cr $ _if ("RTEMS_SUCCESSFUL" @: rtems_status_code @== "status" @: rtems_status_code)
                    $ block $ 
                        pre_cr (((timer @: _PeriodicTimer) @. "__timer" @: __termina_timer_t) @. "current" @: _TimeVal @=
                            deref ("current" @: ptr _TimeVal)) : armTimer
        genRTEMSInstallEmitter (RTEMSSystemInitEmitter {}) = throwError $ InternalError "Initial event does not have to be installed"
    
genCreateTasks :: [RTEMSGlobal] -> CSourceGenerator CFileItem
genCreateTasks tasks = do
    createTasks <- concat <$> mapM genRTEMSCreateTask tasks
    return $ pre_cr $ static_function (namefy "rtems_app" <::> "create_tasks") [] @-> void $ 
        trail_cr . block $ 
            -- rtems_status_code status = RTEMS_SUCCESSFUL;
            pre_cr (var "status" rtems_status_code @:= "RTEMS_SUCCESSFUL" @: rtems_status_code) : createTasks
    where

        genRTEMSCreateTask :: RTEMSGlobal -> CSourceGenerator [CCompoundBlockItem]
        genRTEMSCreateTask (RTEMSTask identifier classId _ priority stackSize _) = do
            let cPriority = genInteger priority
                cStackSize = genInteger stackSize
            return [
                    pre_cr $ "status" @: rtems_status_code @= __rtems__create_task @@
                            [
                                cPriority @: rtems_task_priority,
                                cStackSize @: size_t,
                                namefy ("rtems_task" <::> classId) @: rtems_task_entry,
                                cast rtems_task_argument (addrOf (identifier @: typeDef classId)),
                                addrOf (((identifier @: typeDef classId) @. taskClassIDField @: __termina_task_t) @. "task_id" @: rtems_id)
                            ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if (
                        "RTEMS_SUCCESSFUL" @: rtems_status_code @!= "status" @: rtems_status_code)
                    $ block [
                        -- rtems_shutdown_executive(1);
                        no_cr $ rtems_shutdown_executive @@ [dec 1 @: uint32_t]
                    ]
                ]
        genRTEMSCreateTask obj = throwError $ InternalError $ "Invalid global object (not a task): " ++ show obj

genInitTask :: [RTEMSEmitter] -> CSourceGenerator CFileItem
genInitTask emitters = do
    return $ pre_cr $ function "Init" [
            "_ignored" @: rtems_task_argument
        ] @-> rtems_task $
        trail_cr . block $ 
            [
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                pre_cr $ var "current" _TimeVal,
                pre_cr $ __termina__clock_get_uptime @@ [addrOf ("current" @: _TimeVal)],
                pre_cr $ __termina_app__init_globals @@ [],
                pre_cr $ __rtems_app__init_globals @@ []
             ] ++
                (case find (\case { RTEMSSystemInitEmitter {} -> True; _ -> False }) emitters of
                    Just (RTEMSSystemInitEmitter {}) -> [
                            pre_cr $ __rtems_app__initial_event @@ [addrOf ("current" @: _TimeVal)]
                        ]
                    _ -> []) ++
                [
                    pre_cr $ __rtems_app__enable_protection @@ [],
                    pre_cr $ __rtems_app__install_emitters @@ [addrOf ("current" @: _TimeVal)],
                    pre_cr $ __rtems_app__create_tasks @@ [],
                    pre_cr $ rtems_task_delete @@ ["RTEMS_SELF" @: rtems_id]
                ]

genAppConfig ::
    [RTEMSGlobal]
    -> [RTEMSMsgQueue]
    -> [RTEMSEmitter]
    -> [RTEMSResourceLock]
    -> CSourceGenerator [CFileItem]
genAppConfig tasks msgQueues timers mutexes = do
    messageBufferMemory <- genMessageBufferMemory msgQueues
    return $ [
            -- #define CONFIGURE_MAXIMUM_TASKS
            pre_cr $ _define "CONFIGURE_MAXIMUM_TASKS" (Just [show (length tasks + 1)]),
            -- #define CONFIGURE_MAXIMUM_MESSAGE_QUEUES
            _define "CONFIGURE_MAXIMUM_MESSAGE_QUEUES" (Just [show (length msgQueues)]),
            -- #define CONFIGURE_MAXIMUM_TIMERS
            _define "CONFIGURE_MAXIMUM_TIMERS" (Just [show (length timers)]),
            -- #define CONFIGURE_MAXIMUM_SEMAPHORES
            _define "CONFIGURE_MAXIMUM_SEMAPHORES" (Just [show (length mutexes)])
        ] ++ messageBufferMemory ++
        [
            pre_cr $ _define "CONFIGURE_APPLICATION_DOES_NOT_NEED_CONSOLE_DRIVER" Nothing,
            _define "CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER" Nothing,
            _define "CONFIGURE_MICROSECONDS_PER_TICK" (Just [show (10000 :: Integer)]),
            pre_cr $ _define "CONFIGURE_RTEMS_INIT_TASKS_TABLE" Nothing,
            pre_cr $ _define "CONFIGURE_INIT" Nothing,
            pre_cr $ _include True "rtems/confdefs.h"
        ]

    where

        genMessagesForQueue :: RTEMSMsgQueue -> CSourceGenerator [String]
        genMessagesForQueue (RTEMSTaskMsgQueue _ _ (TInteger size _)) = do
            let cSizeOf = _sizeOfType uint32_t
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> show size <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        genMessagesForQueue (RTEMSChannelMsgQueue _ ts (TInteger size _) _) = do
            cTs <- genType noqual ts
            let cSizeOf = _sizeOfType cTs
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> show size <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        genMessagesForQueue (RTEMSSinkPortMsgQueue _ _ _ ts (TInteger size _)) = do
            cTs <- genType noqual ts
            let cSizeOf = _sizeOfType cTs
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
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
                    CPPDirective (CPPDefine "CONFIGURE_MESSAGE_BUFFER_MEMORY"
                        (Just $
                            "( " : messagesForQueue ++ [")"]
                        )) (internalAnn (CPPDirectiveAnn True))
                ]


genMainFile :: QualifiedName ->  [(QualifiedName, AnnotatedProgram SemanticAnn)] -> CSourceGenerator CFile
genMainFile mName prjprogs = do
    let includeRTEMS = CPPDirective (CPPInclude True "rtems.h") (internalAnn (CPPDirectiveAnn True))
        includeTermina = CPPDirective (CPPInclude True "termina.h") (internalAnn (CPPDirectiveAnn True))
        externInitGlobals = CExtDecl (CEDFunction void (namefy $ "termina_app" <::> "init_globals") []) (internalAnn (CDeclarationAnn True))
    cVariantsForTaskPorts <- concat <$> mapM genVariantsForTaskPorts (M.elems taskClss)
    cPoolMemoryAreas <- genPoolMemoryAreas pools
    cAtomicDeclarations <- genAtomicDeclarations atomics
    cAtomicArrayDeclarations <- genAtomicArrayDeclarations atomicArrays
    cInterruptEmitterDeclarations <- genInterruptEmitterDeclarations interruptEmittersToTasks
    cTaskClassesCode <- mapM genTaskClassCode (M.elems taskClss)
    cEmitters <- mapM genEmitter emitters
    enableProtection <- genEnableProtection (M.mapWithKey (\k v -> (resources M.! k, v)) resLockingMap) 
    initGlobals <- genInitGlobals (M.elems resources) tasksMessageQueues channelMessageQueues interruptEmittersToTasks timersToTasks tasks timers
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
        globals = map (\(mn, elems) -> (mn, [g | (GlobalDeclaration g) <- elems])) prjprogs
        -- | Map between the class identifiers and the class definitions
        classMap = foldr
                (\(_, objs) accMap ->
                    foldr (\obj currMap ->
                        case obj of
                            TypeDefinition cls@(Class _ classId _ _ _) _ -> M.insert classId cls currMap
                            _ -> currMap
                        ) accMap objs
                ) M.empty prjprogs
        -- | List of modules that actually contain the global declarations and are the only ones that must
        -- be included
        glbs = filter (\(_, objs) -> not (null objs)) globals
        -- | List of modules that must be included
        incs = map fst glbs
        -- | List of include directives
        includes = map (\nm -> CPPDirective (CPPInclude False (nm <.> "h")) (internalAnn (CPPDirectiveAnn True))) incs
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
        resources = M.fromList [(ident, r) | r <- rtemsGlbs, 
                case r of (RTEMSResource {}) -> True; (RTEMSPool {}) -> True; _ -> False,
                ident <- [case r of (RTEMSResource resourceId _ _) -> resourceId; (RTEMSPool poolId _ _) -> poolId; _ -> error "Invalid resource"]]    
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
                    RTEMSTask identifier classId _ _ _ ports -> RTEMSTaskMsgQueue identifier classId (TInteger 1 DecRepr) :
                        foldr (\port acc' ->
                            case port of
                                RTEMSEventPort portId _ ts _ -> RTEMSSinkPortMsgQueue identifier classId portId ts (TInteger 1 DecRepr) : acc'
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
        interruptEmittersToTasks = [e | e <- emitters, case e of { RTEMSInterruptEmitter _ _ (RTEMSTask{}) -> True; _ -> False }]
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

runGenMainFile :: QualifiedName -> [(QualifiedName, AnnotatedProgram SemanticAnn)] -> Either CGeneratorError CFile
runGenMainFile mainFilePath prjprogs = runReaderT (genMainFile mainFilePath prjprogs) M.empty