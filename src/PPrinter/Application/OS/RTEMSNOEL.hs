{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module PPrinter.Application.OS.RTEMSNOEL where

import Semantic.Monad
import AST.Seman
import PPrinter.Common
import Prettyprinter
import qualified Data.Map as M
import qualified Data.Set as S

import Modules.Modules
import Modules.Printing
import qualified AST.Seman as SAST
import Data.Maybe (fromJust)
import Data.List (find)

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
      (TypeDef SemanticAnns) -- ^ task class definition
      Integer -- ^ task priority
      Integer -- ^ task stack size
      [RTEMSPort] -- ^ task ports
    -- | RTEMS Handler
    | RTEMSHandler
      Identifier -- ^ handler identifier
      Identifier -- ^ handler class identifier
      (TypeDef SemanticAnns)  -- ^ handler class definition
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
      Integer -- ^ pool size
    deriving Show

data RTEMSEmitter =
    RTEMSInterruptEmitter
      Identifier -- ^ interrupt identifier
      RTEMSGlobal -- ^ target of the interrupt (task or handler)
    | RTEMSPeriodicTimerEmitter
      Identifier -- ^ periodic timer identifier
      (Expression SemanticAnns) -- ^ periodic timer period tv_sec
      (Expression SemanticAnns) -- ^ periodic timer period tv_usec
      RTEMSGlobal -- ^ target of the timer (task or handler)
    | RTEMSSystemInitEmitter
      Identifier -- ^ initial event identifier
      RTEMSGlobal -- ^ target of the initial event (task or handler)
    deriving Show

data RTEMSMsgQueue =
    RTEMSTaskMsgQueue
      Identifier -- ^ message queue identifier
      Integer -- ^ message queue size
    | RTEMSChannelMsgQueue
      Identifier -- ^ name of the channel
      TypeSpecifier -- ^ type of the elements of the message queue
      Integer -- ^ message queue size
      RTEMSGlobal -- ^ task that will receive the messages
    deriving Show

data RTEMSResourceLock =
    RTEMSResourceLockNone |
    RTEMSResourceLockIrq |
    RTEMSResourceLockMutex Integer
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

-- | Returns the value of the "priority" modifier, if present in the list of modifiers.
-- If not, it returns 255, which is the default value for the priority (the lowest).
getPriority :: [Modifier] -> Integer
getPriority [] = 255
getPriority ((Modifier "priority" (Just (KC (I _ priority)))) : _) = priority
getPriority (_ : modifiers) = getPriority modifiers

-- | Returns the value of the "stack_size" modifier, if present in the list of modifiers.
-- If not, it returns 4096, which is the default value for the stack size (RTEMS_MINIUMUM_STACK_SIZE)
getStackSize :: [Modifier] -> Integer
getStackSize [] = 4096
getStackSize ((Modifier "stack_size" (Just (KC (I _ stackSize)))) : _) = stackSize
getStackSize (_ : modifiers) = getStackSize modifiers

ppTaskMessageQueue :: Identifier -> Identifier
ppTaskMessageQueue identifier = "__msgq__" ++ identifier

ppVariantForAction ::
    -- | Name of the task class
    Identifier
    -- | Name of the action
    ->Identifier -> DocStyle
ppVariantForAction taskCls action = namefy $ pretty taskCls <::> pretty action

ppDefineVariantsForActions :: Identifier -> Identifier -> Integer -> DocStyle
ppDefineVariantsForActions taskCls action value =
    pretty "#define" <+> ppVariantForAction taskCls action <+> pretty value

ppVariantsForTaskActions :: TypeDef SemanticAnns -> DocStyle
ppVariantsForTaskActions (Class _ classId members _ _) = vsep $ emptyDoc :
    zipWith (ppDefineVariantsForActions classId) actions [0..]
    where
        actions = foldr (\field acc ->
                        case field of
                            ClassField (FieldDefinition _ (SinkPort _ action)) _ -> action : acc
                            ClassField (FieldDefinition _ (InPort _ action)) _ -> action : acc
                            _ -> acc ) [] members
ppVariantsForTaskActions def = error $ "Definition not a class: " ++ show def

-- Finds the assignment that connects a given port
findPortConnection :: Identifier -> [FieldAssignment a] -> Maybe (FieldAssignment a)
findPortConnection _ [] = Nothing
findPortConnection identifier (assignment : assignments) =
    case assignment of
        FieldPortConnection _ port _ _ | port == identifier -> Just assignment
        _ -> findPortConnection identifier assignments

buildRTEMSGlobal :: Global SemanticAnns -> M.Map Identifier (TypeDef SemanticAnns) -> RTEMSGlobal
buildRTEMSGlobal (Task identifier (DefinedType ty) (Just (FieldAssignmentsExpression _ assignments _)) modifiers _) classMap =
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
buildRTEMSGlobal (Handler identifier (DefinedType ty) (Just (FieldAssignmentsExpression _ assignments _)) _ _) classMap =
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
buildRTEMSGlobal (Resource identifier (DefinedType ty) (Just (FieldAssignmentsExpression _ assignments _)) _ _) classMap =
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
buildRTEMSGlobal (Resource identifier (Pool ty (K size)) _ _ _) _ = RTEMSPool identifier ty size
buildRTEMSGlobal obj _ = error $ "Invalid global object: " ++ show obj

buildRTEMSEmitter :: Global SemanticAnns -> M.Map Identifier RTEMSGlobal -> Maybe RTEMSEmitter
buildRTEMSEmitter (Emitter identifier (DefinedType "Interrupt") _ _ _) connectionsMap =
    case M.lookup identifier connectionsMap of
        Just glb -> Just (RTEMSInterruptEmitter identifier glb)
        Nothing -> Nothing -- Not connected
buildRTEMSEmitter (Emitter identifier (DefinedType "PeriodicTimer") (Just (FieldAssignmentsExpression "PeriodicTimer" fvas _)) _ _) connectionsMap =
    case M.lookup identifier connectionsMap of
        Just glb -> Just (RTEMSPeriodicTimerEmitter identifier tvsec tvusec glb)
        Nothing -> Nothing -- Not connected
    where
        period = case find (\case { FieldValueAssignment "period" _ _ -> True; _ -> False }) fvas of
            Just (FieldValueAssignment _ tvs _) -> tvs
            _ -> error $ "Invalid field assignments for periodic timer: " ++ show fvas
            
        tvsec =
            case period of 
                (FieldAssignmentsExpression "TimeVal" pfvas _) -> 
                    case find (\case { FieldValueAssignment "tv_sec" _ _ -> True; _ -> False }) pfvas of
                        Just (FieldValueAssignment _ tvs _) -> tvs
                        _ -> error $ "Invalid field assignments for periodic timer: " ++ show pfvas
                e -> error $ "Invalid assignment: " ++ show e
        tvusec =
            case period of 
                (FieldAssignmentsExpression "TimeVal" pfvas _) -> 
                    case find (\case { FieldValueAssignment "tv_usec" _ _ -> True; _ -> False }) pfvas of
                        Just (FieldValueAssignment _ tvus _) -> tvus
                        _ -> error $ "Invalid field assignments for periodic timer: " ++ show pfvas
                e -> error $ "Invalid assignment: " ++ show e
buildRTEMSEmitter (Emitter identifier (DefinedType "SystemInit") _ _ _) connectionsMap =
    case M.lookup identifier connectionsMap of
        Just glb -> Just (RTEMSSystemInitEmitter identifier glb)
        Nothing -> Nothing -- Not connected
buildRTEMSEmitter emitter@(Emitter {}) _ = error $ "Unsupported emitter" ++ show emitter
buildRTEMSEmitter _ _ = Nothing
    
addDependency :: RTEMSGlobal -> Maybe (S.Set RTEMSGlobal) -> Maybe (S.Set RTEMSGlobal)
addDependency newGlb Nothing = Just (S.singleton newGlb)
addDependency newGlb (Just prevGlbs) = Just (S.insert newGlb prevGlbs)

-- | Prints the code of the RTEMS tasks that will execute a Termina task.
-- The RTEMS task will implement the main loop of the task.
ppTaskClassCode :: TypeDef SemanticAnns -> DocStyle
ppTaskClassCode (Class TaskClass classId members _ _) = staticC <+>
    ppCFunctionPrototype (namefy (pretty "rtems_task") <::> pretty classId) [pretty "rtems_task_argument arg"] (Just (pretty "rtems_task")) <+>
        braces' (
            (indentTab . align $
                vsep [
                    emptyDoc,
                    -- ClassIdentifier self = &task_identifier;
                    pretty classId <+> pretty "*" <+> pretty "self" <+> pretty "=" <+> parens (pretty classId <+> pretty "*") <> pretty "arg" <> semi,
                    -- Result res = Result__Ok;
                    pretty "rtems_status_code status = RTEMS_SUCESSFUL" <> semi,
                    ppTypeSpecifier UInt32 <+> pretty "next_msg" <+> pretty "= 0" <> semi,
                    emptyDoc,
                    -- for (;;)
                    ppCInfiniteLoop (
                        vsep [
                            emptyDoc,
                            -- Call receive on the task's message queue
                            pretty "status" <+> pretty "=" <+>
                                ppCFunctionCall (pretty "rtems_message_queue_receive")
                                    [pretty "self.__msgq_id",
                                     pretty "&next_msg",
                                     pretty "RTEMS_WAIT",
                                     pretty "RTEMS_NO_TIMEOUT"] <> semi,
                            -- 
                            emptyDoc,
                            -- if (status != RTEMS_SUCCESSFUL)
                            ppCIfBlock (pretty "status" <+> pretty "!=" <+> pretty "RTEMS_SUCCESSFUL")
                                (vsep [pretty "break" <> semi, emptyDoc]),
                            emptyDoc,
                            -- Check the message type and execute the corresponding action
                            ppCSwitchBlock (pretty "next_msg")
                                [
                                    emptyDoc,
                                    vsep (ppExecuteAction classId <$> ports),
                                    ppCDefaultSwitchCase (
                                        vsep [
                                            emptyDoc,
                                            pretty "rtems_shutdown_executive(1)" <> semi,
                                            pretty "break" <> semi, emptyDoc
                                        ]
                                    ),
                                    emptyDoc
                                ],
                            emptyDoc
                        ]
                    ),
                    emptyDoc,
                    pretty "rtems_shutdown_executive(1)" <> semi
                ]
            ) <> line
        ) <> line
    where

        ports = filter (
            \case {
                ClassField (FieldDefinition _ (SinkPort {})) _ -> True;
                ClassField (FieldDefinition _ (InPort {})) _ -> True;
                _ -> False }
            ) members

        ppExecuteAction :: Identifier -> ClassMember SemanticAnns -> DocStyle
        ppExecuteAction taskCls (ClassField (FieldDefinition portIdentifier (SinkPort dts action)) _) =
            ppCSwitchCase (ppVariantForAction taskCls action) $
            vsep [
                emptyDoc,
                ppTypeSpecifier dts <+> pretty action <::> pretty "msg_data" <> semi,
                emptyDoc,
                -- | Extract message from the queue
                pretty "status" <+> pretty "=" <+>
                    ppCFunctionCall (pretty "rtems_message_queue_receive")
                        [pretty "self." <> pretty portIdentifier,
                         ppCReferenceExpression (pretty action <::> pretty "msg_data"),
                         pretty "RTEMS_NO_WAIT",
                         pretty "RTEMS_NO_TIMEOUT"] <> semi,
                emptyDoc,
                pretty "result" <+> pretty "=" <+>
                ppCFunctionCall (classFunctionName (pretty classId) (pretty action)) [pretty "self"] <> semi,
                emptyDoc,
                ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
                    (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc]),
                emptyDoc,
                pretty "break" <> semi,
                emptyDoc
            ]
        ppExecuteAction taskCls (ClassField (FieldDefinition portIdentifier (InPort dts action)) _) =
            ppCSwitchCase (ppVariantForAction taskCls action) $
            vsep [
                emptyDoc,
                ppTypeSpecifier dts <+> pretty action <::> pretty "msg_data" <> semi,
                emptyDoc,
                -- | Extract message from the queue
                pretty "status" <+> pretty "=" <+>
                    ppCFunctionCall (pretty "rtems_message_queue_receive")
                        [pretty "self." <> pretty portIdentifier,
                         ppCReferenceExpression (pretty action <::> pretty "msg_data"),
                         pretty "RTEMS_NO_WAIT",
                         pretty "RTEMS_NO_TIMEOUT"] <> semi,
                emptyDoc,
                pretty "result" <+> pretty "=" <+>
                ppCFunctionCall (classFunctionName (pretty classId) (pretty action)) [pretty "self"] <> semi,
                emptyDoc,
                ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
                    (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc]),
                emptyDoc,
                pretty "break" <> semi,
                emptyDoc
            ]
        ppExecuteAction _ member = error $ "Invalid class member: " ++ show member
ppTaskClassCode obj = error $ "Invalid global object (not a task): " ++ show obj

ppInitPool :: RTEMSGlobal -> DocStyle
ppInitPool (RTEMSPool identifier ts _) = vsep
    [
        emptyDoc,
        pretty identifier <> pretty ".__resource_id.lock = __RTEMSResourceLock__None" <> semi,
        emptyDoc,
        pretty "result" <+> pretty "=" <+>
            ppCFunctionCall (namefy $ pretty "termina" <::> pretty "pool_init")
                [ppCReferenceExpression (pretty identifier),
                 parens (pretty "void *") <> poolMemoryArea (pretty identifier),
                 sizeofC (poolMemoryArea (pretty identifier)),
                 ppSizeOf ts] <> semi,
        emptyDoc,
        ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
            (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
    ]
ppInitPool obj = error $ "Invalid global object (not a pool): " ++ show obj

ppMsgQueueDeclaration :: RTEMSMsgQueue -> DocStyle
ppMsgQueueDeclaration (RTEMSChannelMsgQueue identifier _ _ _) =
    msgQueue <+> pretty identifier <> semi
ppMsgQueueDeclaration (RTEMSTaskMsgQueue identifier _) =
    pretty "rtems_id" <+> pretty identifier <> semi

-- | Prints the code to initialize a message queue. The function is called to generate the code for the
-- message queues corresponding to the channels declared by the user plus the ones that belong to each
-- of the tasks that is used to notify the inclusion of a given message on a specific queue.
ppInitMsgQueue :: RTEMSMsgQueue -> DocStyle
ppInitMsgQueue (RTEMSChannelMsgQueue identifier ts size (RTEMSTask taskId classId _ _ _ ports)) = vsep
    [
        emptyDoc,
        pretty identifier <> pretty ".__task_msgq" <+> pretty "=" <+> pretty (ppTaskMessageQueue taskId) <> semi,
        pretty identifier <> pretty ".__task_port" <+> pretty "=" <+> ppVariantForAction classId portId <> semi,
        pretty identifier <> pretty ".__msgq_size" <+> pretty "=" <+> ppSizeOf ts <> semi,
        emptyDoc,
        ppCFunctionCall (pretty "NEXT_RESOURCE_NAME") [
            pretty "nmsg_queue_name[0]", pretty "nmsg_queue_name[1]",
            pretty "nmsg_queue_name[2]", pretty "nmsg_queue_name[3]"] <> semi,
        pretty "r_name" <+> pretty "=" <+>
            ppCFunctionCall (pretty "rtems_build_name") [
                pretty "nmsg_queue_name[0]", pretty "nmsg_queue_name[1]",
                pretty "nmsg_queue_name[2]", pretty "nmsg_queue_name[3]"] <> semi,
        emptyDoc,
        pretty "status" <+> pretty "=" <+>
            ppCFunctionCall (pretty "rtems_message_queue_create")
                [
                    -- | name
                    pretty "r_name",
                    -- | numer of mesaages
                    pretty (show size),
                    -- | size of the messages
                    ppSizeOf ts,
                    -- | attributes
                    pretty "RTEMS_FIFO",
                    -- | queue id
                    ppCReferenceExpression (pretty identifier) <> pretty ".__msgq_id"] <> semi,
        emptyDoc,
        ppCIfBlock (pretty "status" <+> pretty "!=" <+> pretty "RTEMS_SUCCESSFUL")
            (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
    ]
    where 
        portId = case find (\case {
            RTEMSInputPort _ chid _ _ -> chid == identifier;
            _ -> False }) ports of
                Just (RTEMSInputPort pid _ _ _) -> pid
                _ -> error $ "Invalid port connection for channel: " ++ show identifier
ppInitMsgQueue obj@(RTEMSChannelMsgQueue {}) = error $ "Invalid channel objet: " ++ show obj
ppInitMsgQueue (RTEMSTaskMsgQueue identifier size) = vsep
    [
        emptyDoc,
        ppCFunctionCall (pretty "NEXT_RESOURCE_NAME") [
            pretty "nmsg_queue_name[0]", pretty "nmsg_queue_name[1]",
            pretty "nmsg_queue_name[2]", pretty "nmsg_queue_name[3]"] <> semi,
        pretty "r_name" <+> pretty "=" <+>
            ppCFunctionCall (pretty "rtems_build_name") [
                pretty "nmsg_queue_name[0]", pretty "nmsg_queue_name[1]",
                pretty "nmsg_queue_name[2]", pretty "nmsg_queue_name[3]"] <> semi,
        emptyDoc,
        ppCFunctionCall (pretty "rtems_message_queue_create")
            [
                -- | name
                pretty "r_name",
                -- | numer of mesaages
                pretty (show size),
                -- | size of the messages
                ppSizeOf UInt32,
                -- | attributes
                pretty "RTEMS_FIFO",
                -- | queue id
                ppCReferenceExpression (pretty identifier)] <> semi,
        emptyDoc,
        ppCIfBlock (pretty "status" <+> pretty "!=" <+> pretty "RTEMS_SUCCESSFUL")
            (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
    ]

ppInitResource :: RTEMSGlobal -> DocStyle
ppInitResource (RTEMSResource identifier _ _) =
    pretty identifier <> pretty ".__resource_id.lock = __RTEMSResourceLock__None" <> semi
ppInitResource obj = error $ "Invalid global object (not a resource): " ++ show obj

ppInitTask :: RTEMSGlobal -> DocStyle
ppInitTask (RTEMSTask identifier _ _ _ _ ports) = vsep $
    emptyDoc : [pretty identifier <> pretty "." <> pretty portId
               <+> pretty "=" <+> ppCReferenceExpression (pretty channel) <> semi |
       (RTEMSOutputPort portId channel) <- ports]
ppInitTask obj = error $ "Invalid global object (not a task): " ++ show obj

ppInitHandler :: RTEMSGlobal -> DocStyle
ppInitHandler (RTEMSHandler identifier _ _ _ ports) = vsep $
    emptyDoc : [pretty identifier <> pretty "." <> pretty portId
               <+> pretty "=" <+> ppCReferenceExpression (pretty channel) <> semi |
       (RTEMSOutputPort portId channel) <- ports]
ppInitHandler obj = error $ "Invalid global object (not a task): " ++ show obj

ppRTEMSTaskCreate :: RTEMSGlobal -> DocStyle
ppRTEMSTaskCreate (RTEMSTask identifier _ _ _ _ _) = vsep
    [
        pretty "result" <+> pretty "=" <+>
            ppCFunctionCall (namefy $ pretty "termina" <::> pretty "task_create")
                [ppCReferenceExpression (pretty identifier <> pretty ".__task_id")] <> semi,
        emptyDoc,
        ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
            (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc]),
        emptyDoc
    ]
ppRTEMSTaskCreate obj = error $ "Invalid global object (not a task): " ++ show obj

emitterToVectorMap :: M.Map Identifier Integer
emitterToVectorMap = M.fromList [("irq_0", 0), ("irq_1", 1), ("irq_2", 2), ("irq_3", 3), ("irq_4", 4)]

ppRTEMSEmitter :: RTEMSEmitter -> DocStyle
ppRTEMSEmitter (RTEMSInterruptEmitter interrupt (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) =
    ppCFunctionPrototype (namefy (pretty "rtems_isr") <::> pretty interrupt) [pretty "void * ignored"] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep [
                    pretty classId <+> pretty "*" <+> pretty "self" <+> pretty "=" <+> ppCReferenceExpression (pretty identifier) <> semi,
                    emptyDoc,
                    pretty "Result result" <> semi,
                    emptyDoc,
                    pretty "result.__variant" <+> pretty "=" <+> pretty "Result" <::> pretty "Ok" <> semi,
                    emptyDoc,
                    pretty "result" <+> pretty "=" <+>
                        ppCFunctionCall (classFunctionName (pretty classId) (pretty action)) [pretty "self", pretty (emitterToVectorMap M.! interrupt)] <> semi,
                    emptyDoc,
                    ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
                        (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
                ]
            ) <> line
        ) <> line
ppRTEMSEmitter (RTEMSInterruptEmitter interrupt (RTEMSTask identifier _ _ _ _ ports)) =
    let portIdentifier =
            case fromJust $ find (\case { RTEMSEventPort _ emitter _ _ -> interrupt == emitter; _ -> False }) ports of
                RTEMSEventPort portId _ _ _ -> portId
                _ -> error $ "Invalid port connection for interrupt: " ++ show interrupt
    in
    ppCFunctionPrototype (namefy (pretty "rtems_isr") <::> pretty interrupt) [pretty "void * ignored"] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep [
                    pretty "rtems_status_code status" <> semi,
                    emptyDoc,
                    pretty "status" <+> pretty "=" <+>
                        ppCFunctionCall (pretty "rtems_message_queue_send") [pretty identifier <> pretty "." <> pretty portIdentifier] <> semi,
                    emptyDoc,
                    ppCIfBlock (pretty "status" <+> pretty "!=" <+> pretty "RTEMS_SUCESSFUL")
                        (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
                ]
            ) <> line
        ) <> line
ppRTEMSEmitter (RTEMSInterruptEmitter _ glb) = error $ "Invalid connection for interrupt: " ++ show glb
ppRTEMSEmitter (RTEMSPeriodicTimerEmitter timer tvsec tvusec (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) =
    ppCFunctionPrototype (namefy (pretty "rtems_periodic_timer") <::> pretty timer) [pretty "void * _ingnored"] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep [
                    pretty classId <+> pretty "*" <+> pretty "self" <+> pretty "=" <+> ppCReferenceExpression (pretty identifier) <> semi,
                    emptyDoc,
                    pretty "Result result" <> semi,
                    emptyDoc,
                    pretty "result.__variant" <+> pretty "=" <+> pretty "Result" <::> pretty "Ok" <> semi,
                    emptyDoc,
                    pretty "result" <+> pretty "=" <+>
                        ppCFunctionCall (classFunctionName (pretty classId) (pretty action)) [pretty "self", pretty timer <> pretty "." <> pretty "current"] <> semi,
                    emptyDoc,
                    ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
                        (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
                            -- TODO: Arm the next timer
                ]
            ) <> line
        ) <> line
ppRTEMSEmitter (RTEMSPeriodicTimerEmitter timer tvsec tvusec (RTEMSTask identifier _ _ _ _ ports)) =
    let portIdentifier =
            case fromJust $ find (\case { RTEMSEventPort _ emitter _ _ -> timer == emitter; _ -> False }) ports of
                RTEMSEventPort portId _ _ _ -> portId
                _ -> error $ "Invalid port connection for interrupt: " ++ show timer
    in
    ppCFunctionPrototype (namefy (pretty "rtems_periodic_timer") <::> pretty timer) [pretty "void * _ingnored"] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep [
                    pretty "rtems_status_code status" <> semi,
                    emptyDoc,
                    pretty "status" <+> pretty "=" <+>
                        ppCFunctionCall (pretty "rtems_message_queue_send") [pretty identifier <> pretty "." <> pretty portIdentifier] <> semi,
                    emptyDoc,
                    ppCIfBlock (pretty "status" <+> pretty "!=" <+> pretty "RTEMS_SUCESSFUL")
                        (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
                            -- TODO: Arm the next timer
                ]
            ) <> line
        ) <> line
ppRTEMSEmitter (RTEMSPeriodicTimerEmitter _ _ _ glb) = error $ "Invalid connection for timer: " ++ show glb
ppRTEMSEmitter (RTEMSSystemInitEmitter _ (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) =
    ppCFunctionPrototype (namefy $ pretty "rtems_app" <::> pretty "inital_event") [] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep [
                    pretty classId <+> pretty "*" <+> pretty "self" <+> pretty "=" <+> ppCReferenceExpression (pretty identifier) <> semi,
                    emptyDoc,
                    pretty "Result result" <> semi,
                    emptyDoc,
                    pretty "result.__variant" <+> pretty "=" <+> pretty "Result" <::> pretty "Ok" <> semi,
                    emptyDoc,
                    pretty "result" <+> pretty "=" <+>
                        ppCFunctionCall (classFunctionName (pretty classId) (pretty action)) [pretty "self"] <> semi,
                    emptyDoc,
                    ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
                        (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
                ]
            ) <> line
        ) <> line
ppRTEMSEmitter (RTEMSSystemInitEmitter event (RTEMSTask identifier classId _ _ _ ports)) =
    let action =
            case fromJust $ find (\case { RTEMSEventPort _ emitter _ _ -> event == emitter; _ -> False }) ports of
                RTEMSEventPort _ _ _ actionId -> actionId
                _ -> error $ "Invalid port connection for interrupt: " ++ show event
    in
    ppCFunctionPrototype (namefy $ pretty "rtems_app" <::> pretty "inital_event") [] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep [
                    pretty classId <+> pretty "*" <+> pretty "self" <+> pretty "=" <+> ppCReferenceExpression (pretty identifier) <> semi,
                    emptyDoc,
                    pretty "Result result" <> semi,
                    emptyDoc,
                    pretty "result.__variant" <+> pretty "=" <+> pretty "Result" <::> pretty "Ok" <> semi,
                    emptyDoc,
                    pretty "result" <+> pretty "=" <+>
                        ppCFunctionCall (classFunctionName (pretty classId) (pretty action)) [pretty "self"] <> semi,
                    emptyDoc,
                    ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
                        (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
                ]
            ) <> line
        ) <> line
ppRTEMSEmitter (RTEMSSystemInitEmitter _ glb) = error $ "Invalid connection for initial event: " ++ show glb

ppInitResourceProt :: Identifier -> RTEMSResourceLock -> DocStyle
ppInitResourceProt identifier RTEMSResourceLockNone = vsep $ emptyDoc : [pretty identifier <> pretty ".__resource_id.lock = __RTEMSResourceLock__None" <> semi]
ppInitResourceProt identifier RTEMSResourceLockIrq = vsep $ emptyDoc : [pretty identifier <> pretty ".__resource_id.lock = __RTEMSResourceLock__Irq" <> semi]
ppInitResourceProt identifier (RTEMSResourceLockMutex ceilPrio) = vsep
    [
        emptyDoc,
        pretty identifier <> pretty ".__resource_id.lock = __RTEMSResourceLock__Mutex" <> semi,
        pretty identifier <> pretty ".__resource_id.mutex.policy = __RTEMSMutexPolicy__Ceiling" <> semi,
        pretty identifier <> pretty ".__resource_id.mutex.prio_ceiling = " <> pretty (show ceilPrio) <> semi,
        emptyDoc,
        pretty "Result" <+> pretty "result.__variant" <+> pretty "=" <+> ppCFunctionCall (namefy $ pretty "termina" <::> pretty "resource_init") [ppCReferenceExpression (pretty identifier <> pretty ".__resource_id")] <> semi,
        emptyDoc,
        ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok") (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
    ]

ppMainFile :: [(ModuleName, ModuleMode, SAST.AnnotatedProgram SemanticAnns)] -> DocStyle
ppMainFile prjprogs =
    -- | Set of the first elements of the globals
    vsep $ [
        pretty "#include <rtems.h>",
        emptyDoc,
        pretty "#include <termina.h>"
    ] ++
    [includes incs] ++
    [
        externC <+> ppCFunctionPrototype (namefy $ pretty "termina_app" <::> pretty "init_globals") [] Nothing <> semi
    ] ++
    map ppVariantsForTaskActions (M.elems taskClss) ++
    -- | These arrays are used to store the names of the tasks, handlers and message queues
    
    -- | These arrays are used to store the names of the tasks, handlers and message queues
    [
        emptyDoc,
        ppTypeSpecifier Int8 <+> pretty "ntask_name[5] = \"0000\"" <> semi,
        ppTypeSpecifier Int8 <+> pretty "nsem_name[5] = \"0000\"" <> semi,
        ppTypeSpecifier Int8 <+> pretty "nmsgq_name[5] = \"0000\"" <> semi,
        ppTypeSpecifier Int8 <+> pretty "ntimers_name[5] = \"0000\"" <> semi,
        emptyDoc
    ] ++
    -- | Print the structures corresponding to the declared channels
    
    -- | Print the structures corresponding to the declared channels
    map ppMsgQueueDeclaration msgQueues ++
    [emptyDoc] ++
    map ppTaskClassCode (M.elems taskClss) ++
    map ppRTEMSEmitter emitters ++
    [
        -- | Function __rtems_app__enable_protection. This function is called from the Init task.
        -- It enables the protection of the shared resources when needed. In case the resource uses a mutex,
        -- it also initializes the mutex. The function is called AFTER the initialization of the tasks and handlers.
        staticC <+> ppCFunctionPrototype (namefy $ pretty "rtems_app" <::> pretty "enable_protection") [] Nothing <+>
        braces' (
            indentTab . align $
                vsep (
                    map (uncurry ppInitResourceProt) (M.toList resLockingMap)
                ) <> line
        ),
        emptyDoc,
        -- | Function __rtems_app__init_resources. This function is called from the Init task.
        -- The function is called BEFORE the initialization of the tasks and handlers. The function disables
        -- the protection of the global resources, since it is not needed when running in the Init task. It also
        -- executes the init() method of the resources if defined.
        staticC <+> ppCFunctionPrototype (namefy $ pretty "rtems_app" <::> pretty "init_resources") [] Nothing <+>
        braces' (
            (indentTab . align $
                vsep $
                (if not (null resources) || not (null pools) then
                    [
                        emptyDoc,
                        pretty "Result result" <> semi,
                        emptyDoc,
                        pretty "result.__variant" <+> pretty "=" <+> pretty "Result" <::> pretty "Ok" <> semi
                    ] else []) ++
                (if not (null resources) then emptyDoc : map ppInitResource resources else [])
                ++ (if not (null pools) then map ppInitPool pools else [])
                ++ (if not (null tasksMessageQueues) || not (null channelMessageQueues) then
                    [
                        emptyDoc,
                        pretty "rtems_status_code status" <> semi
                    ] else [])
                -- | Initialize the message queues of the tasks. We need to do this before initializing the
                -- channels so that the identifiers of the task message queues are already available.
                ++ (if not (null tasksMessageQueues) then map ppInitMsgQueue tasksMessageQueues else [])
                ++ (if not (null channelMessageQueues) then map ppInitMsgQueue channelMessageQueues else [])
            ) <> line
        ),
        emptyDoc,
        staticC <+> ppCFunctionPrototype (namefy $ pretty "rtems_app" <::> pretty "init_tasks") [] Nothing <+>
        braces' (
            indentTab . align $
                vsep $ map ppInitTask tasks
        ),
        emptyDoc,
        staticC <+> ppCFunctionPrototype (namefy $ pretty "rtems_app" <::> pretty "init_handlers") [] Nothing <+>
        braces' (
            indentTab . align $
                vsep $ map ppInitHandler handlers        
        ),
        emptyDoc,
        -- | RTEMS Init task
        ppCFunctionPrototype (pretty "Init") [pretty "rtems_task_argument ignored"] (Just (pretty "rtems_task")) <+>
        braces' (
            (indentTab . align $
                vsep $
                [
                    emptyDoc,
                    ppCFunctionCall (namefy $ pretty "termina_app" <::> pretty "init_globals") [] <> semi,
                    ppCFunctionCall (namefy $ pretty "rtems_app" <::> pretty "init_resources") [] <> semi,
                    ppCFunctionCall (namefy $ pretty "rtems_app" <::> pretty "init_tasks") [] <> semi,
                    ppCFunctionCall (namefy $ pretty "rtems_app" <::> pretty "init_handlers") [] <> semi,
                    emptyDoc
                ] ++
                    -- Execute initialization action in case there is one
                    (case find (\case { RTEMSSystemInitEmitter {} -> True; _ -> False }) emitters of
                        Just (RTEMSSystemInitEmitter {}) -> [ppCFunctionCall (namefy $ pretty "rtems_app" <::> pretty "inital_event") [] <> semi, emptyDoc]
                        _ -> [])
                ++ [
                    ppCFunctionCall (namefy $ pretty "rtems_app" <::> pretty "enable_protection") [] <> semi,
                    emptyDoc,
                    ppCFunctionCall (pretty "rtems_task_delete") [pretty "RTEMS_SELF"] <> semi
                ]
            ) <> line
        ),
        emptyDoc,
        -- | RTEMS application configuration
        pretty "#define CONFIGURE_MAXIMUM_TASKS" <+> parens (pretty (show (length tasks + 1))),
        pretty "#define CONFIGURE_MAXIMUM_MESSAGE_QUEUES" <+> parens (pretty (show (length msgQueues)))
    ] ++
    (if not (null msgQueues) then
        [
            pretty "#define CONFIGURE_MESSAGE_BUFFER_MEMORY ( \\"
        ] ++
        punctuate (line <> pretty "    + \\") (map ppMessagesForQueue msgQueues) ++
        [
            pretty ")"
        ]
    else
        []) ++
    [
        emptyDoc,
        pretty "#define CONFIGURE_APPLICATION_DOES_NOT_NEED_CONSOLE_DRIVER",
        pretty "#define CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER",
        pretty "#define CONFIGURE_MICROSECONDS_PER_TICK" <+> parens (pretty $ show (10000 :: Integer)),
        emptyDoc,
        pretty "#define CONFIGURE_RTEMS_INIT_TASKS_TABLE",
        emptyDoc,
        pretty "#define CONFIGURE_INIT",
        emptyDoc,
        pretty "#include <rtems/confdefs.h>"
    ]
    where
        -- | Original program list filtered to only include the global declaration
        globals = map (\(mn, mm, elems) -> (mn, mm, [g | (SAST.GlobalDeclaration g) <- elems])) prjprogs
        -- | Map between the class identifiers and the class definitions
        classMap = foldr
                (\(_, _, objs) accMap ->
                    foldr (\obj currMap ->
                        case obj of
                            SAST.TypeDefinition cls@(Class _ classId _ _ _) _ -> M.insert classId cls currMap
                            _ -> currMap
                        ) accMap objs
                ) M.empty prjprogs
        -- | List of modules that actually contain the global declarations and are the only ones that must
        -- be included
        glbs = filter (\(_, _, objs) -> not (null objs)) globals
        -- | List of modules that must be included
        incs = map (\(nm, mm, _) -> (nm, mm)) glbs
        -- List of RTEMS global declarations (tasks, handlers, resources and channels)
        rtemsGlbs = concatMap (\(_, _, objs) ->
            map (`buildRTEMSGlobal` classMap)
                (filter (\case { Task {} -> True; Resource {} -> True; Handler {} -> True; _ -> False}) objs)
            ) glbs

        -- List of used task classes
        taskClss = foldr (\glb acc -> case glb of
                RTEMSTask _ _ cls@(Class _ classId _ _ _) _ _ _ -> M.insert classId cls acc
                _ -> acc
            ) M.empty rtemsGlbs

        tasks = [t | t@(RTEMSTask {}) <- rtemsGlbs]
        handlers = [h | h@(RTEMSHandler {}) <- rtemsGlbs]
        pools = [p | p@(RTEMSPool {}) <- rtemsGlbs]
        resources = [r | r@(RTEMSResource {}) <- rtemsGlbs]

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

        tasksMessageQueues =
            map (\case {
                    (RTEMSTask identifier _ _ _ _ _) ->  RTEMSTaskMsgQueue (ppTaskMessageQueue identifier) 1;
                    _ -> error "Invalid global object (not a task)"}) tasks

        channelMessageQueues = concatMap (\(_, _, objs) ->
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

        ppMessagesForQueue :: RTEMSMsgQueue -> DocStyle
        ppMessagesForQueue (RTEMSTaskMsgQueue _ size) =
            vsep [
                pretty "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( \\",
                pretty "        " <> pretty size <> pretty ", \\",
                pretty "        " <> ppSizeOf UInt32 <> pretty " \\",
                pretty "    ) \\"
            ]
        ppMessagesForQueue (RTEMSChannelMsgQueue _ ts size _) =
            vsep [
                pretty "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( \\",
                pretty "        " <> pretty size <> pretty ", \\",
                pretty "        " <> ppSizeOf ts <> pretty " \\",
                pretty "    ) \\"
            ]

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

        emitters = foldr (\e acc -> case e of
            Just emitter -> emitter : acc
            Nothing -> acc) [] $ concatMap (\(_, _, objs) -> 
                map (`buildRTEMSEmitter` emitterConnectionsMap) $ objs
                -- Manually add the missing global objects:
                ++ [
                        Emitter "irq_3" (DefinedType "Interrupt") Nothing [] (internalErrorSeman `SemAnn` GTy (GGlob (SEmitter (DefinedType "Interrupt")))),
                        Emitter "system_init" (DefinedType "SystemInit") Nothing [] (internalErrorSeman `SemAnn` GTy (GGlob (SEmitter (DefinedType "SystemInit"))))
                    ]
                ) glbs

        -- | Map between the resources and the locking mechanism that must be used
        resLockingMap = getResLocking . S.elems <$> dependenciesMap
        -- | Obtains the locking mechanism that must be used for a resource
        getResLocking :: [RTEMSGlobal] -> RTEMSResourceLock
        getResLocking [] = RTEMSResourceLockNone
        getResLocking [_] = RTEMSResourceLockNone
        getResLocking ((RTEMSHandler {}) : _) = RTEMSResourceLockIrq
        getResLocking ((RTEMSTask _ _ _ priority _ _) : gs) = getResLocking' priority gs
            where
                getResLocking' :: Integer -> [RTEMSGlobal] -> RTEMSResourceLock
                -- | If we have reach the end of the list, it means that there are at least two different tasks that
                -- access the resource. We are going to force the use of the priority ceiling algorithm. In the
                -- (hopefully near) future, we will support algorithm selection via the configuration file.
                getResLocking' ceilPrio [] = RTEMSResourceLockMutex ceilPrio
                getResLocking' _ ((RTEMSHandler {}) : _) = RTEMSResourceLockIrq
                getResLocking' ceilPrio ((RTEMSTask _ _ _ prio _ _) : gs') = getResLocking' (min prio ceilPrio) gs'
                getResLocking' _ _ = error "Internal error when obtaining the resource dependencies."
        getResLocking _ = error "Internal error when obtaining the resource dependencies."
