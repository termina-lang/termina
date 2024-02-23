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
    | RTEMSAccessPort
        Identifier -- ^ port identifier
        Identifier -- ^ resource identifier
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
    | RTEMSInterruptEmitter
      Identifier -- ^ interrupt identifier
    | RTEMSPeriodicTimerEmitter
      Identifier -- ^ periodic timer identifier
    | RTEMSInitialEventEmitter
      Identifier -- ^ initial event identifier
    deriving Show

data RTEMSMsgQueue =
    RTEMSMsgQueue
      Identifier -- ^ message queue identifier
      TypeSpecifier -- ^ type of the elements of the message queue
      Integer -- ^ message queue size
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
    compare (RTEMSInterruptEmitter id1) (RTEMSInterruptEmitter id2) = compare id1 id2
    compare (RTEMSPeriodicTimerEmitter id1) (RTEMSPeriodicTimerEmitter id2) = compare id1 id2
    compare (RTEMSInitialEventEmitter id1) (RTEMSInitialEventEmitter id2) = compare id1 id2
    compare (RTEMSTask {}) _ = LT
    compare (RTEMSHandler {}) _ = LT
    compare (RTEMSResource {}) _ = LT
    compare (RTEMSPool {}) _ = LT
    compare (RTEMSInterruptEmitter {}) _ = LT
    compare (RTEMSPeriodicTimerEmitter {}) _ = LT
    compare (RTEMSInitialEventEmitter {}) _ = LT

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
                        Just (FieldPortConnection InboundPortConnection _ resourceIdentifier _) -> 
                            [RTEMSEventPort portIdentifier resourceIdentifier dts action]
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
                _ -> error $ "Invalid port connection: " ++ show portIdentifier
        buildEventPort (_ : members) = buildEventPort members

        eventPort = buildEventPort clsMembers

        ports = 
            concatMap (\case 
                ClassField (FieldDefinition portIdentifier (AccessPort {})) _ -> 
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection AccessPortConnection _ resourceIdentifier _) -> 
                            [RTEMSAccessPort portIdentifier resourceIdentifier]
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

addDependency :: RTEMSGlobal -> Maybe (S.Set RTEMSGlobal) -> Maybe (S.Set RTEMSGlobal)
addDependency newGlb Nothing = Just (S.singleton newGlb)
addDependency newGlb (Just prevGlbs) = Just (S.insert newGlb prevGlbs)

-- | Prints the code of the RTEMS tasks that will execute a Termina task.
-- The RTEMS task will implement the main loop of the task.
ppTaskClassCode :: TypeDef SemanticAnns -> DocStyle
ppTaskClassCode (Class TaskClass classId members _ _) = staticC <+>
    ppCFunctionPrototype (namefy (pretty "rtems_task") <::> pretty classId) [pretty "rtems_task_argument arg"] (Just (pretty "rtems_task")) <+>
        braces' (line <>
            (indentTab . align $
                vsep [
                    -- ClassIdentifier self = &task_identifier;
                    pretty classId <+> pretty "*" <+> pretty "self" <+> pretty "=" <+> parens (pretty classId <+> pretty "*") <> pretty "arg" <> semi,
                    emptyDoc,
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
                            -- Check the message type and execute the corresponding action
                            emptyDoc
                        ]
                    ),
                    emptyDoc,
                    pretty "rtems_shutdown_executive(1)" <> semi
                ]
            ) <> line
        ) <> line
ppTaskClassCode obj = error $ "Invalid global object (not a task): " ++ show obj

ppInitPool :: RTEMSGlobal -> DocStyle
ppInitPool (RTEMSPool identifier ts _) = vsep
    [
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

-- | Prints the code to initialize a message queue. The function is called to generate the code for the
-- message queues corresponding to the channels declared by the user plus the ones that belong to each
-- of the tasks that is used to notify the inclusion of a given message on a specific queue.
ppInitMsgQueue :: RTEMSMsgQueue -> DocStyle
ppInitMsgQueue (RTEMSMsgQueue identifier ts size) = vsep
    [
        emptyDoc,
        pretty "status" <+> pretty "=" <+>
            ppCFunctionCall (pretty "rtems_message_queue_create")
                [ppCReferenceExpression (pretty identifier),
                 ppSizeOf ts,
                 pretty (show size)] <> semi,
        emptyDoc,
        ppCIfBlock (pretty "status" <+> pretty "!=" <+> pretty "RTEMS_SUCCESSFUL")
            (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
    ]

ppInitResource :: RTEMSGlobal -> DocStyle
ppInitResource (RTEMSResource identifier _ _) =
    pretty identifier <> pretty ".__resource_id.lock = __RTEMSResourceLock__None" <> semi
ppInitResource obj = error $ "Invalid global object (not a resource): " ++ show obj

ppInitTask :: RTEMSGlobal -> DocStyle
ppInitTask (RTEMSTask identifier classId _ priority stackSize _) = vsep
    [
        emptyDoc,
        pretty identifier <> pretty ".__task_id.priority" <+> pretty "=" <+> pretty (show priority) <> semi,
        pretty identifier <> pretty ".__task_id.stack_size" <+> pretty "=" <+> pretty (show stackSize) <> semi,
        pretty identifier <> pretty ".__task_id.entry" <+> pretty "=" <+> (namefy (pretty "rtems_task") <::> pretty classId) <> semi,
        pretty identifier <> pretty ".__task_id.argument" <+> pretty "=" <+> parens (pretty "rtems_task_argument") <> ppCReferenceExpression (pretty identifier) <> semi
    ]
ppInitTask obj = error $ "Invalid global object (not a task): " ++ show obj

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

ppMsgQueueName :: Identifier -> DocStyle
ppMsgQueueName identifier = namefy (pretty "msgq" <::> pretty identifier)

ppRTEMSEmitter :: RTEMSGlobal -> M.Map Identifier RTEMSGlobal -> DocStyle
ppRTEMSEmitter (RTEMSInterruptEmitter interrupt) connectionsMap = 
    case M.lookup interrupt connectionsMap of
        Nothing -> pretty "//" <+> pretty interrupt <+> pretty " is not connected" <> line
        -- | The interrupt is connected to a handler. In this case, we must execute the action
        Just (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _) ->
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
        Just (RTEMSTask identifier _ _ _ _ ports) ->
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
        Just _ -> error $ "Invalid connection for interrupt: " ++ show interrupt
ppRTEMSEmitter (RTEMSPeriodicTimerEmitter timer) connectionsMap =
    case M.lookup timer connectionsMap of
        Nothing -> pretty "//" <+> pretty timer <+> pretty " is not connected" <> line
        -- | The interrupt is connected to a handler. In this case, we must execute the action
        Just (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _) ->
            ppCFunctionPrototype (namefy (pretty "rtems_periodic_timer") <::> pretty timer) [pretty "void * ingnored"] Nothing <+>
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
        Just (RTEMSTask identifier _ _ _ _ ports) ->
            let portIdentifier =
                    case fromJust $ find (\case { RTEMSEventPort _ emitter _ _ -> timer == emitter; _ -> False }) ports of
                        RTEMSEventPort portId _ _ _ -> portId
                        _ -> error $ "Invalid port connection for interrupt: " ++ show timer
            in
            ppCFunctionPrototype (namefy (pretty "rtems_periodic_timer") <::> pretty timer) [pretty "void * ingnored"] Nothing <+>
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
        Just _ -> error $ "Invalid connection for timer: " ++ show timer
ppRTEMSEmitter (RTEMSInitialEventEmitter event) connectionsMap =
    case M.lookup event connectionsMap of
        Nothing -> pretty "//" <+> pretty event <+> pretty " is not connected" <> line
        Just (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _) ->
            ppCFunctionPrototype (namefy (pretty "rtems_initial_event") <::> pretty event) [] Nothing <+>
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
        Just (RTEMSTask identifier classId _ _ _ ports) ->
            let action =
                    case fromJust $ find (\case { RTEMSEventPort _ emitter _ _ -> event == emitter; _ -> False }) ports of
                        RTEMSEventPort _ _ _ actionId -> actionId
                        _ -> error $ "Invalid port connection for interrupt: " ++ show event
            in
            ppCFunctionPrototype (namefy (pretty "rtems_initial_event") <::> pretty event) [] Nothing <+>
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
        Just _ -> error $ "Invalid connection for event: " ++ show event
ppRTEMSEmitter obj _ = error $ "Invalid global object (not an emitter): " ++ show obj

ppInitResourceProt :: Identifier -> RTEMSResourceLock -> DocStyle
ppInitResourceProt identifier RTEMSResourceLockNone = pretty identifier <> pretty ".__resource_id.lock = __RTEMSResourceLock__None" <> semi
ppInitResourceProt identifier RTEMSResourceLockIrq = pretty identifier <> pretty ".__resource_id.lock = __RTEMSResourceLock__Irq" <> semi
ppInitResourceProt identifier (RTEMSResourceLockMutex ceilPrio) = vsep
    [
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
    -- | These arrays are used to store the names of the tasks, handlers and message queues
    [
        ppTypeSpecifier Int8 <+> pretty "ntask_name[5] = \"0000\"" <> semi,
        ppTypeSpecifier Int8 <+> pretty "nsem_name[5] = \"0000\"" <> semi,
        ppTypeSpecifier Int8 <+> pretty "nmsgq_name[5] = \"0000\"" <> semi,
        ppTypeSpecifier Int8 <+> pretty "ntimers_name[5] = \"0000\"" <> semi
    ] ++
    map ppTaskClassCode (M.elems taskClss) ++
    map (`ppRTEMSEmitter` emitterConnectionsMap) emitters ++
    [
        -- | Function __rtems_app__enable_protection. This function is called from the Init task.
        -- It enables the protection of the shared resources when needed. In case the resource uses a mutex,
        -- it also initializes the mutex. The function is called AFTER the initialization of the tasks and handlers.
        staticC <+> ppCFunctionPrototype (namefy $ pretty "rtems_app" <::> pretty "enable_protection") [] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep (
                    map (uncurry ppInitResourceProt) (M.toList resLockingMap)
                )
            ) <> line
        ),
        emptyDoc,
        -- | RTEMS Init task
        ppCFunctionPrototype (pretty "Init") [pretty "rtems_task_argument ignored"] (Just (pretty "rtems_task")) <+>
        braces' (line <>
            (indentTab . align $
                vsep $
                [
                    pretty "rtems_status_code" <+> pretty "status" <+> pretty "=" <+> pretty "RTEMS_SUCCESSFUL" <> semi,
                    emptyDoc,
                    ppCFunctionCall (namefy $ pretty "termina_app" <::> pretty "init_globals") [] <> semi,
                    emptyDoc,
                    -- TODO: execute initialization action in case there is one
                    ppCFunctionCall (namefy $ pretty "rtems_app" <::> pretty "enable_protection") [] <> semi,
                    emptyDoc
                ]
                ++ [ppCFunctionCall (pretty "rtems_task_delete") [pretty "RTEMS_SELF"] <> semi]
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

        tasks = [task | task@(RTEMSTask {}) <- rtemsGlbs]
        -- handlers = [handler | handler@(RTEMSHandler {}) <- rtemsGlbs]
        emitters = 
            [emitter | emitter <- rtemsGlbs, 
                case emitter of { 
                    RTEMSInterruptEmitter {} -> True; 
                    RTEMSPeriodicTimerEmitter {} -> True; 
                    RTEMSInitialEventEmitter {} -> True; 
                    _ -> False 
                }
            ]
        msgQueues = [] 

        ppMessagesForQueue :: RTEMSMsgQueue -> DocStyle
        ppMessagesForQueue (RTEMSMsgQueue _ ts size) =
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
