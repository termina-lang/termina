{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module PPrinter.Application.OS.RTEMSNOEL where

import Semantic.Monad (SemanticAnns)
import AST.Seman
import PPrinter.Common
import Prettyprinter
import qualified Data.Map as M
import qualified Data.Set as S

import Modules.Modules
import Modules.Printing
import qualified AST.Seman as SAST
import Data.Maybe (fromJust)

data RTEMSGlobal =
    -- | RTEMS Task
    RTEMSTask
      Identifier -- ^ task identifier
      (TypeDef SemanticAnns)  -- ^ task class
      Integer -- ^ task priority
      Integer -- ^ task stack size
      (Maybe (Expression SemanticAnns)) -- ^ task state initialization expression
    -- | RTEMS Handler
    | RTEMSHandler
      Identifier -- ^ handler identifier
      (TypeDef SemanticAnns) -- ^ handler class
      Integer -- ^ interrupt vector
      (Maybe (Expression SemanticAnns)) -- ^ handler state initialization expression
    -- | RTEMS Resource
    | RTEMSResource
      Identifier -- ^ resource identifier
      (TypeDef SemanticAnns) -- ^ resource class
      (Maybe (Expression SemanticAnns)) -- ^ resource state initialization expression
    | RTEMSPool
      Identifier -- ^ pool identifier
      TypeSpecifier -- ^ type of the elements of the pool
      Integer -- ^ pool size
    | RTEMSMsgQueue
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
    (RTEMSTask id1 _ _ _ _) == (RTEMSTask id2 _ _ _ _) = id1 == id2
    (RTEMSHandler id1 _ _ _) == (RTEMSHandler id2 _ _ _) = id1 == id2
    (RTEMSResource id1 _ _) == (RTEMSResource id2 _ _) = id1 == id2
    _ == _ = False

-- | Ord instance for RTEMSGlobal
instance Ord RTEMSGlobal where
    compare (RTEMSTask id1 _ _ _ _) (RTEMSTask id2 _ _ _ _) = compare id1 id2
    compare (RTEMSHandler id1 _ _ _) (RTEMSHandler id2 _ _ _) = compare id1 id2
    compare (RTEMSResource id1 _ _) (RTEMSResource id2 _ _) = compare id1 id2
    compare (RTEMSPool id1 _ _) (RTEMSPool id2 _ _) = compare id1 id2
    compare (RTEMSMsgQueue id1 _ _) (RTEMSMsgQueue id2 _ _) = compare id1 id2
    compare (RTEMSTask {}) _ = LT
    compare (RTEMSHandler {}) _ = LT
    compare (RTEMSResource {}) _ = LT
    compare (RTEMSPool {}) _ = LT
    compare (RTEMSMsgQueue {}) _ = LT

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

getVector :: [Modifier] -> Integer
getVector [] = 0
getVector ((Modifier "vector" (Just (KC (I _ vector)))) : _) = vector
getVector (_ : modifiers) = getVector modifiers

hasInitMethod :: TypeDef SemanticAnns -> Bool
hasInitMethod (Class _ _ members _) = any (
    \member -> case member of
        ClassMethod "init" _ _ _-> True
        _ -> False) members
hasInitMethod ty = error $ "Invalid type (not a class): " ++ show ty

buildRTEMSGlobal :: Global SemanticAnns -> M.Map Identifier (TypeDef SemanticAnns) -> RTEMSGlobal
buildRTEMSGlobal (Task identifier (DefinedType ty) expr modifiers _) classMap = RTEMSTask identifier (fromJust (M.lookup ty classMap)) (getPriority modifiers) (getStackSize modifiers) expr
buildRTEMSGlobal (Handler identifier (DefinedType ty) expr modifiers _) classMap = RTEMSHandler identifier (fromJust (M.lookup ty classMap)) (getVector modifiers) expr
buildRTEMSGlobal (Resource identifier (DefinedType ty) expr _ _) classMap = RTEMSResource identifier (fromJust (M.lookup ty classMap)) expr
buildRTEMSGlobal (Resource identifier (Pool ty (K size)) _ _ _) _ = RTEMSPool identifier ty size
buildRTEMSGlobal (Resource identifier (MsgQueue ts (K size)) _ _ _) _ = RTEMSMsgQueue identifier ts size
buildRTEMSGlobal obj _ = error $ "Invalid global object: " ++ show obj

addDependency :: RTEMSGlobal -> Maybe (S.Set RTEMSGlobal) -> Maybe (S.Set RTEMSGlobal)
addDependency newGlb Nothing = Just (S.singleton newGlb)
addDependency newGlb (Just prevGlbs) = Just (S.insert newGlb prevGlbs)

addDependencies :: S.Set RTEMSGlobal -> Maybe (S.Set RTEMSGlobal) -> Maybe (S.Set RTEMSGlobal)
addDependencies newGlbs Nothing = Just newGlbs
addDependencies newGlbs (Just prevGlbs) = Just (S.union newGlbs prevGlbs)

-- | Prints the code of the RTEMS tasks that will execute a Termina task.
-- The RTEMS task will implement the main loop of the task. This loop
-- will call the run() method of the Termina task, and will handle the
-- return value. If the value is Abort, the task will call
-- rtems_shutdown_executive() to termine the application. If the value
-- is Finish, the task will call rtems_task_delete() to terminate itself.
-- Otherwise, the task will call the run() method again.
ppRTEMSTaskCode :: TypeDef SemanticAnns -> DocStyle
ppRTEMSTaskCode (Class _ classId _ _) = staticC <+>
    ppCFunctionPrototype (namefy (pretty "rtems_task") <::> pretty classId) [pretty "rtems_task_argument arg"] (Just (pretty "rtems_task")) <+>
        braces' (line <>
            (indentTab . align $
                vsep [
                    -- ClassIdentifier self = &task_identifier;
                    pretty classId <+> pretty "*" <+> pretty "self" <+> pretty "=" <+> parens (pretty classId <+> pretty "*") <> pretty "arg" <> semi,
                    emptyDoc,
                    -- TaskRet ret = TaskRet__Continue;
                    pretty "TaskRet ret" <> semi,
                    emptyDoc,
                    pretty "ret.__variant" <+> pretty "=" <+> pretty "TaskRet" <::> pretty "Continue" <> semi,
                    emptyDoc,
                    -- for (;;)
                    ppCInfiniteLoop (
                        vsep [
                            emptyDoc,
                            -- ret = ClassId__run(self);
                            pretty "ret" <+> pretty "=" <+>  ppCFunctionCall (taskRunMethodName classId) [pretty "self"] <> semi,
                            emptyDoc,
                            ppCIfBlock (pretty "ret.__variant" <+> pretty "==" <+> pretty "TaskRet" <::> pretty "Abort")
                                (vsep [pretty "break" <> semi, emptyDoc]) <+>
                            ppCElseIfBlock (pretty "ret.__variant" <+> pretty "==" <+> pretty "TaskRet" <::> pretty "Finish")
                                (vsep [pretty "rtems_task_delete(RTEMS_SELF)" <> semi, emptyDoc]) <+>
                            ppCElseBlock
                                emptyDoc,
                            emptyDoc
                        ]
                    ),
                    emptyDoc,
                    pretty "rtems_shutdown_executive(1)" <> semi
                ]
            ) <> line
        ) <> line
ppRTEMSTaskCode obj = error $ "Invalid global object (not a task): " ++ show obj

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

ppInitMsgQueue :: RTEMSGlobal -> DocStyle
ppInitMsgQueue (RTEMSMsgQueue identifier ts size) = vsep
    [
        emptyDoc,
        pretty "result" <+> pretty "=" <+>
            ppCFunctionCall (namefy $ pretty "termina" <::> pretty "msg_queue_init")
                [ppCReferenceExpression (pretty identifier),
                 ppSizeOf ts,
                 pretty (show size)] <> semi,
        emptyDoc,
        ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
            (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
    ]
ppInitMsgQueue obj = error $ "Invalid global object (not a message queue): " ++ show obj

ppInitResource :: RTEMSGlobal -> DocStyle
ppInitResource (RTEMSResource identifier cls@(Class _ classId _ _) _) = vsep $
    (pretty identifier <> pretty ".__resource_id.lock = __RTEMSResourceLock__None" <> semi) :
    if hasInitMethod cls then
        [
            emptyDoc,
            pretty "result" <+> pretty "=" <+>
                ppCFunctionCall (classFunctionName (pretty classId) (pretty "init"))
                    [ppCReferenceExpression (pretty identifier)] <> semi,
            emptyDoc,
            ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
                (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
        ]
    else []
ppInitResource obj = error $ "Invalid global object (not a resource): " ++ show obj

ppInitTask :: RTEMSGlobal -> DocStyle
ppInitTask (RTEMSTask identifier cls@(Class _ classId _ _) priority stackSize _) = vsep $
    [
        emptyDoc,
        pretty identifier <> pretty ".__task_id.priority" <+> pretty "=" <+> pretty (show priority) <> semi,
        pretty identifier <> pretty ".__task_id.stack_size" <+> pretty "=" <+> pretty (show stackSize) <> semi,
        pretty identifier <> pretty ".__task_id.entry" <+> pretty "=" <+> (namefy (pretty "rtems_task") <::> pretty classId) <> semi,
        pretty identifier <> pretty ".__task_id.argument" <+> pretty "=" <+> parens (pretty "rtems_task_argument") <> ppCReferenceExpression (pretty identifier) <> semi
    ] ++
    (if hasInitMethod cls then
        [
            pretty "result" <+> pretty "=" <+> ppCFunctionCall (classFunctionName (pretty classId) (pretty "init"))
                [ppCReferenceExpression (pretty identifier)] <> semi,
            emptyDoc,
            ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
                (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
        ]
    else [])
ppInitTask obj = error $ "Invalid global object (not a task): " ++ show obj

ppCreateTask :: RTEMSGlobal -> DocStyle
ppCreateTask (RTEMSTask identifier _ _ _ _) = vsep
    [
        pretty "result" <+> pretty "=" <+>
            ppCFunctionCall (namefy $ pretty "termina" <::> pretty "task_create")
                [ppCReferenceExpression (pretty identifier <> pretty ".__task_id")] <> semi,
        emptyDoc,
        ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
            (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc]),
        emptyDoc
    ]
ppCreateTask obj = error $ "Invalid global object (not a task): " ++ show obj

ppInstallHandler :: RTEMSGlobal -> DocStyle
ppInstallHandler (RTEMSHandler identifier _ _ _) = vsep
    [
        pretty "result" <+> pretty "=" <+>
            ppCFunctionCall (namefy $ pretty "termina" <::> pretty "install_handler")
                [ppCReferenceExpression (pretty identifier <> pretty ".__handler_id")] <> semi,
        emptyDoc,
        ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
            (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc]),
        emptyDoc
    ]
ppInstallHandler obj = error $ "Invalid global object (not a handler): " ++ show obj

ppInitHandler :: RTEMSGlobal -> DocStyle
ppInitHandler (RTEMSHandler identifier cls@(Class _ classId _ _) vector _) = vsep $
    [
        emptyDoc,
        pretty identifier <> pretty ".__handler_id.entry" <+> pretty "=" <+> (namefy (pretty "rtems_isr") <::> pretty identifier) <> semi,
        pretty identifier <> pretty ".__handler_id.vector" <+> pretty "=" <+> pretty (show vector) <> semi
    ] ++
    if hasInitMethod cls then
        [
            emptyDoc,
            pretty "result" <+> pretty "=" <+>
                ppCFunctionCall (classFunctionName (pretty classId) (pretty "init"))
                    [ppCReferenceExpression (pretty identifier)] <> semi,
            emptyDoc,
            ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
                (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc]),
            emptyDoc
        ]
    else []
ppInitHandler obj = error $ "Invalid global object (not a handler): " ++ show obj

ppRTEMSHandlerCode :: RTEMSGlobal -> DocStyle
ppRTEMSHandlerCode (RTEMSHandler identifier (Class _ classId _ _) _ _) = staticC <+>
    ppCFunctionPrototype (namefy (pretty "rtems_isr") <::> pretty identifier) [pretty "void * ignored"] Nothing <+>
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
                        ppCFunctionCall (handlerHandleMethodName classId) [pretty "self"] <> semi,
                    emptyDoc,
                    ppCIfBlock (pretty "result.__variant" <+> pretty "!=" <+> pretty "Result" <::> pretty "Ok")
                        (vsep [pretty "rtems_shutdown_executive(1)" <> semi, emptyDoc])
                ]
            ) <> line
        ) <> line
ppRTEMSHandlerCode obj = error $ "Invalid global object (not a task): " ++ show obj

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
    map ppRTEMSTaskCode (M.elems taskClss) ++
    map ppRTEMSHandlerCode handlers ++
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
        -- | Function __rtems_app__init_resources. This function is called from the Init task.
        -- The function is called BEFORE the initialization of the tasks and handlers. The function disables
        -- the protection of the global resources, since it is not needed when running in the Init task. It also
        -- executes the init() method of the resources if defined.
        staticC <+> ppCFunctionPrototype (namefy $ pretty "rtems_app" <::> pretty "init_resources") [] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep $
                (
                    if any hasInitMethod [cls | (RTEMSResource _ cls _) <- rtemsGlbs] then
                    [
                        pretty "Result" <+> pretty "result" <> semi,
                        emptyDoc,
                        pretty "result.__variant" <+> pretty "=" <+> pretty "Result" <::> pretty "Ok" <> semi,
                        emptyDoc
                    ] else []
                ) ++ [ppInitResource resource |resource@(RTEMSResource {}) <- rtemsGlbs]
                ++ [ppInitPool pl |pl@(RTEMSPool {}) <- rtemsGlbs]
                ++ [ppInitMsgQueue queue | queue@(RTEMSMsgQueue {}) <- rtemsGlbs]
            ) <> line
        ),
        emptyDoc,
        staticC <+> ppCFunctionPrototype (namefy $ pretty "rtems_app" <::> pretty "init_tasks") [] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep $ 
                (
                    if any hasInitMethod [cls | (RTEMSTask _ cls _ _ _) <- rtemsGlbs] then
                    [
                        pretty "Result" <+> pretty "result" <> semi,
                        emptyDoc,
                        pretty "result.__variant" <+> pretty "=" <+> pretty "Result" <::> pretty "Ok" <> semi,
                        emptyDoc
                    ] else []
                ) ++ map ppInitTask tasks
            ) <> line
        ),
        emptyDoc,
        staticC <+> ppCFunctionPrototype (namefy $ pretty "rtems_app" <::> pretty "init_handlers") [] Nothing <+>
        braces' (line <>
            (indentTab . align $
                vsep $ 
                [
                    pretty "Result" <+> pretty "result" <> semi,
                    emptyDoc,
                    pretty "result.__variant" <+> pretty "=" <+> pretty "Result" <::> pretty "Ok" <> semi,
                    emptyDoc
                ] ++ map ppInitHandler handlers
            ) <> line
        ),
        emptyDoc,
        -- | RTEMS Init task
        ppCFunctionPrototype (pretty "Init") [pretty "rtems_task_argument ignored"] (Just (pretty "rtems_task")) <+>
        braces' (line <>
            (indentTab . align $
                vsep $
                [
                    pretty "Result" <+> pretty "result" <> semi,
                    emptyDoc,
                    pretty "result.__variant" <+> pretty "=" <+> pretty "Result" <::> pretty "Ok" <> semi,
                    emptyDoc,
                    ppCFunctionCall (namefy $ pretty "termina_app" <::> pretty "init_globals") [] <> semi,
                    emptyDoc,
                    ppCFunctionCall (namefy $ pretty "rtems_app" <::> pretty "init_resources") [] <> semi,
                    emptyDoc
                ] ++
                (if any hasInitMethod [cls | (RTEMSHandler _ cls _ _) <- rtemsGlbs] then
                    [
                        ppCFunctionCall (namefy $ pretty "rtems_app" <::> pretty "init_handlers") [] <> semi,
                        emptyDoc
                    ] else [])
                ++ [
                    ppCFunctionCall (namefy $ pretty "rtems_app" <::> pretty "init_tasks") [] <> semi,
                    emptyDoc,
                    ppCFunctionCall (namefy $ pretty "rtems_app" <::> pretty "enable_protection") [] <> semi,
                    emptyDoc
                ] ++ map ppInstallHandler handlers
                ++ map ppCreateTask tasks
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
                            SAST.TypeDefinition cls@(Class _ classId _ _) _ -> M.insert classId cls currMap
                            _ -> currMap
                        ) accMap objs
                ) M.empty prjprogs
        -- | List of modules that actually contain the global declarations and are the only ones that must
        -- be included
        glbs = filter (\(_, _, objs) -> not (null objs)) globals
        -- | List of modules that must be included
        incs = map (\(nm, mm, _) -> (nm, mm)) glbs
        -- List of RTEMS global declarations (tasks, handlers and resources)
        rtemsGlbs = concatMap (\(_, _, objs) -> map (flip buildRTEMSGlobal classMap) objs) glbs
        tasks = [task | task@(RTEMSTask {}) <- rtemsGlbs]
        handlers = [handler | handler@(RTEMSHandler {}) <- rtemsGlbs]
        msgQueues = [queue | queue@(RTEMSMsgQueue {}) <- rtemsGlbs]

        ppMessagesForQueue :: RTEMSGlobal -> DocStyle
        ppMessagesForQueue (RTEMSMsgQueue _ ts size) =
            vsep [
                pretty "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( \\",
                pretty "        " <> pretty size <> pretty ", \\",
                pretty "        " <> ppSizeOf ts <> pretty " \\",
                pretty "    ) \\"
            ]
        ppMessagesForQueue g = error $ "Invalid global object (not a message queue): " ++ show g

        -- List of used task classes
        taskClss = foldr (\glb acc -> case glb of
                RTEMSTask _ cls@(Class _ classId _ _) _ _ _ -> if M.member classId acc then acc else M.insert classId cls acc
                _ -> acc
            ) M.empty tasks
        -- Map between the resources and the task and handlers that access them
        dependenciesMap = foldr
                (\glb accMap ->
                    case glb of
                    -- | Resources do not have ports, so they cannot produce dependencies
                    RTEMSResource {} -> accMap
                    RTEMSTask _ _ _ _ (Just (FieldAssignmentsExpression _ assignments _)) ->
                        foldr (\assignment currMap ->
                                case assignment of
                                    FieldPortConnection _ identifier _ -> M.alter (addDependency glb) identifier currMap
                                    _ -> currMap
                            ) accMap assignments
                    RTEMSHandler _ _ _ (Just (FieldAssignmentsExpression _ assignments _)) ->
                        foldr (\assignment currMap ->
                                case assignment of
                                    FieldPortConnection _ identifier _ -> M.alter (addDependency glb) identifier currMap
                                    _ -> currMap
                            ) accMap assignments
                    _ -> accMap
                ) M.empty rtemsGlbs
        -- | Map between the resources and the locking mechanism that must be used
        resLockingMap = M.map (getResLocking . S.elems) dependenciesMap
        -- | Obtains the locking mechanism that must be used for a resource
        getResLocking :: [RTEMSGlobal] -> RTEMSResourceLock
        getResLocking [] = RTEMSResourceLockNone
        getResLocking [_] = RTEMSResourceLockNone
        getResLocking ((RTEMSHandler {}) : _) = RTEMSResourceLockIrq
        getResLocking ((RTEMSTask _ _ priority _ _) : gs) = getResLocking' priority gs
            where
                getResLocking' :: Integer -> [RTEMSGlobal] -> RTEMSResourceLock
                -- | If we have reach the end of the list, it means that there are at least two different tasks that
                -- access the resource. We are going to force the use of the priority ceiling algorithm. In the
                -- (hopefully near) future, we will support algorithm selection via the configuration file.
                getResLocking' ceilPrio [] = RTEMSResourceLockMutex ceilPrio
                getResLocking' _ ((RTEMSHandler {}) : _) = RTEMSResourceLockIrq
                getResLocking' ceilPrio ((RTEMSTask _ _ prio _ _) : gs') = getResLocking' (min prio ceilPrio) gs'
                getResLocking' _ _ = error "Internal error when obtaining the resource dependencies."
        getResLocking _ = error "Internal error when obtaining the resource dependencies."

