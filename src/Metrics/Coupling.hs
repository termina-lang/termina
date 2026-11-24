-- | The coupling of a program: the imports between its modules, the calls
-- between its units and the wiring of its instances. The calls are taken on
-- the lowered bodies, where each operation on a port is a block of its own,
-- and a procedure invoked through an access port is resolved through the
-- connections of the architecture.
module Metrics.Coupling
  ( Target(..)
  , UnitKey
  , Call(..)
  , CallSet
  , unitCalls
  , unitCallers
  , moduleImports
  , InstanceKind(..)
  , PortCounts(..)
  , Instance(..)
  , ArchTotals(..)
  , instancesOf
  , archTotals
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import ControlFlow.Architecture.Types
import Core.Tree (childExpressionsOf, indexExpressionsOf)
import Lowering.AST
import Utils.Annotations (QualifiedName)

-- | What a unit calls: another unit, named by its class, if any, and its name,
-- or an operation that is not a unit, such as @send@ or @Pool::alloc@.
data Target = UnitTarget (Maybe Identifier) Identifier | OperationTarget String
  deriving (Show, Eq, Ord)

type UnitKey = (Maybe Identifier, Identifier)

-- | A call to a target: whether the generated C makes it through a function
-- pointer, and the instances that make it. A call from a function, or from a
-- class without instances, has no instances.
data Call = Call
  { callThroughPointer :: Bool
  , callInstances :: S.Set Identifier
  } deriving (Show, Eq)

instance Semigroup Call where
  Call p i <> Call p' i' = Call (p || p') (S.union i i')

-- | The distinct targets of a unit.
type CallSet = M.Map Target Call

-- | Where a block is: the architecture of the application, if the project has
-- one, the class of the unit, if any, and the types of the fields of the
-- class, among them its access ports.
data Context a = Context
  { ctxArch :: Maybe (TerminaProgArch a)
  , ctxClass :: Maybe Identifier
  , ctxFields :: M.Map Identifier (TerminaType a)
  }

-- | The calls of every unit of the given modules, taken on their basic-block
-- AST. The architecture resolves the calls through ports and gives the
-- instances; a library has none.
unitCalls :: Maybe (TerminaProgArch a) -> [AnnotatedProgram a] -> M.Map UnitKey CallSet
unitCalls arch = M.fromList . concatMap topLevel . concat

  where

    topLevel el =
      case el of
        Function name _ _ body _ _ -> [((Nothing, name), blockCalls (Context arch Nothing M.empty) body)]
        TypeDefinition (Class _ cls members _ _) _ ->
          let ctx = Context arch (Just cls) (M.fromList [ (fieldIdentifier f, fieldTerminaType f) | ClassField f <- members ])
          in [ ((Just cls, name), blockCalls ctx body) | (name, body) <- concatMap memberBody members ]
        _ -> []

    memberBody m =
      case m of
        ClassField _ -> []
        ClassMethod _ name _ _ body _ -> [(name, body)]
        ClassProcedure _ name _ body _ -> [(name, body)]
        ClassViewer name _ _ body _ -> [(name, body)]
        ClassAction _ name _ body _ -> [(name, body)]
        ClassCase _ name _ body _ -> [(name, body)]

-- | The distinct units that call each unit, each with the instances through
-- which it calls.
unitCallers :: M.Map UnitKey CallSet -> M.Map UnitKey (M.Map UnitKey (S.Set Identifier))
unitCallers calls =
  M.fromListWith (M.unionWith S.union)
    [ ((cls, name), M.singleton caller (callInstances call))
    | (caller, targets) <- M.toList calls
    , (UnitTarget cls name, call) <- M.toList targets
    ]

-- | Every instance of a class, task, handler, resource or test, with the
-- connections of its access ports, each from port to the element connected to
-- it.
instanceConnections :: TerminaProgArch a -> Identifier -> [(Identifier, M.Map Identifier (Identifier, a))]
instanceConnections arch cls =
  [ (taskName t, taskAPConnections t) | t <- M.elems (tasks arch), taskClass t == cls ]
  ++ [ (handlerName h, handlerAPConnections h) | h <- M.elems (handlers arch), handlerClass h == cls ]
  ++ [ (resourceName r, resAPConnections r) | r <- M.elems (resources arch), resourceClass r == cls ]
  ++ [ (testInstanceName t, testInstanceAPConnections t) | t <- M.elems (testInstances arch), testInstanceClass t == cls ]

-- | The procedures a call through an access port reaches, each with the
-- instances that reach it: the procedure of the class of the resource
-- connected to the port in each instance. Without instances, as in a library,
-- the call goes to the procedure of the interface.
procedureTargets :: Context a -> Identifier -> Identifier -> [(Target, S.Set Identifier)]
procedureTargets ctx port procedure =
  case resolved of
    [] -> [(OperationTarget (interfaceName ++ "::" ++ procedure), S.empty)]
    _ -> M.toList (M.fromListWith S.union resolved)

  where

    resolved =
      [ (UnitTarget (Just (resourceClass res)) procedure, S.singleton inst)
      | Just arch <- [ctxArch ctx]
      , Just c <- [ctxClass ctx]
      , (inst, conns) <- instanceConnections arch c
      , Just (target, _) <- [M.lookup port conns]
      , Just res <- [M.lookup target (resources arch)]
      ]

    interfaceName =
      case M.lookup port (ctxFields ctx) of
        Just (TAccessPort (TInterface _ iface)) -> iface
        _ -> port

-- | The calls of a block. A call whose target does not depend on the
-- connections is made by every instance of the class of the unit.
blockCalls :: Context a -> Block a -> CallSet
blockCalls ctx = M.unionsWith (<>) . map basicBlock . blockBody

  where

    everyInstance =
      case (ctxArch ctx, ctxClass ctx) of
        (Just arch, Just c) -> S.fromList (map fst (instanceConnections arch c))
        _ -> S.empty
    cls = ctxClass ctx
    direct targets = M.fromList [ (t, Call False everyInstance) | t <- targets ]
    operation pointer name = M.singleton (OperationTarget name) (Call pointer everyInstance)
    viaPort obj p = M.fromList [ (t, Call True insts) | (t, insts) <- procedureTargets ctx (portName obj) p ]
    exprs = M.unionsWith (<>) . map (direct . expressionCalls cls)
    blocks = M.unionsWith (<>) . map (blockCalls ctx)

    basicBlock bb =
      case bb of
        RegularBlock stmts -> M.unionsWith (<>) (map statement stmts)
        IfElseBlock ifc elifs els _ ->
          exprs (condIfCond ifc : map condElseIfCond elifs)
            `union` blocks (condIfBody ifc : map condElseIfBody elifs ++ maybe [] ((: []) . condElseBody) els)
        ForLoopBlock _ _ from to cond body _ -> exprs (from : to : maybe [] (: []) cond) `union` blockCalls ctx body
        MatchBlock e cases dflt _ ->
          exprs [e] `union` blocks (map matchBody cases ++ maybe [] (\(DefaultCase b _) -> [b]) dflt)
        SendMessage _ e _ -> exprs [e] `union` operation False "send"
        ProcedureInvoke obj p args _ -> exprs args `union` viaPort obj p
        AtomicLoad _ e _ -> exprs [e] `union` operation False "Atomic::load"
        AtomicStore _ e _ -> exprs [e] `union` operation False "Atomic::store"
        AtomicArrayLoad _ i e _ -> exprs [i, e] `union` operation False "AtomicArray::load"
        AtomicArrayStore _ i e _ -> exprs [i, e] `union` operation False "AtomicArray::store"
        AllocBox _ e _ -> exprs [e] `union` operation True "Pool::alloc"
        FreeBox _ e _ -> exprs [e] `union` operation True "Pool::free"
        DequeTake end _ e _ ->
          exprs [e] `union` operation False (case end of { DequeFront -> "Deque::take"; DequeBack -> "Deque::take_back" })
        DequePut end _ e _ ->
          exprs [e] `union` operation False (case end of { DequeBack -> "Deque::put"; DequeFront -> "Deque::put_front" })
        DequeExchange kind _ e out _ ->
          exprs [e, out] `union` operation False (case kind of { DequeTryPut -> "Deque::try_put"; DequeReplace -> "Deque::replace" })
        ReturnBlock e _ -> exprs (maybe [] (: []) e)
        ContinueBlock e _ -> exprs [e]
        RebootBlock _ -> M.empty
        NoReturnCall e _ -> exprs [e]
        SystemCall obj name args _ -> exprs args `union` viaPort obj name

    statement stmt =
      case stmt of
        Declaration _ _ _ initial _ -> exprs (maybe [] (: []) initial)
        AssignmentStmt obj e _ -> exprs (indexExpressionsOf semanticTree obj ++ [e])
        SingleExpStmt e _ -> exprs [e]

    union = M.unionWith (<>)

-- | The units an expression calls: functions, and methods and viewers of
-- @self@.
expressionCalls :: Maybe Identifier -> Expression a -> [Target]
expressionCalls cls expr = here ++ concatMap (expressionCalls cls) (childExpressionsOf semanticTree expr)

  where

    here =
      case expr of
        FunctionCall name _ _ -> [UnitTarget Nothing name]
        MemberFunctionCall obj name _ _ -> selfCall obj name
        DerefMemberFunctionCall obj name _ _ -> selfCall obj name
        _ -> []

    selfCall obj name = if isSelf obj then [UnitTarget cls name] else []

isSelf :: Object a -> Bool
isSelf obj =
  case obj of
    Variable ident _ -> ident == "self"
    Dereference (Variable ident _) _ -> ident == "self"
    _ -> False

-- | The name of the port that an object of the form @self->port@ names.
portName :: Object a -> Identifier
portName obj =
  case obj of
    MemberAccess inner name _ -> if isSelf inner then name else ""
    DereferenceMemberAccess inner name _ -> if isSelf inner then name else ""
    _ -> ""

-- | The modules each module imports and the modules of the project that
-- import it, both sorted, given the imports of each module of the project.
moduleImports :: M.Map QualifiedName [QualifiedName] -> M.Map QualifiedName ([QualifiedName], [QualifiedName])
moduleImports imports = M.mapWithKey lists imports

  where

    lists name out = (S.toList (S.fromList out), S.toList (M.findWithDefault S.empty name importers))
    importers = M.fromListWith S.union [ (imported, S.singleton name) | (name, out) <- M.toList imports, imported <- out ]

data InstanceKind = TaskInstance | HandlerInstance | ResourceInstance
  deriving (Show, Eq)

data PortCounts = PortCounts
  { portsIn :: Int
  , portsOut :: Int
  , portsSink :: Int
  , portsAccess :: Int
  } deriving (Show, Eq)

-- | The wiring of an instance of @app.fin@: its ports, the elements connected
-- to its access ports, the channels it sends to and receives from, the
-- emitters connected to it, for a resource the elements that use it, and the
-- number of distinct targets that each unit of its class calls when this
-- instance runs it.
data Instance = Instance
  { instanceName :: Identifier
  , instanceClass :: Identifier
  , instanceKind :: InstanceKind
  , instancePorts :: PortCounts
  , instanceAccesses :: [Identifier]
  , instanceSendsTo :: [Identifier]
  , instanceReceivesFrom :: [Identifier]
  , instanceEmitters :: [Identifier]
  , instanceUsers :: Int
  , instanceCalls :: [(Identifier, Int)]
  } deriving (Show, Eq)

-- | The instances of the application, tasks first, then handlers and
-- resources, each group sorted by name, given the calls of the units. Test
-- instances are left out.
instancesOf :: TerminaProgArch a -> M.Map UnitKey CallSet -> [Instance]
instancesOf arch calls =
  map withCalls (map task (M.elems (tasks arch)) ++ map handler (M.elems (handlers arch)) ++ map resource (M.elems (resources arch)))

  where

    withCalls i =
      i { instanceCalls =
            [ (name, M.size (M.filter (S.member (instanceName i) . callInstances) callSet))
            | ((Just cls, name), callSet) <- M.toList calls, cls == instanceClass i ] }

    targets = map fst . M.elems
    ports classes name = maybe (PortCounts 0 0 0 0) portCounts (M.lookup name (classes arch))
    users name = length (M.findWithDefault [] name (resourceSources arch))

    task t =
      Instance (taskName t) (taskClass t) TaskInstance (ports taskClasses (taskClass t))
        (targets (taskAPConnections t)) (targets (taskOutputPortConns t)) (targets (taskInputPortConns t))
        (targets (taskSinkPortConns t)) 0 []

    handler h =
      case handlerSinkPortConn h of
        (_, emitter, _) ->
          Instance (handlerName h) (handlerClass h) HandlerInstance (ports handlerClasses (handlerClass h))
            (targets (handlerAPConnections h)) (targets (handlerOutputPortConns h)) [] [emitter] 0 []

    resource r =
      Instance (resourceName r) (resourceClass r) ResourceInstance (ports resourceClasses (resourceClass r))
        (targets (resAPConnections r)) [] [] [] (users (resourceName r)) []

portCounts :: TPClass a -> PortCounts
portCounts cls = PortCounts (M.size (inputPorts cls)) (M.size (outputPorts cls)) (M.size (sinkPorts cls)) (M.size (accessPorts cls))

-- | The totals of the architecture of the application.
data ArchTotals = ArchTotals
  { totalTasks :: Int
  , totalHandlers :: Int
  , totalResources :: Int
  , totalPools :: Int
  , totalAtomics :: Int
  , totalDeques :: Int
  , totalChannels :: Int
  , totalEmitters :: Int
  , totalConnections :: Int
  } deriving (Show, Eq)

-- | The totals of the application. A connection is a port of an instance
-- connected to a channel, an emitter or a resource.
archTotals :: TerminaProgArch a -> ArchTotals
archTotals arch =
  ArchTotals
    { totalTasks = M.size (tasks arch)
    , totalHandlers = M.size (handlers arch)
    , totalResources = M.size (resources arch)
    , totalPools = M.size (pools arch)
    , totalAtomics = M.size (atomics arch) + M.size (atomicArrays arch)
    , totalDeques = M.size (deques arch)
    , totalChannels = M.size (channels arch)
    , totalEmitters = M.size (emitters arch)
    , totalConnections = sum (map connections (instancesOf arch M.empty))
    }

  where

    connections i =
      length (instanceAccesses i) + length (instanceSendsTo i) + length (instanceReceivesFrom i) + length (instanceEmitters i)
