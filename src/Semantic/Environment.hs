module Semantic.Environment where

import qualified Data.Map.Strict as M
import Semantic.AST
import Core.Utils (getGlobalIdentifier, getTypeIdentifier)
import Utils.Annotations
import Semantic.Types
import Parser.Types (undocumented)
import Configuration.Configuration
import Configuration.Platform (Platform)
import qualified Data.Set as S

----------------------------------------
-- | Global env
-- The declarations of the program by their name
type GlobalEnv = M.Map Identifier (GlobalDecl SemanticAnn)

-- | Local env
-- variables to their type
type LocalEnv = M.Map Identifier (LocatedElement (AccessKind, TerminaType SemanticAnn))

-- | Map with the moved variables
type MovedEnv = M.Map Identifier Location

-- | Map with the identifiers declared at the top level of the project
type DeclaredEnv = M.Map Identifier Location

-- | Environment required to type expression packed into just one type.
data Environment
 = ExprST
 { global :: GlobalEnv
 , local  :: LocalEnv
 , moved  :: MovedEnv
 -- | Set of all the modules that are imported in the current module together with the
 -- | current module.
 , visible :: S.Set QualifiedName
 -- | Every identifier that is taken at the top level of the program: the ones
 -- the language and the platform provide, and the ones declared by any module
 -- of the project, typed or not. The modules are typed one after another, so
 -- the global environment above only holds what the modules typed so far
 -- declare, which is why a local object is checked against this map instead.
 , declared :: DeclaredEnv
 -- | Target platform, for the static range checks (its @usize@ width; see
 -- 'Configuration.Platform').
 , targetPlatform :: Platform
 -- | Whether the program is a test, the only kind of program that may use
 -- test classes, test instances and the components of a unit test
 , testProgram :: Bool
 -- | The modules of the dependencies, which the project cannot change, so a
 -- collision with one of their names is reported in the project
 , dependencyModules :: S.Set QualifiedName
 }

-- | Identifiers that a program declares at the top level, each with the
-- location of its declaration. The constexpr declarations are in, even though
-- the transpiler folds them and they reach no C identifier: an object that took
-- the name of a constexpr would lose every read of it, because the type checker
-- substitutes the folded value before it looks the name up among the local
-- objects.
declaredNames :: Located a => [AnnASTElement' ty expr blk a] -> DeclaredEnv
declaredNames = foldr addName M.empty

  where

    addName :: Located a => AnnASTElement' ty expr blk a -> DeclaredEnv -> DeclaredEnv
    addName element'@(Function ident _ _ _ _ _) = M.insert ident (locationOf element')
    addName element'@(ExternFunction ident _ _ _ _ _) = M.insert ident (locationOf element')
    addName element'@(GlobalDeclaration glb) = M.insert (getGlobalIdentifier glb) (locationOf element')
    addName element'@(TypeDefinition tydef _) = M.insert (getTypeIdentifier tydef) (locationOf element')

    locationOf :: Located a => AnnASTElement' ty expr blk a -> Location
    locationOf = getLocation . getAnnotation

-- | A parameter of one of the interfaces the environment declares by hand,
-- which no source file writes and which therefore has no position.
builtinParameter :: Identifier -> TerminaType SemanticAnn -> Parameter SemanticAnn
builtinParameter ident ty = Parameter ident ty (buildExpAnn (undocumented Internal) ty)

-- | A function of the language whose body the OSAL provides.
builtinFunction :: Identifier -> [Parameter SemanticAnn] -> TerminaType SemanticAnn -> GlobalDecl SemanticAnn
builtinFunction ident params ty =
  ExternFunction ident params (Just ty) Returns [] (SemanticAnn (FnTy (FunctionSeman params ty Returns)) (undocumented Internal))

-- | A type that the language or the platform declares.
builtinType :: SemanTypeDef SemanticAnn -> GlobalDecl SemanticAnn
builtinType tydef = TypeDefinition tydef (buildTypeAnn (undocumented Internal))

-- | An emitter and a resource that the system declares, given their names and
-- the names of their classes.
builtinEmitter, builtinResource :: Identifier -> Identifier -> GlobalDecl SemanticAnn
builtinEmitter ident cls = GlobalDeclaration (Emitter ident ty Nothing [] (buildGlobalAnn (undocumented Internal) ty))

  where

    ty = TGlobal EmitterClass cls
builtinResource ident cls = GlobalDeclaration (Resource ident ty Nothing [] (buildGlobalAnn (undocumented Internal) ty))

  where

    ty = TGlobal ResourceClass cls

stdlibGlobalEnv :: Integer -> Integer -> [GlobalDecl SemanticAnn]
stdlibGlobalEnv outBufSize inBufSize =
  [
    -- | Floating-point bit reinterpretation. These are Prelude functions whose
    -- body is provided by the OSAL (memcpy-based static inline). They lower to a
    -- direct C call of the same name.
    builtinFunction "f32_to_bits" [builtinParameter "value" TFloat32] TUInt32,
    builtinFunction "f32_from_bits" [builtinParameter "bits" TUInt32] TFloat32,
    builtinFunction "f64_to_bits" [builtinParameter "value" TFloat64] TUInt64,
    builtinFunction "f64_from_bits" [builtinParameter "bits" TUInt64] TFloat64,
    builtinType (Struct "TimeVal" [FieldDefinition "tv_sec" TUInt32 (buildExpAnn (undocumented Internal) TUInt32), FieldDefinition "tv_usec" TUInt32 (buildExpAnn (undocumented Internal) TUInt32)] []),
    builtinType (Class EmitterClass "Interrupt" [] [] []),
    builtinType (Class EmitterClass "PeriodicTimer" [ClassField (FieldDefinition "period" (TStruct "TimeVal") (buildExpAnn (undocumented Internal) (TStruct "TimeVal")))] [] []),
    builtinType (Enum "SysPrintBase" [internalVariant "Decimal" [], internalVariant "Hexadecimal" []] []),
    builtinType (Enum "MutexProtocol" [internalVariant "Ceiling" [TUInt32]] []),
    builtinType (Enum "Exception" [
      -- | Error sending a message to a message queue
      internalVariant "EMsgQueueSendError" [
        -- | Address of the call to the runtime that sent the message, or 0
        TUSize,
        -- | ID of the message queue
        TUSize,
        -- | Error code
        TInt32],
      -- | Error receiving a message from a message queue
      internalVariant "EMsgQueueRecvError" [
        -- | ID of the message queue
        TUSize,
        -- | Error code
        TInt32],
      internalVariant "EArrayIndexOutOfBounds" [
        -- | Address of the offending expression
        TUSize,
        -- | Size of the array
        TUSize,
        -- | Offending index
        TUSize],
      internalVariant "EArraySliceOutOfBounds" [
        -- | Address of the offending expression
        TUSize,
        -- | Size of the array
        TUSize,
        -- | Offending upper index
        TUSize],
      internalVariant "EArraySliceNegativeRange" [
        -- | Address of the offending expression
        TUSize,
        -- | Lower index
        TUSize,
        -- | Upper index
        TUSize],
      internalVariant "EArraySliceInvalidRange" [
        -- | Address of the offending expression
        TUSize,
        -- | Expected size of the array
        TUSize,
        -- | Lower index
        TUSize,
        -- | Upper index
        TUSize],
      internalVariant "EShiftAmountOutOfBounds" [
        -- | Address of the offending expression
        TUSize,
        -- | Bit width of the shifted type
        TUSize,
        -- | Offending shift amount
        TUSize],
      internalVariant "EArithmeticOverflow" [
        -- | Address of the offending expression
        TUSize],
      internalVariant "EDivisionByZero" [
        -- | Address of the offending expression
        TUSize],
      internalVariant "ERuntimeFailure" [
        -- | Address of the call to the runtime that failed, or 0
        TUSize,
        -- | Operation of the runtime that failed
        TUInt32,
        -- | Error code of the operating system
        TInt32],
      -- | Fault of the processor. The fields are numbers of the platform,
      -- decoded with the platform at hand.
      internalVariant "EProcessorFault" [
        -- | Exception number of the architecture
        TUInt32,
        -- | Fault status register, or 0 when there is none
        TUInt32,
        -- | Address of the instruction that failed, or 0 when unknown
        TUSize,
        -- | Faulting address, or 0 when the architecture gives none
        TUSize],
      -- | Fatal error raised by the operating system itself
      internalVariant "EKernelFailure" [
        -- | Source of the fatal error, as the operating system numbers it
        TUInt32,
        -- | Code of the fatal error, which may be an address
        TUSize],
      -- | Stack overflow detected by the operating system
      internalVariant "EStackOverflow" [
        -- | Identifier of the task, or the invalid identifier when the stack
        -- is not of a task of the application
        TUSize]
      ] []),
    -- | SysTime interface
    builtinType (Interface SystemInterface "SysTime" [] [
      -- | procedure clock_get_uptime (&mut self, current_time : &mut TimeVal)
      InterfaceProcedure Immutable "clock_get_uptime" [builtinParameter "current_time" (TReference Mutable (TStruct "TimeVal"))] [] (buildExpAnn (undocumented Internal) TUnit)
    ] []),
    -- | SysPrint interface
    builtinType (Interface SystemInterface "SysPrint" [] [
      -- | procedure clock_get_uptime (&mut self, current_time : &mut TimeVal)
      InterfaceProcedure Mutable "print" [builtinParameter "str" (TReference Immutable (TArray TChar (Constant (I (TInteger outBufSize DecRepr) (Just (TConstSubtype TUSize))) (buildExpAnn (undocumented Internal) (TConstSubtype TUSize)))))] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println" [builtinParameter "str" (TReference Immutable (TArray TChar (Constant (I (TInteger outBufSize DecRepr) (Just (TConstSubtype TUSize))) (buildExpAnn (undocumented Internal) (TConstSubtype TUSize)))))] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_char" [builtinParameter "value" TChar] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_char" [builtinParameter "value" TChar] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_u8" [builtinParameter "value" TUInt8, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_u8" [builtinParameter "value" TUInt8, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_u16" [builtinParameter "value" TUInt16, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_u16" [builtinParameter "value" TUInt16, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_u32" [builtinParameter "value" TUInt32, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_u32" [builtinParameter "value" TUInt32, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_u64" [builtinParameter "value" TUInt64, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_u64" [builtinParameter "value" TUInt64, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_i8" [builtinParameter "value" TInt8, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_i8" [builtinParameter "value" TInt8, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_i16" [builtinParameter "value" TInt16, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_i16" [builtinParameter "value" TInt16, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_i32" [builtinParameter "value" TInt32, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_i32" [builtinParameter "value" TInt32, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_i64" [builtinParameter "value" TInt64, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_i64" [builtinParameter "value" TInt64, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_usize" [builtinParameter "value" TUSize, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_usize" [builtinParameter "value" TUSize, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_f32" [builtinParameter "value" TFloat32] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_f32" [builtinParameter "value" TFloat32] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "print_f64" [builtinParameter "value" TFloat64] [] (buildExpAnn (undocumented Internal) TUnit),
      InterfaceProcedure Mutable "println_f64" [builtinParameter "value" TFloat64] [] (buildExpAnn (undocumented Internal) TUnit)
    ] []),
    builtinType (Interface SystemInterface "SysGetChar" [] [
      InterfaceProcedure Mutable "read" [ 
          builtinParameter "str" (TReference Mutable (TArray TChar (Constant (I (TInteger inBufSize DecRepr) (Just (TConstSubtype TUSize))) (buildExpAnn (undocumented Internal) (TConstSubtype TUSize))))),
          builtinParameter "read_bytes" (TReference Mutable TUSize)] [] (buildExpAnn (undocumented Internal) TUnit)
    ] [])
  ]

sysInitGlobalEnv :: [GlobalDecl SemanticAnn]
sysInitGlobalEnv =
  [builtinType (Class EmitterClass "SystemInit" [] [] []),
   builtinEmitter "system_init" "SystemInit"]

sysExceptGlobalEnv :: [GlobalDecl SemanticAnn]
sysExceptGlobalEnv =
  [builtinType (Class EmitterClass "SystemExcept" [] [] []),
   builtinEmitter "system_except" "SystemExcept"]

systemEntryGlobalEnv :: [GlobalDecl SemanticAnn]
systemEntryGlobalEnv =
  [builtinType (Class ResourceClass "SystemEntry" [] ["SystemAPI"] []),
    builtinResource "system_entry" "SystemEntry"
  ]

-- | The resources of a test. A check records its failure and returns, and the
-- first argument of a check_eq is the value obtained and the second the one
-- expected. test_clock only exists in a unit test and test_scenario in a
-- concurrent one, which the architecture checks, since the level depends on
-- the whole deployment.
testGlobalEnv :: [GlobalDecl SemanticAnn]
testGlobalEnv =
  [builtinType (Interface SystemInterface "TestChecks" [] (
      [ procedure "check" [builtinParameter "cond" TBool] ]
      ++ [ procedure ("check_eq_" ++ suffix) [builtinParameter "actual" ty, builtinParameter "expected" ty]
         | (suffix, ty) <- checkedTypes ]
      ++ [ procedure "check_elapsed" [builtinParameter "t0" (TStruct "TimeVal"), builtinParameter "max" (TStruct "TimeVal")],
           procedure "fail" [],
           procedure "step" [builtinParameter "n" TUSize] ]) []),
   builtinType (Class ResourceClass "TestChecksEntry" [] ["TestChecks"] []),
   builtinResource "test_checks" "TestChecksEntry",
   -- | Only a test class reaches the clock
   builtinType (Interface TestInterface "TestClock" [] [
      procedure "advance" [builtinParameter "dt" (TStruct "TimeVal")]
   ] []),
   builtinType (Class ResourceClass "TestClockEntry" [] ["TestClock"] []),
   builtinResource "test_clock" "TestClockEntry",
   builtinType (Interface SystemInterface "TestScenario" [] [
      procedure "finish" []
   ] []),
   builtinType (Class ResourceClass "TestScenarioEntry" [] ["TestScenario"] []),
   builtinResource "test_scenario" "TestScenarioEntry",
   -- | The interface that a TestPool provides to a case, beside the
   -- Allocator<T> through which the unit reaches it
   builtinType (Interface TestInterface "TestPoolAccess" [] [
      viewProcedure "in_use" [builtinParameter "n" (TReference Mutable TUSize)],
      viewProcedure "max_in_use" [builtinParameter "n" (TReference Mutable TUSize)],
      procedure "fail_allocs" [builtinParameter "n" TUSize]
   ] [])]

  where

    procedure procName params = InterfaceProcedure Mutable procName params [] (buildExpAnn (undocumented Internal) TUnit)
    viewProcedure procName params = InterfaceProcedure Immutable procName params [] (buildExpAnn (undocumented Internal) TUnit)

    checkedTypes = [
      ("u8", TUInt8), ("u16", TUInt16), ("u32", TUInt32), ("u64", TUInt64),
      ("i8", TInt8), ("i16", TInt16), ("i32", TInt32), ("i64", TInt64),
      ("usize", TUSize), ("bool", TBool), ("char", TChar)]

-- | The system emitters and the system resource are declared in every
-- program; the ones the application does not connect are dropped from the
-- architecture.
systemGlobalEnv :: [GlobalDecl SemanticAnn]
systemGlobalEnv = sysInitGlobalEnv ++ systemEntryGlobalEnv ++ sysExceptGlobalEnv

makeInitialGlobalEnv :: Maybe TerminaConfig -> Platform -> [GlobalDecl SemanticAnn] -> Environment
makeInitialGlobalEnv (Just config) plt pltEnvironment =
  let 
    globalEnv = mconcat [
      stdlibGlobalEnv (sysPrintOutputBufferSize config) (sysReadInputBufferSize config),
      -- | The platform specific environment. It should declare its own SystemAPI interface.
      pltEnvironment,
      systemGlobalEnv,
      -- | The test resources only exist in a test
      if isTestBuild config then testGlobalEnv else []]
  in
  makeEnvironment globalEnv plt (isTestBuild config)
makeInitialGlobalEnv Nothing plt pltEnvironment =
  let globalEnv = mconcat [stdlibGlobalEnv defaultSysPrintOutputBufferSize defaultSysReadInputBufferSize, pltEnvironment, systemGlobalEnv]
  in
  makeEnvironment globalEnv plt False

-- | The environment a project starts from, holding the declarations that the
-- language and the platform provide. Their names go into the declared map as
-- well: they are as taken as the ones the project declares, and nothing else
-- fills that map for them.
makeEnvironment :: [GlobalDecl SemanticAnn] -> Platform -> Bool -> Environment
makeEnvironment decls plt isTest = ExprST globalEnv M.empty M.empty S.empty (M.map (getLocation . getAnnotation) globalEnv) plt isTest S.empty

  where

    globalEnv = M.fromList [ (globalDeclName decl, decl) | decl <- decls ]

-- | Take note of the names that the project declares at its top level.
addDeclaredNames :: DeclaredEnv -> Environment -> Environment
addDeclaredNames names env = env { declared = M.union names (declared env) }
