module Semantic.Environment where

import qualified Data.Map.Strict as M
import Semantic.AST
import Core.Utils (getGlobalIdentifier, getTypeIdentifier)
import Utils.Annotations
import Semantic.Types
import Configuration.Configuration
import Configuration.Platform (Platform)
import qualified Data.Set as S

----------------------------------------
-- | Global env
-- It has global definitions
type GlobalEnv = M.Map Identifier (LocatedElement (GEntry SemanticAnn))

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
 }

getEntry :: LocatedElement (GEntry SemanticAnn) -> GEntry SemanticAnn
getEntry = element

-- | Identifiers that a program declares at the top level, each with the
-- location of its declaration. The constexpr declarations are in, even though
-- the transpiler folds them and they reach no C identifier: an object that took
-- the name of a constexpr would lose every read of it, because the type checker
-- substitutes the folded value before it looks the name up among the local
-- objects.
declaredNames :: [AnnASTElement' ty expr blk Location] -> DeclaredEnv
declaredNames = foldr addName M.empty

  where

    addName :: AnnASTElement' ty expr blk Location -> DeclaredEnv -> DeclaredEnv
    addName element'@(Function ident _ _ _ _ _) = M.insert ident (getAnnotation element')
    addName element'@(ExternFunction ident _ _ _ _ _) = M.insert ident (getAnnotation element')
    addName element'@(GlobalDeclaration glb) = M.insert (getGlobalIdentifier glb) (getAnnotation element')
    addName element'@(TypeDefinition tydef _) = M.insert (getTypeIdentifier tydef) (getAnnotation element')

-- | A parameter of one of the interfaces the environment declares by hand,
-- which no source file writes and which therefore has no position.
builtinParameter :: Identifier -> TerminaType SemanticAnn -> Parameter SemanticAnn
builtinParameter ident ty = Parameter ident ty (buildExpAnn Internal ty)

stdlibGlobalEnv :: Integer -> Integer -> [(Identifier, LocatedElement (GEntry SemanticAnn))]
stdlibGlobalEnv outBufSize inBufSize =
  [
    -- | Floating-point bit reinterpretation. These are Prelude functions whose
    -- body is provided by the OSAL (memcpy-based static inline). They lower to a
    -- direct C call of the same name.
    ("f32_to_bits", LocatedElement (GFun (FunctionSeman [builtinParameter "value" TFloat32] TUInt32 Returns)) Internal),
    ("f32_from_bits", LocatedElement (GFun (FunctionSeman [builtinParameter "bits" TUInt32] TFloat32 Returns)) Internal),
    ("f64_to_bits", LocatedElement (GFun (FunctionSeman [builtinParameter "value" TFloat64] TUInt64 Returns)) Internal),
    ("f64_from_bits", LocatedElement (GFun (FunctionSeman [builtinParameter "bits" TUInt64] TFloat64 Returns)) Internal),
    ("TimeVal", LocatedElement (GType (Struct "TimeVal" [FieldDefinition "tv_sec" TUInt32 (buildExpAnn Internal TUInt32), FieldDefinition "tv_usec" TUInt32 (buildExpAnn Internal TUInt32)] [])) Internal),
    ("Interrupt", LocatedElement (GType (Class EmitterClass "Interrupt" [] [] [])) Internal),
    ("PeriodicTimer", LocatedElement (GType (Class EmitterClass "PeriodicTimer" [ClassField (FieldDefinition "period" (TStruct "TimeVal") (buildExpAnn Internal (TStruct "TimeVal")))] [] [])) Internal),
    ("SysPrintBase", LocatedElement (GType (Enum "SysPrintBase" [EnumVariant "Decimal" [], EnumVariant "Hexadecimal" []] [])) Internal),
    ("MutexProtocol", LocatedElement (GType (Enum "MutexProtocol" [EnumVariant "Ceiling" [TUInt32]] [])) Internal),
    ("Exception", LocatedElement (GType (Enum "Exception" [
      -- | Error sending a message to a message queue
      EnumVariant "EMsgQueueSendError" [
        -- | ID of the message queue
        TUSize,
        -- | Error code
        TInt32],
      -- | Error receiving a message from a message queue
      EnumVariant "EMsgQueueRecvError" [
        -- | ID of the message queue
        TUSize,
        -- | Error code
        TInt32],
      EnumVariant "EArrayIndexOutOfBounds" [
        -- | Address of the offending expression
        TUSize,
        -- | Size of the array
        TUSize,
        -- | Offending index
        TUSize],
      EnumVariant "EArraySliceOutOfBounds" [
        -- | Address of the offending expression
        TUSize,
        -- | Size of the array
        TUSize,
        -- | Offending upper index
        TUSize],
      EnumVariant "EArraySliceNegativeRange" [
        -- | Address of the offending expression
        TUSize,
        -- | Lower index
        TUSize,
        -- | Upper index
        TUSize],
      EnumVariant "EArraySliceInvalidRange" [
        -- | Address of the offending expression
        TUSize,
        -- | Expected size of the array
        TUSize,
        -- | Lower index
        TUSize,
        -- | Upper index
        TUSize],
      EnumVariant "EShiftAmountOutOfBounds" [
        -- | Address of the offending expression
        TUSize,
        -- | Bit width of the shifted type
        TUSize,
        -- | Offending shift amount
        TUSize],
      EnumVariant "EArithmeticOverflow" [
        -- | Address of the offending expression
        TUSize],
      EnumVariant "EDivisionByZero" [
        -- | Address of the offending expression
        TUSize],
      EnumVariant "ERuntimeFailure" [
        -- | Operation of the runtime that failed
        TUInt32,
        -- | Error code of the operating system
        TInt32],
      -- | Fault of the processor. The fields are numbers of the platform,
      -- decoded with the platform at hand.
      EnumVariant "EProcessorFault" [
        -- | Exception number of the architecture
        TUInt32,
        -- | Fault status register, or 0 when there is none
        TUInt32,
        -- | Address of the instruction that failed, or 0 when unknown
        TUSize,
        -- | Faulting address, or 0 when the architecture gives none
        TUSize],
      -- | Fatal error raised by the operating system itself
      EnumVariant "EKernelFailure" [
        -- | Source of the fatal error, as the operating system numbers it
        TUInt32,
        -- | Code of the fatal error, which may be an address
        TUSize],
      -- | Stack overflow detected by the operating system
      EnumVariant "EStackOverflow" [
        -- | Identifier of the task, or the invalid identifier when the stack
        -- is not of a task of the application
        TUSize]
      ] [])) Internal),
    -- | SysTime interface
    ("SysTime", LocatedElement (GType (Interface SystemInterface "SysTime" [] [
      -- | procedure clock_get_uptime (&mut self, current_time : &mut TimeVal)
      InterfaceProcedure Immutable "clock_get_uptime" [builtinParameter "current_time" (TReference Mutable (TStruct "TimeVal"))] [] (buildExpAnn Internal TUnit)
    ] [])) Internal),
    -- | SysPrint interface
    ("SysPrint", LocatedElement (GType (Interface SystemInterface "SysPrint" [] [
      -- | procedure clock_get_uptime (&mut self, current_time : &mut TimeVal)
      InterfaceProcedure Mutable "print" [builtinParameter "str" (TReference Immutable (TArray TChar (Constant (I (TInteger outBufSize DecRepr) (Just (TConstSubtype TUSize))) (buildExpAnn Internal (TConstSubtype TUSize)))))] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println" [builtinParameter "str" (TReference Immutable (TArray TChar (Constant (I (TInteger outBufSize DecRepr) (Just (TConstSubtype TUSize))) (buildExpAnn Internal (TConstSubtype TUSize)))))] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_char" [builtinParameter "value" TChar] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_char" [builtinParameter "value" TChar] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_u8" [builtinParameter "value" TUInt8, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_u8" [builtinParameter "value" TUInt8, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_u16" [builtinParameter "value" TUInt16, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_u16" [builtinParameter "value" TUInt16, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_u32" [builtinParameter "value" TUInt32, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_u32" [builtinParameter "value" TUInt32, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_u64" [builtinParameter "value" TUInt64, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_u64" [builtinParameter "value" TUInt64, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_i8" [builtinParameter "value" TInt8, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_i8" [builtinParameter "value" TInt8, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_i16" [builtinParameter "value" TInt16, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_i16" [builtinParameter "value" TInt16, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_i32" [builtinParameter "value" TInt32, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_i32" [builtinParameter "value" TInt32, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_i64" [builtinParameter "value" TInt64, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_i64" [builtinParameter "value" TInt64, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_usize" [builtinParameter "value" TUSize, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_usize" [builtinParameter "value" TUSize, builtinParameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_f32" [builtinParameter "value" TFloat32] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_f32" [builtinParameter "value" TFloat32] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_f64" [builtinParameter "value" TFloat64] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_f64" [builtinParameter "value" TFloat64] [] (buildExpAnn Internal TUnit)
    ] [])) Internal),
    ("SysGetChar", LocatedElement (GType (Interface SystemInterface "SysGetChar" [] [
      InterfaceProcedure Mutable "read" [ 
          builtinParameter "str" (TReference Mutable (TArray TChar (Constant (I (TInteger inBufSize DecRepr) (Just (TConstSubtype TUSize))) (buildExpAnn Internal (TConstSubtype TUSize))))),
          builtinParameter "read_bytes" (TReference Mutable TUSize)] [] (buildExpAnn Internal TUnit)
    ] [])) Internal)
  ]

sysInitGlobalEnv :: [(Identifier, LocatedElement (GEntry SemanticAnn))]
sysInitGlobalEnv =
  [("SystemInit", LocatedElement (GType (Class EmitterClass "SystemInit" [] [] [])) Internal),
   ("system_init", LocatedElement (GGlob (TGlobal EmitterClass "SystemInit")) Internal)]
  
sysExceptGlobalEnv :: [(Identifier, LocatedElement (GEntry SemanticAnn))]
sysExceptGlobalEnv =
  [("SystemExcept", LocatedElement (GType (Class EmitterClass "SystemExcept" [] [] [])) Internal),
   ("system_except", LocatedElement (GGlob (TGlobal EmitterClass "SystemExcept")) Internal)]

systemEntryGlobalEnv :: [(Identifier, LocatedElement (GEntry SemanticAnn))]
systemEntryGlobalEnv =
  [("SystemEntry", LocatedElement (GType (Class ResourceClass "SystemEntry" [] ["SystemAPI"] [])) Internal),
    ("system_entry", LocatedElement (GGlob (TGlobal ResourceClass "SystemEntry")) Internal)
  ]

-- | The API that the cases and the scenario of a test reach through
-- test_entry. A check records its failure and returns, and the first argument
-- of a check_eq is the value obtained and the second the one expected.
testEntryGlobalEnv :: [(Identifier, LocatedElement (GEntry SemanticAnn))]
testEntryGlobalEnv =
  [("TestAPI", LocatedElement (GType (Interface SystemInterface "TestAPI" [] (
      [ procedure "check" [builtinParameter "cond" TBool] ]
      ++ [ procedure ("check_eq_" ++ suffix) [builtinParameter "actual" ty, builtinParameter "expected" ty]
         | (suffix, ty) <- checkedTypes ]
      ++ [ procedure "check_elapsed" [builtinParameter "t0" (TStruct "TimeVal"), builtinParameter "max" (TStruct "TimeVal")],
           procedure "fail" [],
           procedure "step" [builtinParameter "n" TUInt32],
           procedure "advance" [builtinParameter "dt" (TStruct "TimeVal")],
           procedure "finish" [] ]) [])) Internal),
   ("TestEntry", LocatedElement (GType (Class ResourceClass "TestEntry" [] ["TestAPI"] [])) Internal),
   ("test_entry", LocatedElement (GGlob (TGlobal ResourceClass "TestEntry")) Internal),
   -- | The interface that a TestPool provides to a case, beside the
   -- Allocator<T> through which the unit reaches it
   ("TestPoolAccess", LocatedElement (GType (Interface SystemInterface "TestPoolAccess" [] [
      viewProcedure "in_use" [builtinParameter "n" (TReference Mutable TUSize)],
      viewProcedure "max_in_use" [builtinParameter "n" (TReference Mutable TUSize)],
      procedure "fail_allocs" [builtinParameter "n" TUSize]
   ] [])) Internal)]

  where

    procedure procName params = InterfaceProcedure Mutable procName params [] (buildExpAnn Internal TUnit)
    viewProcedure procName params = InterfaceProcedure Immutable procName params [] (buildExpAnn Internal TUnit)

    checkedTypes = [
      ("u8", TUInt8), ("u16", TUInt16), ("u32", TUInt32), ("u64", TUInt64),
      ("i8", TInt8), ("i16", TInt16), ("i32", TInt32), ("i64", TInt64),
      ("usize", TUSize), ("bool", TBool), ("char", TChar)]

-- | The system emitters and the system resource are declared in every
-- program; the ones the application does not connect are dropped from the
-- architecture.
systemGlobalEnv :: [(Identifier, LocatedElement (GEntry SemanticAnn))]
systemGlobalEnv = sysInitGlobalEnv ++ systemEntryGlobalEnv ++ sysExceptGlobalEnv

makeInitialGlobalEnv :: Maybe TerminaConfig -> Platform -> [(Identifier, LocatedElement (GEntry SemanticAnn))] -> Environment
makeInitialGlobalEnv (Just config) plt pltEnvironment =
  let 
    globalEnv = mconcat [
      stdlibGlobalEnv (sysPrintOutputBufferSize config) (sysReadInputBufferSize config),
      -- | The platform specific environment. It should declare its own SystemAPI interface.
      pltEnvironment,
      systemGlobalEnv,
      -- | test_entry only exists in a test
      if isTestBuild config then testEntryGlobalEnv else []]
  in
  makeEnvironment (M.fromList globalEnv) plt (isTestBuild config)
makeInitialGlobalEnv Nothing plt pltEnvironment =
  let globalEnv = mconcat [stdlibGlobalEnv defaultSysPrintOutputBufferSize defaultSysReadInputBufferSize, pltEnvironment, systemGlobalEnv]
  in
  makeEnvironment (M.fromList globalEnv) plt False

-- | The environment a project starts from, holding the entries that the
-- language and the platform provide. Their names go into the declared map as
-- well: they are as taken as the ones the project declares, and nothing else
-- fills that map for them.
makeEnvironment :: GlobalEnv -> Platform -> Bool -> Environment
makeEnvironment globalEnv = ExprST globalEnv M.empty M.empty S.empty (M.map location globalEnv)

-- | Take note of the names that the project declares at its top level.
addDeclaredNames :: DeclaredEnv -> Environment -> Environment
addDeclaredNames names env = env { declared = M.union names (declared env) }
