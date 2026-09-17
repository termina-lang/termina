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
    addName element'@(GlobalDeclaration glb) = M.insert (getGlobalIdentifier glb) (getAnnotation element')
    addName element'@(TypeDefinition tydef _) = M.insert (getTypeIdentifier tydef) (getAnnotation element')

stdlibGlobalEnv :: Integer -> Integer -> [(Identifier, LocatedElement (GEntry SemanticAnn))]
stdlibGlobalEnv outBufSize inBufSize =
  [
    -- | Floating-point bit reinterpretation. These are Prelude functions whose
    -- body is provided by the OSAL (memcpy-based static inline). They lower to a
    -- direct C call of the same name.
    ("f32_to_bits", LocatedElement (GFun (FunctionSeman [Parameter "value" TFloat32] TUInt32)) Internal),
    ("f32_from_bits", LocatedElement (GFun (FunctionSeman [Parameter "bits" TUInt32] TFloat32)) Internal),
    ("f64_to_bits", LocatedElement (GFun (FunctionSeman [Parameter "value" TFloat64] TUInt64)) Internal),
    ("f64_from_bits", LocatedElement (GFun (FunctionSeman [Parameter "bits" TUInt64] TFloat64)) Internal),
    ("TimeVal", LocatedElement (GType (Struct "TimeVal" [FieldDefinition "tv_sec" TUInt32 (buildExpAnn Internal TUInt32), FieldDefinition "tv_usec" TUInt32 (buildExpAnn Internal TUInt32)] [])) Internal),
    ("Interrupt", LocatedElement (GType (Class EmitterClass "Interrupt" [] [] [])) Internal),
    ("PeriodicTimer", LocatedElement (GType (Class EmitterClass "PeriodicTimer" [ClassField (FieldDefinition "period" (TStruct "TimeVal") (buildExpAnn Internal (TStruct "TimeVal")))] [] [])) Internal),
    ("SysPrintBase", LocatedElement (GType (Enum "SysPrintBase" [EnumVariant "Decimal" [], EnumVariant "Hexadecimal" []] [])) Internal),
    ("ExceptSource", LocatedElement (GType (Enum "ExceptSource" [EnumVariant "Task" [TUSize], EnumVariant "Handler" [TUSize]] [])) Internal),
    ("MutexProtocol", LocatedElement (GType (Enum "MutexProtocol" [EnumVariant "Ceiling" [TUInt8]] [])) Internal),
    ("Exception", LocatedElement (GType (Enum "Exception" [
      -- | Action failure
      EnumVariant "EActionFailure" [
        -- | Source of the exception
        TEnum "ExceptSource", 
        -- | ID of the source port
        TUSize, 
        -- | Error code returned by the action
        TInt32],
      -- | Message queue full
      EnumVariant "EMsgQueueFull" [
        -- | ID of the message queue
        TUSize],
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
        TUSize]
      ] [])) Internal),
    -- | SysTime interface
    ("SysTime", LocatedElement (GType (Interface SystemInterface "SysTime" [] [
      -- | procedure clock_get_uptime (&mut self, current_time : &mut TimeVal)
      InterfaceProcedure Immutable "clock_get_uptime" [Parameter "current_time" (TReference Mutable (TStruct "TimeVal"))] [] (buildExpAnn Internal TUnit),
      -- | procedure delay_in (&mut self, delay : &TimeVal)
      InterfaceProcedure Mutable "delay_in" [Parameter "delay" (TReference Immutable (TStruct "TimeVal"))] [] (buildExpAnn Internal TUnit)
    ] [])) Internal),
    -- | SysPrint interface
    ("SysPrint", LocatedElement (GType (Interface SystemInterface "SysPrint" [] [
      -- | procedure clock_get_uptime (&mut self, current_time : &mut TimeVal)
      InterfaceProcedure Mutable "print" [Parameter "str" (TReference Immutable (TArray TChar (Constant (I (TInteger outBufSize DecRepr) (Just (TConstSubtype TUSize))) (buildExpAnn Internal (TConstSubtype TUSize)))))] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println" [Parameter "str" (TReference Immutable (TArray TChar (Constant (I (TInteger outBufSize DecRepr) (Just (TConstSubtype TUSize))) (buildExpAnn Internal (TConstSubtype TUSize)))))] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_char" [Parameter "value" TChar] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_char" [Parameter "value" TChar] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_u8" [Parameter "value" TUInt8, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_u8" [Parameter "value" TUInt8, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_u16" [Parameter "value" TUInt16, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_u16" [Parameter "value" TUInt16, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_u32" [Parameter "value" TUInt32, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_u32" [Parameter "value" TUInt32, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_u64" [Parameter "value" TUInt64, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_u64" [Parameter "value" TUInt64, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_i8" [Parameter "value" TInt8, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_i8" [Parameter "value" TInt8, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_i16" [Parameter "value" TInt16, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_i16" [Parameter "value" TInt16, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_i32" [Parameter "value" TInt32, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_i32" [Parameter "value" TInt32, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_i64" [Parameter "value" TInt64, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_i64" [Parameter "value" TInt64, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_usize" [Parameter "value" TUSize, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_usize" [Parameter "value" TUSize, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_f32" [Parameter "value" TFloat32] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_f32" [Parameter "value" TFloat32] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "print_f64" [Parameter "value" TFloat64] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure Mutable "println_f64" [Parameter "value" TFloat64] [] (buildExpAnn Internal TUnit)
    ] [])) Internal),
    ("SysGetChar", LocatedElement (GType (Interface SystemInterface "SysGetChar" [] [
      InterfaceProcedure Mutable "read" [ 
          Parameter "str" (TReference Mutable (TArray TChar (Constant (I (TInteger inBufSize DecRepr) (Just (TConstSubtype TUSize))) (buildExpAnn Internal (TConstSubtype TUSize))))),
          Parameter "read_bytes" (TReference Mutable TUSize)] [] (buildExpAnn Internal TUnit)
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

makeInitialGlobalEnv :: Maybe TerminaConfig -> Platform -> [(Identifier, LocatedElement (GEntry SemanticAnn))] -> Environment
makeInitialGlobalEnv (Just config) plt pltEnvironment =
  let 
    globalEnv = mconcat [
      stdlibGlobalEnv (sysPrintOutputBufferSize config) (sysReadInputBufferSize config),
      -- | The platform specific environment. It should declare its own SystemAPI interface.
      pltEnvironment,
      [
        env | enableSystemInit config, env <- sysInitGlobalEnv
      ],
      [
        env | enableSystemPort config, env <- systemEntryGlobalEnv
      ],
      [
        env | enableSystemExcept config, env <- sysExceptGlobalEnv
      ]]
  in
  makeEnvironment (M.fromList globalEnv) plt
makeInitialGlobalEnv Nothing plt pltEnvironment =
  let globalEnv = mconcat [stdlibGlobalEnv defaultSysPrintOutputBufferSize defaultSysReadInputBufferSize, pltEnvironment]
  in
  makeEnvironment (M.fromList globalEnv) plt

-- | The environment a project starts from, holding the entries that the
-- language and the platform provide. Their names go into the declared map as
-- well: they are as taken as the ones the project declares, and nothing else
-- fills that map for them.
makeEnvironment :: GlobalEnv -> Platform -> Environment
makeEnvironment globalEnv = ExprST globalEnv M.empty M.empty S.empty (M.map location globalEnv)

-- | Take note of the names that the project declares at its top level.
addDeclaredNames :: DeclaredEnv -> Environment -> Environment
addDeclaredNames names env = env { declared = M.union names (declared env) }
