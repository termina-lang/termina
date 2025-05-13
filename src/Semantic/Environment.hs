module Semantic.Environment where

import qualified Data.Map as M
import Semantic.AST
import Utils.Annotations
import Semantic.Types
import Configuration.Configuration
import qualified Data.Set as S

----------------------------------------
-- | Global env
-- It has global definitions
type GlobalEnv = M.Map Identifier (LocatedElement (GEntry SemanticAnn))

-- | Local env
-- variables to their type
type LocalEnv = M.Map Identifier (LocatedElement (AccessKind, TerminaType SemanticAnn))

-- | Map with the moved variables
type MovedEnv = M.Map Identifier Location

-- | Environment required to type expression packed into just one type.
data Environment
 = ExprST
 { global :: GlobalEnv
 , local  :: LocalEnv
 , moved  :: MovedEnv
 -- | Set of all the modules that are imported in the current module together with the
 -- | current module.
 , visible :: S.Set QualifiedName
 }

getEntry :: LocatedElement (GEntry SemanticAnn) -> GEntry SemanticAnn
getEntry = element

stdlibGlobalEnv :: [(Identifier, LocatedElement (GEntry SemanticAnn))]
stdlibGlobalEnv =
  [
    ("TimeVal", LocatedElement (GType (Struct "TimeVal" [FieldDefinition "tv_sec" TUInt32 (buildExpAnn Internal TUInt32), FieldDefinition "tv_usec" TUInt32 (buildExpAnn Internal TUInt32)] [])) Internal),
    ("Interrupt", LocatedElement (GType (Class EmitterClass "Interrupt" [] [] [])) Internal),
    ("PeriodicTimer", LocatedElement (GType (Class EmitterClass "PeriodicTimer" [ClassField (FieldDefinition "period" (TStruct "TimeVal") (buildExpAnn Internal (TStruct "TimeVal")))] [] [])) Internal),
    ("SysPrintBase", LocatedElement (GType (Enum "SysPrintBase" [EnumVariant "Decimal" [], EnumVariant "Hexadecimal" []] [])) Internal),
    ("ExceptSource", LocatedElement (GType (Enum "ExceptSource" [EnumVariant "Task" [TUSize], EnumVariant "Handler" [TUSize]] [])) Internal),
    ("Exception", LocatedElement (GType (Enum "Exception" [
      -- | Action failure
      EnumVariant "EActionFailure" [
        -- | Source of the exception
        TEnum "ExceptSource", 
        -- | ID of the source port
        TUSize, 
        -- | Error code returned by the action
        TInt32],
      -- | Message queue full
      EnumVariant "EMsgQueueFull" [
        -- | ID of the message queue
        TUSize],
      EnumVariant "EArrayIndexOutOfBounds" [
        -- | Address of the offending expression
        TUSize,
        -- | Size of the array
        TUSize,
        -- | Offending index
        TUSize],
      EnumVariant "EArraySliceOutOfBounds" [
        -- | Address of the offending expression
        TUSize,
        -- | Size of the array
        TUSize,
        -- | Offending upper index
        TUSize],
      EnumVariant "EArraySliceNegativeRange" [
        -- | Address of the offending expression
        TUSize,
        -- | Lower index
        TUSize,
        -- | Upper index
        TUSize],
      EnumVariant "EArraySliceInvalidRange" [
        -- | Address of the offending expression
        TUSize,
        -- | Expected size of the array
        TUSize,
        -- | Lower index
        TUSize,
        -- | Upper index
        TUSize]
      ] [])) Internal),
    -- | SysTime interface
    ("SysTime", LocatedElement (GType (Interface SystemInterface "SysTime" [] [
      -- | procedure clock_get_uptime (&mut self, current_time : &mut TimeVal)
      InterfaceProcedure "clock_get_uptime" [Parameter "current_time" (TReference Mutable (TStruct "TimeVal"))] [Modifier "unprotected" Nothing] (buildExpAnn Internal TUnit),
      -- | procedure delay_in (&mut self, delay : &TimeVal)
      InterfaceProcedure "delay_in" [Parameter "delay" (TReference Immutable (TStruct "TimeVal"))] [Modifier "unprotected" Nothing] (buildExpAnn Internal TUnit)
    ] [])) Internal),
    -- | SysPrint interface
    ("SysPrint", LocatedElement (GType (Interface SystemInterface "SysPrint" [] [
      -- | procedure clock_get_uptime (&mut self, current_time : &mut TimeVal)
      InterfaceProcedure "print" [Parameter "size" (TConstSubtype TUSize), Parameter "str" (TReference Immutable (TArray TChar (AccessObject (Variable "size" (buildExpAnnObj Internal Immutable (TConstSubtype TUSize))))))] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println" [Parameter "size" (TConstSubtype TUSize), Parameter "str" (TReference Immutable (TArray TChar (AccessObject (Variable "size" (buildExpAnnObj Internal Immutable (TConstSubtype TUSize))))))] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "print_char" [Parameter "value" TChar] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println_char" [Parameter "value" TChar] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "print_u8" [Parameter "value" TUInt8, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println_u8" [Parameter "value" TUInt8, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "print_u16" [Parameter "value" TUInt16, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println_u16" [Parameter "value" TUInt16, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "print_u32" [Parameter "value" TUInt32, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println_u32" [Parameter "value" TUInt32, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "print_u64" [Parameter "value" TUInt64, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println_u64" [Parameter "value" TUInt64, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "print_i8" [Parameter "value" TInt8, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println_i8" [Parameter "value" TInt8, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "print_i16" [Parameter "value" TInt16, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println_i16" [Parameter "value" TInt16, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "print_i32" [Parameter "value" TInt32, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println_i32" [Parameter "value" TInt32, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "print_i64" [Parameter "value" TInt64, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println_i64" [Parameter "value" TInt64, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "print_usize" [Parameter "value" TUSize, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit),
      InterfaceProcedure "println_usize" [Parameter "value" TUSize, Parameter "base" (TEnum "SysPrintBase")] [] (buildExpAnn Internal TUnit)
    ] [])) Internal),
    -- | SystemAPI interface. This interface extends all the system interfaces.
    -- We are currently assuming that there is a common implementation of the SystemAPI. In the future
    -- this approach could allow to have different implementations of the SystemAPI depending on the
    -- target platform.
    ("SystemAPI", LocatedElement (GType (Interface SystemInterface "SystemAPI" ["SysTime", "SysPrint"] [] [])) Internal)
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

makeInitialGlobalEnv :: Maybe TerminaConfig -> [(Identifier, LocatedElement (GEntry SemanticAnn))] -> Environment
makeInitialGlobalEnv (Just config) pltEnvironment = 
  let 
    globalEnv = mconcat [
      stdlibGlobalEnv,
      [
        env | enableSystemInit config, env <- sysInitGlobalEnv
      ],
      [
        env | enableSystemPort config, env <- systemEntryGlobalEnv
      ],
      [
        env | enableSystemExcept config, env <- sysExceptGlobalEnv
      ], pltEnvironment]
  in
  ExprST (M.fromList globalEnv) M.empty M.empty S.empty
makeInitialGlobalEnv Nothing pltEnvironment = 
  let globalEnv = mconcat [stdlibGlobalEnv, pltEnvironment]
  in
  ExprST (M.fromList globalEnv) M.empty M.empty S.empty
