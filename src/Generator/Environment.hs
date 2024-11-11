module Generator.Environment where
import Command.Configuration
import Semantic.AST
import Utils.Annotations
import Semantic.Types
import ControlFlow.Architecture.Types
import qualified Data.Map as M
import ControlFlow.Architecture
import Generator.Platform.RTEMS5NoelSpike
import Generator.Platform.Configuration

getPlatformInitialGlobalEnv :: TerminaConfig -> Platform -> [(Identifier, LocatedElement (GEntry SemanticAnn))]
getPlatformInitialGlobalEnv config RTEMS5NoelSpike = 
    let platformConfig = rtems5_noel_spike . platformFlags $ config in
    [("irq_1", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | enableIrq1 platformConfig] ++
    [("irq_2", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | enableIrq2 platformConfig] ++
    [("irq_3", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | enableIrq3 platformConfig] ++
    [("irq_4", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | enableIrq4 platformConfig] 
getPlatformInitialGlobalEnv _ TestPlatform = []

getPlatformInitialProgram :: TerminaConfig -> Platform -> TerminaProgArch SemanticAnn
getPlatformInitialProgram config RTEMS5NoelSpike = 
    let platformConfig = rtems5_noel_spike . platformFlags $ config in
    emptyTerminaProgArch {
        emitters = M.union (emitters emptyTerminaProgArch) . M.fromList $ 
        [("irq_1", TPInterruptEmittter "irq_1" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | enableIrq1 platformConfig] ++
        [("irq_2", TPInterruptEmittter "irq_2" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | enableIrq2 platformConfig] ++
        [("irq_3", TPInterruptEmittter "irq_3" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | enableIrq3 platformConfig] ++
        [("irq_4", TPInterruptEmittter "irq_4" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | enableIrq4 platformConfig]
    }
getPlatformInitialProgram _ TestPlatform = emptyTerminaProgArch