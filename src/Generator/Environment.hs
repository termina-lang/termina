module Generator.Environment where
import Command.Configuration
import Generator.Platform
import Semantic.AST
import Utils.Annotations
import Semantic.Types
import ControlFlow.Architecture.Types
import qualified Data.Map as M
import ControlFlow.Architecture
import Generator.Platform.RTEMS5NoelSpike

getPlatformInitialGlobalEnv :: TerminaConfig -> Platform -> [(Identifier, Located (GEntry SemanticAnn))]
getPlatformInitialGlobalEnv config RTEMS5NoelSpike = 
    let platformConfig = rtems5_noel_spike . platformFlags $ config in
    [("irq_1", Located (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | enableIrq1 platformConfig] ++
    [("irq_2", Located (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | enableIrq2 platformConfig] ++
    [("irq_3", Located (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | enableIrq3 platformConfig] ++
    [("irq_4", Located (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | enableIrq4 platformConfig] 
getPlatformInitialGlobalEnv _ TestPlatform = []

getPlatformInitialProgram :: TerminaConfig -> Platform -> TerminaProgArch SemanticAnn
getPlatformInitialProgram config RTEMS5NoelSpike = 
    let platformConfig = rtems5_noel_spike . platformFlags $ config in
    emptyTerminaProgArch {
        emitters = M.union (emitters emptyTerminaProgArch) . M.fromList $ 
        [("irq_1", TPInterruptEmittter "irq_1" (Located (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | enableIrq1 platformConfig] ++
        [("irq_2", TPInterruptEmittter "irq_2" (Located (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | enableIrq2 platformConfig] ++
        [("irq_3", TPInterruptEmittter "irq_3" (Located (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | enableIrq3 platformConfig] ++
        [("irq_4", TPInterruptEmittter "irq_4" (Located (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | enableIrq4 platformConfig]
    }
getPlatformInitialProgram _ TestPlatform = emptyTerminaProgArch