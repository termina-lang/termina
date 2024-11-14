module Generator.Environment where
import Configuration.Configuration
import Semantic.AST
import Utils.Annotations
import Semantic.Types
import ControlFlow.Architecture.Types
import qualified Data.Map as M
import ControlFlow.Architecture
import Configuration.Platform
import qualified Configuration.Platform.RTEMS5NoelSpike as RTEMS5NoelSpike.Config
import qualified Configuration.Platform.RTEMS5LEON3TSIM as RTEMS5LEON3TSIM.Config

getPlatformInitialGlobalEnv :: TerminaConfig -> Platform -> [(Identifier, LocatedElement (GEntry SemanticAnn))]
getPlatformInitialGlobalEnv config RTEMS5NoelSpike = 
    let platformConfig = rtems5_noel_spike . platformFlags $ config in
    [("irq_1", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5NoelSpike.Config.enableIrq1 platformConfig] ++
    [("irq_2", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5NoelSpike.Config.enableIrq2 platformConfig] ++
    [("irq_3", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5NoelSpike.Config.enableIrq3 platformConfig] ++
    [("irq_4", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5NoelSpike.Config.enableIrq4 platformConfig] 
getPlatformInitialGlobalEnv config RTEMS5LEON3TSIM = 
    let platformConfig = rtems5_leon3_tsim . platformFlags $ config in
    [("irq_1", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3TSIM.Config.enableIrq1 platformConfig] ++
    [("irq_2", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3TSIM.Config.enableIrq2 platformConfig] ++
    [("irq_3", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3TSIM.Config.enableIrq3 platformConfig] ++
    [("irq_4", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3TSIM.Config.enableIrq4 platformConfig] 
getPlatformInitialGlobalEnv _ TestPlatform = []

getPlatformInitialProgram :: TerminaConfig -> Platform -> TerminaProgArch SemanticAnn
getPlatformInitialProgram config RTEMS5NoelSpike = 
    let platformConfig = rtems5_noel_spike . platformFlags $ config
        initialProgArch = emptyTerminaProgArch config in
    initialProgArch {
        emitters = M.union (emitters initialProgArch) . M.fromList $ 
        [("irq_1", TPInterruptEmittter "irq_1" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5NoelSpike.Config.enableIrq1 platformConfig] ++
        [("irq_2", TPInterruptEmittter "irq_2" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5NoelSpike.Config.enableIrq2 platformConfig] ++
        [("irq_3", TPInterruptEmittter "irq_3" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5NoelSpike.Config.enableIrq3 platformConfig] ++
        [("irq_4", TPInterruptEmittter "irq_4" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5NoelSpike.Config.enableIrq4 platformConfig]
    }
getPlatformInitialProgram config RTEMS5LEON3TSIM = 
    let platformConfig = rtems5_leon3_tsim . platformFlags $ config
        initialProgArch = emptyTerminaProgArch config in
    initialProgArch {
        emitters = M.union (emitters initialProgArch) . M.fromList $ 
        [("irq_1", TPInterruptEmittter "irq_1" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3TSIM.Config.enableIrq1 platformConfig] ++
        [("irq_2", TPInterruptEmittter "irq_2" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3TSIM.Config.enableIrq2 platformConfig] ++
        [("irq_3", TPInterruptEmittter "irq_3" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3TSIM.Config.enableIrq3 platformConfig] ++
        [("irq_4", TPInterruptEmittter "irq_4" (LocatedElement (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3TSIM.Config.enableIrq4 platformConfig]
    }
getPlatformInitialProgram config TestPlatform = emptyTerminaProgArch config