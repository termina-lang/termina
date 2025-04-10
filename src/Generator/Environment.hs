module Generator.Environment where
import Configuration.Configuration
import Semantic.AST
import Utils.Annotations
import Semantic.Types
import ControlFlow.Architecture.Types
import qualified Data.Map as M
import ControlFlow.Architecture
import Configuration.Platform
import qualified Configuration.Platform.RTEMS5LEON3QEMU as RTEMS5LEON3QEMU.Config

getPlatformInitialGlobalEnv :: TerminaConfig -> Platform -> [(Identifier, LocatedElement (GEntry SemanticAnn))]
getPlatformInitialGlobalEnv _ POSIXGCC = []
getPlatformInitialGlobalEnv config RTEMS5LEON3QEMU = 
    let platformConfig = rtems5_leon3_qemu . platformFlags $ config in
    [("irq_0", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq0 platformConfig] ++
    [("irq_1", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq1 platformConfig] ++
    [("irq_2", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq2 platformConfig] ++
    [("irq_3", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq3 platformConfig] ++
    [("irq_4", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq4 platformConfig] ++
    [("irq_5", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq5 platformConfig] ++
    [("irq_6", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq6 platformConfig] ++
    [("irq_7", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq7 platformConfig] ++
    [("irq_8", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq8 platformConfig] ++
    [("irq_9", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq9 platformConfig] ++
    [("irq_10", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq10 platformConfig] ++
    [("irq_11", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq11 platformConfig] ++
    [("irq_12", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq12 platformConfig] ++
    [("irq_13", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq13 platformConfig] ++
    [("irq_14", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq14 platformConfig] ++
    [("irq_15", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq15 platformConfig]
getPlatformInitialGlobalEnv _ TestPlatform = []

getPlatformInitialProgram :: TerminaConfig -> Platform -> TerminaProgArch SemanticAnn
getPlatformInitialProgram config POSIXGCC = emptyTerminaProgArch config
getPlatformInitialProgram config RTEMS5LEON3QEMU = 
    let platformConfig = rtems5_leon3_qemu . platformFlags $ config
        initialProgArch = emptyTerminaProgArch config in
    initialProgArch {
        emitters = M.union (emitters initialProgArch) . M.fromList $ 
        [("irq_0", TPInterruptEmittter "irq_1" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq0 platformConfig] ++
        [("irq_1", TPInterruptEmittter "irq_1" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq1 platformConfig] ++
        [("irq_2", TPInterruptEmittter "irq_2" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq2 platformConfig] ++
        [("irq_3", TPInterruptEmittter "irq_3" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq3 platformConfig] ++
        [("irq_4", TPInterruptEmittter "irq_4" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq4 platformConfig] ++
        [("irq_5", TPInterruptEmittter "irq_1" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq5 platformConfig] ++
        [("irq_6", TPInterruptEmittter "irq_2" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq6 platformConfig] ++
        [("irq_7", TPInterruptEmittter "irq_3" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq7 platformConfig] ++
        [("irq_8", TPInterruptEmittter "irq_4" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq8 platformConfig] ++
        [("irq_9", TPInterruptEmittter "irq_4" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq9 platformConfig] ++
        [("irq_10", TPInterruptEmittter "irq_4" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq10 platformConfig] ++
        [("irq_11", TPInterruptEmittter "irq_4" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq11 platformConfig] ++
        [("irq_12", TPInterruptEmittter "irq_4" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq12 platformConfig] ++
        [("irq_13", TPInterruptEmittter "irq_4" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq13 platformConfig] ++
        [("irq_14", TPInterruptEmittter "irq_4" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq14 platformConfig] ++
        [("irq_15", TPInterruptEmittter "irq_4" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq15 platformConfig]
    }
getPlatformInitialProgram config TestPlatform = emptyTerminaProgArch config

getPlatformInterruptMap :: Platform -> M.Map Identifier Integer
getPlatformInterruptMap POSIXGCC = M.empty
getPlatformInterruptMap RTEMS5LEON3QEMU =
    M.fromList [("irq_1", 1), ("irq_2", 2), ("irq_3", 3), ("irq_4", 4), 
                ("irq_5", 5), ("irq_6", 6), ("irq_7", 7), ("irq_8", 8), 
                ("irq_9", 9), ("irq_10", 10), ("irq_11", 11), ("irq_12", 12), 
                ("irq_13", 13), ("irq_14", 14), ("irq_15", 15)]
getPlatformInterruptMap TestPlatform = M.empty
