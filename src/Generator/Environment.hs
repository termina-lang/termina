module Generator.Environment where
import Configuration.Configuration
import Semantic.AST
import Utils.Annotations
import Semantic.Types
import ControlFlow.Architecture.Types
import qualified Data.Map.Strict as M
import ControlFlow.Architecture
import Configuration.Platform
import qualified Configuration.Platform.RTEMS5LEON3QEMU as RTEMS5LEON3QEMU.Config
import qualified Configuration.Platform.POSIXGCC as POSIXGCC.Config
import qualified Configuration.Platform.FreeRTOS10STM32L432XX as FreeRTOS10STM32L432XX.Config

getPlatformInitialGlobalEnv :: TerminaConfig -> Platform -> [(Identifier, LocatedElement (GEntry SemanticAnn))]
getPlatformInitialGlobalEnv config POSIXGCC =
    let platformConfig = posix_gcc . platformFlags $ config in
    [("kbd_irq", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | POSIXGCC.Config.enableKbdIrq platformConfig] ++
    -- | SystemAPI interface. This interface extends all the system interfaces.
    -- Each target platform should declare its own SystemAPI interface.
    [("SystemAPI", LocatedElement (GType (Interface SystemInterface "SystemAPI" ["SysTime", "SysPrint", "SysGetChar"] [] [])) Internal)]
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
    [("irq_15", LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | RTEMS5LEON3QEMU.Config.enableIrq15 platformConfig] ++
    -- | SystemAPI interface. This interface extends all the system interfaces.
    -- Each target platform should declare its own SystemAPI interface.
    [("SystemAPI", LocatedElement (GType (Interface SystemInterface "SystemAPI" ["SysTime", "SysPrint"] [] [])) Internal)]
getPlatformInitialGlobalEnv config FreeRTOS10STM32L432XX =
    let platformConfig = freertos10_stm32l432xx . platformFlags $ config in
    [("wwdg_irq",        LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableWwdgIrq          platformConfig] ++
    [("pvd_irq",          LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enablePvdIrq           platformConfig] ++
    [("tamp_stamp_irq",   LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableTampStampIrq     platformConfig] ++
    [("rtc_wkup_irq",     LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableRtcWkupIrq       platformConfig] ++
    [("flash_irq",        LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableFlashIrq          platformConfig] ++
    [("rcc_irq",          LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableRccIrq            platformConfig] ++
    [("exti0_irq",        LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableExti0Irq          platformConfig] ++
    [("exti1_irq",        LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableExti1Irq          platformConfig] ++
    [("exti2_irq",        LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableExti2Irq          platformConfig] ++
    [("exti3_irq",        LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableExti3Irq          platformConfig] ++
    [("exti4_irq",        LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableExti4Irq          platformConfig] ++
    [("dma1_channel1_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma1Channel1Irq  platformConfig] ++
    [("dma1_channel2_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma1Channel2Irq  platformConfig] ++
    [("dma1_channel3_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma1Channel3Irq  platformConfig] ++
    [("dma1_channel4_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma1Channel4Irq  platformConfig] ++
    [("dma1_channel5_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma1Channel5Irq  platformConfig] ++
    [("dma1_channel6_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma1Channel6Irq  platformConfig] ++
    [("dma1_channel7_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma1Channel7Irq  platformConfig] ++
    [("adc1_irq",         LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableAdc1Irq           platformConfig] ++
    [("can1_tx_irq",      LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableCan1TxIrq         platformConfig] ++
    [("can1_rx0_irq",     LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableCan1Rx0Irq        platformConfig] ++
    [("can1_rx1_irq",     LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableCan1Rx1Irq        platformConfig] ++
    [("can1_sce_irq",     LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableCan1SceIrq        platformConfig] ++
    [("exti9_5_irq",      LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableExti9_5Irq        platformConfig] ++
    [("tim1_brk_tim15_irq",    LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableTim1BrkTim15Irq    platformConfig] ++
    [("tim1_up_tim16_irq",     LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableTim1UpTim16Irq     platformConfig] ++
    [("tim1_trg_com_tim17_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableTim1TrgComTim17Irq platformConfig] ++
    [("tim1_cc_irq",      LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableTim1CcIrq         platformConfig] ++
    [("tim2_irq",         LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableTim2Irq           platformConfig] ++
    [("i2c1_ev_irq",      LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableI2c1EvIrq         platformConfig] ++
    [("i2c1_er_irq",      LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableI2c1ErIrq         platformConfig] ++
    [("spi1_irq",         LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableSpi1Irq           platformConfig] ++
    [("usart1_irq",       LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableUsart1Irq         platformConfig] ++
    [("usart2_irq",       LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableUsart2Irq         platformConfig] ++
    [("exti15_10_irq",    LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableExti15_10Irq      platformConfig] ++
    [("rtc_alarm_irq",    LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableRtcAlarmIrq      platformConfig] ++
    [("spi3_irq",         LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableSpi3Irq           platformConfig] ++
    [("tim6_dac_irq",     LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableTim6DacIrq       platformConfig] ++
    [("tim7_irq",         LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableTim7Irq           platformConfig] ++
    [("dma2_channel1_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma2Channel1Irq  platformConfig] ++
    [("dma2_channel2_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma2Channel2Irq  platformConfig] ++
    [("dma2_channel3_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma2Channel3Irq  platformConfig] ++
    [("dma2_channel4_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma2Channel4Irq  platformConfig] ++
    [("dma2_channel5_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma2Channel5Irq  platformConfig] ++
    [("comp_irq",         LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableCompIrq           platformConfig] ++
    [("lptim1_irq",       LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableLptim1Irq         platformConfig] ++
    [("lptim2_irq",       LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableLptim2Irq         platformConfig] ++
    [("usb_irq",          LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableUsbIrq            platformConfig] ++
    [("dma2_channel6_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma2Channel6Irq  platformConfig] ++
    [("dma2_channel7_irq",LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableDma2Channel7Irq  platformConfig] ++
    [("lpuart1_irq",      LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableLpuart1Irq        platformConfig] ++
    [("quad_spi_irq",     LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableQuadSpiIrq        platformConfig] ++
    [("i2c3_ev_irq",      LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableI2c3EvIrq         platformConfig] ++
    [("i2c3_er_irq",      LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableI2c3ErIrq         platformConfig] ++
    [("sai1_irq",         LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableSai1Irq           platformConfig] ++
    [("swpmi1_irq",       LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableSwpmi1Irq         platformConfig] ++
    [("tsc_irq",          LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableTscIrq            platformConfig] ++
    [("rng_irq",          LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableRngIrq            platformConfig] ++
    [("fpu_irq",          LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableFpuIrq            platformConfig] ++
    [("crs_irq",          LocatedElement (GGlob (TGlobal EmitterClass "Interrupt")) Internal) | FreeRTOS10STM32L432XX.Config.enableCrsIrq            platformConfig] ++
    -- | SystemAPI interface.
    [("SystemAPI", LocatedElement (GType (Interface SystemInterface "SystemAPI" ["SysTime"] [] [])) Internal)]
getPlatformInitialGlobalEnv _ TestPlatform = []

getPlatformInitialProgram :: TerminaConfig -> Platform -> TerminaProgArch SemanticAnn
getPlatformInitialProgram config POSIXGCC = 
    let platformConfig = posix_gcc . platformFlags $ config
        initialProgArch = emptyTerminaProgArch config in
    initialProgArch {
        emitters = M.union (emitters initialProgArch) . M.fromList $
        [("kbd_irq", TPInterruptEmitter "kbd_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | POSIXGCC.Config.enableKbdIrq platformConfig] 
    }
getPlatformInitialProgram config RTEMS5LEON3QEMU = 
    let platformConfig = rtems5_leon3_qemu . platformFlags $ config
        initialProgArch = emptyTerminaProgArch config in
    initialProgArch {
        emitters = M.union (emitters initialProgArch) . M.fromList $ 
        [("irq_0", TPInterruptEmitter "irq_1" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq0 platformConfig] ++
        [("irq_1", TPInterruptEmitter "irq_1" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq1 platformConfig] ++
        [("irq_2", TPInterruptEmitter "irq_2" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq2 platformConfig] ++
        [("irq_3", TPInterruptEmitter "irq_3" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq3 platformConfig] ++
        [("irq_4", TPInterruptEmitter "irq_4" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq4 platformConfig] ++
        [("irq_5", TPInterruptEmitter "irq_5" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq5 platformConfig] ++
        [("irq_6", TPInterruptEmitter "irq_6" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq6 platformConfig] ++
        [("irq_7", TPInterruptEmitter "irq_7" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq7 platformConfig] ++
        [("irq_8", TPInterruptEmitter "irq_8" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq8 platformConfig] ++
        [("irq_9", TPInterruptEmitter "irq_9" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq9 platformConfig] ++
        [("irq_10", TPInterruptEmitter "irq_10" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq10 platformConfig] ++
        [("irq_11", TPInterruptEmitter "irq_11" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq11 platformConfig] ++
        [("irq_12", TPInterruptEmitter "irq_12" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq12 platformConfig] ++
        [("irq_13", TPInterruptEmitter "irq_13" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq13 platformConfig] ++
        [("irq_14", TPInterruptEmitter "irq_14" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq14 platformConfig] ++
        [("irq_15", TPInterruptEmitter "irq_15" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | RTEMS5LEON3QEMU.Config.enableIrq15 platformConfig]
    }
getPlatformInitialProgram config FreeRTOS10STM32L432XX =
    let platformConfig = freertos10_stm32l432xx . platformFlags $ config
        initialProgArch = emptyTerminaProgArch config in
    initialProgArch {
        emitters = M.union (emitters initialProgArch) . M.fromList $
        [("wwdg_irq",          TPInterruptEmitter "wwdg_irq"          (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableWwdgIrq          platformConfig] ++
        [("pvd_irq",           TPInterruptEmitter "pvd_irq"           (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enablePvdIrq           platformConfig] ++
        [("tamp_stamp_irq",    TPInterruptEmitter "tamp_stamp_irq"    (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableTampStampIrq     platformConfig] ++
        [("rtc_wkup_irq",      TPInterruptEmitter "rtc_wkup_irq"      (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableRtcWkupIrq       platformConfig] ++
        [("flash_irq",         TPInterruptEmitter "flash_irq"         (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableFlashIrq          platformConfig] ++
        [("rcc_irq",           TPInterruptEmitter "rcc_irq"           (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableRccIrq            platformConfig] ++
        [("exti0_irq",         TPInterruptEmitter "exti0_irq"         (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableExti0Irq          platformConfig] ++
        [("exti1_irq",         TPInterruptEmitter "exti1_irq"         (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableExti1Irq          platformConfig] ++
        [("exti2_irq",         TPInterruptEmitter "exti2_irq"         (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableExti2Irq          platformConfig] ++
        [("exti3_irq",         TPInterruptEmitter "exti3_irq"         (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableExti3Irq          platformConfig] ++
        [("exti4_irq",         TPInterruptEmitter "exti4_irq"         (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableExti4Irq          platformConfig] ++
        [("dma1_channel1_irq", TPInterruptEmitter "dma1_channel1_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma1Channel1Irq  platformConfig] ++
        [("dma1_channel2_irq", TPInterruptEmitter "dma1_channel2_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma1Channel2Irq  platformConfig] ++
        [("dma1_channel3_irq", TPInterruptEmitter "dma1_channel3_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma1Channel3Irq  platformConfig] ++
        [("dma1_channel4_irq", TPInterruptEmitter "dma1_channel4_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma1Channel4Irq  platformConfig] ++
        [("dma1_channel5_irq", TPInterruptEmitter "dma1_channel5_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma1Channel5Irq  platformConfig] ++
        [("dma1_channel6_irq", TPInterruptEmitter "dma1_channel6_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma1Channel6Irq  platformConfig] ++
        [("dma1_channel7_irq", TPInterruptEmitter "dma1_channel7_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma1Channel7Irq  platformConfig] ++
        [("adc1_irq",          TPInterruptEmitter "adc1_irq"          (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableAdc1Irq           platformConfig] ++
        [("can1_tx_irq",       TPInterruptEmitter "can1_tx_irq"       (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableCan1TxIrq         platformConfig] ++
        [("can1_rx0_irq",      TPInterruptEmitter "can1_rx0_irq"      (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableCan1Rx0Irq        platformConfig] ++
        [("can1_rx1_irq",      TPInterruptEmitter "can1_rx1_irq"      (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableCan1Rx1Irq        platformConfig] ++
        [("can1_sce_irq",      TPInterruptEmitter "can1_sce_irq"      (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableCan1SceIrq        platformConfig] ++
        [("exti9_5_irq",       TPInterruptEmitter "exti9_5_irq"       (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableExti9_5Irq        platformConfig] ++
        [("tim1_brk_tim15_irq",     TPInterruptEmitter "tim1_brk_tim15_irq"     (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableTim1BrkTim15Irq    platformConfig] ++
        [("tim1_up_tim16_irq",      TPInterruptEmitter "tim1_up_tim16_irq"      (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableTim1UpTim16Irq     platformConfig] ++
        [("tim1_trg_com_tim17_irq", TPInterruptEmitter "tim1_trg_com_tim17_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableTim1TrgComTim17Irq platformConfig] ++
        [("tim1_cc_irq",       TPInterruptEmitter "tim1_cc_irq"       (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableTim1CcIrq         platformConfig] ++
        [("tim2_irq",          TPInterruptEmitter "tim2_irq"          (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableTim2Irq           platformConfig] ++
        [("i2c1_ev_irq",       TPInterruptEmitter "i2c1_ev_irq"       (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableI2c1EvIrq         platformConfig] ++
        [("i2c1_er_irq",       TPInterruptEmitter "i2c1_er_irq"       (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableI2c1ErIrq         platformConfig] ++
        [("spi1_irq",          TPInterruptEmitter "spi1_irq"          (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableSpi1Irq           platformConfig] ++
        [("usart1_irq",        TPInterruptEmitter "usart1_irq"        (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableUsart1Irq         platformConfig] ++
        [("usart2_irq",        TPInterruptEmitter "usart2_irq"        (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableUsart2Irq         platformConfig] ++
        [("exti15_10_irq",     TPInterruptEmitter "exti15_10_irq"     (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableExti15_10Irq      platformConfig] ++
        [("rtc_alarm_irq",     TPInterruptEmitter "rtc_alarm_irq"     (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableRtcAlarmIrq      platformConfig] ++
        [("spi3_irq",          TPInterruptEmitter "spi3_irq"          (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableSpi3Irq           platformConfig] ++
        [("tim6_dac_irq",      TPInterruptEmitter "tim6_dac_irq"      (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableTim6DacIrq       platformConfig] ++
        [("tim7_irq",          TPInterruptEmitter "tim7_irq"          (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableTim7Irq           platformConfig] ++
        [("dma2_channel1_irq", TPInterruptEmitter "dma2_channel1_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma2Channel1Irq  platformConfig] ++
        [("dma2_channel2_irq", TPInterruptEmitter "dma2_channel2_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma2Channel2Irq  platformConfig] ++
        [("dma2_channel3_irq", TPInterruptEmitter "dma2_channel3_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma2Channel3Irq  platformConfig] ++
        [("dma2_channel4_irq", TPInterruptEmitter "dma2_channel4_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma2Channel4Irq  platformConfig] ++
        [("dma2_channel5_irq", TPInterruptEmitter "dma2_channel5_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma2Channel5Irq  platformConfig] ++
        [("comp_irq",          TPInterruptEmitter "comp_irq"          (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableCompIrq           platformConfig] ++
        [("lptim1_irq",        TPInterruptEmitter "lptim1_irq"        (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableLptim1Irq         platformConfig] ++
        [("lptim2_irq",        TPInterruptEmitter "lptim2_irq"        (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableLptim2Irq         platformConfig] ++
        [("usb_irq",           TPInterruptEmitter "usb_irq"           (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableUsbIrq            platformConfig] ++
        [("dma2_channel6_irq", TPInterruptEmitter "dma2_channel6_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma2Channel6Irq  platformConfig] ++
        [("dma2_channel7_irq", TPInterruptEmitter "dma2_channel7_irq" (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableDma2Channel7Irq  platformConfig] ++
        [("lpuart1_irq",       TPInterruptEmitter "lpuart1_irq"       (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableLpuart1Irq        platformConfig] ++
        [("quad_spi_irq",      TPInterruptEmitter "quad_spi_irq"      (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableQuadSpiIrq        platformConfig] ++
        [("i2c3_ev_irq",       TPInterruptEmitter "i2c3_ev_irq"       (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableI2c3EvIrq         platformConfig] ++
        [("i2c3_er_irq",       TPInterruptEmitter "i2c3_er_irq"       (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableI2c3ErIrq         platformConfig] ++
        [("sai1_irq",          TPInterruptEmitter "sai1_irq"          (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableSai1Irq           platformConfig] ++
        [("swpmi1_irq",        TPInterruptEmitter "swpmi1_irq"        (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableSwpmi1Irq         platformConfig] ++
        [("tsc_irq",           TPInterruptEmitter "tsc_irq"           (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableTscIrq            platformConfig] ++
        [("rng_irq",           TPInterruptEmitter "rng_irq"           (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableRngIrq            platformConfig] ++
        [("fpu_irq",           TPInterruptEmitter "fpu_irq"           (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableFpuIrq            platformConfig] ++
        [("crs_irq",           TPInterruptEmitter "crs_irq"           (SemanticAnn (GTy (TGlobal EmitterClass "Interrupt")) Internal)) | FreeRTOS10STM32L432XX.Config.enableCrsIrq            platformConfig]
    }
getPlatformInitialProgram config TestPlatform = emptyTerminaProgArch config

getPlatformInterruptMap :: Platform -> M.Map Identifier Integer
getPlatformInterruptMap POSIXGCC =
    M.fromList [("kbd_irq", 0)]
getPlatformInterruptMap RTEMS5LEON3QEMU =
    M.fromList [("irq_1", 1), ("irq_2", 2), ("irq_3", 3), ("irq_4", 4), 
                ("irq_5", 5), ("irq_6", 6), ("irq_7", 7), ("irq_8", 8), 
                ("irq_9", 9), ("irq_10", 10), ("irq_11", 11), ("irq_12", 12), 
                ("irq_13", 13), ("irq_14", 14), ("irq_15", 15)]
getPlatformInterruptMap FreeRTOS10STM32L432XX =
    M.fromList [
        ("wwdg_irq",          0),
        ("pvd_irq",           1),
        ("tamp_stamp_irq",    2),
        ("rtc_wkup_irq",      3),
        ("flash_irq",         4),
        ("rcc_irq",           5),
        ("exti0_irq",         6),
        ("exti1_irq",         7),
        ("exti2_irq",         8),
        ("exti3_irq",         9),
        ("exti4_irq",         10),
        ("dma1_channel1_irq", 11),
        ("dma1_channel2_irq", 12),
        ("dma1_channel3_irq", 13),
        ("dma1_channel4_irq", 14),
        ("dma1_channel5_irq", 15),
        ("dma1_channel6_irq", 16),
        ("dma1_channel7_irq", 17),
        ("adc1_irq",          18),
        ("can1_tx_irq",       19),
        ("can1_rx0_irq",      20),
        ("can1_rx1_irq",      21),
        ("can1_sce_irq",      22),
        ("exti9_5_irq",       23),
        ("tim1_brk_tim15_irq",     24),
        ("tim1_up_tim16_irq",      25),
        ("tim1_trg_com_tim17_irq", 26),
        ("tim1_cc_irq",       27),
        ("tim2_irq",          28),
        ("i2c1_ev_irq",       31),
        ("i2c1_er_irq",       32),
        ("spi1_irq",          35),
        ("usart1_irq",        37),
        ("usart2_irq",        38),
        ("exti15_10_irq",     40),
        ("rtc_alarm_irq",     41),
        ("spi3_irq",          51),
        ("tim6_dac_irq",      54),
        ("tim7_irq",          55),
        ("dma2_channel1_irq", 56),
        ("dma2_channel2_irq", 57),
        ("dma2_channel3_irq", 58),
        ("dma2_channel4_irq", 59),
        ("dma2_channel5_irq", 60),
        ("comp_irq",          64),
        ("lptim1_irq",        65),
        ("lptim2_irq",        66),
        ("usb_irq",           67),
        ("dma2_channel6_irq", 68),
        ("dma2_channel7_irq", 69),
        ("lpuart1_irq",       70),
        ("quad_spi_irq",      71),
        ("i2c3_ev_irq",       72),
        ("i2c3_er_irq",       73),
        ("sai1_irq",          74),
        ("swpmi1_irq",        76),
        ("tsc_irq",           77),
        ("rng_irq",           80),
        ("fpu_irq",           81),
        ("crs_irq",           82)
    ]
getPlatformInterruptMap TestPlatform = M.empty
