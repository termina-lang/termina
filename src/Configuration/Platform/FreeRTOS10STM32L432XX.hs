{-# LANGUAGE OverloadedStrings #-}

module Configuration.Platform.FreeRTOS10STM32L432XX where
import Data.Yaml

-- | Interrupt emitter enable flags for the FreeRTOS V10 / STM32L432XX platform.
-- Each field corresponds to a peripheral interrupt source in the STM32L432xx
-- vector table (startup_stm32l432xx.s).  Set a flag to True in the project
-- configuration to deploy an interrupt emitter for that interrupt source.
data FreeRTOS10STM32L432XXFlags = FreeRTOS10STM32L432XXFlags {
    enableWwdgIrq          :: !Bool,  -- ^ Window Watchdog
    enablePvdIrq           :: !Bool,  -- ^ PVD through EXTI Line detection
    enableTampStampIrq     :: !Bool,  -- ^ Tamper and TimeStamp through EXTI line
    enableRtcWkupIrq       :: !Bool,  -- ^ RTC Wakeup through EXTI line
    enableFlashIrq         :: !Bool,  -- ^ FLASH global interrupt
    enableRccIrq           :: !Bool,  -- ^ RCC global interrupt
    enableExti0Irq         :: !Bool,  -- ^ EXTI Line 0
    enableExti1Irq         :: !Bool,  -- ^ EXTI Line 1
    enableExti2Irq         :: !Bool,  -- ^ EXTI Line 2
    enableExti3Irq         :: !Bool,  -- ^ EXTI Line 3
    enableExti4Irq         :: !Bool,  -- ^ EXTI Line 4
    enableDma1Channel1Irq  :: !Bool,  -- ^ DMA1 Channel 1
    enableDma1Channel2Irq  :: !Bool,  -- ^ DMA1 Channel 2
    enableDma1Channel3Irq  :: !Bool,  -- ^ DMA1 Channel 3
    enableDma1Channel4Irq  :: !Bool,  -- ^ DMA1 Channel 4
    enableDma1Channel5Irq  :: !Bool,  -- ^ DMA1 Channel 5
    enableDma1Channel6Irq  :: !Bool,  -- ^ DMA1 Channel 6
    enableDma1Channel7Irq  :: !Bool,  -- ^ DMA1 Channel 7
    enableAdc1Irq          :: !Bool,  -- ^ ADC1 and ADC2
    enableCan1TxIrq        :: !Bool,  -- ^ CAN1 TX
    enableCan1Rx0Irq       :: !Bool,  -- ^ CAN1 RX0
    enableCan1Rx1Irq       :: !Bool,  -- ^ CAN1 RX1
    enableCan1SceIrq       :: !Bool,  -- ^ CAN1 SCE
    enableExti9_5Irq       :: !Bool,  -- ^ EXTI Line[9:5]
    enableTim1BrkTim15Irq  :: !Bool,  -- ^ TIM1 Break and TIM15
    enableTim1UpTim16Irq   :: !Bool,  -- ^ TIM1 Update and TIM16
    enableTim1TrgComTim17Irq :: !Bool, -- ^ TIM1 Trigger and Commutation and TIM17
    enableTim1CcIrq        :: !Bool,  -- ^ TIM1 Capture Compare
    enableTim2Irq          :: !Bool,  -- ^ TIM2
    enableI2c1EvIrq        :: !Bool,  -- ^ I2C1 Event
    enableI2c1ErIrq        :: !Bool,  -- ^ I2C1 Error
    enableSpi1Irq          :: !Bool,  -- ^ SPI1
    enableUsart1Irq        :: !Bool,  -- ^ USART1
    enableUsart2Irq        :: !Bool,  -- ^ USART2
    enableExti15_10Irq     :: !Bool,  -- ^ EXTI Line[15:10]
    enableRtcAlarmIrq      :: !Bool,  -- ^ RTC Alarm (A and B) through EXTI Line
    enableSpi3Irq          :: !Bool,  -- ^ SPI3
    enableTim6DacIrq       :: !Bool,  -- ^ TIM6 and DAC underrun errors
    enableTim7Irq          :: !Bool,  -- ^ TIM7
    enableDma2Channel1Irq  :: !Bool,  -- ^ DMA2 Channel 1
    enableDma2Channel2Irq  :: !Bool,  -- ^ DMA2 Channel 2
    enableDma2Channel3Irq  :: !Bool,  -- ^ DMA2 Channel 3
    enableDma2Channel4Irq  :: !Bool,  -- ^ DMA2 Channel 4
    enableDma2Channel5Irq  :: !Bool,  -- ^ DMA2 Channel 5
    enableCompIrq          :: !Bool,  -- ^ COMP1 and COMP2
    enableLptim1Irq        :: !Bool,  -- ^ LPTIM1
    enableLptim2Irq        :: !Bool,  -- ^ LPTIM2
    enableUsbIrq           :: !Bool,  -- ^ USB event through EXTI Line
    enableDma2Channel6Irq  :: !Bool,  -- ^ DMA2 Channel 6
    enableDma2Channel7Irq  :: !Bool,  -- ^ DMA2 Channel 7
    enableLpuart1Irq       :: !Bool,  -- ^ LPUART1
    enableQuadSpiIrq       :: !Bool,  -- ^ Quad SPI
    enableI2c3EvIrq        :: !Bool,  -- ^ I2C3 Event
    enableI2c3ErIrq        :: !Bool,  -- ^ I2C3 Error
    enableSai1Irq          :: !Bool,  -- ^ SAI1
    enableSwpmi1Irq        :: !Bool,  -- ^ SWPMI1
    enableTscIrq           :: !Bool,  -- ^ TSC
    enableRngIrq           :: !Bool,  -- ^ RNG
    enableFpuIrq           :: !Bool,  -- ^ FPU
    enableCrsIrq           :: !Bool   -- ^ CRS
} deriving (Eq, Show)

defaultFreeRTOS10STM32L432XXFlags :: FreeRTOS10STM32L432XXFlags
defaultFreeRTOS10STM32L432XXFlags = FreeRTOS10STM32L432XXFlags {
    enableWwdgIrq            = False,
    enablePvdIrq             = False,
    enableTampStampIrq       = False,
    enableRtcWkupIrq         = False,
    enableFlashIrq           = False,
    enableRccIrq             = False,
    enableExti0Irq           = False,
    enableExti1Irq           = False,
    enableExti2Irq           = False,
    enableExti3Irq           = False,
    enableExti4Irq           = False,
    enableDma1Channel1Irq    = False,
    enableDma1Channel2Irq    = False,
    enableDma1Channel3Irq    = False,
    enableDma1Channel4Irq    = False,
    enableDma1Channel5Irq    = False,
    enableDma1Channel6Irq    = False,
    enableDma1Channel7Irq    = False,
    enableAdc1Irq            = False,
    enableCan1TxIrq          = False,
    enableCan1Rx0Irq         = False,
    enableCan1Rx1Irq         = False,
    enableCan1SceIrq         = False,
    enableExti9_5Irq         = False,
    enableTim1BrkTim15Irq    = False,
    enableTim1UpTim16Irq     = False,
    enableTim1TrgComTim17Irq = False,
    enableTim1CcIrq          = False,
    enableTim2Irq            = False,
    enableI2c1EvIrq          = False,
    enableI2c1ErIrq          = False,
    enableSpi1Irq            = False,
    enableUsart1Irq          = False,
    enableUsart2Irq          = False,
    enableExti15_10Irq       = False,
    enableRtcAlarmIrq        = False,
    enableSpi3Irq            = False,
    enableTim6DacIrq         = False,
    enableTim7Irq            = False,
    enableDma2Channel1Irq    = False,
    enableDma2Channel2Irq    = False,
    enableDma2Channel3Irq    = False,
    enableDma2Channel4Irq    = False,
    enableDma2Channel5Irq    = False,
    enableCompIrq            = False,
    enableLptim1Irq          = False,
    enableLptim2Irq          = False,
    enableUsbIrq             = False,
    enableDma2Channel6Irq    = False,
    enableDma2Channel7Irq    = False,
    enableLpuart1Irq         = False,
    enableQuadSpiIrq         = False,
    enableI2c3EvIrq          = False,
    enableI2c3ErIrq          = False,
    enableSai1Irq            = False,
    enableSwpmi1Irq          = False,
    enableTscIrq             = False,
    enableRngIrq             = False,
    enableFpuIrq             = False,
    enableCrsIrq             = False
}

instance FromJSON FreeRTOS10STM32L432XXFlags where
  parseJSON (Object o) =
    FreeRTOS10STM32L432XXFlags <$>
    o .:? "enable-wwdg-irq"              .!= False <*>
    o .:? "enable-pvd-irq"               .!= False <*>
    o .:? "enable-tamp-stamp-irq"        .!= False <*>
    o .:? "enable-rtc-wkup-irq"          .!= False <*>
    o .:? "enable-flash-irq"             .!= False <*>
    o .:? "enable-rcc-irq"               .!= False <*>
    o .:? "enable-exti0-irq"             .!= False <*>
    o .:? "enable-exti1-irq"             .!= False <*>
    o .:? "enable-exti2-irq"             .!= False <*>
    o .:? "enable-exti3-irq"             .!= False <*>
    o .:? "enable-exti4-irq"             .!= False <*>
    o .:? "enable-dma1-channel1-irq"     .!= False <*>
    o .:? "enable-dma1-channel2-irq"     .!= False <*>
    o .:? "enable-dma1-channel3-irq"     .!= False <*>
    o .:? "enable-dma1-channel4-irq"     .!= False <*>
    o .:? "enable-dma1-channel5-irq"     .!= False <*>
    o .:? "enable-dma1-channel6-irq"     .!= False <*>
    o .:? "enable-dma1-channel7-irq"     .!= False <*>
    o .:? "enable-adc1-irq"              .!= False <*>
    o .:? "enable-can1-tx-irq"           .!= False <*>
    o .:? "enable-can1-rx0-irq"          .!= False <*>
    o .:? "enable-can1-rx1-irq"          .!= False <*>
    o .:? "enable-can1-sce-irq"          .!= False <*>
    o .:? "enable-exti9-5-irq"           .!= False <*>
    o .:? "enable-tim1-brk-tim15-irq"    .!= False <*>
    o .:? "enable-tim1-up-tim16-irq"     .!= False <*>
    o .:? "enable-tim1-trg-com-tim17-irq" .!= False <*>
    o .:? "enable-tim1-cc-irq"           .!= False <*>
    o .:? "enable-tim2-irq"              .!= False <*>
    o .:? "enable-i2c1-ev-irq"           .!= False <*>
    o .:? "enable-i2c1-er-irq"           .!= False <*>
    o .:? "enable-spi1-irq"              .!= False <*>
    o .:? "enable-usart1-irq"            .!= False <*>
    o .:? "enable-usart2-irq"            .!= False <*>
    o .:? "enable-exti15-10-irq"         .!= False <*>
    o .:? "enable-rtc-alarm-irq"         .!= False <*>
    o .:? "enable-spi3-irq"              .!= False <*>
    o .:? "enable-tim6-dac-irq"          .!= False <*>
    o .:? "enable-tim7-irq"              .!= False <*>
    o .:? "enable-dma2-channel1-irq"     .!= False <*>
    o .:? "enable-dma2-channel2-irq"     .!= False <*>
    o .:? "enable-dma2-channel3-irq"     .!= False <*>
    o .:? "enable-dma2-channel4-irq"     .!= False <*>
    o .:? "enable-dma2-channel5-irq"     .!= False <*>
    o .:? "enable-comp-irq"              .!= False <*>
    o .:? "enable-lptim1-irq"            .!= False <*>
    o .:? "enable-lptim2-irq"            .!= False <*>
    o .:? "enable-usb-irq"               .!= False <*>
    o .:? "enable-dma2-channel6-irq"     .!= False <*>
    o .:? "enable-dma2-channel7-irq"     .!= False <*>
    o .:? "enable-lpuart1-irq"           .!= False <*>
    o .:? "enable-quad-spi-irq"          .!= False <*>
    o .:? "enable-i2c3-ev-irq"           .!= False <*>
    o .:? "enable-i2c3-er-irq"           .!= False <*>
    o .:? "enable-sai1-irq"              .!= False <*>
    o .:? "enable-swpmi1-irq"            .!= False <*>
    o .:? "enable-tsc-irq"               .!= False <*>
    o .:? "enable-rng-irq"               .!= False <*>
    o .:? "enable-fpu-irq"               .!= False <*>
    o .:? "enable-crs-irq"               .!= False
  parseJSON _ = fail "Expected configuration object"

instance ToJSON FreeRTOS10STM32L432XXFlags where
    toJSON (
        FreeRTOS10STM32L432XXFlags
            flagsEnableWwdgIrq
            flagsEnablePvdIrq
            flagsEnableTampStampIrq
            flagsEnableRtcWkupIrq
            flagsEnableFlashIrq
            flagsEnableRccIrq
            flagsEnableExti0Irq
            flagsEnableExti1Irq
            flagsEnableExti2Irq
            flagsEnableExti3Irq
            flagsEnableExti4Irq
            flagsEnableDma1Channel1Irq
            flagsEnableDma1Channel2Irq
            flagsEnableDma1Channel3Irq
            flagsEnableDma1Channel4Irq
            flagsEnableDma1Channel5Irq
            flagsEnableDma1Channel6Irq
            flagsEnableDma1Channel7Irq
            flagsEnableAdc1Irq
            flagsEnableCan1TxIrq
            flagsEnableCan1Rx0Irq
            flagsEnableCan1Rx1Irq
            flagsEnableCan1SceIrq
            flagsEnableExti9_5Irq
            flagsEnableTim1BrkTim15Irq
            flagsEnableTim1UpTim16Irq
            flagsEnableTim1TrgComTim17Irq
            flagsEnableTim1CcIrq
            flagsEnableTim2Irq
            flagsEnableI2c1EvIrq
            flagsEnableI2c1ErIrq
            flagsEnableSpi1Irq
            flagsEnableUsart1Irq
            flagsEnableUsart2Irq
            flagsEnableExti15_10Irq
            flagsEnableRtcAlarmIrq
            flagsEnableSpi3Irq
            flagsEnableTim6DacIrq
            flagsEnableTim7Irq
            flagsEnableDma2Channel1Irq
            flagsEnableDma2Channel2Irq
            flagsEnableDma2Channel3Irq
            flagsEnableDma2Channel4Irq
            flagsEnableDma2Channel5Irq
            flagsEnableCompIrq
            flagsEnableLptim1Irq
            flagsEnableLptim2Irq
            flagsEnableUsbIrq
            flagsEnableDma2Channel6Irq
            flagsEnableDma2Channel7Irq
            flagsEnableLpuart1Irq
            flagsEnableQuadSpiIrq
            flagsEnableI2c3EvIrq
            flagsEnableI2c3ErIrq
            flagsEnableSai1Irq
            flagsEnableSwpmi1Irq
            flagsEnableTscIrq
            flagsEnableRngIrq
            flagsEnableFpuIrq
            flagsEnableCrsIrq
        ) = object [
            "enable-wwdg-irq"               .= flagsEnableWwdgIrq,
            "enable-pvd-irq"                .= flagsEnablePvdIrq,
            "enable-tamp-stamp-irq"         .= flagsEnableTampStampIrq,
            "enable-rtc-wkup-irq"           .= flagsEnableRtcWkupIrq,
            "enable-flash-irq"              .= flagsEnableFlashIrq,
            "enable-rcc-irq"                .= flagsEnableRccIrq,
            "enable-exti0-irq"              .= flagsEnableExti0Irq,
            "enable-exti1-irq"              .= flagsEnableExti1Irq,
            "enable-exti2-irq"              .= flagsEnableExti2Irq,
            "enable-exti3-irq"              .= flagsEnableExti3Irq,
            "enable-exti4-irq"              .= flagsEnableExti4Irq,
            "enable-dma1-channel1-irq"      .= flagsEnableDma1Channel1Irq,
            "enable-dma1-channel2-irq"      .= flagsEnableDma1Channel2Irq,
            "enable-dma1-channel3-irq"      .= flagsEnableDma1Channel3Irq,
            "enable-dma1-channel4-irq"      .= flagsEnableDma1Channel4Irq,
            "enable-dma1-channel5-irq"      .= flagsEnableDma1Channel5Irq,
            "enable-dma1-channel6-irq"      .= flagsEnableDma1Channel6Irq,
            "enable-dma1-channel7-irq"      .= flagsEnableDma1Channel7Irq,
            "enable-adc1-irq"               .= flagsEnableAdc1Irq,
            "enable-can1-tx-irq"            .= flagsEnableCan1TxIrq,
            "enable-can1-rx0-irq"           .= flagsEnableCan1Rx0Irq,
            "enable-can1-rx1-irq"           .= flagsEnableCan1Rx1Irq,
            "enable-can1-sce-irq"           .= flagsEnableCan1SceIrq,
            "enable-exti9-5-irq"            .= flagsEnableExti9_5Irq,
            "enable-tim1-brk-tim15-irq"     .= flagsEnableTim1BrkTim15Irq,
            "enable-tim1-up-tim16-irq"      .= flagsEnableTim1UpTim16Irq,
            "enable-tim1-trg-com-tim17-irq" .= flagsEnableTim1TrgComTim17Irq,
            "enable-tim1-cc-irq"            .= flagsEnableTim1CcIrq,
            "enable-tim2-irq"               .= flagsEnableTim2Irq,
            "enable-i2c1-ev-irq"            .= flagsEnableI2c1EvIrq,
            "enable-i2c1-er-irq"            .= flagsEnableI2c1ErIrq,
            "enable-spi1-irq"               .= flagsEnableSpi1Irq,
            "enable-usart1-irq"             .= flagsEnableUsart1Irq,
            "enable-usart2-irq"             .= flagsEnableUsart2Irq,
            "enable-exti15-10-irq"          .= flagsEnableExti15_10Irq,
            "enable-rtc-alarm-irq"          .= flagsEnableRtcAlarmIrq,
            "enable-spi3-irq"               .= flagsEnableSpi3Irq,
            "enable-tim6-dac-irq"           .= flagsEnableTim6DacIrq,
            "enable-tim7-irq"               .= flagsEnableTim7Irq,
            "enable-dma2-channel1-irq"      .= flagsEnableDma2Channel1Irq,
            "enable-dma2-channel2-irq"      .= flagsEnableDma2Channel2Irq,
            "enable-dma2-channel3-irq"      .= flagsEnableDma2Channel3Irq,
            "enable-dma2-channel4-irq"      .= flagsEnableDma2Channel4Irq,
            "enable-dma2-channel5-irq"      .= flagsEnableDma2Channel5Irq,
            "enable-comp-irq"               .= flagsEnableCompIrq,
            "enable-lptim1-irq"             .= flagsEnableLptim1Irq,
            "enable-lptim2-irq"             .= flagsEnableLptim2Irq,
            "enable-usb-irq"                .= flagsEnableUsbIrq,
            "enable-dma2-channel6-irq"      .= flagsEnableDma2Channel6Irq,
            "enable-dma2-channel7-irq"      .= flagsEnableDma2Channel7Irq,
            "enable-lpuart1-irq"            .= flagsEnableLpuart1Irq,
            "enable-quad-spi-irq"           .= flagsEnableQuadSpiIrq,
            "enable-i2c3-ev-irq"            .= flagsEnableI2c3EvIrq,
            "enable-i2c3-er-irq"            .= flagsEnableI2c3ErIrq,
            "enable-sai1-irq"               .= flagsEnableSai1Irq,
            "enable-swpmi1-irq"             .= flagsEnableSwpmi1Irq,
            "enable-tsc-irq"                .= flagsEnableTscIrq,
            "enable-rng-irq"                .= flagsEnableRngIrq,
            "enable-fpu-irq"                .= flagsEnableFpuIrq,
            "enable-crs-irq"                .= flagsEnableCrsIrq
        ]