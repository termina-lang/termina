{-# LANGUAGE OverloadedStrings #-}

module Configuration.Platform.RTEMS6ZYNQ7000PYNQZ2 where
import Data.Yaml

-- | Interrupt emitter enable flags for the RTEMS 6 / Zynq-7000 / PYNQ-Z2
-- platform. Each field corresponds to a shared peripheral interrupt of the
-- Zynq-7000, named after the vector the board support package of RTEMS declares
-- for it in bsp/irq.h. Set a flag to True in the project configuration to
-- deploy an interrupt emitter for that interrupt source.
data RTEMS6ZYNQ7000PYNQZ2Flags = RTEMS6ZYNQ7000PYNQZ2Flags {
    enableCpu0Irq            :: !Bool,  -- ^ CPU 0
    enableCpu1Irq            :: !Bool,  -- ^ CPU 1
    enableL2CacheIrq         :: !Bool,  -- ^ L2 cache
    enableOcmIrq             :: !Bool,  -- ^ On-chip memory
    enablePmu0Irq            :: !Bool,  -- ^ Performance monitor unit 0
    enablePmu1Irq            :: !Bool,  -- ^ Performance monitor unit 1
    enableXadcIrq            :: !Bool,  -- ^ XADC
    enableDviIrq             :: !Bool,  -- ^ Device configuration
    enableSwdtIrq            :: !Bool,  -- ^ System watchdog timer
    enableTtc00Irq           :: !Bool,  -- ^ Triple timer counter 0, timer 0
    enableTtc10Irq           :: !Bool,  -- ^ Triple timer counter 0, timer 1
    enableTtc20Irq           :: !Bool,  -- ^ Triple timer counter 0, timer 2
    enableDmacAbortIrq       :: !Bool,  -- ^ DMA controller abort
    enableDmac0Irq           :: !Bool,  -- ^ DMA controller channel 0
    enableDmac1Irq           :: !Bool,  -- ^ DMA controller channel 1
    enableDmac2Irq           :: !Bool,  -- ^ DMA controller channel 2
    enableDmac3Irq           :: !Bool,  -- ^ DMA controller channel 3
    enableSmcIrq             :: !Bool,  -- ^ Static memory controller
    enableQuadSpiIrq         :: !Bool,  -- ^ Quad-SPI
    enableGpioIrq            :: !Bool,  -- ^ GPIO
    enableUsb0Irq            :: !Bool,  -- ^ USB 0
    enableEthernet0Irq       :: !Bool,  -- ^ Ethernet 0
    enableEthernet0WakeupIrq :: !Bool,  -- ^ Ethernet 0 wake-up
    enableSdio0Irq           :: !Bool,  -- ^ SDIO 0
    enableI2c0Irq            :: !Bool,  -- ^ I2C 0
    enableSpi0Irq            :: !Bool,  -- ^ SPI 0
    enableUart0Irq           :: !Bool,  -- ^ UART 0
    enableCan0Irq            :: !Bool,  -- ^ CAN 0
    enableFpga0Irq           :: !Bool,  -- ^ Programmable logic interrupt 0
    enableFpga1Irq           :: !Bool,  -- ^ Programmable logic interrupt 1
    enableFpga2Irq           :: !Bool,  -- ^ Programmable logic interrupt 2
    enableFpga3Irq           :: !Bool,  -- ^ Programmable logic interrupt 3
    enableFpga4Irq           :: !Bool,  -- ^ Programmable logic interrupt 4
    enableFpga5Irq           :: !Bool,  -- ^ Programmable logic interrupt 5
    enableFpga6Irq           :: !Bool,  -- ^ Programmable logic interrupt 6
    enableFpga7Irq           :: !Bool,  -- ^ Programmable logic interrupt 7
    enableTtc01Irq           :: !Bool,  -- ^ Triple timer counter 1, timer 0
    enableTtc11Irq           :: !Bool,  -- ^ Triple timer counter 1, timer 1
    enableTtc21Irq           :: !Bool,  -- ^ Triple timer counter 1, timer 2
    enableDmac4Irq           :: !Bool,  -- ^ DMA controller channel 4
    enableDmac5Irq           :: !Bool,  -- ^ DMA controller channel 5
    enableDmac6Irq           :: !Bool,  -- ^ DMA controller channel 6
    enableDmac7Irq           :: !Bool,  -- ^ DMA controller channel 7
    enableUsb1Irq            :: !Bool,  -- ^ USB 1
    enableEthernet1Irq       :: !Bool,  -- ^ Ethernet 1
    enableEthernet1WakeupIrq :: !Bool,  -- ^ Ethernet 1 wake-up
    enableSdio1Irq           :: !Bool,  -- ^ SDIO 1
    enableI2c1Irq            :: !Bool,  -- ^ I2C 1
    enableSpi1Irq            :: !Bool,  -- ^ SPI 1
    enableUart1Irq           :: !Bool,  -- ^ UART 1
    enableCan1Irq            :: !Bool,  -- ^ CAN 1
    enableFpga8Irq           :: !Bool,  -- ^ Programmable logic interrupt 8
    enableFpga9Irq           :: !Bool,  -- ^ Programmable logic interrupt 9
    enableFpga10Irq          :: !Bool,  -- ^ Programmable logic interrupt 10
    enableFpga11Irq          :: !Bool,  -- ^ Programmable logic interrupt 11
    enableFpga12Irq          :: !Bool,  -- ^ Programmable logic interrupt 12
    enableFpga13Irq          :: !Bool,  -- ^ Programmable logic interrupt 13
    enableFpga14Irq          :: !Bool,  -- ^ Programmable logic interrupt 14
    enableFpga15Irq          :: !Bool,  -- ^ Programmable logic interrupt 15
    enableParityIrq          :: !Bool   -- ^ Parity error
} deriving (Eq, Show)

defaultRTEMS6ZYNQ7000PYNQZ2Flags :: RTEMS6ZYNQ7000PYNQZ2Flags
defaultRTEMS6ZYNQ7000PYNQZ2Flags = RTEMS6ZYNQ7000PYNQZ2Flags {
    enableCpu0Irq            = False,
    enableCpu1Irq            = False,
    enableL2CacheIrq         = False,
    enableOcmIrq             = False,
    enablePmu0Irq            = False,
    enablePmu1Irq            = False,
    enableXadcIrq            = False,
    enableDviIrq             = False,
    enableSwdtIrq            = False,
    enableTtc00Irq           = False,
    enableTtc10Irq           = False,
    enableTtc20Irq           = False,
    enableDmacAbortIrq       = False,
    enableDmac0Irq           = False,
    enableDmac1Irq           = False,
    enableDmac2Irq           = False,
    enableDmac3Irq           = False,
    enableSmcIrq             = False,
    enableQuadSpiIrq         = False,
    enableGpioIrq            = False,
    enableUsb0Irq            = False,
    enableEthernet0Irq       = False,
    enableEthernet0WakeupIrq = False,
    enableSdio0Irq           = False,
    enableI2c0Irq            = False,
    enableSpi0Irq            = False,
    enableUart0Irq           = False,
    enableCan0Irq            = False,
    enableFpga0Irq           = False,
    enableFpga1Irq           = False,
    enableFpga2Irq           = False,
    enableFpga3Irq           = False,
    enableFpga4Irq           = False,
    enableFpga5Irq           = False,
    enableFpga6Irq           = False,
    enableFpga7Irq           = False,
    enableTtc01Irq           = False,
    enableTtc11Irq           = False,
    enableTtc21Irq           = False,
    enableDmac4Irq           = False,
    enableDmac5Irq           = False,
    enableDmac6Irq           = False,
    enableDmac7Irq           = False,
    enableUsb1Irq            = False,
    enableEthernet1Irq       = False,
    enableEthernet1WakeupIrq = False,
    enableSdio1Irq           = False,
    enableI2c1Irq            = False,
    enableSpi1Irq            = False,
    enableUart1Irq           = False,
    enableCan1Irq            = False,
    enableFpga8Irq           = False,
    enableFpga9Irq           = False,
    enableFpga10Irq          = False,
    enableFpga11Irq          = False,
    enableFpga12Irq          = False,
    enableFpga13Irq          = False,
    enableFpga14Irq          = False,
    enableFpga15Irq          = False,
    enableParityIrq          = False
}

instance FromJSON RTEMS6ZYNQ7000PYNQZ2Flags where
  parseJSON (Object o) =
    RTEMS6ZYNQ7000PYNQZ2Flags <$>
    o .:? "enable-cpu-0-irq"             .!= False <*>
    o .:? "enable-cpu-1-irq"             .!= False <*>
    o .:? "enable-l2-cache-irq"          .!= False <*>
    o .:? "enable-ocm-irq"               .!= False <*>
    o .:? "enable-pmu-0-irq"             .!= False <*>
    o .:? "enable-pmu-1-irq"             .!= False <*>
    o .:? "enable-xadc-irq"              .!= False <*>
    o .:? "enable-dvi-irq"               .!= False <*>
    o .:? "enable-swdt-irq"              .!= False <*>
    o .:? "enable-ttc-0-0-irq"           .!= False <*>
    o .:? "enable-ttc-1-0-irq"           .!= False <*>
    o .:? "enable-ttc-2-0-irq"           .!= False <*>
    o .:? "enable-dmac-abort-irq"        .!= False <*>
    o .:? "enable-dmac-0-irq"            .!= False <*>
    o .:? "enable-dmac-1-irq"            .!= False <*>
    o .:? "enable-dmac-2-irq"            .!= False <*>
    o .:? "enable-dmac-3-irq"            .!= False <*>
    o .:? "enable-smc-irq"               .!= False <*>
    o .:? "enable-quad-spi-irq"          .!= False <*>
    o .:? "enable-gpio-irq"              .!= False <*>
    o .:? "enable-usb-0-irq"             .!= False <*>
    o .:? "enable-ethernet-0-irq"        .!= False <*>
    o .:? "enable-ethernet-0-wakeup-irq" .!= False <*>
    o .:? "enable-sdio-0-irq"            .!= False <*>
    o .:? "enable-i2c-0-irq"             .!= False <*>
    o .:? "enable-spi-0-irq"             .!= False <*>
    o .:? "enable-uart-0-irq"            .!= False <*>
    o .:? "enable-can-0-irq"             .!= False <*>
    o .:? "enable-fpga-0-irq"            .!= False <*>
    o .:? "enable-fpga-1-irq"            .!= False <*>
    o .:? "enable-fpga-2-irq"            .!= False <*>
    o .:? "enable-fpga-3-irq"            .!= False <*>
    o .:? "enable-fpga-4-irq"            .!= False <*>
    o .:? "enable-fpga-5-irq"            .!= False <*>
    o .:? "enable-fpga-6-irq"            .!= False <*>
    o .:? "enable-fpga-7-irq"            .!= False <*>
    o .:? "enable-ttc-0-1-irq"           .!= False <*>
    o .:? "enable-ttc-1-1-irq"           .!= False <*>
    o .:? "enable-ttc-2-1-irq"           .!= False <*>
    o .:? "enable-dmac-4-irq"            .!= False <*>
    o .:? "enable-dmac-5-irq"            .!= False <*>
    o .:? "enable-dmac-6-irq"            .!= False <*>
    o .:? "enable-dmac-7-irq"            .!= False <*>
    o .:? "enable-usb-1-irq"             .!= False <*>
    o .:? "enable-ethernet-1-irq"        .!= False <*>
    o .:? "enable-ethernet-1-wakeup-irq" .!= False <*>
    o .:? "enable-sdio-1-irq"            .!= False <*>
    o .:? "enable-i2c-1-irq"             .!= False <*>
    o .:? "enable-spi-1-irq"             .!= False <*>
    o .:? "enable-uart-1-irq"            .!= False <*>
    o .:? "enable-can-1-irq"             .!= False <*>
    o .:? "enable-fpga-8-irq"            .!= False <*>
    o .:? "enable-fpga-9-irq"            .!= False <*>
    o .:? "enable-fpga-10-irq"           .!= False <*>
    o .:? "enable-fpga-11-irq"           .!= False <*>
    o .:? "enable-fpga-12-irq"           .!= False <*>
    o .:? "enable-fpga-13-irq"           .!= False <*>
    o .:? "enable-fpga-14-irq"           .!= False <*>
    o .:? "enable-fpga-15-irq"           .!= False <*>
    o .:? "enable-parity-irq"            .!= False
  parseJSON _ = fail "Expected configuration object"

instance ToJSON RTEMS6ZYNQ7000PYNQZ2Flags where
    toJSON (
        RTEMS6ZYNQ7000PYNQZ2Flags
            flagsEnableCpu0Irq
            flagsEnableCpu1Irq
            flagsEnableL2CacheIrq
            flagsEnableOcmIrq
            flagsEnablePmu0Irq
            flagsEnablePmu1Irq
            flagsEnableXadcIrq
            flagsEnableDviIrq
            flagsEnableSwdtIrq
            flagsEnableTtc00Irq
            flagsEnableTtc10Irq
            flagsEnableTtc20Irq
            flagsEnableDmacAbortIrq
            flagsEnableDmac0Irq
            flagsEnableDmac1Irq
            flagsEnableDmac2Irq
            flagsEnableDmac3Irq
            flagsEnableSmcIrq
            flagsEnableQuadSpiIrq
            flagsEnableGpioIrq
            flagsEnableUsb0Irq
            flagsEnableEthernet0Irq
            flagsEnableEthernet0WakeupIrq
            flagsEnableSdio0Irq
            flagsEnableI2c0Irq
            flagsEnableSpi0Irq
            flagsEnableUart0Irq
            flagsEnableCan0Irq
            flagsEnableFpga0Irq
            flagsEnableFpga1Irq
            flagsEnableFpga2Irq
            flagsEnableFpga3Irq
            flagsEnableFpga4Irq
            flagsEnableFpga5Irq
            flagsEnableFpga6Irq
            flagsEnableFpga7Irq
            flagsEnableTtc01Irq
            flagsEnableTtc11Irq
            flagsEnableTtc21Irq
            flagsEnableDmac4Irq
            flagsEnableDmac5Irq
            flagsEnableDmac6Irq
            flagsEnableDmac7Irq
            flagsEnableUsb1Irq
            flagsEnableEthernet1Irq
            flagsEnableEthernet1WakeupIrq
            flagsEnableSdio1Irq
            flagsEnableI2c1Irq
            flagsEnableSpi1Irq
            flagsEnableUart1Irq
            flagsEnableCan1Irq
            flagsEnableFpga8Irq
            flagsEnableFpga9Irq
            flagsEnableFpga10Irq
            flagsEnableFpga11Irq
            flagsEnableFpga12Irq
            flagsEnableFpga13Irq
            flagsEnableFpga14Irq
            flagsEnableFpga15Irq
            flagsEnableParityIrq
        ) = object [
            "enable-cpu-0-irq"             .= flagsEnableCpu0Irq,
            "enable-cpu-1-irq"             .= flagsEnableCpu1Irq,
            "enable-l2-cache-irq"          .= flagsEnableL2CacheIrq,
            "enable-ocm-irq"               .= flagsEnableOcmIrq,
            "enable-pmu-0-irq"             .= flagsEnablePmu0Irq,
            "enable-pmu-1-irq"             .= flagsEnablePmu1Irq,
            "enable-xadc-irq"              .= flagsEnableXadcIrq,
            "enable-dvi-irq"               .= flagsEnableDviIrq,
            "enable-swdt-irq"              .= flagsEnableSwdtIrq,
            "enable-ttc-0-0-irq"           .= flagsEnableTtc00Irq,
            "enable-ttc-1-0-irq"           .= flagsEnableTtc10Irq,
            "enable-ttc-2-0-irq"           .= flagsEnableTtc20Irq,
            "enable-dmac-abort-irq"        .= flagsEnableDmacAbortIrq,
            "enable-dmac-0-irq"            .= flagsEnableDmac0Irq,
            "enable-dmac-1-irq"            .= flagsEnableDmac1Irq,
            "enable-dmac-2-irq"            .= flagsEnableDmac2Irq,
            "enable-dmac-3-irq"            .= flagsEnableDmac3Irq,
            "enable-smc-irq"               .= flagsEnableSmcIrq,
            "enable-quad-spi-irq"          .= flagsEnableQuadSpiIrq,
            "enable-gpio-irq"              .= flagsEnableGpioIrq,
            "enable-usb-0-irq"             .= flagsEnableUsb0Irq,
            "enable-ethernet-0-irq"        .= flagsEnableEthernet0Irq,
            "enable-ethernet-0-wakeup-irq" .= flagsEnableEthernet0WakeupIrq,
            "enable-sdio-0-irq"            .= flagsEnableSdio0Irq,
            "enable-i2c-0-irq"             .= flagsEnableI2c0Irq,
            "enable-spi-0-irq"             .= flagsEnableSpi0Irq,
            "enable-uart-0-irq"            .= flagsEnableUart0Irq,
            "enable-can-0-irq"             .= flagsEnableCan0Irq,
            "enable-fpga-0-irq"            .= flagsEnableFpga0Irq,
            "enable-fpga-1-irq"            .= flagsEnableFpga1Irq,
            "enable-fpga-2-irq"            .= flagsEnableFpga2Irq,
            "enable-fpga-3-irq"            .= flagsEnableFpga3Irq,
            "enable-fpga-4-irq"            .= flagsEnableFpga4Irq,
            "enable-fpga-5-irq"            .= flagsEnableFpga5Irq,
            "enable-fpga-6-irq"            .= flagsEnableFpga6Irq,
            "enable-fpga-7-irq"            .= flagsEnableFpga7Irq,
            "enable-ttc-0-1-irq"           .= flagsEnableTtc01Irq,
            "enable-ttc-1-1-irq"           .= flagsEnableTtc11Irq,
            "enable-ttc-2-1-irq"           .= flagsEnableTtc21Irq,
            "enable-dmac-4-irq"            .= flagsEnableDmac4Irq,
            "enable-dmac-5-irq"            .= flagsEnableDmac5Irq,
            "enable-dmac-6-irq"            .= flagsEnableDmac6Irq,
            "enable-dmac-7-irq"            .= flagsEnableDmac7Irq,
            "enable-usb-1-irq"             .= flagsEnableUsb1Irq,
            "enable-ethernet-1-irq"        .= flagsEnableEthernet1Irq,
            "enable-ethernet-1-wakeup-irq" .= flagsEnableEthernet1WakeupIrq,
            "enable-sdio-1-irq"            .= flagsEnableSdio1Irq,
            "enable-i2c-1-irq"             .= flagsEnableI2c1Irq,
            "enable-spi-1-irq"             .= flagsEnableSpi1Irq,
            "enable-uart-1-irq"            .= flagsEnableUart1Irq,
            "enable-can-1-irq"             .= flagsEnableCan1Irq,
            "enable-fpga-8-irq"            .= flagsEnableFpga8Irq,
            "enable-fpga-9-irq"            .= flagsEnableFpga9Irq,
            "enable-fpga-10-irq"           .= flagsEnableFpga10Irq,
            "enable-fpga-11-irq"           .= flagsEnableFpga11Irq,
            "enable-fpga-12-irq"           .= flagsEnableFpga12Irq,
            "enable-fpga-13-irq"           .= flagsEnableFpga13Irq,
            "enable-fpga-14-irq"           .= flagsEnableFpga14Irq,
            "enable-fpga-15-irq"           .= flagsEnableFpga15Irq,
            "enable-parity-irq"            .= flagsEnableParityIrq
        ]
