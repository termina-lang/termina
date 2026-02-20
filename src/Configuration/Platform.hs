{-# LANGUAGE OverloadedStrings #-}

module Configuration.Platform where

import Data.Yaml
import Configuration.Platform.RTEMS5LEON3QEMU
import Configuration.Platform.POSIXGCC
import Configuration.Platform.FreeRTOS10STM32L432XX

data Platform = 
    POSIXGCC
    | RTEMS5LEON3QEMU
    | FreeRTOS10STM32L432XX
    | TestPlatform
    deriving Eq

data PlatformFlags = PlatformFlags {
    rtems5_leon3_qemu        :: RTEMS5LEON3QEMUFlags,
    posix_gcc                :: POSIXGCCFlags,
    freertos10_stm32l432xx   :: FreeRTOS10STM32L432XXFlags
} deriving (Eq, Show)

defaultPlatformFlags :: PlatformFlags
defaultPlatformFlags = PlatformFlags {
    rtems5_leon3_qemu      = defaultRTEMS5LEON3QEMUFlags,
    posix_gcc              = defaultPOSIXGCCFlags,
    freertos10_stm32l432xx = defaultFreeRTOS10STM32L432XXFlags
}

instance FromJSON PlatformFlags where
  parseJSON (Object o) =
    PlatformFlags <$>
    o .:? "rtems5-leon3-qemu"      .!= defaultRTEMS5LEON3QEMUFlags <*>
    o .:? "posix-gcc"              .!= defaultPOSIXGCCFlags <*>
    o .:? "freertos10-stm32l432xx" .!= defaultFreeRTOS10STM32L432XXFlags
  parseJSON _ = fail "Expected configuration object"

instance Show Platform where
    show POSIXGCC = "posix-gcc"
    show RTEMS5LEON3QEMU = "rtems5-leon3-qemu"
    show FreeRTOS10STM32L432XX = "freertos-stm32l432xx"
    show TestPlatform = "test-platform"

instance ToJSON PlatformFlags where
    toJSON (
        PlatformFlags
            flagsRTEMSLEON3QEMU
            flagsPOSIXGCC
            flagsFreeRTOS10STM32L432XX
        ) = object [
            "rtems5-leon3-qemu"      .= flagsRTEMSLEON3QEMU,
            "posix-gcc"              .= flagsPOSIXGCC,
            "freertos10-stm32l432xx" .= flagsFreeRTOS10STM32L432XX
        ]

checkPlatform :: String -> Maybe Platform
checkPlatform "posix-gcc" = Just POSIXGCC
checkPlatform "rtems5-leon3-qemu" = Just RTEMS5LEON3QEMU
checkPlatform "freertos10-stm32l432xx" = Just FreeRTOS10STM32L432XX
checkPlatform _ = Nothing

supportedPlatforms :: [(Platform, String)]
supportedPlatforms = [
        (POSIXGCC, "POSIX on GCC"),
        (RTEMS5LEON3QEMU, "RTEMS version 5 for LEON3 QEMU simulator"),
        (FreeRTOS10STM32L432XX, "FreeRTOS V10 for STM32L432XX microcontroller")
    ]
