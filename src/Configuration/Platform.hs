{-# LANGUAGE OverloadedStrings #-}

module Configuration.Platform where

import Data.Yaml
import Configuration.Platform.RTEMS5LEON3QEMU
import Configuration.Platform.POSIXGCC

data Platform = 
    POSIXGCC
    | RTEMS5LEON3QEMU
    | TestPlatform
    deriving Eq

data PlatformFlags = PlatformFlags {
    rtems5_leon3_qemu :: RTEMS5LEON3QEMUFlags,
    posix_gcc :: POSIXGCCFlags
} deriving (Eq, Show)

defaultPlatformFlags :: PlatformFlags
defaultPlatformFlags = PlatformFlags {
    rtems5_leon3_qemu = defaultRTEMS5LEON3QEMUFlags,
    posix_gcc = defaultPOSIXGCCFlags
}

instance FromJSON PlatformFlags where
  parseJSON (Object o) =
    PlatformFlags <$>
    o .:? "rtems5-leon3-qemu" .!= defaultRTEMS5LEON3QEMUFlags <*>
    o .:? "posix-gcc" .!= defaultPOSIXGCCFlags
  parseJSON _ = fail "Expected configuration object"

instance Show Platform where
    show POSIXGCC = "posix-gcc"
    show RTEMS5LEON3QEMU = "rtems5-leon3-qemu"
    show TestPlatform = "test-platform"

instance ToJSON PlatformFlags where
    toJSON (
        PlatformFlags 
            flagsRTEMSLEON3QEMU
            flagsPOSIXGCC
        ) = object [
            "rtems5-leon3-qemu" .= flagsRTEMSLEON3QEMU,
            "posix-gcc" .= flagsPOSIXGCC
        ]

checkPlatform :: String -> Maybe Platform
checkPlatform "posix-gcc" = Just POSIXGCC
checkPlatform "rtems5-leon3-qemu" = Just RTEMS5LEON3QEMU
checkPlatform _ = Nothing

supportedPlatforms :: [(Platform, String)]
supportedPlatforms = [
        (POSIXGCC, "POSIX on GCC"),
        (RTEMS5LEON3QEMU, "RTEMS version 5 for LEON3 QEMU simulator")
    ]
