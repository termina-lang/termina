{-# LANGUAGE OverloadedStrings #-}

module Configuration.Platform where

import Data.Yaml
import Configuration.Platform.RTEMS5LEON3NEXYSA7
import Configuration.Platform.POSIXGCC
import Configuration.Platform.FreeRTOS10STM32L432XX

data Platform =
    POSIXGCC
    | RTEMS5LEON3NEXYSA7
    | FreeRTOS10STM32L432XX
    | TestPlatform
    deriving Eq

-- | The bit width of @usize@ on a platform, i.e. the width of C @size_t@ on the
-- target. Used only for the transpiler's static range and shift-amount checks;
-- the generated C uses @size_t@, which the C compiler sizes for the target.
-- POSIX-gcc is fixed at 64 (matching a 64-bit host, the normal case); a 32-bit
-- host would warrant a separate platform (e.g. @POSIXGCC32b@).
usizeWidth :: Platform -> Integer
usizeWidth POSIXGCC              = 64
usizeWidth RTEMS5LEON3NEXYSA7    = 32
usizeWidth FreeRTOS10STM32L432XX = 32
usizeWidth TestPlatform          = 32

-- | Whether the target requires naturally-aligned memory accesses, i.e. a
-- misaligned load/store traps or is penalized instead of being handled
-- transparently. On such targets, taking a reference to a member of a @packed@
-- struct is rejected: the reference would carry an under-aligned address whose
-- packed provenance is lost at the call boundary, so the callee emits an
-- aligned access (undefined behavior, MISRA-C:2023 Rule 1.3). Hosts that handle
-- misaligned accesses (x86) do not need the restriction.
strictAlignment :: Platform -> Bool
strictAlignment POSIXGCC              = False   -- x86 host: misaligned access is fine
strictAlignment RTEMS5LEON3NEXYSA7    = True    -- SPARC/LEON3: traps
strictAlignment FreeRTOS10STM32L432XX = True    -- Cortex-M: conservative
strictAlignment TestPlatform          = True

-- | The maximum number of significant initial characters a generated
-- identifier may have on a platform's toolchain, or @Nothing@ when the toolchain
-- treats all characters as significant. C11 guarantees only 31 significant
-- characters in an external identifier and 63 in an internal identifier or macro
-- name; a concrete toolchain may raise those limits or keep them. Every
-- currently supported platform uses a GCC-family compiler, which imposes no
-- limit, so the transpiler's identifier-length check never fires. Declaring the
-- limit here (rather than assuming it) turns the toolchain property into an
-- enforced check: a future platform whose toolchain caps significant length
-- states the cap here, and the generator then rejects any longer identifier at
-- generation time. A per-identifier cap at or below the limit is sufficient to
-- rule out significant-character collisions, so no pairwise analysis is needed.
maxIdentifierLength :: Platform -> Maybe Integer
maxIdentifierLength POSIXGCC              = Nothing
maxIdentifierLength RTEMS5LEON3NEXYSA7    = Nothing
maxIdentifierLength FreeRTOS10STM32L432XX = Nothing
maxIdentifierLength TestPlatform          = Nothing

data PlatformFlags = PlatformFlags {
    rtems5_leon3_nexysa7        :: RTEMS5LEON3NEXYSA7Flags,
    posix_gcc                :: POSIXGCCFlags,
    freertos10_stm32l432xx   :: FreeRTOS10STM32L432XXFlags
} deriving (Eq, Show)

defaultPlatformFlags :: PlatformFlags
defaultPlatformFlags = PlatformFlags {
    rtems5_leon3_nexysa7      = defaultRTEMS5LEON3NEXYSA7Flags,
    posix_gcc              = defaultPOSIXGCCFlags,
    freertos10_stm32l432xx = defaultFreeRTOS10STM32L432XXFlags
}

instance FromJSON PlatformFlags where
  parseJSON (Object o) =
    PlatformFlags <$>
    o .:? "rtems5-leon3-nexysa7"      .!= defaultRTEMS5LEON3NEXYSA7Flags <*>
    o .:? "posix-gcc"              .!= defaultPOSIXGCCFlags <*>
    o .:? "freertos10-stm32l432xx" .!= defaultFreeRTOS10STM32L432XXFlags
  parseJSON _ = fail "Expected configuration object"

instance Show Platform where
    show POSIXGCC = "posix-gcc"
    show RTEMS5LEON3NEXYSA7 = "rtems5-leon3-nexysa7"
    show FreeRTOS10STM32L432XX = "freertos-stm32l432xx"
    show TestPlatform = "test-platform"

instance ToJSON PlatformFlags where
    toJSON (
        PlatformFlags
            flagsRTEMSLEON3NEXYSA7
            flagsPOSIXGCC
            flagsFreeRTOS10STM32L432XX
        ) = object [
            "rtems5-leon3-nexysa7"      .= flagsRTEMSLEON3NEXYSA7,
            "posix-gcc"              .= flagsPOSIXGCC,
            "freertos10-stm32l432xx" .= flagsFreeRTOS10STM32L432XX
        ]

checkPlatform :: String -> Maybe Platform
checkPlatform "posix-gcc" = Just POSIXGCC
checkPlatform "rtems5-leon3-nexysa7" = Just RTEMS5LEON3NEXYSA7
checkPlatform "freertos10-stm32l432xx" = Just FreeRTOS10STM32L432XX
checkPlatform _ = Nothing

supportedPlatforms :: [(Platform, String)]
supportedPlatforms = [
        (POSIXGCC, "POSIX on GCC"),
        (RTEMS5LEON3NEXYSA7, "RTEMS version 5 for LEON3 Nexys A7 board"),
        (FreeRTOS10STM32L432XX, "FreeRTOS V10 for STM32L432XX microcontroller")
    ]
