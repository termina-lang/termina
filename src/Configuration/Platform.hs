{-# LANGUAGE OverloadedStrings #-}

module Configuration.Platform where

import Data.Yaml
import qualified Data.Map.Strict as M
import Core.AST (Identifier)
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

-- | The bit width of C @int@ on the target, which is what decides whether an
-- operation on a narrower type is carried out in that narrower type or in
-- @int@: C promotes an operand whose type is narrower than @int@, so the
-- result comes back wider than the type holds and the generator masks it back.
-- A type as wide as @int@, or wider, is not promoted and wraps on its own.
intWidth :: Platform -> Integer
intWidth POSIXGCC              = 32
intWidth RTEMS5LEON3NEXYSA7    = 32
intWidth FreeRTOS10STM32L432XX = 32
intWidth TestPlatform          = 32

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

-- | The interrupts the platform exposes, each one with the vector it is wired
-- to. It lives here, beside the rest of what the transpiler knows about a
-- target, so that a single file answers what a platform is.
getPlatformInterruptMap :: Platform -> M.Map Identifier Integer
getPlatformInterruptMap POSIXGCC =
    M.fromList [("kbd_irq", 0)]
getPlatformInterruptMap RTEMS5LEON3NEXYSA7 =
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

-- | The size of the interrupt table of the platform, which the runtime
-- declares and indexes by vector. It is therefore the highest vector plus one
-- and not the number of interrupts: a platform may leave gaps, as the STM32
-- does with 60 interrupts over a table of 83. Derived from the map rather than
-- declared, so that adding an interrupt cannot leave the table short.
interruptTableSize :: Platform -> Integer
interruptTableSize plt =
    case M.elems (getPlatformInterruptMap plt) of
        [] -> 0
        vectors -> maximum vectors + 1

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
