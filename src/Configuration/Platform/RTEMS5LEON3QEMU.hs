{-# LANGUAGE OverloadedStrings #-}

module Configuration.Platform.RTEMS5LEON3QEMU where
import Data.Yaml

data RTEMS5LEON3QEMUFlags = RTEMS5LEON3QEMUFlags {
    enableIrq0 :: !Bool,
    enableIrq1 :: !Bool,
    enableIrq2 :: !Bool,
    enableIrq3 :: !Bool,
    enableIrq4 :: !Bool,
    enableIrq5 :: !Bool,
    enableIrq6 :: !Bool,
    enableIrq7 :: !Bool,
    enableIrq8 :: !Bool,
    enableIrq9 :: !Bool,
    enableIrq10 :: !Bool,
    enableIrq11 :: !Bool,
    enableIrq12 :: !Bool,
    enableIrq13 :: !Bool,
    enableIrq14 :: !Bool,
    enableIrq15 :: !Bool
} deriving (Eq, Show)

defaultRTEMS5LEON3QEMUFlags :: RTEMS5LEON3QEMUFlags
defaultRTEMS5LEON3QEMUFlags = RTEMS5LEON3QEMUFlags {
    enableIrq0 = False,
    enableIrq1 = False,
    enableIrq2 = False,
    enableIrq3 = False,
    enableIrq4 = False,
    enableIrq5 = False,
    enableIrq6 = False,
    enableIrq7 = False,
    enableIrq8 = False,
    enableIrq9 = False,
    enableIrq10 = False,
    enableIrq11 = False,
    enableIrq12 = False,
    enableIrq13 = False,
    enableIrq14 = False,
    enableIrq15 = False
}

instance FromJSON RTEMS5LEON3QEMUFlags where
  parseJSON (Object o) =
    RTEMS5LEON3QEMUFlags <$>
    o .:? "enable-irq-0" .!= False <*>
    o .:? "enable-irq-1" .!= False <*>
    o .:? "enable-irq-2" .!= False <*>
    o .:? "enable-irq-3" .!= False <*>
    o .:? "enable-irq-4" .!= False <*>
    o .:? "enable-irq-5" .!= False <*>
    o .:? "enable-irq-6" .!= False <*>
    o .:? "enable-irq-7" .!= False <*>
    o .:? "enable-irq-8" .!= False <*>
    o .:? "enable-irq-9" .!= False <*>
    o .:? "enable-irq-10" .!= False <*>
    o .:? "enable-irq-11" .!= False <*>
    o .:? "enable-irq-12" .!= False <*>
    o .:? "enable-irq-13" .!= False <*>
    o .:? "enable-irq-14" .!= False <*>
    o .:? "enable-irq-15" .!= False
  parseJSON _ = fail "Expected configuration object"

instance ToJSON RTEMS5LEON3QEMUFlags where
    toJSON (
        RTEMS5LEON3QEMUFlags 
            flagsRTEMS5LEON3QEMUEnableIrq0
            flagsRTEMS5LEON3QEMUEnableIrq1
            flagsRTEMS5LEON3QEMUEnableIrq2
            flagsRTEMS5LEON3QEMUEnableIrq3
            flagsRTEMS5LEON3QEMUEnableIrq4
            flagsRTEMS5LEON3QEMUEnableIrq5
            flagsRTEMS5LEON3QEMUEnableIrq6
            flagsRTEMS5LEON3QEMUEnableIrq7
            flagsRTEMS5LEON3QEMUEnableIrq8
            flagsRTEMS5LEON3QEMUEnableIrq9
            flagsRTEMS5LEON3QEMUEnableIrq10
            flagsRTEMS5LEON3QEMUEnableIrq11
            flagsRTEMS5LEON3QEMUEnableIrq12
            flagsRTEMS5LEON3QEMUEnableIrq13
            flagsRTEMS5LEON3QEMUEnableIrq14
            flagsRTEMS5LEON3QEMUEnableIrq15
        ) = object [
            "enable-irq-0" .= flagsRTEMS5LEON3QEMUEnableIrq0,
            "enable-irq-1" .= flagsRTEMS5LEON3QEMUEnableIrq1,
            "enable-irq-2" .= flagsRTEMS5LEON3QEMUEnableIrq2,
            "enable-irq-3" .= flagsRTEMS5LEON3QEMUEnableIrq3,
            "enable-irq-4" .= flagsRTEMS5LEON3QEMUEnableIrq4,
            "enable-irq-5" .= flagsRTEMS5LEON3QEMUEnableIrq5,
            "enable-irq-6" .= flagsRTEMS5LEON3QEMUEnableIrq6,
            "enable-irq-7" .= flagsRTEMS5LEON3QEMUEnableIrq7,
            "enable-irq-8" .= flagsRTEMS5LEON3QEMUEnableIrq8,
            "enable-irq-9" .= flagsRTEMS5LEON3QEMUEnableIrq9,
            "enable-irq-10" .= flagsRTEMS5LEON3QEMUEnableIrq10,
            "enable-irq-11" .= flagsRTEMS5LEON3QEMUEnableIrq11,
            "enable-irq-12" .= flagsRTEMS5LEON3QEMUEnableIrq12,
            "enable-irq-13" .= flagsRTEMS5LEON3QEMUEnableIrq13,
            "enable-irq-14" .= flagsRTEMS5LEON3QEMUEnableIrq14,
            "enable-irq-15" .= flagsRTEMS5LEON3QEMUEnableIrq15
        ]