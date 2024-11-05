{-# LANGUAGE OverloadedStrings #-}

module Generator.Platform.RTEMS5NoelSpike where
import Data.Yaml

data RTEMS5NoelSpikeFlags = RTEMS5NoelSpikeFlags {
    enableIrq1 :: !Bool,
    enableIrq2 :: !Bool,
    enableIrq3 :: !Bool,
    enableIrq4 :: !Bool
} deriving (Eq, Show)

defaultRTEMS5NoelSpikeFlags :: RTEMS5NoelSpikeFlags
defaultRTEMS5NoelSpikeFlags = RTEMS5NoelSpikeFlags {
    enableIrq1 = False,
    enableIrq2 = False,
    enableIrq3 = False,
    enableIrq4 = False
}

instance FromJSON RTEMS5NoelSpikeFlags where
  parseJSON (Object o) =
    RTEMS5NoelSpikeFlags <$>
    o .:? "enable-irq-1" .!= False <*>
    o .: "enable-irq-2" .!= False <*>
    o .: "enable-irq-3" .!= False <*>
    o .: "enable-irq-4" .!= False
  parseJSON _ = fail "Expected configuration object"

instance ToJSON RTEMS5NoelSpikeFlags where
    toJSON (
        RTEMS5NoelSpikeFlags 
            flagsRTEMSNoelSpikeEnableIrq1
            flagsRTEMSNoelSpikeEnableIrq2
            flagsRTEMSNoelSpikeEnableIrq3
            flagsRTEMSNoelSpikeEnableIrq4
        ) = object [
            "enable-irq-1" .= flagsRTEMSNoelSpikeEnableIrq1,
            "enable-irq-2" .= flagsRTEMSNoelSpikeEnableIrq2,
            "enable-irq-3" .= flagsRTEMSNoelSpikeEnableIrq3,
            "enable-irq-4" .= flagsRTEMSNoelSpikeEnableIrq4
        ]