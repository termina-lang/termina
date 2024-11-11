{-# LANGUAGE OverloadedStrings #-}

module Generator.Platform.RTEMS5NoelSpike where
import Data.Yaml

data ProjectBuilder = None | Make deriving (Eq, Show)

instance FromJSON ProjectBuilder where
    parseJSON (String "none") = return None
    parseJSON (String "make") = return Make
    parseJSON _ = fail "Expected builder type"

instance ToJSON ProjectBuilder where
    toJSON None = String "none"
    toJSON Make = String "make"

data RTEMS5NoelSpikeFlags = RTEMS5NoelSpikeFlags {
    enableIrq1 :: !Bool,
    enableIrq2 :: !Bool,
    enableIrq3 :: !Bool,
    enableIrq4 :: !Bool,
    builder :: !ProjectBuilder
} deriving (Eq, Show)

defaultRTEMS5NoelSpikeFlags :: RTEMS5NoelSpikeFlags
defaultRTEMS5NoelSpikeFlags = RTEMS5NoelSpikeFlags {
    enableIrq1 = False,
    enableIrq2 = False,
    enableIrq3 = False,
    enableIrq4 = False,
    builder = None
}

instance FromJSON RTEMS5NoelSpikeFlags where
  parseJSON (Object o) =
    RTEMS5NoelSpikeFlags <$>
    o .:? "enable-irq-1" .!= False <*>
    o .:? "enable-irq-2" .!= False <*>
    o .:? "enable-irq-3" .!= False <*>
    o .:? "enable-irq-4" .!= False <*>
    o .:? "builder" .!= None
  parseJSON _ = fail "Expected configuration object"

instance ToJSON RTEMS5NoelSpikeFlags where
    toJSON (
        RTEMS5NoelSpikeFlags 
            flagsRTEMSNoelSpikeEnableIrq1
            flagsRTEMSNoelSpikeEnableIrq2
            flagsRTEMSNoelSpikeEnableIrq3
            flagsRTEMSNoelSpikeEnableIrq4
            flagsRTEMSNoelSpikeBuilder
        ) = object [
            "enable-irq-1" .= flagsRTEMSNoelSpikeEnableIrq1,
            "enable-irq-2" .= flagsRTEMSNoelSpikeEnableIrq2,
            "enable-irq-3" .= flagsRTEMSNoelSpikeEnableIrq3,
            "enable-irq-4" .= flagsRTEMSNoelSpikeEnableIrq4,
            "builder" .= flagsRTEMSNoelSpikeBuilder
        ]