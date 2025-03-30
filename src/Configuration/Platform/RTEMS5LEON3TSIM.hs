{-# LANGUAGE OverloadedStrings #-}

module Configuration.Platform.RTEMS5LEON3TSIM where
import Data.Yaml

data ProjectBuilder = None | Make deriving (Eq, Show)

instance FromJSON ProjectBuilder where
    parseJSON (String "none") = return None
    parseJSON (String "make") = return Make
    parseJSON _ = fail "Expected builder type"

instance ToJSON ProjectBuilder where
    toJSON None = String "none"
    toJSON Make = String "make"

data RTEMS5LEON3TSIMFlags = RTEMS5LEON3TSIMFlags {
    enableIrq0 :: !Bool,
    enableIrq1 :: !Bool,
    enableIrq2 :: !Bool,
    enableIrq3 :: !Bool,
    enableIrq4 :: !Bool,
    builder :: !ProjectBuilder
} deriving (Eq, Show)

defaultRTEMS5LEON3TSIMFlags :: RTEMS5LEON3TSIMFlags
defaultRTEMS5LEON3TSIMFlags = RTEMS5LEON3TSIMFlags {
    enableIrq0 = False,
    enableIrq1 = False,
    enableIrq2 = False,
    enableIrq3 = False,
    enableIrq4 = False,
    builder = None
}

instance FromJSON RTEMS5LEON3TSIMFlags where
  parseJSON (Object o) =
    RTEMS5LEON3TSIMFlags <$>
    o .:? "enable-irq-0" .!= False <*>
    o .:? "enable-irq-1" .!= False <*>
    o .:? "enable-irq-2" .!= False <*>
    o .:? "enable-irq-3" .!= False <*>
    o .:? "enable-irq-4" .!= False <*>
    o .:? "builder" .!= None
  parseJSON _ = fail "Expected configuration object"

instance ToJSON RTEMS5LEON3TSIMFlags where
    toJSON (
        RTEMS5LEON3TSIMFlags 
            flagsRTEMS5LEON3TSIMEnableIrq0
            flagsRTEMS5LEON3TSIMEnableIrq1
            flagsRTEMS5LEON3TSIMEnableIrq2
            flagsRTEMS5LEON3TSIMEnableIrq3
            flagsRTEMS5LEON3TSIMEnableIrq4
            flagsRTEMS5LEON3TSIMBuilder
        ) = object [
            "enable-irq-0" .= flagsRTEMS5LEON3TSIMEnableIrq0,
            "enable-irq-1" .= flagsRTEMS5LEON3TSIMEnableIrq1,
            "enable-irq-2" .= flagsRTEMS5LEON3TSIMEnableIrq2,
            "enable-irq-3" .= flagsRTEMS5LEON3TSIMEnableIrq3,
            "enable-irq-4" .= flagsRTEMS5LEON3TSIMEnableIrq4,
            "builder" .= flagsRTEMS5LEON3TSIMBuilder
        ]