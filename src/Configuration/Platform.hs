{-# LANGUAGE OverloadedStrings #-}

module Configuration.Platform where

import Data.Yaml
import qualified Data.Text as T
import Configuration.Platform.RTEMS5NoelSpike
import Configuration.Platform.RTEMS5LEON3TSIM

data Platform = 
    RTEMS5NoelSpike
    | RTEMS5LEON3TSIM
    | TestPlatform
    deriving Eq

data PlatformFlags = PlatformFlags {
    rtems5_noel_spike :: RTEMS5NoelSpikeFlags,
    rtems5_leon3_tsim :: RTEMS5LEON3TSIMFlags
} deriving (Eq, Show)

defaultPlatformFlags :: PlatformFlags
defaultPlatformFlags = PlatformFlags {
    rtems5_noel_spike = defaultRTEMS5NoelSpikeFlags,
    rtems5_leon3_tsim = defaultRTEMS5LEON3TSIMFlags
}

instance FromJSON PlatformFlags where
  parseJSON (Object o) =
    PlatformFlags <$>
    o .:? "rtems5-noel-spike" .!= defaultRTEMS5NoelSpikeFlags <*>
    o .:? "rtems5-leon3-tsim" .!= defaultRTEMS5LEON3TSIMFlags
  parseJSON _ = fail "Expected configuration object"

instance Show Platform where
    show RTEMS5NoelSpike = "rtems5-noel-spike"
    show RTEMS5LEON3TSIM = "rtems5-leon3-tsim"
    show TestPlatform = "test-platform"

instance ToJSON PlatformFlags where
    toJSON (
        PlatformFlags 
            flagsRTEMSNoelSpike
            flagsRTEMSLEON3TSIM
        ) = object [
            "rtems5-noel-spike" .= flagsRTEMSNoelSpike,
            "rtems5-leon3-tsim" .= flagsRTEMSLEON3TSIM
        ]

checkPlatform :: T.Text -> Maybe Platform
checkPlatform "rtems5-noel-spike" = Just RTEMS5NoelSpike
checkPlatform "rtems5-leon3-tsim" = Just RTEMS5LEON3TSIM
checkPlatform _ = Nothing

supportedPlatforms :: [(Platform, String)]
supportedPlatforms = [
        (RTEMS5NoelSpike, "RTEMS version 5 for NOEL-Spike simulator"),
        (RTEMS5LEON3TSIM, "RTEMS version 5 for LEON3 TSIM simulator")
    ]
