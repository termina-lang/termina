{-# LANGUAGE OverloadedStrings #-}

module Generator.Platform where

import qualified Data.Text as T
import Generator.Platform.RTEMS5NoelSpike

import Data.Yaml

data Platform = 
    RTEMS5NoelSpike
    | TestPlatform
    deriving Eq

newtype PlatformFlags = PlatformFlags {
    rtems5_noel_spike :: RTEMS5NoelSpikeFlags
} deriving (Eq, Show)

defaultPlatformFlags :: PlatformFlags
defaultPlatformFlags = PlatformFlags {
    rtems5_noel_spike = defaultRTEMS5NoelSpikeFlags
}

instance FromJSON PlatformFlags where
  parseJSON (Object o) =
    PlatformFlags <$>
    o .:? "rtems5-noel-spike" .!= defaultRTEMS5NoelSpikeFlags
  parseJSON _ = fail "Expected configuration object"

instance Show Platform where
    show RTEMS5NoelSpike = "rtems5-noel-spike"
    show TestPlatform = "test-platform"

instance ToJSON PlatformFlags where
    toJSON (
        PlatformFlags 
            flagsRTEMSNoelSpike
        ) = object [
            "rtems5-noel-spike" .= flagsRTEMSNoelSpike
        ]

checkPlatform :: T.Text -> Maybe Platform
checkPlatform "rtems5-noel-spike" = Just RTEMS5NoelSpike
checkPlatform _ = Nothing

supportedPlatforms :: [(Platform, String)]
supportedPlatforms = [
        (RTEMS5NoelSpike, "RTEMS version 5 for NOEL-Spike simulator")
    ]