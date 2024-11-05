{-# LANGUAGE OverloadedStrings #-}

module Generator.Platform where

import qualified Data.Text as T
import Core.AST
import Semantic.Types
import ControlFlow.Architecture.Types
import qualified Data.Map as M
import ControlFlow.Architecture (emptyTerminaProgArch)
import Utils.Annotations ( Location(Internal), Located(Located) )
import Generator.Platform.RTEMS5NoelSpike

import Data.Yaml

data Platform = 
    RTEMS5NoelSpike
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

getPlatformInitialGlobalEnv :: Platform -> [(Identifier, Located (GEntry SemanticAnn))]
getPlatformInitialGlobalEnv RTEMS5NoelSpike = 
    [
       ("irq_1", Located (GGlob (TGlobal EmitterClass "Interrupt")) Internal),
       ("irq_2", Located (GGlob (TGlobal EmitterClass "Interrupt")) Internal),
       ("irq_3", Located (GGlob (TGlobal EmitterClass "Interrupt")) Internal),
       ("irq_4", Located (GGlob (TGlobal EmitterClass "Interrupt")) Internal)
    ]

getPlatformInitialProgram :: Platform -> TerminaProgArch SemanticAnn
getPlatformInitialProgram RTEMS5NoelSpike = 
    emptyTerminaProgArch {
        emitters = M.union (emitters emptyTerminaProgArch) $ M.fromList [
            ("irq_1", TPInterruptEmittter "irq_1" (Located (GTy (TGlobal EmitterClass "Interrupt")) Internal)),
            ("irq_2", TPInterruptEmittter "irq_2" (Located (GTy (TGlobal EmitterClass "Interrupt")) Internal)),
            ("irq_3", TPInterruptEmittter "irq_3" (Located (GTy (TGlobal EmitterClass "Interrupt")) Internal)),
            ("irq_4", TPInterruptEmittter "irq_4" (Located (GTy (TGlobal EmitterClass "Interrupt")) Internal))
        ]
    }