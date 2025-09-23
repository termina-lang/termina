{-# LANGUAGE OverloadedStrings #-}

module Configuration.Platform.POSIXGCC where
import Data.Yaml

newtype POSIXGCCFlags = POSIXGCCFlags {
    enableKbdIrq :: Bool
} deriving (Eq, Show)

defaultPOSIXGCCFlags :: POSIXGCCFlags
defaultPOSIXGCCFlags = POSIXGCCFlags {
    enableKbdIrq = False
}

instance FromJSON POSIXGCCFlags where
  parseJSON (Object o) =
    POSIXGCCFlags <$>
    o .:? "enable-kbd-irq" .!= False
  parseJSON _ = fail "Expected configuration object"

instance ToJSON POSIXGCCFlags where
    toJSON (
        POSIXGCCFlags 
            flagsPOSIXGCCEnableKbdIrq
        ) = object [
            "enable-kbd-irq" .= flagsPOSIXGCCEnableKbdIrq
        ]