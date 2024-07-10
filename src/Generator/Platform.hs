{-# LANGUAGE OverloadedStrings #-}

module Generator.Platform where

import qualified Data.Text as T

data SupportedPlatform = 
    RTEMS5NoelSpike

instance Show SupportedPlatform where
    show RTEMS5NoelSpike = "rtems5-noel-spike"

checkPlatform :: T.Text -> Maybe SupportedPlatform
checkPlatform "rtems5-noel-spike" = Just RTEMS5NoelSpike
checkPlatform _ = Nothing