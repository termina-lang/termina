{-# LANGUAGE OverloadedStrings #-}

module Configuration.Configuration (
    TerminaConfig(..),
    ProjectProfile(..),
    defaultConfig
) where

import qualified Data.Text as T
import Data.Yaml

import Configuration.Platform

data ProjectProfile = Debug | Release deriving (Eq, Show)

instance FromJSON ProjectProfile where
    parseJSON (String "debug") = return Debug
    parseJSON (String "release") = return Release
    parseJSON _ = fail "Expected profile type"
  
instance ToJSON ProjectProfile where
    toJSON Debug = String "debug"
    toJSON Release = String "release"

-- | Data type for the "termina.yaml" configuration file
data TerminaConfig =
  TerminaConfig {
    name :: !T.Text,
    platform :: !T.Text,
    appFolder :: !FilePath,
    appFilename :: !FilePath,
    sourceModulesFolder :: !FilePath,
    outputFolder :: !FilePath,
    profile :: !ProjectProfile,
    enableSystemInit :: !Bool,
    enableSystemPort :: !Bool,
    platformFlags :: !PlatformFlags
  } deriving (Eq, Show)

-- | Instance for parsing the "termina.yaml" configuration file
instance FromJSON TerminaConfig where
  parseJSON (Object o) =
    TerminaConfig <$>
    o .:   "name"           <*>
    o .:   "platform"       <*>
    o .:   "app-folder"     <*>
    o .:   "app-file"       <*>
    o .:   "source-modules" <*>
    o .:   "output-folder"  <*>
    o .:?  "profile" .!= Release <*>         
    o .:?  "enable-system-init" .!= False <*>
    o .:?  "enable-system-port" .!= False <*>
    o .:?  "platform-flags" .!= defaultPlatformFlags
  parseJSON _ = fail "Expected configuration object"

instance ToJSON TerminaConfig where
    toJSON (
        TerminaConfig 
            prjName 
            prjPlatform 
            prjAppFolder 
            prjAppFilename 
            prjSourceModulesFolder 
            prjOutputFolder
            prjProfile
            prjEnableSystemInit
            prjEnableSystemPort
            prjPlatformFlags
        ) = object $ [
            "name" .= prjName,
            "platform" .= prjPlatform,
            "app-folder" .= prjAppFolder,
            "app-file" .= prjAppFilename,
            "source-modules" .= prjSourceModulesFolder,
            "output-folder" .= prjOutputFolder
        ]   -- We only serialize the profile if it is different from the default value
            <> case prjProfile of
                Debug -> ["profile" .= prjProfile]
                _ -> []
            -- We only serialize the enable-system-init flag if it is different from the default value
            <> if prjEnableSystemInit then ["enable-system-init" .= prjEnableSystemInit] else []
            <> if prjEnableSystemInit then ["enable-system-port" .= prjEnableSystemPort] else []
            -- We only serialize the platform flags corresponding to the selected platform
            <> case prjPlatform of
                "rtems5-noel-spike" -> ["platform-flags" .= object ["rtems5-noel-spike" .= rtems5_noel_spike prjPlatformFlags]]
                "rtems5-leon3-tsim" -> ["platform-flags" .= object ["rtems5-leon3-tsim" .= rtems5_leon3_tsim prjPlatformFlags]]
                _ -> []

defaultConfig :: String -> Platform -> TerminaConfig
defaultConfig projectName plt = TerminaConfig {
    name = T.pack projectName,
    platform = T.pack $ show plt,
    appFolder = "app",
    appFilename = "app",
    sourceModulesFolder = "src",
    outputFolder = "output",
    profile = Release,
    enableSystemInit = False,
    enableSystemPort = False,
    platformFlags = defaultPlatformFlags
}