{-# LANGUAGE OverloadedStrings #-}

module Configuration.Configuration (
    TerminaConfig(..),
    ProjectProfile(..),
    ProjectBuilder(..),
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

data ProjectBuilder = None | Make deriving (Eq, Show)

instance FromJSON ProjectBuilder where
    parseJSON (String "none") = return None
    parseJSON (String "make") = return Make
    parseJSON _ = fail "Expected builder type"

instance ToJSON ProjectBuilder where
    toJSON None = String "none"
    toJSON Make = String "make"

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
    enableSystemExcept :: !Bool,
    builder :: !ProjectBuilder,
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
    o .:?  "enable-system-except" .!= False <*>
    o .:   "builder" <*>
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
            prjEnableSystemExcept
            prjBuilder
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
            <> if prjEnableSystemPort then ["enable-system-port" .= prjEnableSystemPort] else []
            <> if prjEnableSystemExcept then ["enable-system-except" .= prjEnableSystemExcept] else []
            <> if prjBuilder /= None then ["builder" .= prjBuilder] else []
            -- We only serialize the platform flags corresponding to the selected platform
            <> case prjPlatform of
                "rtems5-leon3-qemu" -> ["platform-flags" .= object ["rtems5-leon3-qemu" .= rtems5_leon3_qemu prjPlatformFlags]]
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
    enableSystemExcept = False,
    builder = Make,
    platformFlags = defaultPlatformFlags
}