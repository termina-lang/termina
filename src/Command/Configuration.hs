{-# LANGUAGE OverloadedStrings #-}

module Command.Configuration (
    TerminaConfig(..),
    ProjectProfile(..),
    loadConfig,
    serializeConfig,
    defaultConfig
) where

import Command.Utils

import qualified Data.Text as T
import Data.Yaml

import System.FilePath
import System.Exit
import Generator.Platform.Configuration

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
            prjBuild
            prjPlatformFlags
        ) = object [
            "name" .= prjName,
            "platform" .= prjPlatform,
            "app-folder" .= prjAppFolder,
            "app-file" .= prjAppFilename,
            "source-modules" .= prjSourceModulesFolder,
            "output-folder" .= prjOutputFolder,
            "profile" .= prjBuild,
            "platform-flags" .= prjPlatformFlags
        ]

-- | Load "termina.yaml" configuration file
loadConfig :: IO TerminaConfig
loadConfig = do
    config <- decodeFileEither "termina.yaml"
    case config of
        Left (InvalidYaml (Just (YamlException err))) -> die . errorMessage $ err
        Left err -> die . errorMessage $ show err
        Right c -> return c

serializeConfig :: FilePath -> TerminaConfig -> IO ()
serializeConfig filePath config = do
    encodeFile (filePath </> "termina" <.> "yaml") config

defaultConfig :: String -> Platform -> TerminaConfig
defaultConfig projectName plt = TerminaConfig {
    name = T.pack projectName,
    platform = T.pack $ show plt,
    appFolder = "app",
    appFilename = "app",
    sourceModulesFolder = "src",
    outputFolder = "output",
    profile = Release,
    platformFlags = defaultPlatformFlags
}