{-# LANGUAGE OverloadedStrings #-}

module Command.Configuration (
    TerminaYaml(..),
    loadConfig,
    serializeConfig,
    defaultConfig
) where

import Command.Utils
import Generator.Platform

import qualified Data.Text as T
import Data.Yaml

import System.FilePath
import System.Exit

-- | Data type for the "termina.yaml" configuration file
data TerminaYaml =
  TerminaYaml {
    name :: T.Text,
    platform :: T.Text,
    appFolder :: FilePath,
    appFilename :: FilePath,
    sourceModulesFolder :: FilePath,
    outputFolder :: FilePath,
    platformFlags :: PlatformFlags
  } deriving (Eq, Show)

-- | Instance for parsing the "termina.yaml" configuration file
instance FromJSON TerminaYaml where
  parseJSON (Object v) =
    TerminaYaml <$>
    v .:   "name"           <*>
    v .:   "platform"       <*>
    v .:   "app-folder"     <*>
    v .:   "app-file"       <*>
    v .:   "source-modules" <*>
    v .:   "output-folder"  <*>
    v .:?  "platform-flags" .!= defaultPlatformFlags
  parseJSON _ = fail "Expected configuration object"

instance ToJSON TerminaYaml where
    toJSON (
        TerminaYaml 
            prjName 
            prjPlatform 
            prjAppFolder 
            prjAppFilename 
            prjSourceModulesFolder 
            prjOutputFolder
            prjPlatformFlags
        ) = object [
            "name" .= prjName,
            "platform" .= prjPlatform,
            "app-folder" .= prjAppFolder,
            "app-file" .= prjAppFilename,
            "source-modules" .= prjSourceModulesFolder,
            "output-folder" .= prjOutputFolder,
            "platform-flags" .= prjPlatformFlags
        ]

-- | Load "termina.yaml" configuration file
loadConfig :: IO TerminaYaml
loadConfig = do
    config <- decodeFileEither "termina.yaml"
    case config of
        Left (InvalidYaml (Just (YamlException err))) -> die . errorMessage $ err
        Left err -> die . errorMessage $ show err
        Right c -> return c

serializeConfig :: FilePath -> TerminaYaml -> IO ()
serializeConfig filePath config = do
    encodeFile (filePath </> "termina" <.> "yaml") config

defaultConfig :: String -> Platform -> TerminaYaml
defaultConfig projectName plt = TerminaYaml {
    name = T.pack projectName,
    platform = T.pack $ show plt,
    appFolder = "app",
    appFilename = "app",
    sourceModulesFolder = "src",
    outputFolder = "output",
    platformFlags = defaultPlatformFlags
}