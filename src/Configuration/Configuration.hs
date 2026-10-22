{-# LANGUAGE OverloadedStrings #-}

module Configuration.Configuration (
    TerminaConfig(..),
    ExternConfig(..),
    emptyExternConfig,
    ProjectProfile(..),
    ProjectBuilder(..),
    defaultConfig,
    defaultSysPrintOutputBufferSize,
    defaultSysReadInputBufferSize,
    defaultMicrosecondsPerTick,
    appFolder,
    appFilename
) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Yaml

import Configuration.Platform

data ProjectProfile = Debug | Release deriving (Eq, Show)

-- | The C of the project, in the "extern" folder: the sources compiled with
-- the generated code, and the precompiled libraries of each platform. The
-- paths are relative to the "extern" folder.
data ExternConfig = ExternConfig {
    externSources :: ![FilePath],
    externLibraries :: !(M.Map T.Text [FilePath])
  } deriving (Eq, Show)

emptyExternConfig :: ExternConfig
emptyExternConfig = ExternConfig [] M.empty

instance FromJSON ExternConfig where
  parseJSON (Object o) =
    ExternConfig <$>
    o .:? "sources" .!= [] <*>
    o .:? "libraries" .!= M.empty
  parseJSON _ = fail "Expected extern object"

instance ToJSON ExternConfig where
  toJSON (ExternConfig srcs libs) = object $
    ["sources" .= srcs | not (null srcs)] <> ["libraries" .= libs | not (M.null libs)]

defaultSysPrintOutputBufferSize, defaultSysReadInputBufferSize :: Integer
defaultSysPrintOutputBufferSize = 256
defaultSysReadInputBufferSize = 256

-- | Length of the tick of the system clock in microseconds. The runtime
-- derives the ticks per second as 1000000 divided by it, so it has to divide
-- 1000000 exactly.
defaultMicrosecondsPerTick :: Integer
defaultMicrosecondsPerTick = 10000

-- | Folder that holds the application module and name of the module itself.
-- They are fixed and not configurable: the application module is always
-- "app/app.fin" and its generated header is always "app.h", guarded by
-- APP_H__. A configurable name could collide with the headers that the
-- generator emits on its own, such as "config.h".
appFolder, appFilename :: FilePath
appFolder = "app"
appFilename = "app"

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

-- | The configuration of one deployment, the application or a test on one
-- platform, which "Configuration.Manifest" derives from "termina.yaml"
data TerminaConfig =
  TerminaConfig {
    name :: !T.Text,
    platform :: !T.Text,
    sourceModulesFolder :: !FilePath,
    outputFolder :: !FilePath,
    efpFolder :: !FilePath,
    profile :: !ProjectProfile,
    sysPrintOutputBufferSize :: !Integer,
    sysReadInputBufferSize :: !Integer,
    microsecondsPerTick :: !Integer,
    builder :: !ProjectBuilder
  } deriving (Eq, Show)

defaultConfig :: String -> Platform -> TerminaConfig
defaultConfig projectName plt = TerminaConfig {
    name = T.pack projectName,
    platform = T.pack $ show plt,
    sourceModulesFolder = "src",
    outputFolder = "output",
    efpFolder = "efp",
    profile = Release,
    sysPrintOutputBufferSize = defaultSysPrintOutputBufferSize,
    sysReadInputBufferSize = defaultSysReadInputBufferSize,
    microsecondsPerTick = defaultMicrosecondsPerTick,
    builder = Make
}