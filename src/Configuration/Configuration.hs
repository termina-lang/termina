{-# LANGUAGE OverloadedStrings #-}

module Configuration.Configuration (
    TerminaConfig(..),
    ExternConfig(..),
    emptyExternConfig,
    ProjectProfile(..),
    defaultConfig,
    DeploymentKind(..),
    isTestBuild,
    defaultSysPrintOutputBufferSize,
    defaultSysReadInputBufferSize,
    defaultMicrosecondsPerTick,
    defaultTestLogSize,
    appFolder,
    appFilename,
    outputFolderName,
    efpFolderName
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

-- | Folders of the generated output and of the files of the schedulability
-- analysis, both in the project and fixed like the rest of its layout. The
-- generated C in the output is kept under version control.
outputFolderName, efpFolderName :: FilePath
outputFolderName = "output"
efpFolderName = "efp"

instance FromJSON ProjectProfile where
    parseJSON (String "debug") = return Debug
    parseJSON (String "release") = return Release
    parseJSON _ = fail "Expected profile type"
  
instance ToJSON ProjectProfile where
    toJSON Debug = String "debug"
    toJSON Release = String "release"

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
    deploymentKind :: !DeploymentKind,
    -- | Bytes of the record area of a test, its test-log-size
    recordCapacity :: !Integer,
    -- | Stack size of the task of the harness of a test, or the default of the platform
    harnessStackSize :: !(Maybe Integer),
    -- | Whether the generated C carries #line pragmas that point to the .fin.
    -- Without them it is broken to the usual width, the plain C.
    lineDirectives :: !Bool
  } deriving (Eq, Show)

-- | What a deployment builds. What a test needs and the application does not
-- is derived from it.
data DeploymentKind =
    ApplicationBuild
    | TestBuild !T.Text -- ^ The name of the test
  deriving (Eq, Show)

isTestBuild :: TerminaConfig -> Bool
isTestBuild config = case deploymentKind config of
  TestBuild _ -> True
  ApplicationBuild -> False

defaultConfig :: String -> Platform -> TerminaConfig
defaultConfig projectName plt = TerminaConfig {
    name = T.pack projectName,
    platform = T.pack $ show plt,
    sourceModulesFolder = "src",
    outputFolder = outputFolderName,
    efpFolder = efpFolderName,
    profile = Release,
    sysPrintOutputBufferSize = defaultSysPrintOutputBufferSize,
    sysReadInputBufferSize = defaultSysReadInputBufferSize,
    microsecondsPerTick = defaultMicrosecondsPerTick,
    deploymentKind = ApplicationBuild,
    recordCapacity = defaultTestLogSize,
    harnessStackSize = Nothing,
    lineDirectives = False
}

-- | Bytes of the record of results of a test when its entry does not say.
defaultTestLogSize :: Integer
defaultTestLogSize = 8192