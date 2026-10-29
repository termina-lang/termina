{-# LANGUAGE OverloadedStrings #-}

module Configuration.Configuration (
    TerminaConfig(..),
    ExternConfig(..),
    ExternEntry(..),
    emptyExternConfig,
    externSourceFolder,
    externHeaderFolder,
    externLibraryFolder,
    externFiles,
    listedMatches,
    listedFiles,
    insideFolder,
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
import Data.List (nub, tails)
import System.FilePath (isRelative, splitDirectories, takeDirectory, takeFileName, (</>))

import Configuration.Platform

data ProjectProfile = Debug | Release deriving (Eq, Show)

-- | The C of the project, in the "extern" folder, under the module whose
-- extern functions it defines, named with dots as in an import.
newtype ExternConfig = ExternConfig {
    externModules :: M.Map T.Text ExternEntry
  } deriving (Eq, Show)

-- | The C of one module: the sources of "extern/src", the headers of
-- "extern/include" and the archives of "extern/lib/<platform>", each list
-- relative to its folder.
data ExternEntry = ExternEntry {
    entrySources :: ![FilePath],
    entryHeaders :: ![FilePath],
    entryLibraries :: !(M.Map T.Text [FilePath])
  } deriving (Eq, Show)

emptyExternConfig :: ExternConfig
emptyExternConfig = ExternConfig M.empty

-- | The folders of "extern": sources, headers and the archives of each platform.
externSourceFolder, externHeaderFolder, externLibraryFolder :: FilePath
externSourceFolder = "src"
externHeaderFolder = "include"
externLibraryFolder = "lib"

-- | Every file that the section lists, the libraries of all the platforms
-- included, relative to the "extern" folder and once each.
externFiles :: ExternConfig -> [FilePath]
externFiles (ExternConfig modules) = nub $ concatMap entryFiles (M.elems modules)

  where

    entryFiles (ExternEntry srcs hdrs libs) =
        map (externSourceFolder </>) srcs
        ++ map (externHeaderFolder </>) hdrs
        ++ [ externLibraryFolder </> T.unpack plt </> lib | (plt, pltLibs) <- M.toList libs, lib <- pltLibs ]

-- | Whether a listed path, a file or a pattern whose * matches any part of a
-- file name, names the path.
listedMatches :: FilePath -> FilePath -> Bool
listedMatches listed path
    | '*' `elem` listed = takeDirectory listed == takeDirectory path && matches (takeFileName listed) (takeFileName path)
    | otherwise = listed == path

  where

    matches ('*' : ps) s = any (matches ps) (tails s)
    matches (p : ps) (c : cs) = p == c && matches ps cs
    matches [] [] = True
    matches _ _ = False

-- | The paths that a listed path names among the given ones.
listedFiles :: [FilePath] -> FilePath -> [FilePath]
listedFiles present listed = filter (listedMatches listed) present

-- | Whether a path of an extern section stays inside the extern folder: it
-- is relative and does not go up with "..".
insideFolder :: FilePath -> Bool
insideFolder path = isRelative path && ".." `notElem` splitDirectories path

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
    -- | Seconds a test may run, its timeout; nothing in the application
    runTimeout :: !(Maybe Integer),
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
    runTimeout = Nothing,
    lineDirectives = False
}

-- | Bytes of the record of results of a test when its entry does not say.
defaultTestLogSize :: Integer
defaultTestLogSize = 8192