{-# LANGUAGE OverloadedStrings #-}

-- | The source folders of a build and what the modules of each one see: their
-- own folder and those of the dependencies that their manifest declares.
module Command.Sources (
    Owner(..),
    SourceFolder(..),
    Sources,
    Lookup(..),
    buildSources,
    loadSources,
    plainSources,
    lookupModule,
    hiddenMessage,
    collisionMessage
) where

import Control.Monad
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Command.Utils (dieWithFirstError, loadLock)
import Configuration.Lock
import Configuration.Manifest
import Configuration.Vendor (packageDir)
import Modules.Utils (qualifiedToModuleName)
import Utils.Annotations (QualifiedName)

-- | Who a source folder belongs to.
data Owner =
    -- | The project: its src/ and the folder of app.fin
    Project
    -- | A test: its src/ and the folder of test.fin
    | TestOf !String
    -- | A package of vendor/: its name, the package that first brought it
    -- (none when the project or a test declares it), and the test that
    -- declares it when only a test does
    | Package !String !(Maybe String) !(Maybe String)
  deriving (Eq, Show)

data SourceFolder = SourceFolder {
    folderPath :: !FilePath,
    folderOwner :: !Owner,
    -- | The folders that the modules of this one see, itself first
    folderSees :: ![FilePath]
  } deriving (Eq, Show)

type Sources = [SourceFolder]

-- | The folders of the build of the application, or of a test, with the
-- folder of its main module. The packages are those of the lock, read from
-- vendor/, which the build has already brought in line with the manifest.
buildSources :: Manifest -> Maybe String -> FilePath -> IO (Either [String] Sources)
buildSources manifest mTest mainFolder = do
    lockResult <- loadLock "."
    case lockResult of
        Left errs -> return (Left errs)
        Right lock -> do
            vendored <- forM (lockPackages lock) $ \l -> do
                m <- decodeManifestFile (packageDir "." (lockedUrl l) </> "termina.yaml")
                return (normalizeUrl (lockedUrl l), (lockedUrl l, m))
            case [err | (_, (_, Left err)) <- vendored] of
                (err : _) -> return (Left ["vendor/: " ++ err])
                [] -> return (Right (sources (M.fromList [(k, (u, m)) | (k, (u, Right m)) <- vendored])))

  where

    keysOf deps = [normalizeUrl (gitUrl g) | DepGit g <- deps]

    rootKeys = keysOf (manifestDependencies manifest)

    testKeys = maybe [] (\t -> maybe [] (keysOf . testDependencies) (M.lookup (T.pack t) (manifestTests manifest))) mTest

    sources packages =
        let folderOf k = maybe "" (\(u, _) -> normalise (packageDir "." u </> "src")) (M.lookup k packages)
            declared k = maybe [] (keysOf . manifestDependencies . snd) (M.lookup k packages)
            -- The packages reached from some keys, each with the package that first brought it.
            closure starts = go (M.fromList [(k, Nothing) | k <- starts]) starts
              where
                go seen [] = seen
                go seen (k : rest) =
                    let new = [d | d <- declared k, not (M.member d seen)]
                        name = maybe "" (T.unpack . projectName . manifestProject . snd) (M.lookup k packages)
                    in go (foldr (\d -> M.insert d (Just name)) seen new) (rest ++ new)
            rootGraph = closure rootKeys
            graph = closure (rootKeys ++ testKeys)
            projectSees = "src" : map folderOf rootKeys
            testSees = "src" : testSrc ++ map folderOf (rootKeys ++ testKeys)
            testSrc = maybe [] (\t -> ["tests" </> t </> "src"]) mTest
            mainSees = maybe projectSees (const testSees) mTest
            mainOwner = maybe Project TestOf mTest
            packageFolder k via = SourceFolder (folderOf k)
                (Package (maybe "" (T.unpack . projectName . manifestProject . snd) (M.lookup k packages)) via
                    (if M.member k rootGraph then Nothing else mTest))
                (folderOf k : map folderOf (declared k))
        in [SourceFolder mainFolder mainOwner mainSees, SourceFolder "src" Project projectSees]
            ++ [SourceFolder f (TestOf t) testSees | Just t <- [mTest], f <- testSrc]
            ++ [packageFolder k via | (k, via) <- M.toList graph, M.member k packages]

-- | The folders of a build, or the first error that keeps them from being
-- known, and stop.
loadSources :: Manifest -> Maybe String -> FilePath -> IO Sources
loadSources manifest mTest mainFolder = buildSources manifest mTest mainFolder >>= either dieWithFirstError return

-- | Folders that all see each other, as before packages, for the commands
-- that do not read them yet.
plainSources :: FilePath -> [FilePath] -> Sources
plainSources mainFolder folders = [SourceFolder f Project folders | f <- mainFolder : folders]

data Lookup =
    Found FilePath
    -- | The module is in two folders that the importer sees
    | Ambiguous FilePath FilePath
    -- | The module is in a folder of the build that the importer does not see
    | Hidden SourceFolder
    | Missing

-- | Where a module that a module of a folder imports is.
lookupModule :: Sources -> FilePath -> QualifiedName -> IO Lookup
lookupModule sources importer qname = do
    let sees = maybe [importer] folderSees (find ((== importer) . folderPath) sources)
        has folder = doesFileExist (folder </> qname <.> "fin")
    visible <- filterM has (dedup sees)
    case visible of
        [folder] -> return (Found folder)
        (a : b : _) -> return (Ambiguous a b)
        [] -> do
            others <- filterM (has . folderPath) [f | f <- sources, folderPath f `notElem` sees]
            return (maybe Missing Hidden (safeHead others))

  where

    dedup = go S.empty
      where
        go _ [] = []
        go seen (x : xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs

    safeHead (x : _) = Just x
    safeHead [] = Nothing

-- | Why the module of a folder that the importer does not see cannot be
-- imported, said by the place of the import.
hiddenMessage :: SourceFolder -> Owner -> String -> String
hiddenMessage folder importerOwner dotted =
    case (folderOwner folder, importerOwner) of
        (Package name _ (Just t), Project) ->
            name ++ " is a dependency of the test " ++ t ++ " only, so the code of src/ cannot import " ++ dotted ++ "."
        (Package name (Just via) _, Package from _ _) ->
            name ++ " reaches this build through " ++ via ++ ", but the termina.yaml of " ++ from
                ++ " does not declare it, so " ++ from ++ " cannot import " ++ dotted ++ "."
        (Package name (Just via) _, _) ->
            name ++ " reaches this build through " ++ via ++ ", but termina.yaml does not declare it, so "
                ++ who importerOwner ++ " cannot import " ++ dotted ++ ". Add " ++ name ++ " to the dependencies to use it."
        (TestOf t, Project) ->
            dotted ++ " is a module of the test " ++ t ++ ", so the code of src/ cannot import it."
        (Project, Package from _ _) ->
            dotted ++ " is a module of this project, and the dependency " ++ from ++ " cannot import it."
        _ -> dotted ++ " is in " ++ folderPath folder ++ ", which " ++ who importerOwner ++ " does not see."

  where

    who Project = "this project"
    who (TestOf t) = "the test " ++ t
    who (Package name _ _) = name

-- | Two modules of a build with one name, the one of the project first.
collisionMessage :: Sources -> QualifiedName -> FilePath -> FilePath -> String
collisionMessage sources qname a b =
    let (first, second) = if isPackage a then (b, a) else (a, b)
    in describe first ++ " has the same name as " ++ describe second ++ ". Two modules of a build cannot have the same name."

  where

    ownerOf folder = folderOwner <$> find ((== folder) . folderPath) sources

    isPackage folder = case ownerOf folder of
        Just (Package {}) -> True
        _ -> False

    describe folder = case ownerOf folder of
        Just (Package name _ _) -> "the module " ++ qualifiedToModuleName qname ++ " of the dependency " ++ name ++ ", in " ++ folder ++ "/"
        _ -> folder </> qname <.> "fin"
