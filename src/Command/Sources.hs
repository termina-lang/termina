{-# LANGUAGE OverloadedStrings #-}

-- | A project with its dependencies, and the source folders of a build with
-- what the modules of each one see: their own folder and those of the
-- dependencies that their manifest declares.
module Command.Sources (
    Owner(..),
    SourceFolder(..),
    Sources,
    Lookup(..),
    LoadedProject(..),
    readProject,
    readProjectModules,
    buildSources,
    plainSources,
    lookupModule,
    hiddenMessage,
    notExportedMessage,
    collisionMessage,
    localFolders
) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.List (find)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Command.Utils (dieWithFirstError, loadLock)
import Configuration.Fetch (FetchedFile(..))
import Configuration.Local
import Configuration.Lock
import Configuration.Manifest
import qualified Configuration.Package as P
import Configuration.Parsed
import Configuration.Prefix (manifestPrefixes, ownsModule)
import Configuration.Vendor (packageDir, readPackage)
import Utils.Files (filesUnder)
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
    -- | A library of a local folder: its name, and the test that declares it
    -- when only a test does
    | Local !String !(Maybe String)
  deriving (Eq, Show)

data SourceFolder = SourceFolder {
    folderPath :: !FilePath,
    folderOwner :: !Owner,
    -- | The folders that the modules of this one see, itself first
    folderSees :: ![FilePath],
    -- | The prefixes whose modules can be imported from it, in a local
    -- folder, which holds the whole library
    folderPrefixes :: !(Maybe [Prefix]),
    -- | Its modules, by their path from the project folder
    folderModules :: !ParsedFiles
  } deriving Show

type Sources = [SourceFolder]

-- | A project and its dependencies.
data LoadedProject = LoadedProject {
    loadedManifest :: !Manifest,
    -- | The packages of vendor/ by normalized URL
    loadedPackages :: !(M.Map T.Text P.Package),
    -- | The libraries of local folders by folder
    loadedLocals :: !(M.Map FilePath P.Package),
    -- | The modules of the project, by their path from the project folder
    loadedModules :: !ParsedFiles
  } deriving Show

-- | Read a project and its dependencies from disk, without updating the lock
-- or vendor/. It stops at the first error.
readProject :: Manifest -> IO LoadedProject
readProject manifest = do
    lock <- loadLock "." >>= either dieWithFirstError return
    locals <- readLocalPackages [(".", p) | DepPath p <- allDependencies] >>= either dieWithFirstError return
    vendored <- forM (lockPackages lock) $ \l -> do
        let url = lockedUrl l
        files <- readPackage "." url
        either (\err -> dieWithFirstError ["vendor/" ++ T.unpack (normalizeUrl url) ++ ": " ++ err]) (return . (,) (normalizeUrl url)) $
            P.packageFromFiles (P.FromGit url (lockedRev l) (lockedCommit l)) (packageDir "." url) [FetchedFile p "100644" b | (p, b) <- files]
    own <- readProjectModules manifest
    return (LoadedProject manifest (M.fromList vendored) locals own)

  where

    allDependencies = manifestDependencies manifest ++ concatMap testDependencies (M.elems (manifestTests manifest))

-- | The modules of a project: those of src/ and of the src/ of each test, and
-- the main modules of app/ and of each test, read from disk.
readProjectModules :: Manifest -> IO ParsedFiles
readProjectModules manifest = do
    sources <- mapM readSourceFolder ("src" : map (</> "src") tests)
    mains <- mapM readMainFolder ("app" : tests)
    return (ML.unions (sources ++ mains))

  where

    tests = ["tests" </> T.unpack t | t <- M.keys (manifestTests manifest)]

-- | The modules of the folder of a main module, without its subfolders, read
-- from disk.
readMainFolder :: FilePath -> IO ParsedFiles
readMainFolder folder = do
    exists <- doesDirectoryExist folder
    names <- if exists then listDirectory folder else return []
    files <- filterM (doesFileExist . (folder </>)) [n | n <- names, takeExtension n == ".fin"]
    parseFolder folder <$> mapM (\f -> (,) f <$> BS.readFile (folder </> f)) files

-- | The modules under a source folder, read from disk.
readSourceFolder :: FilePath -> IO ParsedFiles
readSourceFolder folder = do
    files <- filter ((== ".fin") . takeExtension) <$> filesUnder folder
    parseFolder folder <$> mapM (\f -> (,) f <$> BS.readFile (folder </> f)) files

-- | The folders of the build of the application, or of a test, with the
-- folder of its main module. Only the local folders that the dependencies of
-- the project and of the test reach are included.
buildSources :: LoadedProject -> Maybe String -> FilePath -> Sources
buildSources (LoadedProject manifest vendored admitted own) mTest mainFolder =
    sources vendored (reachedLocals admitted [(".", p) | DepPath p <- manifestDependencies manifest])
        (reachedLocals admitted [(".", p) | DepPath p <- testDeps])

  where

    keysOf deps = [normalizeUrl (gitUrl g) | DepGit g <- deps]

    nameOf = T.unpack . projectName . manifestProject

    testDeps = maybe [] (\t -> maybe [] testDependencies (M.lookup (T.pack t) (manifestTests manifest))) mTest

    rootKeys = keysOf (manifestDependencies manifest)

    testKeys = keysOf testDeps

    sources packages rootLocals testLocals =
        let folderOf k = maybe "" (localSrc . P.packageFolder) (M.lookup k packages)
            declared k = maybe [] (keysOf . manifestDependencies . P.packageManifest) (M.lookup k packages)
            -- The packages reached from some keys, each with the package that first brought it.
            closure starts = go (M.fromList starts) (map fst starts)
              where
                go seen [] = seen
                go seen (k : rest) =
                    let new = [d | d <- declared k, not (M.member d seen)]
                        name = maybe "" (nameOf . P.packageManifest) (M.lookup k packages)
                    in go (foldr (\d -> M.insert d (Just name)) seen new) (rest ++ new)
            localSrc folder = normalise (folder </> "src")
            localKeys l = keysOf (manifestDependencies (P.packageManifest l))
            localPaths l = [localFolderOf (P.packageFolder l) p | DepPath p <- manifestDependencies (P.packageManifest l)]
            startsOf deps = [localFolderOf "." p | DepPath p <- deps]
            gitStartsOf keys ls = [(k, Nothing) | k <- keys] ++ [(k, Just (nameOf (P.packageManifest l))) | l <- M.elems ls, k <- localKeys l]
            rootStarts = gitStartsOf rootKeys rootLocals
            rootGraph = closure rootStarts
            graph = closure (rootStarts ++ gitStartsOf testKeys testLocals)
            locals = M.union rootLocals testLocals
            projectSees = "src" : map folderOf rootKeys ++ map localSrc (startsOf (manifestDependencies manifest))
            testSees = "src" : testSrc ++ map folderOf (rootKeys ++ testKeys)
                ++ map localSrc (startsOf (manifestDependencies manifest) ++ startsOf testDeps)
            testSrc = maybe [] (\t -> ["tests" </> t </> "src"]) mTest
            mainSees = maybe projectSees (const testSees) mTest
            mainOwner = maybe Project TestOf mTest
            vendoredFolder k p via = SourceFolder (folderOf k)
                (Package (nameOf (P.packageManifest p)) via (if M.member k rootGraph then Nothing else mTest))
                (folderOf k : map folderOf (declared k)) Nothing (P.packageModules p)
            localFolder' l = SourceFolder (localSrc (P.packageFolder l))
                (Local (nameOf (P.packageManifest l)) (if M.member (P.packageFolder l) rootLocals then Nothing else mTest))
                (localSrc (P.packageFolder l) : map folderOf (localKeys l) ++ map localSrc (localPaths l))
                (Just (manifestPrefixes (P.packageManifest l))) (P.packageModules l)
        in [SourceFolder mainFolder mainOwner mainSees Nothing own, SourceFolder "src" Project projectSees Nothing own]
            ++ [SourceFolder f (TestOf t) testSees Nothing own | Just t <- [mTest], f <- testSrc]
            ++ [vendoredFolder k p via | (k, via) <- M.toList graph, Just p <- [M.lookup k packages]]
            ++ map localFolder' (M.elems locals)

-- | Folders that all see each other, as before packages, with their modules
-- read from disk, for the commands that do not read packages yet.
plainSources :: FilePath -> [FilePath] -> IO Sources
plainSources mainFolder folders = do
    mainModules <- readMainFolder mainFolder
    modules <- ML.unions . (mainModules :) <$> mapM readSourceFolder folders
    return [SourceFolder f Project folders Nothing modules | f <- mainFolder : folders]

data Lookup =
    Found FilePath
    -- | The module is in two folders that the importer sees
    | Ambiguous FilePath FilePath
    -- | The module is in a folder of the build that the importer does not see
    | Hidden SourceFolder
    -- | The module is in a local library that the importer sees, outside its
    -- exports
    | NotExported SourceFolder
    | Missing

-- | Where a module that a module of a folder imports is, among the modules of
-- the folders of the build.
lookupModule :: Sources -> FilePath -> QualifiedName -> Lookup
lookupModule sources importer qname =
    case filter exported present of
        [folder] -> Found folder
        (a : b : _) -> Ambiguous a b
        [] -> case [f | folder <- present, Just f <- [folderAt folder]] of
            (f : _) -> NotExported f
            [] -> maybe Missing Hidden (safeHead [f | f <- sources, folderPath f `notElem` sees, has (folderPath f)])

  where

    sees = maybe [importer] folderSees (folderAt importer)

    present = filter has (dedup sees)

    has folder = maybe False (ML.member (normalise (folder </> qname <.> "fin")) . folderModules) (folderAt folder)

    exported folder = maybe True (any (`ownsModule` splitDirectories qname)) (folderAt folder >>= folderPrefixes)

    folderAt folder = find ((== folder) . folderPath) sources

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
    case (dependency (folderOwner folder), importerOwner) of
        (Just (name, _, Just t), Project) ->
            name ++ " is a dependency of the test " ++ t ++ " only, so the code of src/ cannot import " ++ dotted ++ "."
        (Just (name, Just via, _), importer) | Just (from, _, _) <- dependency importer ->
            name ++ " reaches this build through " ++ via ++ ", but the termina.yaml of " ++ from
                ++ " does not declare it, so " ++ from ++ " cannot import " ++ dotted ++ "."
        (Just (name, Just via, _), _) ->
            name ++ " reaches this build through " ++ via ++ ", but termina.yaml does not declare it, so "
                ++ who importerOwner ++ " cannot import " ++ dotted ++ ". Add " ++ name ++ " to the dependencies to use it."
        _ -> case (folderOwner folder, dependency importerOwner) of
            (TestOf t, Nothing) | importerOwner == Project ->
                dotted ++ " is a module of the test " ++ t ++ ", so the code of src/ cannot import it."
            (Project, Just (from, _, _)) ->
                dotted ++ " is a module of this project, and the dependency " ++ from ++ " cannot import it."
            _ -> dotted ++ " is in " ++ folderPath folder ++ ", which " ++ who importerOwner ++ " does not see."

  where

    who Project = "this project"
    who (TestOf t) = "the test " ++ t
    who owner = maybe "" (\(name, _, _) -> name) (dependency owner)

-- | The local folders that a build takes libraries from, as the project
-- names them.
localFolders :: Sources -> [FilePath]
localFolders sources = [takeDirectory (folderPath f) | f <- sources, Local {} <- [folderOwner f]]

-- | Why a module of a local library outside its exports cannot be imported.
notExportedMessage :: SourceFolder -> String -> String
notExportedMessage folder dotted =
    dotted ++ " is in " ++ folderPath folder ++ ", outside the exports of " ++ maybe "the library" (\(n, _, _) -> n) (dependency (folderOwner folder))
        ++ ", so it cannot be imported from it."

-- | The name of a dependency, the package that brought it and the test
-- that declares it when only a test does.
dependency :: Owner -> Maybe (String, Maybe String, Maybe String)
dependency (Package name via test) = Just (name, via, test)
dependency (Local name test) = Just (name, Nothing, test)
dependency _ = Nothing

-- | Two modules of a build with one name, the one of the project first.
collisionMessage :: Sources -> QualifiedName -> FilePath -> FilePath -> String
collisionMessage sources qname a b =
    let (first, second) = if isDependency a then (b, a) else (a, b)
    in describe first ++ " has the same name as " ++ describe second ++ ". Two modules of a build cannot have the same name."

  where

    ownerOf folder = folderOwner <$> find ((== folder) . folderPath) sources

    isDependency folder = maybe False (\o -> dependency o /= Nothing) (ownerOf folder)

    describe folder = case ownerOf folder >>= dependency of
        Just (name, _, _) -> "the module " ++ qualifiedToModuleName qname ++ " of the dependency " ++ name ++ ", in " ++ folder ++ "/"
        _ -> folder </> qname <.> "fin"
