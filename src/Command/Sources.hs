{-# LANGUAGE OverloadedStrings #-}

-- | Loading of a project with its dependencies, and the source folders of
-- each build.
-- A source folder gives the folders whose modules its own modules can import.
module Command.Sources (
    Owner(..),
    SourceFolder(..),
    Sources,
    Lookup(..),
    LoadedProject(..),
    readProject,
    readProjectModules,
    withTexts,
    buildSources,
    lookupModule,
    hiddenMessage,
    notExportedMessage,
    collisionMessage,
    localFolders
) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.List (find, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Command.Utils (loadLock)
import Configuration.Fetch (FetchedFile(..))
import Configuration.Local
import Configuration.Lock
import Configuration.Manifest
import Configuration.Package
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
    | RemotePackage !String !(Maybe String) !(Maybe String)
    -- | A library of a local folder: its name, and the test that declares it
    -- when only a test does
    | LocalPackage !String !(Maybe String)
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
    folderModules :: !ParsedFiles,
    -- | The package it belongs to, for a dependency
    folderPackage :: !(Maybe Package)
  } deriving Show

type Sources = [SourceFolder]

-- | A project and its dependencies.
data LoadedProject = LoadedProject {
    -- | The folder of the project, which starts the path of every folder and
    -- file of the project and of its dependencies
    loadedRoot :: !FilePath,
    loadedManifest :: !Manifest,
    -- | The packages of vendor/ by normalized URL
    loadedRemotes :: !(M.Map T.Text Package),
    -- | The libraries of local folders by folder
    loadedLocals :: !(M.Map FilePath Package),
    -- | The modules of the project, by their path from the project folder
    loadedProjectModules :: !ParsedFiles
  } deriving Show

-- | Read a project and its dependencies from disk, without updating the lock
-- or vendor/, given the folder of the project. It gives the errors of the
-- lock, of the local folders or of the first package of vendor/ that cannot be
-- read.
readProject :: FilePath -> Manifest -> IO (Either [String] LoadedProject)
readProject root manifest = do
    lockRead <- loadLock root
    case lockRead of
        Left errs -> return (Left errs)
        Right lock -> do
            localsRead <- readLocalPackages [(root, p) | DepPath p <- allDependencies]
            case localsRead of
                Left errs -> return (Left errs)
                Right locals -> do
                    vendored <- mapM vendoredPackage (lockPackages lock)
                    own <- readProjectModules root manifest
                    return ((\packages -> LoadedProject root manifest (M.fromList packages) locals own) <$> sequence vendored)

  where

    allDependencies = manifestDependencies manifest ++ concatMap testDependencies (M.elems (manifestTests manifest))

    vendoredPackage l = do
        let url = lockedUrl l
        files <- readPackage root url
        return $ either (\err -> Left ["vendor/" ++ T.unpack (normalizeUrl url) ++ ": " ++ err]) (Right . (,) (normalizeUrl url)) $
            packageFromFiles (FromGit url (lockedRev l) (lockedCommit l)) (packageDir root url) [FetchedFile p "100644" b | (p, b) <- files]

-- | The project with the text of some of its module files replaced.
-- The texts are given by the path of their file, which starts with the folder
-- of the project.
-- A file that the project and its dependencies do not have is left out.
withTexts :: M.Map FilePath T.Text -> LoadedProject -> LoadedProject
withTexts texts project = project {
    loadedRemotes = M.map inPackage (loadedRemotes project),
    loadedLocals = M.map inPackage (loadedLocals project),
    loadedProjectModules = replaced (sourceFolders ++ mainFolders) (loadedProjectModules project)
  }

  where

    tests = ["tests" </> T.unpack t | t <- M.keys (manifestTests (loadedManifest project))]

    -- | The folders of the project, and whether they hold only main modules
    sourceFolders = [ (inRoot folder, False) | folder <- "src" : map (</> "src") tests ]
    mainFolders = [ (inRoot folder, True) | folder <- "app" : tests ]

    inRoot folder = normalise (loadedRoot project </> folder)

    inPackage package = package {
        packageModules = replaced [(normalise (packageFolder package </> "src"), False)] (packageModules package)
      }

    replaced folders = ML.mapWithKey (\file parsed -> fromMaybe parsed (M.lookup file texts >>= reparsed folders file))

    -- | The file parsed again with its text, as a module of the first folder
    -- that holds it
    reparsed folders file text = listToMaybe
        [ parseText (dropExtension rel) file text
        | (folder, mainOnly) <- folders
        , Just rel <- [stripPrefix (addTrailingPathSeparator folder) file]
        , not mainOnly || length (splitDirectories rel) == 1 ]

-- | The modules of a project: those of src/ and of the src/ of each test, and
-- the main modules of app/ and of each test, read from disk, given the folder
-- of the project.
readProjectModules :: FilePath -> Manifest -> IO ParsedFiles
readProjectModules root manifest = do
    sources <- mapM (readSourceFolder . inRoot) ("src" : map (</> "src") tests)
    mains <- mapM (readMainFolder . inRoot) ("app" : tests)
    return (ML.unions (sources ++ mains))

  where

    tests = ["tests" </> T.unpack t | t <- M.keys (manifestTests manifest)]

    inRoot folder = normalise (root </> folder)

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
buildSources (LoadedProject root manifest vendored admitted own) mTest mainFolder =
    sources vendored (reachedLocals admitted [(root, p) | DepPath p <- manifestDependencies manifest])
        (reachedLocals admitted [(root, p) | DepPath p <- testDeps])

  where

    keysOf deps = [normalizeUrl (gitUrl g) | DepGit g <- deps]

    src = normalise (root </> "src")

    nameOf = T.unpack . projectName . manifestProject

    testDeps = maybe [] (\t -> maybe [] testDependencies (M.lookup (T.pack t) (manifestTests manifest))) mTest

    rootKeys = keysOf (manifestDependencies manifest)

    testKeys = keysOf testDeps

    sources packages rootLocals testLocals =
        let folderOf k = maybe "" (localSrc . packageFolder) (M.lookup k packages)
            declared k = maybe [] (keysOf . manifestDependencies . packageManifest) (M.lookup k packages)
            -- The packages reached from some keys, each with the package that first brought it.
            closure starts = go (M.fromList starts) (map fst starts)
              where
                go seen [] = seen
                go seen (k : rest) =
                    let new = [d | d <- declared k, not (M.member d seen)]
                        name = maybe "" (nameOf . packageManifest) (M.lookup k packages)
                    in go (foldr (\d -> M.insert d (Just name)) seen new) (rest ++ new)
            localSrc folder = normalise (folder </> "src")
            localKeys l = keysOf (manifestDependencies (packageManifest l))
            localPaths l = [localFolderOf (packageFolder l) p | DepPath p <- manifestDependencies (packageManifest l)]
            startsOf deps = [localFolderOf root p | DepPath p <- deps]
            gitStartsOf keys ls = [(k, Nothing) | k <- keys] ++ [(k, Just (nameOf (packageManifest l))) | l <- M.elems ls, k <- localKeys l]
            rootStarts = gitStartsOf rootKeys rootLocals
            rootGraph = closure rootStarts
            graph = closure (rootStarts ++ gitStartsOf testKeys testLocals)
            locals = M.union rootLocals testLocals
            projectSees = src : map folderOf rootKeys ++ map localSrc (startsOf (manifestDependencies manifest))
            testSees = src : testSrc ++ map folderOf (rootKeys ++ testKeys)
                ++ map localSrc (startsOf (manifestDependencies manifest) ++ startsOf testDeps)
            testSrc = maybe [] (\t -> [normalise (root </> "tests" </> t </> "src")]) mTest
            mainSees = maybe projectSees (const testSees) mTest
            mainOwner = maybe Project TestOf mTest
            vendoredFolder k p via = SourceFolder (folderOf k)
                (RemotePackage (nameOf (packageManifest p)) via (if M.member k rootGraph then Nothing else mTest))
                (folderOf k : map folderOf (declared k)) Nothing (packageModules p) (Just p)
            localFolder' l = SourceFolder (localSrc (packageFolder l))
                (LocalPackage (nameOf (packageManifest l)) (if M.member (packageFolder l) rootLocals then Nothing else mTest))
                (localSrc (packageFolder l) : map folderOf (localKeys l) ++ map localSrc (localPaths l))
                (Just (manifestPrefixes (packageManifest l))) (packageModules l) (Just l)
        in [SourceFolder mainFolder mainOwner mainSees Nothing own Nothing, SourceFolder src Project projectSees Nothing own Nothing]
            ++ [SourceFolder f (TestOf t) testSees Nothing own Nothing | Just t <- [mTest], f <- testSrc]
            ++ [vendoredFolder k p via | (k, via) <- M.toList graph, Just p <- [M.lookup k packages]]
            ++ map localFolder' (M.elems locals)

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
localFolders sources = [takeDirectory (folderPath f) | f <- sources, LocalPackage {} <- [folderOwner f]]

-- | Why a module of a local library outside its exports cannot be imported.
notExportedMessage :: SourceFolder -> String -> String
notExportedMessage folder dotted =
    dotted ++ " is in " ++ folderPath folder ++ ", outside the exports of " ++ maybe "the library" (\(n, _, _) -> n) (dependency (folderOwner folder))
        ++ ", so it cannot be imported from it."

-- | The name of a dependency, the package that brought it and the test
-- that declares it when only a test does.
dependency :: Owner -> Maybe (String, Maybe String, Maybe String)
dependency (RemotePackage name via test) = Just (name, via, test)
dependency (LocalPackage name test) = Just (name, Nothing, test)
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
