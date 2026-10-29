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
    notExportedMessage,
    collisionMessage,
    localFolders
) where

import Control.Monad
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Command.Utils (dieWithFirstError, loadLock)
import Configuration.Local
import Configuration.Lock
import Configuration.Manifest
import Configuration.Prefix (manifestPrefixes, ownsModule)
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
    folderPrefixes :: !(Maybe [Prefix])
  } deriving (Eq, Show)

type Sources = [SourceFolder]

-- | The folders of the build of the application, or of a test, with the
-- folder of its main module. The packages are those of the lock, read from
-- vendor/, which the build has already brought in line with the manifest,
-- and the libraries of local folders.
buildSources :: Manifest -> Maybe String -> FilePath -> IO (Either [String] Sources)
buildSources manifest mTest mainFolder = do
    lockResult <- loadLock "."
    rootLocals <- readLocalPackages [(".", p) | DepPath p <- manifestDependencies manifest]
    testLocals <- readLocalPackages [(".", p) | DepPath p <- testDeps]
    case (,,) <$> lockResult <*> rootLocals <*> testLocals of
        Left errs -> return (Left errs)
        Right (lock, rl, tl) -> do
            vendored <- forM (lockPackages lock) $ \l -> do
                m <- decodeManifestFile (packageDir "." (lockedUrl l) </> "termina.yaml")
                return (normalizeUrl (lockedUrl l), (lockedUrl l, m))
            case [err | (_, (_, Left err)) <- vendored] of
                (err : _) -> return (Left ["vendor/: " ++ err])
                [] -> return (Right (sources (M.fromList [(k, (u, m)) | (k, (u, Right m)) <- vendored]) rl tl))

  where

    keysOf deps = [normalizeUrl (gitUrl g) | DepGit g <- deps]

    nameOf = T.unpack . projectName . manifestProject

    testDeps = maybe [] (\t -> maybe [] testDependencies (M.lookup (T.pack t) (manifestTests manifest))) mTest

    rootKeys = keysOf (manifestDependencies manifest)

    testKeys = keysOf testDeps

    sources packages rootLocals testLocals =
        let folderOf k = maybe "" (\(u, _) -> normalise (packageDir "." u </> "src")) (M.lookup k packages)
            declared k = maybe [] (keysOf . manifestDependencies . snd) (M.lookup k packages)
            -- The packages reached from some keys, each with the package that first brought it.
            closure starts = go (M.fromList starts) (map fst starts)
              where
                go seen [] = seen
                go seen (k : rest) =
                    let new = [d | d <- declared k, not (M.member d seen)]
                        name = maybe "" (nameOf . snd) (M.lookup k packages)
                    in go (foldr (\d -> M.insert d (Just name)) seen new) (rest ++ new)
            localSrc folder = normalise (folder </> "src")
            localKeys l = keysOf (manifestDependencies (localManifest l))
            localPaths l = [localFolderOf (localFolder l) p | DepPath p <- manifestDependencies (localManifest l)]
            startsOf deps = [localFolderOf "." p | DepPath p <- deps]
            gitStartsOf keys ls = [(k, Nothing) | k <- keys] ++ [(k, Just (nameOf (localManifest l))) | l <- M.elems ls, k <- localKeys l]
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
            packageFolder k via = SourceFolder (folderOf k)
                (Package (maybe "" (nameOf . snd) (M.lookup k packages)) via (if M.member k rootGraph then Nothing else mTest))
                (folderOf k : map folderOf (declared k)) Nothing
            localFolder' l = SourceFolder (localSrc (localFolder l))
                (Local (nameOf (localManifest l)) (if M.member (localFolder l) rootLocals then Nothing else mTest))
                (localSrc (localFolder l) : map folderOf (localKeys l) ++ map localSrc (localPaths l))
                (Just (manifestPrefixes (localManifest l)))
        in [SourceFolder mainFolder mainOwner mainSees Nothing, SourceFolder "src" Project projectSees Nothing]
            ++ [SourceFolder f (TestOf t) testSees Nothing | Just t <- [mTest], f <- testSrc]
            ++ [packageFolder k via | (k, via) <- M.toList graph, M.member k packages]
            ++ map localFolder' (M.elems locals)

-- | The folders of a build, or the first error that keeps them from being
-- known, and stop.
loadSources :: Manifest -> Maybe String -> FilePath -> IO Sources
loadSources manifest mTest mainFolder = buildSources manifest mTest mainFolder >>= either dieWithFirstError return

-- | Folders that all see each other, as before packages, for the commands
-- that do not read them yet.
plainSources :: FilePath -> [FilePath] -> Sources
plainSources mainFolder folders = [SourceFolder f Project folders Nothing | f <- mainFolder : folders]

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

-- | Where a module that a module of a folder imports is.
lookupModule :: Sources -> FilePath -> QualifiedName -> IO Lookup
lookupModule sources importer qname = do
    let sees = maybe [importer] folderSees (folderAt importer)
        has folder = doesFileExist (folder </> qname <.> "fin")
        exported folder = maybe True (any (`ownsModule` splitDirectories qname)) (folderAt folder >>= folderPrefixes)
    present <- filterM has (dedup sees)
    case filter exported present of
        [folder] -> return (Found folder)
        (a : b : _) -> return (Ambiguous a b)
        [] -> case [f | folder <- present, Just f <- [folderAt folder]] of
            (f : _) -> return (NotExported f)
            [] -> do
                others <- filterM (has . folderPath) [f | f <- sources, folderPath f `notElem` sees]
                return (maybe Missing Hidden (safeHead others))

  where

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
