{-# LANGUAGE OverloadedStrings #-}

-- | What the commands that bring dependencies share: bringing the packages of
-- the manifest into the lock and "vendor/", and the description of what fails.
module Command.Dependencies (
    PackageError(..),
    Fetching(..),
    LockChange(..),
    Sync(..),
    syncDependencies,
    describeChange,
    describePackageError,
    describeObtainErrors,
    credentialsMessage,
    overrideWarnings
) where

import Control.Monad
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Command.Utils (Place(..))
import Configuration.Fetch
import Configuration.Graph
import Configuration.Local
import Configuration.Lock
import Configuration.Manifest
import Configuration.Resolve
import Configuration.Validate
import Configuration.Vendor

-- | Why a package could not be brought.
data PackageError =
    FetchFailed FetchError
    | InvalidPackage [String]
    -- | The copy in vendor/ of a locked package is missing or does not match
    -- its hash, and why.
    | VendorMismatch String
    -- | The package has to be fetched, which the command may not do: the
    -- tag that the lock has for it, if any.
    | NeedsNetwork (Maybe T.Text)
  deriving (Eq, Show)

-- | Where the packages of a synchronization come from.
data Fetching =
    -- | Only from vendor/: the lock has to have every package with its tag
    Offline
    -- | From vendor/ when the lock has the package with the same tag, and
    -- from its repository otherwise
    | Reuse
    -- | From the repository for the packages that the function selects, by
    -- normalized URL, and as Reuse for the rest
    | Refresh (T.Text -> Bool)

-- | How the lock changes: a package that enters it, one that moves to
-- another tag or commit, and one that leaves it.
data LockChange =
    Added ResolvedPackage
    | Changed LockedPackage ResolvedPackage
    | Removed LockedPackage
  deriving (Eq, Show)

-- | What a synchronization changed, and what the user has to know about the
-- resolution.
data Sync = Sync {
    syncChanges :: ![LockChange],
    syncWarnings :: ![String]
  } deriving (Eq, Show)

-- | Bring the lock and vendor/ in line with a manifest: resolve it, take from
-- vendor/ every package that the lock has with the same tag, fetch and
-- validate the rest, check the graph and, only then, write the copies that
-- change, remove those that left and write the lock. Nothing is written
-- when anything fails, nor when the caller only wants to know the changes.
syncDependencies :: Place -> String -> Fetching -> Bool -> FilePath -> Manifest -> Lock -> IO (Either [String] Sync)
syncDependencies place commandName fetching write root manifest lock = do
    used <- newIORef []
    localResult <- readLocalPackages [(root, p) | DepPath p <- allDependencies]
    case localResult of
        Left errs -> return (Left errs)
        Right locals -> do
            let extra = [ (g, [rootName, T.unpack (projectName (manifestProject (localManifest l)))])
                        | l <- M.elems locals, DepGit g <- manifestDependencies (localManifest l) ]
            resolution <- resolveWith (obtain used) manifest extra
            case resolution of
                Left errs -> return (Left (describeObtainErrors place commandName errs))
                Right res -> case checkGraph manifest res locals of
                    [] -> Right <$> commit res used
                    errs -> return (Left errs)

  where

    rootName = T.unpack (projectName (manifestProject manifest))

    allDependencies = manifestDependencies manifest ++ concatMap testDependencies (M.elems (manifestTests manifest))

    oldPackages = M.fromList [(normalizeUrl (lockedUrl l), l) | l <- lockPackages lock]

    obtain used u t =
        let locked = M.lookup (normalizeUrl u) oldPackages
        in case (fetching, locked) of
            (Refresh selected, _) | selected (normalizeUrl u) -> fromGit used u t
            (_, Just l) | lockedRev l == t -> fromVendor l
            (Offline, _) -> return (Left (NeedsNetwork (lockedRev <$> locked)))
            _ -> fromGit used u t

    fromVendor locked = do
        copy <- lockedPackage root locked
        return $ case copy of
            Left err -> Left (VendorMismatch err)
            Right files -> case maybe (Left "vendor/ has no termina.yaml") decodeManifest (lookup "termina.yaml" files) of
                Left err -> Left (InvalidPackage ["termina.yaml: " ++ err])
                Right m -> Right (Obtained (lockedCommit locked) m [FetchedFile p "100644" b | (p, b) <- files])

    fromGit used u t = do
        fetched <- fetchTag root u t
        case fetched of
            Left err -> return (Left (FetchFailed err))
            Right (Fetched sha files fromPrefetch) -> case validatePackage files of
                Left errs -> return (Left (InvalidPackage errs))
                Right m -> do
                    when fromPrefetch (modifyIORef used (prefetchDir root u :))
                    return (Right (Obtained sha m files))

    commit res used = do
        let packages = M.elems (resolvedPackages res)
            vendored p = [ (path, bytes) | FetchedFile path _ bytes <- obtainedFiles (resolvedObtained p)
                         , isVendored (obtainedManifest (resolvedObtained p)) path ]
            entry p = LockedPackage (resolvedUrl p) (resolvedTag p) (obtainedSha (resolvedObtained p)) (contentHash (vendored p))
            changes = [ change | p <- packages, let old = M.lookup (normalizeUrl (resolvedUrl p)) oldPackages
                      , Just change <- [case old of
                            Nothing -> Just (Added p)
                            Just l | l /= entry p -> Just (Changed l p)
                            _ -> Nothing] ]
                ++ [Removed l | (k, l) <- M.toList oldPackages, not (M.member k (resolvedPackages res))]
        when (write && not (null changes)) $ do
            forM_ changes $ \change -> case change of
                Added p -> writePackage root (resolvedUrl p) (vendored p)
                Changed _ p -> writePackage root (resolvedUrl p) (vendored p)
                Removed l -> removePackage (lockedUrl l)
            writeLockFile (root </> "termina.lock") (Lock (lockVersion emptyLock) (map entry packages))
        when write (readIORef used >>= mapM_ removePathForcibly)
        return (Sync changes (overrideWarnings res))

    -- | The folder of a package that left the lock, and the folders above it
    -- that it leaves empty.
    removePackage url = do
        removePathForcibly (packageDir root url)
        let parents = takeWhile (/= vendorDir root) (iterate takeDirectory (takeDirectory (packageDir root url)))
        forM_ parents $ \dir -> do
            exists <- doesDirectoryExist dir
            empty <- if exists then null <$> listDirectory dir else return False
            when empty (removeDirectory dir)

-- | A change of the lock as the user reads it.
describeChange :: LockChange -> String
describeChange (Added p) = "added " ++ T.unpack (resolvedUrl p) ++ " " ++ T.unpack (resolvedTag p)
describeChange (Changed l p)
    | lockedRev l == resolvedTag p =
        T.unpack (resolvedUrl p) ++ " " ++ T.unpack (resolvedTag p) ++ " now at " ++ T.unpack (obtainedSha (resolvedObtained p))
    | otherwise = T.unpack (resolvedUrl p) ++ " " ++ T.unpack (lockedRev l) ++ " -> " ++ T.unpack (resolvedTag p)
describeChange (Removed l) = "removed " ++ T.unpack (lockedUrl l) ++ " " ++ T.unpack (lockedRev l)

-- | The overrides of the root in force, and those of the dependencies that
-- do not count.
overrideWarnings :: Resolution -> [String]
overrideWarnings res =
    [ "override active: " ++ T.unpack (overriddenUrl o) ++ " forced to " ++ T.unpack (forcedTag o) ++ "; "
        ++ intercalate " -> " (requestedBy o) ++ " requested " ++ T.unpack (requestedTag o)
    | o <- appliedOverrides res ]
    ++ [ "the overrides of " ++ T.unpack name ++ " are ignored (" ++ T.unpack u ++ "): only those of the project count"
       | (name, u) <- ignoredOverrides res ]

describePackageError :: PackageError -> String
describePackageError (FetchFailed (TagNotFound _ t)) = "the repository has no tag " ++ T.unpack t
describePackageError (FetchFailed (TagMoved t expected actual)) =
    "the tag " ++ T.unpack t ++ " moved while it was fetched, from " ++ T.unpack expected ++ " to " ++ T.unpack actual
describePackageError (FetchFailed (NeedsCredentials _ err)) = "git has no credentials for the repository:\n" ++ err ++ misspelled
describePackageError (FetchFailed (GitFailed err)) = "git failed:\n" ++ err
describePackageError (InvalidPackage errs) = "the package is not valid:\n" ++ intercalate "\n" (map ("  " ++) errs)
describePackageError (VendorMismatch err) = err
describePackageError (NeedsNetwork old) =
    maybe "termina.lock does not have it" (\rev -> "termina.lock has " ++ T.unpack rev ++ " instead") old
        ++ ", and fetching it needs the network. Run termina build without --offline, or termina update, to update termina.lock."

-- | GitHub asks for credentials for a repository that does not exist, so as
-- not to tell whether a private one has that name.
misspelled :: String
misspelled = "GitHub also asks for credentials when a repository does not exist, so if it is public, check its URL."

-- | The errors of bringing packages. In a command-line container, the
-- packages that need credentials go first and together, with the ways to
-- bring them, so that the user clones all of them on the computer at once.
describeObtainErrors :: Place -> String -> [ResolveError PackageError] -> [String]
describeObtainErrors place commandName errs
    | place == CommandLineContainer && not (null credentials) =
        credentialsMessage commandName credentials : map (describeResolveError describePackageError) (filter (not . needsCredentials) errs)
    | otherwise = map (describeResolveError describePackageError) errs

  where

    credentials = [(u, t) | ObtainFailed u t _ (FetchFailed (NeedsCredentials _ _)) <- errs]

    needsCredentials (ObtainFailed _ _ _ (FetchFailed (NeedsCredentials _ _))) = True
    needsCredentials _ = False

-- | The ways to bring the packages that need credentials when the tool runs
-- in a command-line container, which has none, for the command that needs
-- them.
credentialsMessage :: String -> [(T.Text, T.Text)] -> String
credentialsMessage commandName packages = intercalate "\n" $
    ["git has no credentials in this container for:"]
    ++ ["  " ++ T.unpack u ++ " " ++ T.unpack t | (u, t) <- packages]
    ++ [ "To bring them, do one of these:"
       , "  1. On your computer, from the project folder, run:" ]
    ++ [ "       git clone --branch " ++ T.unpack t ++ " --depth 1 " ++ T.unpack u ++ " .termina/fetch/" ++ T.unpack (normalizeUrl u)
       | (u, t) <- packages ]
    ++ [ "     and then run " ++ commandName ++ " again here."
       , "  2. Run " ++ commandName ++ " with termina installed on your computer."
       , "  3. Open the project in VS Code, which passes your credentials to its container."
       , misspelled ]
