{-# LANGUAGE OverloadedStrings #-}

-- | "termina add": a dependency of the project or of one of its tests, with
-- its transitive closure, into "termina.yaml", "termina.lock" and "vendor/",
-- all of them or none.
module Command.Add (
    addCmdArgsParser, addCommand, AddCmdArgs,
    AddError(..), AddReport(..), addDependency, describeObtainErrors, credentialsMessage
) where

import Control.Monad
import Data.IORef
import Data.List (find, intercalate)
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Options.Applicative
import System.Directory
import System.FilePath

import Command.Utils (Place(..), currentPlace, dieWithFirstError, warnMessage, infoMessage, loadLock)
import Configuration.Fetch
import Configuration.Graph
import Configuration.Lock
import Configuration.Manifest
import Configuration.Resolve
import Configuration.Validate
import Configuration.Vendor

data AddCmdArgs = AddCmdArgs String String (Maybe String)
  deriving Show

addCmdArgsParser :: Parser AddCmdArgs
addCmdArgsParser = AddCmdArgs
    <$> argument str (metavar "URL" <> help "https:// URL of the git repository of the package")
    <*> argument str (metavar "TAG" <> help "Tag of the version to add")
    <*> optional (strOption (long "test" <> metavar "NAME" <> help "Add it to the dependencies of this test only"))

-- | Why a package could not be brought.
data AddError =
    FetchFailed FetchError
    | InvalidPackage [String]
    -- | The copy in vendor/ of a locked package is missing or does not match
    -- its hash, and why.
    | VendorMismatch String
  deriving (Eq, Show)

-- | What "termina add" did, and what the user has to know about it.
data AddReport = AddReport {
    reportLines :: ![String],
    reportWarnings :: ![String]
  } deriving (Eq, Show)

addCommand :: AddCmdArgs -> IO ()
addCommand (AddCmdArgs url tag mTest) = do
    place <- currentPlace
    result <- addDependency place "." (T.pack url) (T.pack tag) mTest
    case result of
        Left errs -> dieWithFirstError errs
        Right report -> do
            mapM_ (putStrLn . warnMessage) (reportWarnings report)
            mapM_ (putStrLn . infoMessage) (reportLines report)

-- | Add the dependency to the project in the folder, or give every problem
-- found and leave the project as it was. In a command-line container, the
-- packages that need credentials are reported together with the ways to
-- bring them.
addDependency :: Place -> FilePath -> T.Text -> T.Text -> Maybe String -> IO (Either [String] AddReport)
addDependency place root url tag mTest = do
    bytes <- BS.readFile (root </> "termina.yaml")
    lockResult <- loadLock root
    let text = either (const (Left ["termina.yaml: the file is not valid UTF-8"])) (Right . T.unpack) (TE.decodeUtf8' bytes)
    case (,) <$> (text >>= either (Left . pure) Right . prepare) <*> lockResult of
        Left errs -> return (Left errs)
        Right ((newText, manifest), lock) -> do
            used <- newIORef []
            resolution <- resolve (obtain lock used) manifest
            case resolution of
                Left errs -> return (Left (describeObtainErrors place "termina add" errs))
                Right res -> case checkGraph manifest res of
                    [] -> Right <$> commit newText lock res used
                    errs -> return (Left errs)

  where

    key = normalizeUrl url

    parse = either (Left . ("termina.yaml: " ++)) Right . decodeManifest

    utf8 = TE.encodeUtf8 . T.pack

    -- | The new text of the manifest and the manifest it gives.
    prepare text = do
        either (Left . (++ sshNote place)) Right (checkGitUrl url)
        old <- parse (utf8 text)
        checkAbsent old
        newText <- addDependencyEntry text mTest url tag
        (,) newText <$> parse (utf8 newText)

    keysOf deps = [normalizeUrl (gitUrl g) | DepGit g <- deps]

    checkAbsent m =
        let inRoot = key `elem` keysOf (manifestDependencies m)
            inTests = [T.unpack t | (t, e) <- M.toList (manifestTests m), key `elem` keysOf (testDependencies e)]
        in case mTest of
            Nothing
                | inRoot -> Left $ T.unpack url ++ " is already a dependency of the project: use termina update to change its tag"
                | (t : _) <- inTests -> Left $ T.unpack url ++ " is already a dependency of the test " ++ t
                    ++ ": remove it there to make it a dependency of the project"
                | otherwise -> Right ()
            Just t
                | inRoot -> Left $ T.unpack url ++ " is already a dependency of the project, which the test " ++ t ++ " sees"
                | t `elem` inTests -> Left $ T.unpack url ++ " is already a dependency of the test " ++ t
                    ++ ": use termina update to change its tag"
                | otherwise -> Right ()

    -- | A package locked with the same tag is read from vendor/, without
    -- network and whatever its tag points to now; any other is fetched.
    obtain lock used u t =
        case find ((== normalizeUrl u) . normalizeUrl . lockedUrl) (lockPackages lock) of
            Just locked | lockedRev locked == t -> fromVendor locked
            _ -> fromGit used u t

    fromVendor locked = do
        copy <- lockedPackage root locked
        return $ case copy of
            Left err -> Left (VendorMismatch err)
            Right files -> case maybe (Left "vendor/ has no termina.yaml") parse (lookup "termina.yaml" files) of
                Left err -> Left (InvalidPackage [err])
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

    -- | Write vendor/, the lock and the manifest, in that order, and remove
    -- the clones of the prefetch folder that were used.
    commit newText lock res used = do
        let packages = M.elems (resolvedPackages res)
            oldKeys = M.fromList [(normalizeUrl (lockedUrl l), l) | l <- lockPackages lock]
            vendored p = [ (path, bytes) | FetchedFile path _ bytes <- obtainedFiles (resolvedObtained p)
                         , isVendored (obtainedManifest (resolvedObtained p)) path ]
            entry p = LockedPackage (resolvedUrl p) (resolvedTag p) (obtainedSha (resolvedObtained p)) (contentHash (vendored p))
            unchanged p = M.lookup (normalizeUrl (resolvedUrl p)) oldKeys == Just (entry p)
        forM_ packages $ \p -> unless (unchanged p) (writePackage root (resolvedUrl p) (vendored p))
        writeLockFile (root </> "termina.lock") (Lock (lockVersion emptyLock) (map entry packages))
        BS.writeFile (root </> "termina.yaml") (utf8 newText)
        readIORef used >>= mapM_ removePathForcibly
        let pulled = [ "pulled " ++ T.unpack (resolvedUrl p) ++ " " ++ T.unpack (resolvedTag p) ++ " (" ++ intercalate " -> " (resolvedPath p) ++ ")"
                     | p <- packages, let k = normalizeUrl (resolvedUrl p), k /= key, not (M.member k oldKeys) ]
        return AddReport {
            reportLines = ("added " ++ T.unpack url ++ " " ++ T.unpack tag ++ maybe "" (" to the test " ++) mTest) : pulled,
            reportWarnings =
                [ "override active: " ++ T.unpack (overriddenUrl o) ++ " forced to " ++ T.unpack (forcedTag o) ++ "; "
                    ++ intercalate " -> " (requestedBy o) ++ " requested " ++ T.unpack (requestedTag o)
                | o <- appliedOverrides res ]
                ++ [ "the overrides of " ++ T.unpack name ++ " are ignored (" ++ T.unpack u ++ "): only those of the project count"
                   | (name, u) <- ignoredOverrides res ]
          }

describeAdd :: AddError -> String
describeAdd (FetchFailed (TagNotFound _ t)) = "the repository has no tag " ++ T.unpack t
describeAdd (FetchFailed (TagMoved t expected actual)) =
    "the tag " ++ T.unpack t ++ " moved while it was fetched, from " ++ T.unpack expected ++ " to " ++ T.unpack actual
describeAdd (FetchFailed (NeedsCredentials _ err)) = "git has no credentials for the repository:\n" ++ err ++ misspelled
describeAdd (FetchFailed (GitFailed err)) = "git failed:\n" ++ err
describeAdd (InvalidPackage errs) = "the package is not valid:\n" ++ intercalate "\n" (map ("  " ++) errs)
describeAdd (VendorMismatch err) = err

-- | What the rule of git for SSH needs where termina runs. It only matters
-- in a container, which does not keep it or has no SSH keys.
sshNote :: Place -> String
sshNote Computer = ""
sshNote VSCodeContainer = "\nWhen it is rebuilt, this container loses its git configuration."
sshNote CommandLineContainer = "\nThis container has no SSH keys, so it cannot clone over SSH. Run termina add on your computer or in VS Code."

-- | GitHub asks for credentials for a repository that does not exist, so as
-- not to tell whether a private one has that name.
misspelled :: String
misspelled = "GitHub also asks for credentials when a repository does not exist, so if it is public, check its URL."

-- | The errors of bringing packages. In a command-line container, the
-- packages that need credentials go first and together, with the ways to
-- bring them, so that the user clones all of them on the computer at once.
describeObtainErrors :: Place -> String -> [ResolveError AddError] -> [String]
describeObtainErrors place commandName errs
    | place == CommandLineContainer && not (null credentials) =
        credentialsMessage commandName credentials : map (describeResolveError describeAdd) (filter (not . needsCredentials) errs)
    | otherwise = map (describeResolveError describeAdd) errs

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
