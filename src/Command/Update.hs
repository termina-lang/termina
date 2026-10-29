{-# LANGUAGE OverloadedStrings #-}

-- | "termina update", which resolves the tags of the manifest again, even
-- those that the lock already has, and "termina verify", which checks
-- without network that vendor/ and the lock match the manifest.
module Command.Update (
    updateCmdArgsParser, updateCommand, UpdateCmdArgs,
    verifyCmdArgsParser, verifyCommand, VerifyCmdArgs,
    updateDependencies, checkTags, verifyProject
) where

import Control.Monad
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Options.Applicative
import System.Exit (exitFailure)
import System.FilePath

import Command.Dependencies
import Command.Utils (Place, currentPlace, dieWithFirstError, infoMessage, loadLock, warnMessage)
import Configuration.Fetch (resolveTag)
import Configuration.Lock
import Configuration.Manifest
import Configuration.Vendor (packageDir)

data UpdateCmdArgs = UpdateCmdArgs (Maybe String) Bool
  deriving Show

updateCmdArgsParser :: Parser UpdateCmdArgs
updateCmdArgsParser = UpdateCmdArgs
    <$> optional (argument str (metavar "DEPENDENCY" <> help "URL or name of the only dependency to update"))
    <*> switch (long "check" <> help "Report what update would change, and fail if anything would, without writing")

data VerifyCmdArgs = VerifyCmdArgs
  deriving Show

verifyCmdArgsParser :: Parser VerifyCmdArgs
verifyCmdArgsParser = pure VerifyCmdArgs

updateCommand :: UpdateCmdArgs -> IO ()
updateCommand (UpdateCmdArgs _ True) = do
    result <- checkTags "."
    case result of
        Left errs -> dieWithFirstError errs
        Right [] -> putStrLn (infoMessage "termina.lock matches termina.yaml and the tags of its repositories")
        Right drift -> mapM_ (putStrLn . warnMessage) drift >> exitFailure
updateCommand (UpdateCmdArgs mDep False) = do
    place <- currentPlace
    result <- updateDependencies place "." mDep
    case result of
        Left errs -> dieWithFirstError errs
        Right (warnings, report) -> do
            mapM_ (putStrLn . warnMessage) warnings
            mapM_ (putStrLn . infoMessage) report

verifyCommand :: VerifyCmdArgs -> IO ()
verifyCommand _ = do
    place <- currentPlace
    verifyProject place "." >>= either dieWithFirstError (const (putStrLn (infoMessage "vendor/ and termina.lock match termina.yaml")))

-- | The manifest and the lock of the project in a folder.
loadProjectFiles :: FilePath -> IO (Either [String] (Manifest, Lock))
loadProjectFiles root = do
    manifest <- either (Left . pure . ("termina.yaml: " ++)) Right <$> decodeManifestFile (root </> "termina.yaml")
    lock <- loadLock root
    return ((,) <$> manifest <*> lock)

-- | The git dependencies of the project and of its tests.
gitDependencies :: Manifest -> [GitDependency]
gitDependencies m = [g | DepGit g <- manifestDependencies m ++ concatMap testDependencies (M.elems (manifestTests m))]

-- | Bring the lock and vendor/ in line with the manifest, fetching again
-- every package, or only the one named, and give the warnings and what
-- changed.
updateDependencies :: Place -> FilePath -> Maybe String -> IO (Either [String] ([String], [String]))
updateDependencies place root mDep = do
    files <- loadProjectFiles root
    case files of
        Left errs -> return (Left errs)
        Right (manifest, lock) -> do
            names <- forM (lockPackages lock) $ \l -> do
                vendored <- decodeManifestFile (packageDir root (lockedUrl l) </> "termina.yaml")
                return (normalizeUrl (lockedUrl l), either (const Nothing) (Just . T.unpack . projectName . manifestProject) vendored)
            let known = map (normalizeUrl . gitUrl) (gitDependencies manifest) ++ map fst names
                matches dep key = key == normalizeUrl (T.pack dep) || lookup key names == Just (Just dep)
                    || last (T.splitOn "/" key) == T.pack dep
            case mDep of
                Just dep | not (any (matches dep) known) ->
                    return (Left [dep ++ " is not a dependency of this project."])
                _ -> do
                    synced <- syncDependencies place "termina update" (Refresh (maybe (const True) matches mDep)) True root manifest lock
                    return $ flip fmap synced $ \(Sync changes warnings) ->
                        (warnings, if null changes then ["termina.lock is up to date"] else map (("termina.lock: " ++) . describeChange) changes)

-- | What "termina update" would change, without writing: the tags of the
-- manifest that the lock has at another tag or not at all, and the tags of
-- the lock that now point to another commit.
checkTags :: FilePath -> IO (Either [String] [String])
checkTags root = do
    files <- loadProjectFiles root
    case files of
        Left errs -> return (Left errs)
        Right (manifest, lock) -> do
            let locked url = find ((== normalizeUrl url) . normalizeUrl . lockedUrl) (lockPackages lock)
                manifestDrift =
                    [ case locked (gitUrl g) of
                        Nothing -> "termina.yaml asks for " ++ T.unpack (gitUrl g) ++ " " ++ T.unpack (gitRev g) ++ ", which termina.lock does not have"
                        Just l -> "termina.yaml asks for " ++ T.unpack (gitRev g) ++ " of " ++ T.unpack (gitUrl g)
                            ++ ", and termina.lock has " ++ T.unpack (lockedRev l)
                    | g <- gitDependencies manifest, maybe True ((/= gitRev g) . lockedRev) (locked (gitUrl g)) ]
            tags <- forM (lockPackages lock) $ \l -> do
                resolved <- resolveTag (lockedUrl l) (lockedRev l)
                return $ case resolved of
                    Left err -> Left (T.unpack (lockedUrl l) ++ " " ++ T.unpack (lockedRev l) ++ ": " ++ describePackageError (FetchFailed err))
                    Right sha
                        | sha /= lockedCommit l -> Right ["the tag " ++ T.unpack (lockedRev l) ++ " of " ++ T.unpack (lockedUrl l)
                            ++ " now points to " ++ shortSha sha ++ ", and termina.lock has " ++ shortSha (lockedCommit l)]
                        | otherwise -> Right []
            return $ case [err | Left err <- tags] of
                (err : _) -> Left [err]
                [] -> Right (manifestDrift ++ concat [drift | Right drift <- tags])

  where

    shortSha = take 7 . T.unpack

-- | Check, without network and without writing, that every package that the
-- manifest needs is in the lock at its tag, that its copy in vendor/ matches
-- the lock, that the lock holds nothing else, and that no dependency comes
-- from a local folder.
verifyProject :: Place -> FilePath -> IO (Either [String] ())
verifyProject place root = do
    files <- loadProjectFiles root
    case files of
        Left errs -> return (Left errs)
        Right (manifest, lock) ->
            case [p | DepPath p <- manifestDependencies manifest ++ concatMap testDependencies (M.elems (manifestTests manifest))] of
                (p : _) -> return (Left ["termina.yaml takes " ++ p ++ " from a local folder, which termina.lock cannot pin."])
                [] -> do
                    synced <- syncDependencies place "termina verify" Offline False root manifest lock
                    return $ synced >>= \(Sync changes _) -> case changes of
                        [] -> Right ()
                        _ -> Left (map describe changes)

  where

    describe (Removed l) = "termina.lock has " ++ T.unpack (lockedUrl l) ++ " " ++ T.unpack (lockedRev l)
        ++ ", which termina.yaml no longer needs. Run termina build or termina update to update termina.lock."
    describe change = "termina.lock does not match termina.yaml: " ++ describeChange change
        ++ ". Run termina build or termina update to update termina.lock."
