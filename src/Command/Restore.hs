{-# LANGUAGE OverloadedStrings #-}

-- | "termina restore": the copies in "vendor/" of the packages of the lock,
-- brought back from upstream when they are missing or were changed. It reads
-- neither the manifest nor resolves any tag, and writes only "vendor/".
module Command.Restore (
    restoreCmdArgsParser, restoreCommand, RestoreCmdArgs, restoreVendor
) where

import Control.Monad
import Data.Either (isLeft)
import qualified Data.Text as T
import Options.Applicative
import System.Directory (removePathForcibly)

import Command.Add (AddError(..), describeObtainErrors)
import Command.Utils (Place, currentPlace, dieWithFirstError, infoMessage, loadLock)
import Configuration.Fetch
import Configuration.Lock
import Configuration.Resolve (ResolveError(..))
import Configuration.Validate
import Configuration.Vendor

data RestoreCmdArgs = RestoreCmdArgs
  deriving Show

restoreCmdArgsParser :: Parser RestoreCmdArgs
restoreCmdArgsParser = pure RestoreCmdArgs

restoreCommand :: RestoreCmdArgs -> IO ()
restoreCommand _ = do
    place <- currentPlace
    result <- restoreVendor place "."
    case result of
        Left errs -> dieWithFirstError errs
        Right [] -> putStrLn (infoMessage "vendor/ matches termina.lock")
        Right restored -> mapM_ (putStrLn . infoMessage) restored

-- | Bring back every package of the lock whose copy in vendor/ is missing or
-- was changed, and give what was restored. Nothing is written unless every
-- package could be brought.
restoreVendor :: Place -> FilePath -> IO (Either [String] [String])
restoreVendor place root = do
    lockResult <- loadLock root
    case lockResult of
        Left errs -> return (Left errs)
        Right lock -> do
            broken <- filterM (fmap isLeft . lockedPackage root) (lockPackages lock)
            results <- mapM bring broken
            case [err | Left err <- results] of
                [] -> do
                    forM_ [restored | Right restored <- results] $ \(locked, files, fromPrefetch) -> do
                        writePackage root (lockedUrl locked) files
                        when fromPrefetch (removePathForcibly (prefetchDir root (lockedUrl locked)))
                    return $ Right
                        [ "restored vendor/" ++ T.unpack (normalizeUrl (lockedUrl locked)) ++ " (" ++ T.unpack (lockedRev locked) ++ ")"
                        | Right (locked, _, _) <- results ]
                errs -> return (Left (describeObtainErrors place "termina restore" errs))

  where

    -- | The locked commit of a package, validated and with the content-hash
    -- of the lock.
    bring locked = do
        let url = lockedUrl locked
            tag = lockedRev locked
            failed = Left . ObtainFailed url tag ["termina.lock"]
        fetched <- fetchLocked root url tag (lockedCommit locked)
        return $ case fetched of
            Left err -> failed (FetchFailed err)
            Right (Fetched _ files fromPrefetch) -> case validatePackage files of
                Left errs -> failed (InvalidPackage errs)
                Right manifest ->
                    let vendored = [(p, b) | FetchedFile p _ b <- files, isVendored manifest p]
                    in if contentHash vendored == lockedContentHash locked
                        then Right (locked, vendored, fromPrefetch)
                        else failed (VendorMismatch "the files that termina copies from this commit do not match the content-hash of termina.lock")
