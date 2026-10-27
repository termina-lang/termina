{-# LANGUAGE OverloadedStrings #-}

-- | The fetch of a package with the git of the system: the commit a tag
-- points to, and the files of that commit as git stores them, read from its
-- objects without a working tree.
module Configuration.Fetch (
    FetchError(..),
    FetchedFile(..),
    prefetchDir,
    resolveTag,
    fetchTag,
    fetchPackage
) where

import Control.Concurrent.Async (withAsync, wait)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (isInfixOf)
import qualified Data.Text as T
import System.Directory
import System.Environment (getEnvironment)
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process

import Configuration.Lock (normalizeUrl)

data FetchError =
    -- | The repository has no such tag.
    TagNotFound !T.Text !T.Text
    -- | The tag points to another commit than the expected one: the tag,
    -- the expected commit and the one it points to.
    | TagMoved !T.Text !T.Text !T.Text
    -- | Git could not get credentials for the repository: its URL and the
    -- error of git.
    | NeedsCredentials !T.Text !String
    -- | Any other error of git.
    | GitFailed !String
  deriving (Eq, Show)

-- | A file of a commit: its path, its mode as git stores it ("100644",
-- "100755", "120000" for a symbolic link, "160000" for a submodule) and its
-- bytes (none for a submodule).
data FetchedFile = FetchedFile {
    fetchedPath :: !String,
    fetchedMode :: !String,
    fetchedBytes :: !BS.ByteString
  } deriving (Eq, Show)

-- | The folder where the user may leave a clone of a package for the tool to
-- take instead of fetching it.
prefetchDir :: FilePath -> T.Text -> FilePath
prefetchDir root url = root </> ".termina" </> "fetch" </> joinPath (T.unpack <$> T.splitOn "/" (normalizeUrl url))

-- | Git in the C locale, so that its errors are in English whatever the
-- language of the user, and can be told apart.
gitProcess :: [String] -> IO CreateProcess
gitProcess args = do
    current <- getEnvironment
    return (proc "git" args) { env = Just (("LC_ALL", "C") : filter ((/= "LC_ALL") . fst) current) }

-- | Run git and give its output, or its error.
git :: T.Text -> [String] -> IO (Either FetchError String)
git url args = do
    p <- gitProcess args
    (code, out, err) <- readCreateProcessWithExitCode p ""
    return $ case code of
        ExitSuccess -> Right out
        ExitFailure _
            | any (`isInfixOf` err) ["could not read Username", "could not read Password", "Authentication failed"] ->
                Left (NeedsCredentials url err)
            | otherwise -> Left (GitFailed err)

-- | The commit that a tag of a repository points to, the one the tag
-- references when it is annotated.
resolveTag :: T.Text -> T.Text -> IO (Either FetchError T.Text)
resolveTag url tag = do
    result <- git url ["ls-remote", T.unpack url, "refs/tags/" ++ T.unpack tag, "refs/tags/" ++ T.unpack tag ++ "^{}"]
    return $ result >>= \out ->
        let refs = [(ref, sha) | l <- lines out, [sha, ref] <- [words l]]
            plain = "refs/tags/" ++ T.unpack tag
        in case (lookup (plain ++ "^{}") refs, lookup plain refs) of
            (Just sha, _) -> Right (T.pack sha)
            (Nothing, Just sha) -> Right (T.pack sha)
            _ -> Left (TagNotFound url tag)

-- | The commit of a tag and its files. The commit comes from the clone in the
-- prefetch folder of the project when there is one, and from the repository
-- otherwise.
fetchTag :: FilePath -> T.Text -> T.Text -> IO (Either FetchError (T.Text, [FetchedFile]))
fetchTag root url tag = do
    let local = prefetchDir root url
    prefetched <- doesDirectoryExist local
    commit <- if prefetched
        then either (const (Left (TagNotFound url tag))) (Right . T.pack . takeWhile (`notElem` ("\r\n" :: String)))
            <$> git url ["-C", local, "rev-parse", "--verify", "-q", "refs/tags/" ++ T.unpack tag ++ "^{commit}"]
        else resolveTag url tag
    case commit of
        Left err -> return (Left err)
        Right sha -> fmap ((,) sha) <$> fetchPackage root url tag sha

-- | The files of the commit of a tag, which has to be the expected one. A
-- clone in the prefetch folder of the project is taken instead of the
-- repository when there is one.
fetchPackage :: FilePath -> T.Text -> T.Text -> T.Text -> IO (Either FetchError [FetchedFile])
fetchPackage root url tag expected = do
    let local = prefetchDir root url
    prefetched <- doesDirectoryExist local
    if prefetched
        then readCommit local ("refs/tags/" ++ T.unpack tag ++ "^{commit}")
        else withSystemTempDirectory "termina-fetch" $ \tmp -> do
            initialized <- git url ["init", "--bare", "-q", tmp]
            fetched <- either (return . Left) (const $ git url ["-C", tmp, "fetch", "-q", "--depth", "1", "--no-tags", T.unpack url, "refs/tags/" ++ T.unpack tag]) initialized
            either (return . Left) (const $ readCommit tmp "FETCH_HEAD^{commit}") fetched

  where

    readCommit repo rev = do
        commit <- git url ["-C", repo, "rev-parse", "--verify", "-q", rev]
        case commit of
            Left _ -> return (Left (TagNotFound url tag))
            Right out
                | T.pack (trim out) /= expected -> return (Left (TagMoved tag expected (T.pack (trim out))))
                | otherwise -> readTree repo (T.unpack expected)

    trim = takeWhile (`notElem` ("\r\n" :: String))

    readTree repo sha = do
        listing <- git url ["-C", repo, "ls-tree", "-r", "-z", sha]
        case listing of
            Left err -> return (Left err)
            Right out -> do
                let entries = [entry | e <- splitNul out, not (null e), Just entry <- [parseEntry e]]
                    blobs = [oid | (_, kind, oid, _) <- entries, kind == "blob"]
                contents <- catBlobs repo blobs
                return $ fmap (\bs -> zipFiles entries bs) contents

    splitNul s = case break (== '\0') s of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitNul rest

    parseEntry e = case break (== '\t') e of
        (meta, _ : path) | [mode, kind, oid] <- words meta -> Just (mode, kind, oid, path)
        _ -> Nothing

    zipFiles [] _ = []
    zipFiles ((mode, "blob", _, path) : rest) (b : bs) = FetchedFile path mode b : zipFiles rest bs
    zipFiles ((mode, _, _, path) : rest) bs = FetchedFile path mode BS.empty : zipFiles rest bs

    -- The contents of the blobs, in their order, from one "git cat-file
    -- --batch"; the input is written apart so that neither pipe fills up.
    catBlobs _ [] = return (Right [])
    catBlobs repo oids = do
        p <- gitProcess ["-C", repo, "cat-file", "--batch"]
        (Just hin, Just hout, Just herr, ph) <- createProcess p {
            std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
        mapM_ (`hSetBinaryMode` True) [hin, hout]
        withAsync (BS.hPut hin (BC.pack (unlines oids)) >> hClose hin) $ \writer -> do
            out <- BS.hGetContents hout
            err <- hGetContents herr
            wait writer
            code <- waitForProcess ph
            return $ case code of
                ExitSuccess -> maybe (Left (GitFailed "git cat-file gave an output it does not document")) Right (parseBatch out)
                ExitFailure _ -> Left (GitFailed err)

    -- Each object of the output is "<oid> blob <size>\n<bytes>\n".
    parseBatch out
        | BS.null out = Just []
        | otherwise = do
            let (header, rest) = BC.break (== '\n') out
            sizeText <- case BC.words header of
                [_, "blob", s] -> Just s
                _ -> Nothing
            (size, _) <- BC.readInt sizeText
            let content = BS.take size (BS.drop 1 rest)
            others <- parseBatch (BS.drop (size + 2) rest)
            return (content : others)
