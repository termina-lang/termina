{-# LANGUAGE OverloadedStrings #-}

-- | "termina add": a dependency of the project or of one of its tests, with
-- its transitive closure, into "termina.yaml", "termina.lock" and "vendor/",
-- all of them or none.
module Command.Add (
    addCmdArgsParser, addCommand, AddCmdArgs,
    AddReport(..), addDependency
) where

import Data.List (intercalate, partition)
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Options.Applicative
import System.FilePath

import Command.Dependencies
import Command.Utils (Place(..), currentPlace, dieWithFirstError, warnMessage, infoMessage, loadLock)
import Configuration.Lock
import Configuration.Manifest
import Configuration.Resolve (ResolvedPackage(..))

data AddCmdArgs = AddCmdArgs String String (Maybe String)
  deriving Show

addCmdArgsParser :: Parser AddCmdArgs
addCmdArgsParser = AddCmdArgs
    <$> argument str (metavar "URL" <> help "https:// URL of the git repository of the package")
    <*> argument str (metavar "TAG" <> help "Tag of the version to add")
    <*> optional (strOption (long "test" <> metavar "NAME" <> help "Add it to the dependencies of this test only"))

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
-- found and leave the project as it was. The lock and vendor/ are written
-- first, and the manifest last.
addDependency :: Place -> FilePath -> T.Text -> T.Text -> Maybe String -> IO (Either [String] AddReport)
addDependency place root url tag mTest = do
    bytes <- BS.readFile (root </> "termina.yaml")
    lockResult <- loadLock root
    let text = either (const (Left ["termina.yaml: the file is not valid UTF-8"])) (Right . T.unpack) (TE.decodeUtf8' bytes)
    case (,) <$> (text >>= either (Left . pure) Right . prepare) <*> lockResult of
        Left errs -> return (Left errs)
        Right ((newText, manifest), lock) -> do
            synced <- syncDependencies place "termina add" root manifest lock
            case synced of
                Left errs -> return (Left errs)
                Right (Sync changes warnings) -> do
                    BS.writeFile (root </> "termina.yaml") (utf8 newText)
                    let (added, others) = partition isThis changes
                    return (Right (AddReport (map describe (added ++ others)) warnings))

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

    isThis (Added p) = normalizeUrl (resolvedUrl p) == key
    isThis _ = False

    -- | The added package first, and the ones it brings with the path that
    -- brought each.
    describe change@(Added p)
        | isThis change = "added " ++ T.unpack url ++ " " ++ T.unpack tag ++ maybe "" (" to the test " ++) mTest
        | otherwise = "pulled " ++ T.unpack (resolvedUrl p) ++ " " ++ T.unpack (resolvedTag p)
            ++ " (" ++ intercalate " -> " (resolvedPath p) ++ ")"
    describe change = describeChange change

-- | What the rule of git for SSH needs where termina runs. It only matters
-- in a container, which does not keep it or has no SSH keys.
sshNote :: Place -> String
sshNote Computer = ""
sshNote VSCodeContainer = "\nWhen it is rebuilt, this container loses its git configuration."
sshNote CommandLineContainer = "\nThis container has no SSH keys, so it cannot clone over SSH. Run termina add on your computer or in VS Code."
