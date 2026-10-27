{-# LANGUAGE OverloadedStrings #-}

-- | The manifest of a project, "termina.yaml": what the project is, what it
-- produces (a library with "exports" or an application with "app/"), what it
-- depends on, and the deployments it builds (the application and the tests).
module Configuration.Manifest (
    Manifest(..),
    ProjectInfo(..),
    Assurance(..),
    Standard(..),
    Nature(..),
    Prefix(..),
    Dependency(..),
    GitDependency(..),
    AppDeployment(..),
    TestEntry(..),
    knownManifestVersions,
    defaultTestLogSize,
    parseManifest,
    decodeManifest,
    decodeManifestFile,
    checkLayout,
    Deployment(..),
    appDeployment,
    appConfig,
    defaultTestProfile,
    testDeployment,
    testConfig,
    newApplicationManifest,
    newLibraryManifest,
    defaultTestPlatforms,
    addTestEntry,
    addDependencyEntry,
    checkGitUrl,
    isSnakeCase
) where

import Control.Monad
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isDigit)
import Data.List (intercalate, isPrefixOf, nub, sort, stripPrefix, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Yaml
import System.Directory
import System.FilePath

import Configuration.Configuration
import Configuration.Platform

-- | The versions of the format of the manifest that this transpiler reads.
knownManifestVersions :: [Int]
knownManifestVersions = [1]

-- | Bytes of the record of results of a test when its entry does not say.
defaultTestLogSize :: Integer
defaultTestLogSize = 8192

data Manifest = Manifest {
    manifestProject :: !ProjectInfo,
    manifestNature :: !Nature,
    manifestDependencies :: ![Dependency],
    manifestOverrides :: ![GitDependency],
    manifestPrintBufSize :: !Integer,
    manifestReadBufSize :: !Integer,
    manifestExtern :: !ExternConfig,
    manifestTests :: !(M.Map T.Text TestEntry)
  } deriving (Eq, Show)

data ProjectInfo = ProjectInfo {
    projectName :: !T.Text,
    projectVersion :: !T.Text,
    projectManifestVersion :: !Int,
    projectAssurance :: ![Assurance]
  } deriving (Eq, Show)

data Standard = ECSSEST40 | DO178C
  deriving (Eq, Show)

data Assurance = Assurance {
    assuranceStandard :: !Standard,
    assuranceLevel :: !Char
  } deriving (Eq, Show)

-- | A namespace prefix that a library owns, such as "drivers.gaisler".
newtype Prefix = Prefix [T.Text]
  deriving (Eq, Show)

-- | A project is a library, with "exports" and no "app/", or an application,
-- with "app/" and no "exports".
data Nature = Library ![Prefix] | Application !AppDeployment
  deriving (Eq, Show)

data GitDependency = GitDependency {
    gitUrl :: !T.Text,
    gitRev :: !T.Text
  } deriving (Eq, Show)

data Dependency = DepGit !GitDependency | DepPath !FilePath
  deriving (Eq, Show)

data AppDeployment = AppDeployment {
    appPlatform :: !Platform,
    appProfile :: !ProjectProfile,
    appMicrosecondsPerTick :: !(Maybe Integer)
  } deriving (Eq, Show)

data TestEntry = TestEntry {
    testPlatforms :: ![Platform],
    testTimeout :: !Integer,
    testLogSize :: !Integer,
    testMicrosecondsPerTick :: !(Maybe Integer),
    testExtern :: !ExternConfig,
    testDependencies :: ![Dependency]
  } deriving (Eq, Show)

-- | Parse a manifest. The version of the format is checked first, so that a
-- manifest of another version is rejected for its version and not for the
-- first field this transpiler does not know.
parseManifest :: Value -> Either String Manifest
parseManifest v = do
    ver <- parseEither manifestVersion v
    unless (ver `elem` knownManifestVersions) $
        Left $ "manifest-version " ++ show ver ++ " is not supported by this version of termina (supported: "
            ++ unwords (map show knownManifestVersions) ++ ")"
    parseEither manifestParser v

  where

    manifestVersion :: Value -> Parser Int
    manifestVersion = withObject "manifest" $ \o -> do
        mProject <- o .:? "project"
        mVersion <- maybe (return Nothing) (withObject "project" (.:? "manifest-version")) mProject
        maybe (fail "the manifest has no project.manifest-version") return mVersion

-- | Parse the bytes of a manifest.
decodeManifest :: BS.ByteString -> Either String Manifest
decodeManifest = fromYaml . decodeEither'

-- | Parse the manifest in a file; a missing file is an error, not an exception.
decodeManifestFile :: FilePath -> IO (Either String Manifest)
decodeManifestFile path = fromYaml <$> decodeFileEither path

fromYaml :: Either ParseException Value -> Either String Manifest
fromYaml = either (Left . prettyPrintParseException) parseManifest

-- | Fail on a field that the object does not admit, suggesting the admitted
-- one closest to it.
onlyFields :: String -> [K.Key] -> Object -> Parser ()
onlyFields what allowed o =
    case filter (`notElem` allowed) (KM.keys o) of
        [] -> return ()
        (k : _) -> fail $ "unknown field \"" ++ K.toString k ++ "\" in " ++ what ++ suggestion (K.toString k)

  where

    suggestion field =
        case [a | a <- map K.toString allowed, editDistance field a <= 2] of
            (a : _) -> ", did you mean \"" ++ a ++ "\"?"
            [] -> ""

editDistance :: String -> String -> Int
editDistance a b = last (foldl step [0 .. length a] b)

  where

    step prev@(p : ps) c = scanl compute (p + 1) (zip3 a prev ps)
      where compute left (ca, diag, up) = minimum [up + 1, left + 1, diag + fromEnum (ca /= c)]
    step [] _ = []

-- | Lowercase snake_case, the form of every segment of a module name.
isSnakeCase :: String -> Bool
isSnakeCase (c : cs) = isAsciiLower c && all (\x -> isAsciiLower x || isDigit x || x == '_') cs
isSnakeCase [] = False

manifestParser :: Value -> Parser Manifest
manifestParser = withObject "manifest" $ \o -> do
    onlyFields "the manifest" ["project", "exports", "dependencies", "overrides",
        "sys-print-output-buffer-size", "sys-read-input-buffer-size", "extern", "app", "tests"] o
    project <- o .: "project" >>= projectParser
    mExports <- o .:? "exports"
    mApp <- o .:? "app"
    nature <- case (mExports, mApp) of
        (Just _, Just _) -> fail "a project has either exports (a library) or app (an application), not both"
        (Nothing, Nothing) -> fail "a project needs either exports (a library) or app (an application)"
        (Just exports, Nothing) -> Library <$> mapM prefixParser exports
        (Nothing, Just app) -> Application <$> appParser app
    Manifest project nature
        <$> (o .:? "dependencies" .!= [] >>= mapM dependencyParser)
        <*> (o .:? "overrides" .!= [] >>= mapM gitDependencyParser)
        <*> (o .:? "sys-print-output-buffer-size" .!= defaultSysPrintOutputBufferSize >>= positive "sys-print-output-buffer-size")
        <*> (o .:? "sys-read-input-buffer-size" .!= defaultSysReadInputBufferSize >>= positive "sys-read-input-buffer-size")
        <*> (o .:? "extern" >>= maybe (return emptyExternConfig) externParser)
        <*> (o .:? "tests" .!= M.empty >>= M.traverseWithKey testParser)

projectParser :: Value -> Parser ProjectInfo
projectParser = withObject "project" $ \o -> do
    onlyFields "project" ["name", "version", "manifest-version", "assurance"] o
    ProjectInfo
        <$> o .: "name"
        <*> o .: "version"
        <*> o .: "manifest-version"
        <*> (o .:? "assurance" .!= [] >>= mapM assuranceParser >>= noRepeatedStandard)

  where

    noRepeatedStandard assurances = do
        let standards = map assuranceStandard assurances
        when (length (nub standards) /= length standards) $
            fail "a standard appears twice in project.assurance"
        return assurances

assuranceParser :: Value -> Parser Assurance
assuranceParser = withObject "assurance" $ \o -> do
    onlyFields "an assurance entry" ["standard", "level"] o
    standard <- o .: "standard"
    level <- o .: "level"
    case (standard :: T.Text, T.unpack level) of
        ("ecss-e-st-40", [l]) | l `elem` ['A' .. 'D'] -> return (Assurance ECSSEST40 l)
        ("do-178c", [l]) | l `elem` ['A' .. 'E'] -> return (Assurance DO178C l)
        ("ecss-e-st-40", _) -> fail $ "the level of ecss-e-st-40 is a criticality from A to D, not " ++ T.unpack level
        ("do-178c", _) -> fail $ "the level of do-178c is a DAL from A to E, not " ++ T.unpack level
        _ -> fail $ "unknown standard \"" ++ T.unpack standard ++ "\" (known: ecss-e-st-40, do-178c)"

prefixParser :: T.Text -> Parser Prefix
prefixParser txt =
    let segments = T.splitOn "." txt in
    if all (isSnakeCase . T.unpack) segments
        then return (Prefix segments)
        else fail $ "the export \"" ++ T.unpack txt ++ "\" is not a module prefix (lowercase snake_case segments separated by dots)"

dependencyParser :: Value -> Parser Dependency
dependencyParser = withObject "dependency" $ \o ->
    if KM.member "path" o
        then do
            onlyFields "a path dependency" ["path"] o
            DepPath <$> o .: "path"
        else DepGit <$> gitDependencyParser (Object o)

gitDependencyParser :: Value -> Parser GitDependency
gitDependencyParser = withObject "dependency" $ \o -> do
    onlyFields "a git dependency" ["git", "rev"] o
    url <- o .: "git"
    either fail return (checkGitUrl url)
    rev <- o .: "rev"
    GitDependency url <$> case rev of
        String tag -> return tag
        -- YAML reads 1.0 or 1.10 as a number, and 1.10 becomes 1.1, so the tag is lost.
        Number _ -> fail $ "the rev of " ++ T.unpack url ++ " is read as a number. Write the tag in quotes, as in rev: \"1.10\""
        _ -> fail $ "the rev of " ++ T.unpack url ++ " is not a tag"

-- | A dependency is written with https://. An SSH address gets its https://
-- form and the rule that makes git use SSH for it on this machine.
checkGitUrl :: T.Text -> Either String ()
checkGitUrl url
    | "https://" `T.isPrefixOf` url = Right ()
    | otherwise = Left (notHttps (T.unpack url))

  where

    notHttps u = case sshParts u of
        Just (host, path) -> intercalate "\n" [
            u ++ " is an SSH address; dependencies only take https:// addresses.",
            "Use https://" ++ host ++ "/" ++ path ++ " instead.",
            "If you want git to clone it over SSH, run this once on your computer:",
            "  git config --global url.\"git@" ++ host ++ ":\".insteadOf \"https://" ++ host ++ "/\"",
            "git will then use SSH for every https://" ++ host ++ "/ address." ]
        Nothing -> u ++ " is not an https:// URL, and dependencies are fetched with https://"

    sshParts s
        | Just rest <- stripPrefix "ssh://" s = case break (== '/') (dropUser rest) of
            (host, _ : path) -> Just (host, path)
            _ -> Nothing
        | (user, '@' : rest) <- break (== '@') s, not (null user), '/' `notElem` user = case break (== ':') rest of
            (host, _ : path) -> Just (host, path)
            _ -> Nothing
        | otherwise = Nothing

    dropUser s = case break (== '@') s of
        (_, _ : host) -> host
        _ -> s

platformParser :: T.Text -> Parser Platform
platformParser pltName =
    maybe (fail $ "unsupported platform \"" ++ T.unpack pltName ++ "\"") return (checkPlatform (T.unpack pltName))

profileParser :: T.Text -> Parser ProjectProfile
profileParser "debug" = return Debug
profileParser "release" = return Release
profileParser other = fail $ "unknown profile \"" ++ T.unpack other ++ "\" (known: debug, release)"

-- | The length of the tick, which has to divide 1000000 exactly because the
-- runtime derives the ticks per second from it.
tickParser :: Maybe Integer -> Parser (Maybe Integer)
tickParser Nothing = return Nothing
tickParser (Just ticks) =
    if ticks > 0 && 1000000 `mod` ticks == 0
        then return (Just ticks)
        else fail "microseconds-per-tick must divide 1000000 exactly"

positive :: String -> Integer -> Parser Integer
positive what n = if n > 0 then return n else fail $ what ++ " must be positive"

externParser :: Value -> Parser ExternConfig
externParser v = do
    withObject "extern" (onlyFields "extern" ["sources", "libraries"]) v
    parseJSON v

appParser :: Value -> Parser AppDeployment
appParser = withObject "app" $ \o -> do
    onlyFields "app" ["platform", "profile", "microseconds-per-tick"] o
    AppDeployment
        <$> (o .: "platform" >>= platformParser)
        <*> (o .: "profile" >>= profileParser)
        <*> (o .:? "microseconds-per-tick" >>= tickParser)

testParser :: T.Text -> Value -> Parser TestEntry
testParser testName v = do
    let what = "test " ++ T.unpack testName
    unless (isSnakeCase (T.unpack testName)) $
        fail $ "the test name \"" ++ T.unpack testName ++ "\" is not lowercase snake_case"
    withObject what (\o -> do
        -- One resolution serves the whole project, so an override is forced in every build.
        when (KM.member "overrides" o) $
            fail $ what ++ " has overrides: they go at the top level of the manifest and apply to the whole project"
        onlyFields what ["platforms", "timeout", "test-log-size",
            "microseconds-per-tick", "extern", "dependencies"] o
        platformNames <- o .: "platforms"
        when (null platformNames) $ fail $ what ++ " has no platforms"
        when (length (nub platformNames) /= length platformNames) $
            fail $ what ++ " lists a platform twice"
        TestEntry
            <$> mapM platformParser platformNames
            <*> (o .: "timeout" >>= positive "timeout")
            <*> (o .:? "test-log-size" .!= defaultTestLogSize >>= positive "test-log-size")
            <*> (o .:? "microseconds-per-tick" >>= tickParser)
            <*> (o .:? "extern" >>= maybe (return emptyExternConfig) externParser)
            <*> (o .:? "dependencies" .!= [] >>= mapM dependencyParser)) v

-- | Check the folders of the project against the manifest, and collect every
-- mismatch so that all of them are reported at once.
checkLayout :: FilePath -> Manifest -> IO [String]
checkLayout root manifest = do
    appErrors <- checkApp
    testsErrors <- checkTests
    srcErrors <- checkSources "src"
    testSrcErrors <- concat <$> mapM (\t -> checkSources ("tests" </> T.unpack t </> "src")) (M.keys (manifestTests manifest))
    shadowErrors <- concat <$> mapM (checkShadowing . T.unpack) (M.keys (manifestTests manifest))
    return (appErrors ++ testsErrors ++ srcErrors ++ testSrcErrors ++ shadowErrors)

  where

    -- | A module of a test with the name of a module of the project would
    -- leave one of the two out of the build.
    checkShadowing t = do
        let testSrc = "tests" </> t </> "src"
        modules <- finFiles testSrc
        clashes <- filterM (\m -> doesFileExist (root </> "src" </> m)) modules
        return [testSrc </> m ++ ": the project has a module with the same name in src/" | m <- clashes]

    finFiles dir = do
        exists <- doesDirectoryExist (root </> dir)
        if not exists then return [] else go dir ""
      where
        go base rel = do
            entries <- visible <$> listDirectory (root </> base </> rel)
            concat <$> forM entries (\e -> do
                let relEntry = if null rel then e else rel </> e
                isDir <- doesDirectoryExist (root </> base </> relEntry)
                if isDir then go base relEntry
                    else return [relEntry | takeExtension e == ".fin"])

    visible = filter (not . ("." `isPrefixOf`))

    checkApp = do
        hasApp <- doesDirectoryExist (root </> appFolder)
        case manifestNature manifest of
            Library _ ->
                return ["a library, with exports, cannot have an app/ folder" | hasApp]
            Application _ | not hasApp ->
                return ["the manifest has an app section but there is no app/ folder"]
            Application _ -> do
                entries <- visible <$> listDirectory (root </> appFolder)
                let appFile = appFilename <.> "fin"
                return $ ["app/ has no " ++ appFile | appFile `notElem` entries]
                    ++ ["app/" ++ e ++ ": app/ may only contain " ++ appFile | e <- entries, e /= appFile]

    checkTests = do
        hasTests <- doesDirectoryExist (root </> "tests")
        entries <- if hasTests then sort . visible <$> listDirectory (root </> "tests") else return []
        let declared = map T.unpack (M.keys (manifestTests manifest))
        entryErrors <- forM entries $ \e -> do
            isDir <- doesDirectoryExist (root </> "tests" </> e)
            hasTestFile <- doesFileExist (root </> "tests" </> e </> "test.fin")
            return $ if not isDir
                then ["tests/" ++ e ++ " is not a folder"]
                else ["tests/" ++ e ++ " has no test.fin" | not hasTestFile]
                    ++ ["tests/" ++ e ++ " is not declared in the tests section of the manifest" | e `notElem` declared]
        return $ concat entryErrors
            ++ ["the test " ++ t ++ " has no folder tests/" ++ t | t <- declared \\ entries]

    -- | No module directly under the folder, and every segment of a module
    -- name in lowercase snake_case.
    checkSources dir = do
        exists <- doesDirectoryExist (root </> dir)
        if not exists then return [] else do
            entries <- visible <$> listDirectory (root </> dir)
            topErrors <- forM entries $ \e -> do
                isDir <- doesDirectoryExist (root </> dir </> e)
                return [dir </> e ++ ": a module cannot live directly under " ++ dir ++ "/" | not isDir && takeExtension e == ".fin"]
            nameErrors <- concat <$> mapM (checkNames dir) entries
            return (concat topErrors ++ nameErrors)

    checkNames parent e = do
        let path = parent </> e
        isDir <- doesDirectoryExist (root </> path)
        if isDir
            then do
                children <- visible <$> listDirectory (root </> path)
                rest <- concat <$> mapM (checkNames path) children
                return $ [path ++ ": the folders of modules are lowercase snake_case" | not (isSnakeCase e)] ++ rest
            else return [path ++ ": the names of modules are lowercase snake_case"
                | takeExtension e == ".fin", not (isSnakeCase (dropExtension e))]

-- | What the build of one deployment needs: the application, or a test on one
-- of its platforms.
data Deployment = Deployment {
    -- | How messages name the deployment
    deploymentLabel :: !String,
    deploymentConfig :: !TerminaConfig,
    deploymentPlatform :: !Platform,
    -- | Folder and name of the module that wires the deployment
    deploymentMainFolder :: !FilePath,
    deploymentMainModule :: !FilePath,
    -- | Folders where the imported modules are looked up
    deploymentSources :: ![FilePath],
    -- | Folders of C, each one with its section of the manifest
    deploymentExterns :: ![(FilePath, ExternConfig)],
    -- | The C without #line pragmas of the deployment, for static analysis
    -- and for delivery
    deploymentPlainConfig :: !TerminaConfig
  } deriving (Eq, Show)

-- | The configuration of a deployment, in the form the rest of the transpiler
-- reads. The application is built into the "app" folder of the output and its
-- binary takes the name of the project; each test on each of its platforms
-- into "tests/<name>/<platform>", and its binary takes the name of the test.
configFor :: Manifest -> DeploymentKind -> Platform -> ProjectProfile -> Maybe Integer -> TerminaConfig
configFor manifest kind plt prf tick = TerminaConfig {
    name = binaryName,
    platform = T.pack (show plt),
    sourceModulesFolder = "src",
    outputFolder = output,
    efpFolder = efpFolderName,
    profile = prf,
    sysPrintOutputBufferSize = manifestPrintBufSize manifest,
    sysReadInputBufferSize = manifestReadBufSize manifest,
    microsecondsPerTick = fromMaybe defaultMicrosecondsPerTick tick,
    deploymentKind = kind,
    lineDirectives = True
  }

  where

    (binaryName, output) = case kind of
        ApplicationBuild -> (projectName (manifestProject manifest), outputFolderName </> "app")
        TestBuild testName -> (testName, outputFolderName </> "tests" </> T.unpack testName </> show plt)

-- | The application, or nothing in a library.
appDeployment :: Manifest -> Maybe Deployment
appDeployment manifest =
    case manifestNature manifest of
        Library _ -> Nothing
        Application app ->
          let config = configFor manifest ApplicationBuild (appPlatform app) (appProfile app) (appMicrosecondsPerTick app)
          in Just Deployment {
            deploymentLabel = "the application",
            deploymentConfig = config,
            deploymentPlatform = appPlatform app,
            deploymentMainFolder = appFolder,
            deploymentMainModule = appFilename,
            deploymentSources = ["src"],
            deploymentExterns = [("extern", manifestExtern manifest)],
            deploymentPlainConfig = plainConfigFor config
          }

-- | The configuration of the plain C of a deployment, which goes to the same
-- folder under "plain" in the output: "plain/app", "plain/tests/<name>/<platform>".
plainConfigFor :: TerminaConfig -> TerminaConfig
plainConfigFor config = config {
    lineDirectives = False,
    outputFolder = outputFolderName </> "plain" </> makeRelative outputFolderName (outputFolder config)
  }

-- | The configuration of the application, or nothing in a library.
appConfig :: Manifest -> Maybe TerminaConfig
appConfig = fmap deploymentConfig . appDeployment

-- | The profile of a test when the run does not choose one: the one of the
-- application, and debug in a library.
defaultTestProfile :: Manifest -> ProjectProfile
defaultTestProfile manifest =
    case manifestNature manifest of
        Application app -> appProfile app
        Library _ -> Debug

-- | A test on one of its platforms, built with the given profile. Its modules
-- are looked up in the "src" of the project and in its own, and its C is the
-- one of the project plus its own.
testDeployment :: Manifest -> T.Text -> TestEntry -> Platform -> ProjectProfile -> Deployment
testDeployment manifest testName entry plt prf = Deployment {
    deploymentLabel = "the test " ++ T.unpack testName ++ " on " ++ show plt,
    deploymentConfig = config,
    deploymentPlatform = plt,
    deploymentMainFolder = testFolder,
    deploymentMainModule = "test",
    deploymentSources = ["src", testFolder </> "src"],
    deploymentExterns = [("extern", manifestExtern manifest), (testFolder </> "extern", testExtern entry)],
    deploymentPlainConfig = plainConfigFor config
  }

  where

    config = testConfig manifest testName entry plt prf

    testFolder = "tests" </> T.unpack testName

-- | The configuration of a test on one of its platforms, built with the given
-- profile. A test without its own tick takes the one of the application.
testConfig :: Manifest -> T.Text -> TestEntry -> Platform -> ProjectProfile -> TerminaConfig
testConfig manifest testName entry plt prf =
    configFor manifest (TestBuild testName) plt prf tick

  where

    tick = case (testMicrosecondsPerTick entry, manifestNature manifest) of
        (Just t, _) -> Just t
        (Nothing, Application app) -> appMicrosecondsPerTick app
        (Nothing, Library _) -> Nothing

-- | The manifest of a new library, which exports one prefix.
newLibraryManifest :: String -> String -> String
newLibraryManifest projectNameStr prefix = unlines [
        "project:",
        "  name: " ++ projectNameStr,
        "  version: 0.1.0",
        "  manifest-version: " ++ show (maximum knownManifestVersions),
        "",
        "exports:",
        "  - " ++ prefix
    ]

-- | The platforms of a new test: the host, and the platform of the
-- application when it is another one, so that the test passes on both.
defaultTestPlatforms :: Manifest -> [Platform]
defaultTestPlatforms manifest =
    case manifestNature manifest of
        Application app | appPlatform app /= POSIXGCC -> [POSIXGCC, appPlatform app]
        _ -> [POSIXGCC]

-- | Add the entry of a test at the end of the "tests" section of the text of
-- a manifest, or in a new section at the end of the text. The rest of the
-- text is kept as it is, comments included.
addTestEntry :: String -> String -> [Platform] -> Integer -> Either String String
addTestEntry original testName plts timeout =
    case break (isSectionLine "tests") ls of
        (_, []) -> Right $ unlines (ls ++ ["", "tests:"] ++ entry "  ")
        (before, header : rest)
            | not (isBlockHeader header) -> Left "the tests section of termina.yaml is not written in block style"
            | otherwise ->
                let (block, after) = break startsSection rest
                    (content, blanks) = splitTrailingBlanks block
                    unit = case filter (not . all (== ' ')) content of
                        (l : _) -> takeWhile (== ' ') l
                        [] -> "  "
                in Right $ unlines (before ++ [header] ++ content ++ entry unit ++ blanks ++ after)

  where

    ls = lines original

    entry unit = [
        unit ++ testName ++ ":",
        unit ++ unit ++ "platforms: [" ++ intercalate ", " (map show plts) ++ "]",
        unit ++ unit ++ "timeout: " ++ show timeout
      ]

    isSectionLine key l = (key ++ ":") `isPrefixOf` l

    isBlockHeader l = all (== ' ') (takeWhile (/= '#') (drop (length ("tests:" :: String)) l))

    startsSection (c : _) = c /= ' '
    startsSection [] = False

-- | The lines of a block without its trailing blank lines, and those lines.
splitTrailingBlanks :: [String] -> ([String], [String])
splitTrailingBlanks block =
    let (blanks, content) = span (all (== ' ')) (reverse block)
    in (reverse content, reverse blanks)

-- | Add a git dependency at the end of the "dependencies" of the text of a
-- manifest, or of those of one of its tests, and add the section when there
-- is none. The rest of the text is kept as it is, comments included.
addDependencyEntry :: String -> Maybe String -> T.Text -> T.Text -> Either String String
addDependencyEntry original mTest url tag =
    unlines <$> insertUnder (maybe ["dependencies"] (\t -> ["tests", t, "dependencies"]) mTest) "" (lines original)

  where

    item ind = [ind ++ "- git: " ++ T.unpack url, ind ++ "  rev: \"" ++ T.unpack tag ++ "\""]

    -- | Insert the item under the path of keys, in a block whose keys are
    -- indented by "cur".
    insertUnder [] _ block = Right block
    insertUnder (key : rest) cur block =
        case break (isKey cur key) block of
            (_, [])
                | null rest -> Right (appendEnd block (["" | null cur] ++ [cur ++ key ++ ":"] ++ item (cur ++ "  ")))
                | otherwise -> Left $ "the manifest declares no test " ++ fromMaybe "" mTest
            (before, header : after)
                | not (isBlockHeader cur key header) ->
                    Left $ "the " ++ key ++ " section of termina.yaml is not written in block style"
                | otherwise -> do
                    let (child, others) = span (inside cur) after
                        ind = childIndent cur child
                    child' <- if null rest then Right (appendEnd child (item ind)) else insertUnder rest ind child
                    Right (before ++ [header] ++ child' ++ others)

    isKey cur key l = (cur ++ key ++ ":") `isPrefixOf` l

    isBlockHeader cur key l = all (== ' ') (takeWhile (/= '#') (drop (length cur + length key + 1) l))

    -- | A line of the block under a key: blank, more indented, or an item of
    -- a sequence at the indentation of the key.
    inside cur l =
        let (spaces, text) = span (== ' ') l
        in null text || length spaces > length cur || (length spaces == length cur && "-" `isPrefixOf` text)

    childIndent cur child =
        case [takeWhile (== ' ') l | l <- child, let text = dropWhile (== ' ') l, not (null text), not ("#" `isPrefixOf` text)] of
            (ind : _) -> ind
            [] -> cur ++ "  "

    appendEnd block new = let (content, blanks) = splitTrailingBlanks block in content ++ new ++ blanks

-- | The manifest of a new application.
newApplicationManifest :: String -> Platform -> String
newApplicationManifest projectNameStr plt = unlines [
        "project:",
        "  name: " ++ projectNameStr,
        "  version: 0.1.0",
        "  manifest-version: " ++ show (maximum knownManifestVersions),
        "",
        "app:",
        "  platform: " ++ show plt,
        "  profile: release"
    ]
