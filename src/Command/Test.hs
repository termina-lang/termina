{-# LANGUAGE OverloadedStrings #-}

-- | The tests of the project: it builds and runs them, or lists them with
-- their platforms and their cases, which it reads from the modules of each
-- test without type checking or building them.
module Command.Test (
    testCmdArgsParser, testCommand, TestCmdArgs,
    TestCase(..), testCases
) where

import Command.Build (BuildCmdArgs(..), Selection(..), announce, buildDeployment, selectDeployments)
import Command.Common (loadTerminaModule, loadModules)
import Configuration.Configuration
import Configuration.Platform (Platform(..), checkPlatform)
import Runner.Analysis (TestResult(TestResult), Run(..), analyse, addressesOf, renderReport, exitCodeOf)
import Runner.Descriptor (Descriptor(..), DescriptorCase, Symbols(..), readDescriptor)
import qualified Runner.Descriptor as D
import Runner.Gdb (GdbTarget(..), GdbEnd(..), GdbRun(..), runGdb)
import Runner.Posix (PosixRun(..), runPosix)
import Runner.Record (Header(..), Layout, decodeHeader, decodeRecords, headerSize, platformLayout, relocate)
import Runner.Results (Evidence(..), readProvenance, resultsJson, renderResults, renderJUnit)
import Runner.Symbols (locateAddresses, stampAddress)
import qualified Runner.Symbols as S

import Control.Exception (try)
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Info (os)
import System.IO (BufferMode(LineBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import Command.Types
import Command.Utils
import Configuration.Manifest
import Core.AST
import Core.TestCases (NumberedCase(caseLocation), numberCases)
import qualified Core.TestCases as NC
import Modules.Modules
import Parser.AST
import Parser.Types (ParserAnn)
import Utils.Annotations

import Control.Monad
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Options.Applicative as O
import System.Exit
import System.FilePath
import Text.Parsec.Pos (sourceLine, sourceName)

-- | Data type for the "test" command arguments
data TestCmdArgs =
    TestCmdArgs
        (Maybe String) -- ^ The test to run, or every test
        (Maybe Platform) -- ^ Only this platform
        (Maybe ProjectProfile) -- ^ Profile of the tests
        (Maybe String) -- ^ Only this case
        Bool -- ^ Run each case in a process of its own
        (Maybe Executor) -- ^ How the binary of a test is run
        Bool -- ^ List the tests instead of running them
        Bool -- ^ Write the list in JSON
    deriving Show

-- | Parser for the "test" command arguments
testCmdArgsParser :: O.Parser TestCmdArgs
testCmdArgsParser = TestCmdArgs
    <$> O.optional (O.strArgument (O.metavar "NAME" <> O.help "The test to run (default: every test)"))
    <*> O.optional (O.option (O.maybeReader checkPlatform)
          ( O.long "platform"
          <> O.metavar "PLATFORM"
          <> O.help "Only this platform"))
    <*> O.optional (O.option (O.eitherReader readProfile)
          ( O.long "profile"
          <> O.metavar "debug|release"
          <> O.help "Profile of the tests (default: the one of the application, debug in a library)"))
    <*> O.optional (O.strOption
          ( O.long "case"
          <> O.metavar "INSTANCE.CASE"
          <> O.help "Only this case"))
    <*> O.switch (O.long "isolated"
        <> O.help "Run each case in a process of its own")
    <*> O.optional (O.option (O.eitherReader readExecutor)
          ( O.long "executor"
          <> O.metavar "process|gdb"
          <> O.help "On posix-gcc, run the binary as a process (default) or under gdb, as on a board"))
    <*> O.switch (O.long "list"
        <> O.help "List the tests, their platforms and their cases, without building them")
    <*> O.switch (O.long "json"
        <> O.help "Write the list in JSON")

  where

    readProfile :: String -> Either String ProjectProfile
    readProfile profileName =
        case profileName of
            "debug" -> Right Debug
            "release" -> Right Release
            other -> Left $ "Unknown profile: " ++ other ++ " (use debug or release)"

    readExecutor :: String -> Either String Executor
    readExecutor executorName =
        case executorName of
            "process" -> Right ProcessExecutor
            "gdb" -> Right GdbExecutor
            other -> Left $ "Unknown executor: " ++ other ++ " (use process or gdb)"

-- | How the binary of a test is run: as a process of the host, or under GDB
data Executor = ProcessExecutor | GdbExecutor
  deriving (Eq, Show)

-- | A case of a test, named by its test instance
data TestCase = TestCase {
    caseInstance :: Identifier,
    caseName :: Identifier,
    caseFile :: FilePath,
    caseLine :: Int
  } deriving (Eq, Show)

-- | How the cases of a test are run
data Mode =
    AllCases
    | OneCase String -- ^ only the case with this identifier
    | Isolated -- ^ each case in a process of its own
  deriving (Eq, Show)

-- | Command handler for the "test" command
testCommand :: TestCmdArgs -> IO ()
testCommand (TestCmdArgs mName mPlatform mProfile mCase isolated mExecutor list json) =
    if list then listTests json
    else either cannotRun (runTests mName mPlatform mProfile mExecutor) (modeOf mName mCase isolated)

-- | A test that cannot be run exits with 2
cannotRun :: String -> IO a
cannotRun reason = hPutStrLn stderr (errorMessage reason) >> exitWith (ExitFailure 2)

-- | The mode of the run, from the options
modeOf :: Maybe String -> Maybe String -> Bool -> Either String Mode
modeOf mName mCase isolated =
    case (mCase, isolated) of
        (Nothing, False) -> Right AllCases
        (Nothing, True) -> Right Isolated
        (Just _, True) -> Left "--isolated runs every case and does not go with --case"
        (Just c, False) ->
            case mName of
                Nothing -> Left "--case needs the name of the test"
                Just _ -> Right (OneCase c)

-- | Builds and runs each selected test on each of its platforms, and exits with
-- the worst code of them: 0 when every case passes, 1 when one does not, 2 when
-- a test cannot be built or read
runTests :: Maybe String -> Maybe Platform -> Maybe ProjectProfile -> Maybe Executor -> Mode -> IO ()
runTests mName mPlatform mProfile mExecutor mode = do
    -- | The report goes in order with the output of make
    hSetBuffering stdout LineBuffering
    manifest <- loadManifest >>= either dieWithErrors return
    let selection = maybe SelectTests SelectTest mName
        buildArgs = BuildCmdArgs False False Nothing selection mPlatform mProfile
    deployments <- either cannotRun return (selectDeployments manifest selection mPlatform mProfile)
    codes <- forM deployments $ \deployment -> do
        announce deployments ("Building " ++ deploymentLabel deployment)
        built <- try (buildDeployment buildArgs deployment) :: IO (Either ExitCode [(QualifiedName, FilePath)])
        case built of
            Left _ -> do
                let config = deploymentConfig deployment
                createDirectoryIfMissing True (outputFolder config)
                evidence <- evidenceOf config mode Nothing
                finish config evidence Nothing M.empty (invalid config "it does not build")
            Right _ -> runDeployment (maybe ProcessExecutor id mExecutor) mode deployment
    let worst = maximum (0 : codes)
    if worst == 0 then exitSuccess else exitWith (ExitFailure worst)

-- | The label of a test in the reports
labelOf :: TerminaConfig -> String
labelOf config = T.unpack (name config) ++ " (" ++ T.unpack (platform config) ++ ")"

invalid :: TerminaConfig -> String -> TestResult
invalid config reason = TestResult (labelOf config) (Just reason) [] Nothing

-- | A run of the binary that the mode plans: the case to select, the cases of
-- the descriptor it runs, and its files of the record and of the output
data Planned = Planned Integer [DescriptorCase] FilePath FilePath

-- | Runs a test that is built, on posix; a board comes with the executor of GDB
runDeployment :: Executor -> Mode -> Deployment -> IO Int
runDeployment executor mode deployment = do
    let config = deploymentConfig deployment
        plt = deploymentPlatform deployment
        folder = outputFolder config
        binary = folder </> "bin" </> T.unpack (name config)
    if plt /= POSIXGCC
      then do
        putStrLn . warnMessage $ labelOf config ++ ": running on a board comes with the executor of GDB, the test is only built"
        return 0
      else do
        loaded <- readDescriptor (folder </> "test" <.> "json")
        case loaded of
          Left err -> do
            evidence <- evidenceOf config mode Nothing
            finish config evidence Nothing M.empty (invalid config ("its descriptor cannot be read: " ++ err))
          Right descriptor -> do
            evidence <- evidenceOf config mode (descriptorTimeout descriptor)
            case plan folder descriptor of
              Left err -> finish config evidence (Just descriptor) M.empty (invalid config err)
              Right planned -> do
                stamp <- stampAddress binary
                either (putStrLn . warnMessage) (const (return ())) stamp
                ran <- runAll (execute config executor (platformLayout plt) descriptor binary (either (const Nothing) Just stamp)) planned
                case ran of
                  Left err -> finish config evidence (Just descriptor) M.empty (invalid config err)
                  Right runs -> do
                    let results = concatMap (analyse . fst) runs
                        overflowed = [ (headerCasesEnded h, headerChecksFailed h) | (r, h) <- runs, runOverflow r ]
                        overflow = if null overflowed then Nothing else Just (sum (map fst overflowed), sum (map snd overflowed))
                    (locations, warning) <- locateAddresses binary (addressesOf results)
                    maybe (return ()) (putStrLn . warnMessage) warning
                    finish config evidence (Just descriptor) locations (TestResult (labelOf config) Nothing results overflow)

  where

    plan folder descriptor =
        let cases = D.descriptorCases descriptor
            record n = folder </> ("record" ++ n) <.> "bin"
            output n = folder </> ("test" ++ n) <.> "log"
        in if mode /= AllCases && descriptorLevel descriptor /= "unit"
            then Left "--case and --isolated run the cases of a unit test"
            else case mode of
                AllCases -> Right [Planned 0 cases (record "") (output "")]
                OneCase c -> case [ dc | dc <- cases, D.caseId dc == c ] of
                    (dc : _) -> Right [Planned (D.caseNumber dc) [dc] (record "") (output "")]
                    [] -> Left ("the test has no case " ++ c ++ "; its cases are " ++ intercalate ", " (map D.caseId cases))
                Isolated -> Right [ Planned (D.caseNumber dc) [dc] (record ("-" ++ show (D.caseNumber dc))) (output ("-" ++ show (D.caseNumber dc)))
                                  | dc <- cases ]

    -- | The runs one after another, up to the first that cannot be read
    runAll run planned =
        case planned of
            [] -> return (Right [])
            p : rest -> do
                first <- run p
                case first of
                    Left err -> return (Left err)
                    Right done -> fmap (done :) <$> runAll run rest

-- | Runs the binary once and decodes its record, without the load offset in its addresses
execute :: TerminaConfig -> Executor -> Layout -> Descriptor -> FilePath -> Maybe Integer -> Planned -> IO (Either String (Run, Header))
execute config executor layout descriptor binary stamp (Planned selected cases recordFile logFile) = do
    ran <- case executor of
        ProcessExecutor -> fmap fromProcess <$>
            runPosix layout binary recordFile logFile (recordCapacity config) selected (descriptorTimeout descriptor)
        GdbExecutor -> fmap fromGdb <$>
            runGdb "gdb" binary NativeProcess (takeDirectory recordFile) logFile selected (descriptorTimeout descriptor)
    return $ ran >>= \(bytes, killed, frames, noRecord) -> do
        header <- decodeHeader layout bytes
        when (headerMagic header /= BS.pack (map (fromIntegral . fromEnum) "TRMTEST\0")) $
            Left noRecord
        when (headerStamp header /= descriptorStamp descriptor) $
            Left ("the stamp of the record, 0x" ++ showHex (headerStamp header) ""
                  ++ ", is not the one of test.json, 0x" ++ showHex (descriptorStamp descriptor) "" ++ ": the binary is not the one of this descriptor")
        let area = BS.take (fromIntegral (headerWriteOffset header)) (BS.drop (headerSize layout) bytes)
            offset = maybe 0 (headerAnchor header -) stamp
        records <- decodeRecords layout area
        return (Run cases (map (relocate offset) records) killed (testBit (headerFlags header) 1) (headerLastStep header) frames, header)

  where

    -- | The bytes of the record, whether the run was stopped at the timeout,
    -- the frames at an exception and why a record without header is empty
    fromProcess posixRun =
        (posixRecord posixRun, posixExit posixRun == Nothing, [],
         "the binary wrote no record" ++ (case posixExit posixRun of
            Just (ExitFailure code) -> ", and exited with " ++ show code
            _ -> "") ++ "; its output is in " ++ logFile)

    fromGdb gdbRun =
        (gdbRecord gdbRun, gdbEnd gdbRun == AtTimeout, gdbBacktrace gdbRun,
         "the binary did not initialize its record; the log of gdb is in " ++ logFile)

-- | Writes the report on the console, results.json and report.xml, and gives the exit code
finish :: TerminaConfig -> Evidence -> Maybe Descriptor -> M.Map Integer (Maybe S.Location) -> TestResult -> IO Int
finish config evidence mDescriptor locations result = do
    let folder = outputFolder config
    mapM_ putStrLn (renderReport (fromMaybe emptyDescriptor mDescriptor) locations result)
    BL.writeFile (folder </> "results" <.> "json") (renderResults (resultsJson evidence mDescriptor locations result))
    writeFile (folder </> "report" <.> "xml") (renderJUnit evidence mDescriptor locations result)
    return (exitCodeOf result)

  where

    emptyDescriptor = Descriptor 1 0 "" "unit" "" Nothing [] [] M.empty M.empty (Symbols "" "" "" "")

-- | What ties the result to the build: the commit of the project, the hash of
-- the binary and the hashes of provenance.json
evidenceOf :: TerminaConfig -> Mode -> Maybe Integer -> IO Evidence
evidenceOf config mode seconds = do
    let folder = outputFolder config
        binary = folder </> "bin" </> T.unpack (name config)
        provenanceFile = folder </> "provenance" <.> "json"
    sha <- runGit ["rev-parse", "HEAD"]
    status <- runGit ["status", "--porcelain"]
    hasBinary <- doesFileExist binary
    binarySha <- if hasBinary then Just <$> sha256File binary else return Nothing
    hasProvenance <- doesFileExist provenanceFile
    provenance <- if hasProvenance then readProvenance provenanceFile else return Nothing
    return Evidence {
        evidenceName = T.unpack (name config),
        evidencePlatform = T.unpack (platform config),
        evidenceHost = os,
        evidenceProfile = case profile config of
            Debug -> "debug"
            Release -> "release",
        evidenceMode = case mode of
            AllCases -> "all"
            OneCase _ -> "case"
            Isolated -> "isolated",
        evidenceTimeout = seconds,
        evidenceCommit = case (sha, status) of
            (Right s, Right st) -> Just (concat (lines s), not (null (lines st)))
            _ -> Nothing,
        evidenceBinarySha = binarySha,
        evidenceProvenance = provenance
      }

-- | Lists the tests, their platforms and their cases
listTests :: Bool -> IO ()
listTests json = do
    manifest <- loadManifest >>= either dieWithErrors return
    listed <- forM (M.toList (manifestTests manifest)) $ \(testName, entry) -> do
      cases <- loadTestCases (T.unpack testName)
      return (testName, entry, cases)
    if json
      then BLC.putStrLn $ encodePretty' defConfig { confCompare = keyOrder keys } (object [
          "tests" .= [ object [
              "name" .= testName,
              "folder" .= ("tests" </> T.unpack testName),
              "level" .= level cases,
              "platforms" .= map show (testPlatforms entry),
              "cases" .= [ object [
                  "id" .= caseId c,
                  "instance" .= caseInstance c,
                  "case" .= caseName c,
                  "file" .= caseFile c,
                  "line" .= caseLine c
                ] | c <- cases ]
            ] | (testName, entry, cases) <- listed ]
        ])
      else forM_ listed $ \(testName, entry, cases) -> do
        putStrLn $ T.unpack testName ++ " (" ++ level cases ++ ") on "
          ++ joinWith ", " (map show (testPlatforms entry))
        forM_ cases $ \c -> putStrLn ("  " ++ caseId c)

  where

    keys = ["tests", "name", "folder", "level", "platforms", "cases", "id", "instance", "case", "file", "line"]

    caseId c = caseInstance c ++ "." ++ caseName c

    -- | A test without test instances is a concurrent test
    level :: [TestCase] -> String
    level [] = "concurrent"
    level _ = "unit"

    joinWith sep = foldr1 (\a b -> a ++ sep ++ b)

-- | Parse the modules of a test and read its cases
loadTestCases :: String -> IO [TestCase]
loadTestCases testName = do
    let testFolder = "tests" </> testName
        sources = ["src", testFolder </> "src"]
    testModule <- loadTerminaModule testFolder "test" sources
    parsedModules <- loadModules (importedModules testModule) sources
    let parsedProject = M.insert (qualifiedName testModule) testModule parsedModules
    orderedModules <- either
        (\cycleFiles -> die . errorMessage $ "Detected cycle between the source files of the test " ++ testName ++ ": " ++ show cycleFiles)
        return
        (sortProjectDepsOrLoop (M.map importedModules parsedProject))
    either (die . errorMessage) return $
      testCases [parsedAST . metadata $ parsedProject M.! m | m <- orderedModules]

-- | The cases of a test in the order in which the harness numbers them.
testCases :: [AnnotatedProgram ParserAnn] -> Either String [TestCase]
testCases programs =
    case numberCases instances testClasses of
        Left (ident, cls) -> Left $ "the test instance " ++ ident ++ " is of type " ++ cls ++ ", which is not a test class"
        Right cases -> Right [ TestCase (NC.caseInstance c) (NC.caseName c) (fileOf loc) (lineOf loc)
                             | c <- cases, let loc = caseLocation c ]

  where

    elements = concat programs

    instances = [ (ident, cls, ann) | GlobalDeclaration (Test ident (TSDefinedType cls []) _ _ ann) <- elements ]

    testClasses = M.fromList [ (ident, [ (cname, ann) | ClassCase _ cname _ _ ann <- members ])
                             | TypeDefinition (Class TestClass ident members _ _) _ <- elements ]

    fileOf (Position _ start _) = sourceName start
    fileOf _ = ""

    lineOf (Position _ start _) = sourceLine start
    lineOf _ = 0
