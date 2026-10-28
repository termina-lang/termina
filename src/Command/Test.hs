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
import Command.Deliver (deliverableInfo)
import Configuration.Configuration
import Configuration.Platform (Platform(..), callSite, checkPlatform, gdbServerPort, xsdbServerPort)
import Runner.Analysis (TestResult(TestResult), Run(..), RunEnd(..), AddressKind(..), analyse, addressesOf, renderReport, exitCodeOf)
import Runner.Descriptor (Descriptor(..), DescriptorCase, Symbols(..), readDescriptor)
import qualified Runner.Descriptor as D
import Runner.Gdb (GdbTarget(..), GdbEnd(..), GdbRun(..), runGdb)
import Runner.Posix (PosixRun(..), runPosix)
import Runner.Record (Header(..), Layout, decodeHeader, decodeRecords, headerSize, platformLayout, relocate)
import Runner.Results (Evidence(..), readProvenance, resultsJson, renderResults, renderJUnit)
import Runner.Symbols (locateAddresses, stampAddress)
import Runner.Xsdb (prepareBoard)
import qualified Runner.Symbols as S

import Control.Exception (IOException, try)
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
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
        (Maybe FilePath) -- ^ The folder of a record dumped outside the runner
        (Maybe Executor) -- ^ How the binary of a test is run
        (Maybe Server) -- ^ The GDB server of the board
        (Maybe Server) -- ^ The XSDB command server of the PYNQ
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
    <*> O.optional (O.strOption
          ( O.long "record"
          <> O.metavar "FOLDER"
          <> O.help "Report the record that a debugging session dumped to FOLDER/header.bin and FOLDER/area.bin, without building nor running"))
    <*> O.optional (O.option (O.eitherReader readExecutor)
          ( O.long "executor"
          <> O.metavar "process|gdb"
          <> O.help "On posix-gcc, run the binary as a process (default) or under gdb, as on a board"))
    <*> O.optional (O.option (O.eitherReader (readServer "GDB server"))
          ( O.long "gdb"
          <> O.metavar "HOST[:PORT]"
          <> O.help "The GDB server of the board (default: localhost, or host.docker.internal in the container, at the port of the platform)"))
    <*> O.optional (O.option (O.eitherReader (readServer "XSDB server"))
          ( O.long "xsdb"
          <> O.metavar "HOST[:PORT]"
          <> O.help "The XSDB command server that prepares the PYNQ (default: the host of the GDB server, at the port of the platform)"))
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

    -- | A host, or a host and a port after the last colon
    readServer :: String -> String -> Either String Server
    readServer kind text =
        let (port, rest) = break (== ':') (reverse text) in
        case rest of
            ':' : host ->
                if not (null port) && all isDigit port && not (null host)
                    then Right (Server (reverse host) (Just (read (reverse port))))
                    else Left ("the " ++ kind ++ " " ++ text ++ " is not HOST or HOST:PORT")
            _ -> if null text then Left ("the " ++ kind ++ " is empty") else Right (Server text Nothing)

-- | How the binary of a test is run on posix: as a process of the host, or under GDB
data Executor = ProcessExecutor | GdbExecutor
  deriving (Eq, Show)

-- | The host of a server of a board, and its port when it is not the one of the platform
data Server = Server String (Maybe Int)
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
    | Recorded FilePath -- ^ no run: the record dumped to this folder
  deriving (Eq, Show)

-- | Command handler for the "test" command
testCommand :: TestCmdArgs -> IO ()
testCommand (TestCmdArgs mName mPlatform mProfile mCase isolated mRecord mExecutor mServer mXsdb list json) =
    if list then listTests json
    else either cannotRun (runTests mName mPlatform mProfile mExecutor mServer mXsdb) (modeOf mName mCase isolated mRecord)

-- | A test that cannot be run exits with 2
cannotRun :: String -> IO a
cannotRun reason = hPutStrLn stderr (errorMessage reason) >> exitWith (ExitFailure 2)

-- | The mode of the run, from the options
modeOf :: Maybe String -> Maybe String -> Bool -> Maybe FilePath -> Either String Mode
modeOf mName mCase isolated mRecord =
    case (mCase, isolated, mRecord) of
        (Nothing, False, Nothing) -> Right AllCases
        (Nothing, True, Nothing) -> Right Isolated
        (Just _, True, _) -> Left "--isolated runs every case and does not go with --case"
        (Just c, False, Nothing) ->
            case mName of
                Nothing -> Left "--case needs the name of the test"
                Just _ -> Right (OneCase c)
        (Nothing, False, Just folder) ->
            case mName of
                Nothing -> Left "--record needs the name of the test"
                Just _ -> Right (Recorded folder)
        (_, _, Just _) -> Left "--record reports the cases of the record, and does not go with --case nor --isolated"

-- | Builds and runs each selected test on each of its platforms, and exits with
-- the worst code of them: 0 when every case passes, 1 when one does not, 2 when
-- a test cannot be built or read
runTests :: Maybe String -> Maybe Platform -> Maybe ProjectProfile -> Maybe Executor -> Maybe Server -> Maybe Server -> Mode -> IO ()
runTests mName mPlatform mProfile mExecutor mServer mXsdb mode = do
    -- | The report goes in order with the output of make
    hSetBuffering stdout LineBuffering
    manifest <- loadManifest >>= either dieWithErrors return
    let selection = maybe SelectTests SelectTest mName
        buildArgs = BuildCmdArgs False False Nothing selection mPlatform mProfile
    deployments <- either cannotRun return (selectDeployments manifest selection mPlatform mProfile)
    -- | A port in --gdb is the port of one board
    let boards = nub [ deploymentPlatform d | d <- deployments, deploymentPlatform d /= POSIXGCC ]
    when (mServer /= Nothing && length boards /= 1) $
        cannotRun ("--gdb names the GDB server of one board, and the command selects "
            ++ (if null boards then "none" else intercalate ", " (map show boards)) ++ "; choose one with --platform")
    when (mXsdb /= Nothing && RTEMS6ZYNQ7000PYNQZ2 `notElem` boards) $
        cannotRun ("--xsdb names the XSDB server that prepares " ++ show RTEMS6ZYNQ7000PYNQZ2 ++ ", which the command does not select")
    when (mExecutor == Just ProcessExecutor && not (null boards)) $
        cannotRun ("a board runs only under GDB, and the command selects "
            ++ intercalate ", " (map show boards) ++ "; choose " ++ show POSIXGCC ++ " with --platform")
    host <- gdbHost mServer
    let servers = Servers (host, mServer >>= \(Server _ port) -> port)
                          (maybe host (\(Server h _) -> h) mXsdb, mXsdb >>= \(Server _ port) -> port)
    codes <- case (mode, deployments) of
        -- | A record is decoded with the binary already built, which made it
        (Recorded _, [deployment]) -> (: []) <$> runDeployment GdbExecutor servers mode deployment
        (Recorded _, _) -> cannotRun ("--record decodes the record of one platform, and the test has "
            ++ intercalate ", " (map (show . deploymentPlatform) deployments) ++ "; choose one with --platform")
        _ -> forM deployments $ \deployment -> do
            announce deployments ("Building " ++ deploymentLabel deployment)
            built <- try (buildDeployment buildArgs deployment) :: IO (Either ExitCode [(QualifiedName, FilePath)])
            case built of
                Left _ -> do
                    let config = deploymentConfig deployment
                    createDirectoryIfMissing True (outputFolder config)
                    evidence <- evidenceOf config mode Nothing (executorOf (maybe ProcessExecutor id mExecutor) (deploymentPlatform deployment))
                    finish config evidence Nothing M.empty (invalid config "it does not build")
                Right _ -> runDeployment (maybe ProcessExecutor id mExecutor) servers mode deployment
    let worst = maximum (0 : codes)
    if worst == 0 then exitSuccess else exitWith (ExitFailure worst)

-- | The label of a test in the reports
labelOf :: TerminaConfig -> String
labelOf config = T.unpack (name config) ++ " (" ++ T.unpack (platform config) ++ ")"

-- | The host of the GDB servers: the one of --gdb, the one of
-- TERMINA_GDB_HOST, the host of the container, or the machine itself
gdbHost :: Maybe Server -> IO String
gdbHost mServer =
    case mServer of
        Just (Server host _) -> return host
        Nothing -> do
            fromEnvironment <- lookupEnv "TERMINA_GDB_HOST"
            marked <- lookupEnv "TERMINA_CONTAINER"
            docker <- doesFileExist "/.dockerenv"
            return $ case fromEnvironment of
                Just host -> host
                Nothing -> if marked /= Nothing || docker then "host.docker.internal" else "localhost"

-- | The executor of a test: a board runs only under GDB
executorOf :: Executor -> Platform -> Executor
executorOf executor plt = if plt /= POSIXGCC then GdbExecutor else executor

-- | The host and the port of the GDB server of a board and of the XSDB server
-- of the PYNQ, each port when it is not the one of the platform
data Servers = Servers (String, Maybe Int) (String, Maybe Int)

-- | The script of the project that resets the PYNQ, initializes its PS and
-- loads its bitstream, which the XSDB server sources before each run
xsdbScript :: FilePath
xsdbScript = "assets" </> "xsdb" </> "init" <.> "tcl"

-- | The commands of the console that reset a board and load the binary; the
-- PYNQ is prepared before with the XSDB script of the project
launchSequence :: Platform -> [String]
launchSequence plt =
    case plt of
        FreeRTOS10STM32L432NUCLEOL432KC -> ["monitor reset", "load"]
        RTEMS5LEON3SFNEXYSA7 -> ["monitor reset", "load"]
        _ -> ["load"]

invalid :: TerminaConfig -> String -> TestResult
invalid config reason = TestResult (labelOf config) (Just reason) [] Nothing

-- | A run of the binary that the mode plans: the case to select, the cases of
-- the descriptor it runs, and its files of the record and of the output
data Planned = Planned Integer [DescriptorCase] FilePath FilePath

-- | How execute starts the binary: as a process of posix, or under the GDB of
-- the toolchain, natively or through the GDB server of a board
data Launcher = AsProcess | UnderGdb FilePath GdbTarget

-- | Runs a test that is built: on posix as a process or under gdb, and on a
-- board under the GDB of its toolchain, through its GDB server at the host and
-- at the port of the platform or of --gdb; the PYNQ is prepared before each
-- run through its XSDB server
runDeployment :: Executor -> Servers -> Mode -> Deployment -> IO Int
runDeployment executor (Servers (host, mPort) (xsdbHost, mXsdbPort)) mode deployment = do
    let config = deploymentConfig deployment
        plt = deploymentPlatform deployment
        folder = outputFolder config
        binary = folder </> "bin" </> T.unpack (name config)
        board = plt /= POSIXGCC
        address = host ++ ":" ++ maybe "" show (maybe (gdbServerPort plt) Just mPort)
        xsdbPort = maybe (xsdbServerPort plt) Just mXsdbPort
    loaded <- readDescriptor (folder </> "test" <.> "json")
    case loaded of
      Left err -> do
        evidence <- evidenceOf config mode Nothing (executorOf executor plt)
        finish config evidence Nothing M.empty (invalid config ("its descriptor cannot be read: " ++ err))
      Right descriptor -> do
        evidence <- evidenceOf config mode (descriptorTimeout descriptor) (executorOf executor plt)
        prefix <- if board
            then M.findWithDefault "" "EXPANDED_CROSS" <$> deliverableInfo (labelOf config) folder
            else return ""
        let launcher =
                if board then UnderGdb (prefix ++ "gdb") (RemoteServer address (launchSequence plt))
                else if executor == GdbExecutor then UnderGdb "gdb" NativeProcess
                else AsProcess
        hasScript <- doesFileExist xsdbScript
        let fromRecord = case mode of
                Recorded _ -> True
                _ -> False
            prepared = case (xsdbPort, fromRecord) of
                (Nothing, _) -> Right Nothing
                (_, True) -> Right Nothing
                (Just port, False) -> if hasScript then Right (Just port)
                    else Left ("the " ++ show plt ++ " is prepared before each run with " ++ xsdbScript
                               ++ ", which resets the board, initializes its PS and loads its bitstream, and the project has none")
        case prepared >> plan folder descriptor of
          Left err -> finish config evidence (Just descriptor) M.empty (invalid config err)
          Right planned -> do
            stamp <- stampAddress prefix binary
            either (putStrLn . warnMessage) (const (return ())) stamp
            let prepare = case either (const Nothing) id prepared of
                    Nothing -> return (Right ())
                    Just port -> do
                        done <- prepareBoard xsdbHost port xsdbScript
                        return $ either (\err -> Left ("the XSDB server at " ++ xsdbHost ++ ":" ++ show port
                            ++ " did not prepare the board: " ++ err ++ "; it is started from the root of the project with xsdb -eval \"xsdbserver start -port "
                            ++ show port ++ "; vwait forever\"")) Right done
                run p = case mode of
                    Recorded given -> readRecord (platformLayout plt) descriptor (either (const Nothing) Just stamp) given
                    _ -> prepare >>= either (return . Left) (const (execute config launcher (platformLayout plt) descriptor binary (either (const Nothing) Just stamp) p))
            ran <- runAll run planned
            case ran of
              Left err -> finish config evidence (Just descriptor) M.empty (invalid config err)
              Right runs -> do
                let results = concatMap (analyse . fst) runs
                    overflowed = [ (headerCasesEnded h, headerChecksFailed h) | (r, h) <- runs, runOverflow r ]
                    overflow = if null overflowed then Nothing else Just (sum (map fst overflowed), sum (map snd overflowed))
                let lookups = [ (recorded, if kind == ReturnAddress then callSite plt recorded else recorded)
                              | (recorded, kind) <- addressesOf results ]
                (locations, warning) <- locateAddresses prefix binary lookups
                maybe (return ()) (putStrLn . warnMessage) warning
                finish config evidence (Just descriptor) locations (TestResult (labelOf config) Nothing results overflow)

  where

    plan folder descriptor =
        let cases = D.descriptorCases descriptor
            record n = folder </> ("record" ++ n) <.> "bin"
            output n = folder </> ("test" ++ n) <.> "log"
            byCase = case mode of
                OneCase _ -> True
                Isolated -> True
                _ -> False
        in if byCase && descriptorLevel descriptor /= "unit"
            then Left "--case and --isolated run the cases of a unit test"
            else case mode of
                AllCases -> Right [Planned 0 cases (record "") (output "")]
                Recorded _ -> Right [Planned 0 cases (record "") (output "")]
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

-- | Decodes the header followed by the area, checks that they come from the
-- binary of the descriptor, and takes the load offset out of the addresses;
-- the cases of the run come from the header
decodeRun :: Layout -> Descriptor -> Maybe Integer -> (Header -> Either String [DescriptorCase]) -> String
          -> BS.ByteString -> RunEnd -> [String] -> Either String (Run, Header)
decodeRun layout descriptor stamp casesOf noRecord bytes end frames = do
    header <- decodeHeader layout bytes
    when (headerMagic header /= BS.pack (map (fromIntegral . fromEnum) "TRMTEST\0")) $
        Left noRecord
    when (headerStamp header /= descriptorStamp descriptor) $
        Left ("the stamp of the record, 0x" ++ showHex (headerStamp header) ""
              ++ ", is not the one of test.json, 0x" ++ showHex (descriptorStamp descriptor) "" ++ ": the binary is not the one of this descriptor")
    cases <- casesOf header
    let area = BS.take (fromIntegral (headerWriteOffset header)) (BS.drop (headerSize layout) bytes)
        offset = maybe 0 (headerAnchor header -) stamp
    records <- decodeRecords layout area
    return (Run cases (map (relocate offset) records) end (testBit (headerFlags header) 1) (headerLastStep header) frames, header)

-- | Reads the header and the area that a debugging session dumped to the
-- folder, and decodes them with the cases that the header selected
readRecord :: Layout -> Descriptor -> Maybe Integer -> FilePath -> IO (Either String (Run, Header))
readRecord layout descriptor stamp folder = do
    let files = [folder </> "header" <.> "bin", folder </> "area" <.> "bin"]
    read' <- try (mapM BS.readFile files)
    return $ case read' of
        Left err -> Left ("the record cannot be read: " ++ show (err :: IOException))
        Right parts -> decodeRun layout descriptor stamp selectedCases
            ("the header in " ++ head files ++ " is not the one of a record of a test") (BS.concat parts) Dumped []

  where

    selectedCases header =
        let cases = D.descriptorCases descriptor
            selected = headerSelectedCase header
        in if selected == 0 then Right cases
           else case [ dc | dc <- cases, D.caseNumber dc == selected ] of
                [] -> Left ("the record selects the case " ++ show selected ++ ", which test.json does not list")
                found -> Right found

-- | Runs the binary once and decodes its record, without the load offset in its addresses
execute :: TerminaConfig -> Launcher -> Layout -> Descriptor -> FilePath -> Maybe Integer -> Planned -> IO (Either String (Run, Header))
execute config launcher layout descriptor binary stamp (Planned selected cases recordFile logFile) = do
    ran <- case launcher of
        AsProcess -> fmap fromProcess <$>
            runPosix layout binary recordFile logFile (recordCapacity config) selected (descriptorTimeout descriptor)
        UnderGdb gdb target -> fmap fromGdb <$>
            runGdb gdb binary target (takeDirectory recordFile) logFile selected (descriptorTimeout descriptor)
    return $ ran >>= \(bytes, end, frames, noRecord) ->
        decodeRun layout descriptor stamp (const (Right cases)) noRecord bytes end frames

  where

    -- | The bytes of the record, how the run ended, the frames at an
    -- exception and why a record without header is empty
    fromProcess posixRun =
        (posixRecord posixRun, if posixExit posixRun == Nothing then Killed else Finished, [],
         "the binary wrote no record" ++ (case posixExit posixRun of
            Just (ExitFailure code) -> ", and exited with " ++ show code
            _ -> "") ++ "; its output is in " ++ logFile)

    fromGdb gdbRun =
        (gdbRecord gdbRun, if gdbEnd gdbRun == AtTimeout then Killed else Finished, gdbBacktrace gdbRun,
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
evidenceOf :: TerminaConfig -> Mode -> Maybe Integer -> Executor -> IO Evidence
evidenceOf config mode seconds executor = do
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
            Isolated -> "isolated"
            Recorded _ -> "record",
        evidenceExecutor = case executor of
            ProcessExecutor -> "process"
            GdbExecutor -> "gdb",
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
