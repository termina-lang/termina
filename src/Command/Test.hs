{-# LANGUAGE OverloadedStrings #-}

-- | The tests of the project: it builds and runs them, or lists them with
-- their platforms and their cases, which it reads from the modules of each
-- test without type checking or building them.
module Command.Test (
    testCmdArgsParser, testCommand, TestCmdArgs,
    TestCase(..), testCases,
    Servers(..), Target(..), Launcher(..), targetOf,
    Mode(..), modeOf, runTests, cannotRun
) where

import Command.Build (BuildCmdArgs(..), Selection(..), announce, buildDeployment, checkExportedModules, nothingSelected, selectDeployments, warnLocalFolders, refuseLocalFoldersInRelease)
import Command.Dependencies (Fetching(..), FetchingCommand(..))
import Command.Project (Dependencies(..), loadProject)
import Command.Common (loadMainModule, orderedModules, sourceTexts)
import Command.Provenance (buildInfo, terminaVersion)
import Command.Sources (LoadedProject(..), buildSources, readProject)
import Configuration.Configuration
import Configuration.Platform (Platform(..), callSite, checkPlatform, gdbLaunch, xsdbServerPort)
import Runner.Analysis (TestResult(TestResult), CaseResult(..), Run(..), RunEnd(..), AddressKind(..), analyse, addressesOf, renderReport, exitCodeOf)
import qualified Runner.Analysis as A
import Runner.Coverage (readMaps, coverageArrays, linkedArrays, writeTestResult)
import Runner.Descriptor (Descriptor(..), DescriptorCase, Symbols(..), readDescriptor)
import qualified Runner.Descriptor as D
import Runner.Gdb (GdbTarget(..), GdbEnd(..), GdbRun(..), runGdb)
import Runner.Posix (PosixRun(..), runPosix)
import Runner.Record (Header(..), Layout, decodeHeader, decodeRecords, headerSize, platformLayout, relocate)
import Runner.Results (Evidence(..), readProvenance, resultsJson, renderResults, renderJUnit)
import Runner.Symbols (locateAddresses, stampAddress)
import Runner.Servers (Server(..), readServer, gdbPortsParser, checkGdbPorts, gdbHost, xsdbHost, gdbAddress)
import Runner.Xsdb (prepareBoard)
import qualified Runner.Symbols as S

import Control.Exception (IOException, try)
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe, isJust)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Info (os)
import System.Process (readProcessWithExitCode)
import System.IO (hPutStrLn, stderr)
import Command.Types
import Command.Utils
import Configuration.Manifest
import Configuration.Namespace (locationLine)
import Core.AST
import Core.TestCases (NumberedCase(caseLocation), parsedCases)
import qualified Core.TestCases as NC
import Modules.Modules
import Parser.AST
import Parser.Types (ParserAnn)
import Utils.Annotations
import Utils.Hash (sha256File)

import Control.Monad
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Options.Applicative as O
import System.Exit
import System.FilePath
import Text.Parsec.Pos (sourceName)

-- | Data type for the "test" command arguments
data TestCmdArgs =
    TestCmdArgs
        (Maybe String) -- ^ The test to run, or every test
        (Maybe Platform) -- ^ Only this platform
        (Maybe ProjectProfile) -- ^ Profile of the tests
        (Maybe String) -- ^ Only this case
        Bool -- ^ Run each case in a process of its own
        (Maybe FilePath) -- ^ The folder of a record dumped outside the runner
        Bool -- ^ Run the binary of posix-gcc under gdb
        (Maybe Server) -- ^ The GDB server of the board
        [(Platform, Int)] -- ^ The port of the GDB server of some platforms
        (Maybe Server) -- ^ The XSDB command server of the PYNQ
        Bool -- ^ Fail instead of fetching the dependencies
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
    <*> O.switch (O.long "with-gdb"
        <> O.help "On posix-gcc, run the binary under gdb, which gives the backtrace of an exception")
    <*> O.optional (O.option (O.eitherReader (readServer "GDB server"))
          ( O.long "gdb"
          <> O.metavar "HOST[:PORT]"
          <> O.help "The host of the GDB servers of the boards, each at the port of its platform, or the host and the port of the server of a single board (default: localhost, or host.docker.internal in the container)"))
    <*> gdbPortsParser
    <*> O.optional (O.option (O.eitherReader (readServer "XSDB server"))
          ( O.long "xsdb"
          <> O.metavar "HOST[:PORT]"
          <> O.help "The XSDB command server that prepares the PYNQ (default: the host of the GDB server, at the port of the platform)"))
    <*> O.switch (O.long "offline"
        <> O.help "Fail instead of fetching when termina.lock does not match termina.yaml")
    <*> O.switch (O.long "list"
        <> O.help "List the tests, their platforms and their cases, without building them")
    <*> O.switch (O.long "json"
        <> O.help "Write the list in JSON")

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
testCommand (TestCmdArgs mName mPlatform mProfile mCase isolated mRecord withGdb mServer gdbPorts mXsdb offline list json)
    | list = either cannotRun (const (listTests json)) (listOnly [
        (isJust mName, "the name of a test"), (isJust mPlatform, "--platform"), (isJust mProfile, "--profile"),
        (isJust mCase, "--case"), (isolated, "--isolated"), (isJust mRecord, "--record"),
        (withGdb, "--with-gdb"), (isJust mServer, "--gdb"), (not (null gdbPorts), "--gdb-port"), (isJust mXsdb, "--xsdb"), (offline, "--offline")])
    | json = cannotRun "--json needs --list"
    | otherwise = either cannotRun (runTests mName mPlatform mProfile withGdb mServer gdbPorts mXsdb offline NoCoverage) (modeOf mName mCase isolated mRecord offline)

-- | A test that cannot be run exits with 2
cannotRun :: String -> IO a
cannotRun reason = hPutStrLn stderr (errorMessage reason) >> exitWith (ExitFailure 2)

-- | --list takes only --json: the first other option given is an error
listOnly :: [(Bool, String)] -> Either String ()
listOnly given =
    case [ option | (True, option) <- given ] of
        [] -> Right ()
        option : _ -> Left ("--list cannot be used with " ++ option)

-- | The mode of the run, from the options
modeOf :: Maybe String -> Maybe String -> Bool -> Maybe FilePath -> Bool -> Either String Mode
modeOf mName mCase isolated mRecord offline =
    case (mCase, isolated, mRecord) of
        (Nothing, False, Nothing) -> Right AllCases
        (Nothing, True, Nothing) -> Right Isolated
        (Just _, True, _) -> Left "--case and --isolated cannot be used together"
        (Just c, False, Nothing) ->
            case mName of
                Nothing -> Left "--case needs the name of the test"
                Just _ -> Right (OneCase c)
        (Nothing, False, Just folder) ->
            case mName of
                Nothing -> Left "--record needs the name of the test"
                Just _ -> if offline then Left "--record and --offline cannot be used together" else Right (Recorded folder)
        (_, _, Just _) -> Left "--record cannot be used with --case or --isolated"

-- | Builds and runs each selected test on each of its platforms, and exits with
-- the worst code of them: 0 when every case passes, 1 when one does not, 2 when
-- a test cannot be built or read, also when the project cannot be loaded. A
-- record is decoded with the binary already built, and only the manifest is
-- read.
runTests :: Maybe String -> Maybe Platform -> Maybe ProjectProfile -> Bool -> Maybe Server -> [(Platform, Int)] -> Maybe Server -> Bool -> Instrumentation -> Mode -> IO ()
runTests mName mPlatform mProfile withGdb mServer gdbPorts mXsdb offline instrumentation mode = do
    -- | A port for a platform without tests in the selection is left unused
    ports <- either cannotRun return (checkGdbPorts gdbPorts)
    loaded <- try (case mode of
        Recorded _ -> (\m -> LoadedProject projectRoot m M.empty M.empty M.empty) <$> (loadManifest projectRoot >>= either dieWithFirstError return)
        _ -> do
            refuseLocalFoldersInRelease (maybe SelectTests SelectTest mName) mPlatform mProfile
            loadedProject <- loadProject (Update (if offline then Offline else Reuse TestCommand))
            case manifestNature (loadedManifest loadedProject) of
                Library _ -> checkExportedModules False loadedProject
                Application _ -> return ()
            return loadedProject) :: IO (Either ExitCode LoadedProject)
    project <- either (const (exitWith (ExitFailure 2))) return loaded
    let manifest = loadedManifest project
        selection = maybe SelectTests SelectTest mName
        buildArgs = BuildCmdArgs False False Nothing selection mPlatform mProfile offline NoCoverage
    deployments <- either cannotRun return (selectDeployments manifest selection mPlatform mProfile)
    when (null deployments) $
        putStrLn (infoMessage (nothingSelected manifest selection mPlatform)) >> exitSuccess
    warnLocalFolders project deployments
    -- | The host of --gdb is the one of every board, and a port the port of one
    let platforms = nub (map deploymentPlatform deployments)
        boards = filter (/= POSIXGCC) platforms
    when (mServer /= Nothing && null boards) $
        cannotRun "--gdb is for the boards, and no test of the selection runs on one"
    when (maybe False (\(Server _ port) -> isJust port) mServer && length boards > 1) $
        cannotRun "A port in --gdb is the port of a single board, and the tests run on several. Choose one with --platform, or give only the host"
    when (mXsdb /= Nothing && RTEMS6ZYNQ7000PYNQZ2 `notElem` platforms) $
        cannotRun ("--xsdb is for " ++ show RTEMS6ZYNQ7000PYNQZ2 ++ ", and no test of the selection runs on it")
    when (withGdb && POSIXGCC `notElem` platforms) $
        cannotRun ("--with-gdb is for " ++ show POSIXGCC ++ ", and no test of the selection runs on it")
    host <- gdbHost mServer
    let servers = Servers (host, mServer >>= \(Server _ port) -> port) ports
                          (xsdbHost host mXsdb, mXsdb >>= \(Server _ port) -> port)
    codes <- case (mode, deployments) of
        -- | A record is decoded with the binary already built, which made it
        (Recorded _, [deployment]) -> (: []) . runCode <$> runDeployment True True [] servers mode deployment
        (Recorded _, _) -> cannotRun ("--record decodes the record of one platform, and the test has "
            ++ intercalate ", " (map (show . deploymentPlatform) deployments) ++ "; choose one with --platform")
        _ -> forM deployments $ \deployment -> do
            announce deployments ("Building " ++ deploymentLabel deployment)
            built <- try (void (buildDeployment buildArgs project deployment)) :: IO (Either ExitCode ())
            case built of
                Left _ -> do
                    let config = deploymentConfig deployment
                    createDirectoryIfMissing True (outputFolder config)
                    evidence <- evidenceOf config mode Nothing (targetLauncher (targetOf (deploymentPlatform deployment) (xsdbInit config) withGdb servers))
                    finish True config evidence Nothing M.empty (invalid config "it does not build")
                Right _ -> do
                    plain <- runDeployment withGdb True [] servers mode deployment
                    when (instrumentation /= NoCoverage) $ runCovered project servers mode instrumentation deployment (runResult plain)
                    return (runCode plain)
    let worst = maximum (0 : codes)
    if worst == 0 then exitSuccess else exitWith (ExitFailure worst)

-- | Builds and runs a test with the coverage, after its run without it.
runCovered :: LoadedProject -> Servers -> Mode -> Instrumentation -> Deployment -> Maybe TestResult -> IO ()
runCovered project servers mode instrumentation deployment plainResult = do
    let covered = deployment { deploymentConfig = coverageConfigFor instrumentation (deploymentConfig deployment) }
        config = deploymentConfig covered
        label = labelOf config
        buildArgs = BuildCmdArgs False False Nothing SelectTests Nothing Nothing False NoCoverage
    built <- try (void (buildDeployment buildArgs project covered)) :: IO (Either ExitCode ())
    case built of
        Left _ -> putStrLn (warnMessage (label ++ " does not build with coverage"))
        Right _ -> do
            maps <- readMaps (outputFolder config)
            case maps of
                Left err -> putStrLn (warnMessage (label ++ ": " ++ err))
                Right moduleMaps -> do
                    started <- timestamp
                    run <- runDeployment True False (coverageArrays moduleMaps) servers mode covered
                    ended <- timestamp
                    case (plainResult, runResult run) of
                        (Just plain, Just instrumented) | verdicts plain /= verdicts instrumented ->
                            putStrLn (warnMessage (label ++ ": the verdicts with coverage differ from those without it"))
                        (_, Nothing) -> putStrLn (warnMessage (label ++ ": the run with coverage could not be read"))
                        _ -> return ()
                    file <- writeTestResult outputFolderName (T.unpack (name config)) (T.unpack (platform config))
                        started ended (maybe "not read" (A.verdictSummary . A.testCases) (runResult run)) moduleMaps (runDumps run)
                    putStrLn (infoMessage ("The coverage of " ++ label ++ " is in " ++ file))
                    -- | The report takes every result whose stamps match the build
                    writeCoverageReport terminaVersion

  where

    verdicts result = [ (D.caseId (resultCase c), resultVerdict c) | c <- A.testCases result ]

-- | The label of a test in the reports
labelOf :: TerminaConfig -> String
labelOf config = T.unpack (name config) ++ " (" ++ T.unpack (platform config) ++ ")"

-- | The host and the port of the GDB server of a board, the ports of
-- --gdb-port, and the host and the port of the XSDB server of the PYNQ, each
-- port when it is not the one of the platform
data Servers = Servers (String, Maybe Int) [(Platform, Int)] (String, Maybe Int)

-- | How the binary of a test is launched on a platform.
data Target = Target {
    -- | The test runs on a board, and its binary is read with the tools of the
    -- cross toolchain
    targetBoard :: Bool,
    targetLauncher :: Launcher,
    -- | What is done before each run
    targetPrepare :: IO (Either String ())
  }

-- | The target of a platform, with the XSDB script of the deployment. On
-- posix-gcc, the binary runs under gdb when asked. A board is loaded with the
-- commands of gdbLaunch, and one prepared through XSDB is first prepared by
-- its XSDB server, which sources the script of the manifest: on the PYNQ it
-- resets the board, initializes its PS and loads its bitstream.
targetOf :: Platform -> Maybe FilePath -> Bool -> Servers -> Target
targetOf POSIXGCC _ False _ = Target False AsProcess (return (Right ()))
targetOf POSIXGCC _ True _ = Target False (UnderGdb "gdb" NativeProcess) (return (Right ()))
targetOf TestPlatform _ _ _ = error "no test runs on the test platform"
targetOf plt mScript _ servers@(Servers _ _ (xsdbServerHost, mXsdbPort)) =
    case (gdbLaunch plt, xsdbServerPort plt) of
        -- | The NOEL-V needs its device tree loaded by GRMON before each run
        (Nothing, _) -> notYetTarget plt servers
        (Just commands, Nothing) -> boardTarget plt commands servers
        (Just commands, Just defaultPort) -> (boardTarget plt commands servers) { targetPrepare = prepare (fromMaybe defaultPort mXsdbPort) }

  where

    prepare port = case mScript of
        Nothing -> return (Left ("the " ++ show plt ++ " is prepared before each run with an XSDB script, "
            ++ "and termina.yaml gives none: add xsdb-init to app, or to the test for " ++ show plt))
        Just script -> do
            done <- prepareBoard xsdbServerHost port script
            return (either (Left . notPrepared port) Right done)

    notPrepared port err = "the XSDB server at " ++ xsdbServerHost ++ ":" ++ show port ++ " did not prepare the board: "
        ++ err ++ "; it is started from the root of the project with xsdb -eval \"xsdbserver start -port "
        ++ show port ++ "; vwait forever\""

-- | A board on which no test runs yet: every run stops before loading.
notYetTarget :: Platform -> Servers -> Target
notYetTarget plt servers = (boardTarget plt ["load"] servers) {
    targetPrepare = return (Left ("termina test does not run the tests on " ++ show plt ++ " yet"))
  }

-- | A board under gdb-multiarch, through its GDB server.
-- The commands given reset the board and load the binary.
boardTarget :: Platform -> [String] -> Servers -> Target
boardTarget plt commands (Servers gdb ports _) =
    Target True (UnderGdb "gdb-multiarch" (RemoteServer (gdbAddress plt ports gdb) commands)) (return (Right ()))

invalid :: TerminaConfig -> String -> TestResult
invalid config reason = TestResult (labelOf config) (Just reason) [] Nothing

-- | A run of the binary that the mode plans: the case to select, the cases of
-- the descriptor it runs, and its files of the record and of the output
data Planned = Planned Integer [DescriptorCase] FilePath FilePath

-- | How execute starts the binary: as a process of posix, or under the GDB of
-- the toolchain, natively or through the GDB server of a board
data Launcher = AsProcess | UnderGdb FilePath GdbTarget

-- | The exit code, the verdicts and the flags of the coverage of a run of a deployment
data DeploymentRun = DeploymentRun {
    runCode :: Int,
    runResult :: Maybe TestResult,
    runDumps :: [(String, BS.ByteString)]
  }

-- | Run a test that is built on the target of its platform, and write its
-- report. With --record, it decodes the record of a previous run instead.
runDeployment :: Bool -> Bool -> [(String, Integer)] -> Servers -> Mode -> Deployment -> IO DeploymentRun
runDeployment withGdb report arrays servers mode deployment = do
    let config = deploymentConfig deployment
        plt = deploymentPlatform deployment
        folder = outputFolder config
        binary = folder </> "bin" </> T.unpack (name config)
        target = targetOf plt (xsdbInit config) withGdb servers
        -- | The debugger that runs the test, which a record does not need
        debugger = case (targetLauncher target, mode) of
            (_, Recorded _) -> Nothing
            (UnderGdb gdb _, _) -> Just gdb
            (AsProcess, _) -> Nothing
    loaded <- readDescriptor (folder </> "test" <.> "json")
    case loaded of
      Left err -> do
        evidence <- evidenceOf config mode Nothing (targetLauncher target)
        unread <$> finish report config evidence Nothing M.empty (invalid config ("its descriptor cannot be read: " ++ err))
      Right descriptor -> do
        prefix <- if targetBoard target
            then M.findWithDefault "" "EXPANDED_CROSS" <$> buildInfo (labelOf config) folder
            else return ""
        found <- maybe (return Nothing) findExecutable debugger
        version <- maybe (return Nothing) debuggerVersion found
        evidence <- (\e -> e { evidenceDebugger = version })
            <$> evidenceOf config mode (descriptorTimeout descriptor) (targetLauncher target)
        let available = case (debugger, found) of
                (Just gdb, Nothing) -> Left (gdb ++ ", which runs the test"
                    ++ (if targetBoard target then " on the board" else "") ++ ", is not in the PATH")
                _ -> Right ()
        case available >> plan folder descriptor of
          Left err -> unread <$> finish report config evidence (Just descriptor) M.empty (invalid config err)
          Right planned -> do
            stamp <- stampAddress binary
            either (putStrLn . warnMessage) (const (return ())) stamp
            (linked, unread') <- linkedArrays binary arrays
            maybe (return ()) (putStrLn . warnMessage) unread'
            let run p = case mode of
                    Recorded given -> fmap (\r -> (r, [])) <$> readRecord (platformLayout plt) descriptor (either (const Nothing) Just stamp) given
                    _ -> targetPrepare target >>= either (return . Left)
                        (const (execute config (targetLauncher target) (platformLayout plt) descriptor binary (either (const Nothing) Just stamp) linked p))
            ran <- runAll run planned
            case ran of
              Left err -> unread <$> finish report config evidence (Just descriptor) M.empty (invalid config err)
              Right done -> do
                let runs = map fst done
                    results = concatMap (analyse . fst) runs
                    overflowed = [ (headerCasesEnded h, headerChecksFailed h) | (r, h) <- runs, runOverflow r ]
                    overflow = if null overflowed then Nothing else Just (sum (map fst overflowed), sum (map snd overflowed))
                let lookups = [ (recorded, if kind == ReturnAddress then callSite plt recorded else recorded)
                              | (recorded, kind) <- addressesOf results ]
                (locations, warning) <- locateAddresses prefix binary lookups
                maybe (return ()) (putStrLn . warnMessage) warning
                let result = TestResult (labelOf config) Nothing results overflow
                code <- finish report config evidence (Just descriptor) locations result
                -- | A flag is set when any of the processes set it
                return (DeploymentRun code (Just result) (mergeDumps (map snd done)))

  where

    unread code = DeploymentRun code Nothing []

    mergeDumps dumps =
        case dumps of
            [] -> []
            first : rest -> foldl (zipWith (\(s, a) (_, b) -> (s, BS.pack (BS.zipWith max a b)))) first rest

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
    return (Run cases (map (relocate offset) records) end (testBit (headerFlags header) 1) (headerLastStep header) frames
                (testBit (headerFlags header) 0), header)

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
execute :: TerminaConfig -> Launcher -> Layout -> Descriptor -> FilePath -> Maybe Integer -> [(String, Integer)] -> Planned
        -> IO (Either String ((Run, Header), [(String, BS.ByteString)]))
execute config launcher layout descriptor binary stamp arrays (Planned selected cases recordFile logFile) = do
    ran <- case launcher of
        AsProcess -> fmap (\r -> (fromProcess r, [])) <$>
            runPosix layout binary recordFile logFile (recordCapacity config) selected (descriptorTimeout descriptor)
        UnderGdb gdb target -> fmap (\r -> (fromGdb r, gdbCoverage r)) <$>
            runGdb gdb binary target (takeDirectory recordFile) logFile selected (descriptorTimeout descriptor) arrays
    return $ ran >>= \((bytes, end, frames, noRecord), dumps) ->
        (\decoded -> (decoded, dumps)) <$> decodeRun layout descriptor stamp (const (Right cases)) noRecord bytes end frames

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
finish :: Bool -> TerminaConfig -> Evidence -> Maybe Descriptor -> M.Map Integer (Maybe S.Location) -> TestResult -> IO Int
finish report config evidence mDescriptor locations result = do
    let folder = outputFolder config
    when report $ mapM_ putStrLn (renderReport (fromMaybe emptyDescriptor mDescriptor) locations result)
    BL.writeFile (folder </> "results" <.> "json") (renderResults (resultsJson evidence mDescriptor locations result))
    writeFile (folder </> "report" <.> "xml") (renderJUnit evidence mDescriptor locations result)
    return (exitCodeOf result)

  where

    emptyDescriptor = Descriptor 1 0 "" "unit" "" Nothing [] [] M.empty M.empty (Symbols "" "" "" "")

-- | What ties the result to the build: the commit of the project, the hash of
-- the binary and the hashes of provenance.json
evidenceOf :: TerminaConfig -> Mode -> Maybe Integer -> Launcher -> IO Evidence
evidenceOf config mode seconds launcher = do
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
        evidenceExecutor = case launcher of
            AsProcess -> "process"
            UnderGdb _ _ -> "gdb",
        evidenceDebugger = Nothing,
        evidenceTimeout = seconds,
        evidenceCommit = case (sha, status) of
            (Right s, Right st) -> Just (concat (lines s), not (null (lines st)))
            _ -> Nothing,
        evidenceBinarySha = binarySha,
        evidenceProvenance = provenance
      }

-- | The first line of --version of a GDB, if it gives one
debuggerVersion :: FilePath -> IO (Maybe String)
debuggerVersion gdb = do
    ran <- try (readProcessWithExitCode gdb ["--version"] "")
    return $ case ran of
        Right (ExitSuccess, out, _) -> case lines out of
            (first : _) -> Just first
            [] -> Nothing
        Right _ -> Nothing
        Left err -> const Nothing (err :: IOException)

-- | Lists the tests, their platforms and their cases
listTests :: Bool -> IO ()
listTests json = do
    -- | A test that cannot be read exits with 2, as in a run
    loaded <- try (do
        manifest <- loadManifest projectRoot >>= either dieWithFirstError return
        project <- readProject projectRoot manifest >>= either dieWithFirstError return
        tests <- forM (M.toList (manifestTests manifest)) $ \(testName, entry) -> do
          cases <- loadTestCases project (T.unpack testName)
          return (testName, entry, cases)
        return (manifest, tests)) :: IO (Either ExitCode (Manifest, [(T.Text, TestEntry, [TestCase])]))
    (manifest, listed) <- either (const (exitWith (ExitFailure 2))) return loaded
    if json
      then BLC.putStrLn $ encodePretty' defConfig { confCompare = keyOrder keys } (object (
        -- | The application, which a debugging session of VS Code loads as a test
        [ "app" .= object [
              "name" .= projectName (manifestProject manifest),
              "platform" .= show (appPlatform app),
              "xsdb-init" .= appXsdbInit app ]
        | Application app <- [manifestNature manifest] ] ++ [
          "tests" .= [ object [
              "name" .= testName,
              "folder" .= ("tests" </> T.unpack testName),
              "level" .= level cases,
              "platforms" .= map show (testPlatforms entry),
              -- | The XSDB script of each platform that has one, the one of the application included
              "xsdb-init" .= M.fromList [ (show plt, script)
                  | plt <- testPlatforms entry, Just script <- [testXsdbInitOn manifest entry plt] ],
              "cases" .= [ object [
                  "id" .= caseId c,
                  "instance" .= caseInstance c,
                  "case" .= caseName c,
                  "file" .= caseFile c,
                  "line" .= caseLine c
                ] | c <- cases ]
            ] | (testName, entry, cases) <- listed ]
        ]))
      else forM_ listed $ \(testName, entry, cases) -> do
        putStrLn $ T.unpack testName ++ " (" ++ level cases ++ ") on "
          ++ joinWith ", " (map show (testPlatforms entry))
        forM_ cases $ \c -> putStrLn ("  " ++ caseId c)

  where

    keys = ["app", "tests", "name", "folder", "level", "platform", "platforms", "xsdb-init", "cases", "id", "instance", "case", "file", "line"]

    caseId c = caseInstance c ++ "." ++ caseName c

    -- | A test without test instances is a concurrent test
    level :: [TestCase] -> String
    level [] = "concurrent"
    level _ = "unit"

    joinWith sep = foldr1 (\a b -> a ++ sep ++ b)

-- | Parse the modules of a test and read its cases
loadTestCases :: LoadedProject -> String -> IO [TestCase]
loadTestCases project testName = do
    let testFolder = "tests" </> testName
        sources = buildSources project (Just testName) testFolder
        files = sourceTexts sources
    (_, parsedProject) <- loadMainModule sources testFolder "test" >>= orDieWithFailure files
    order <- orDieWithFailure files (orderedModules parsedProject)
    either dieWithError return $
      testCases [parsedAST . metadata $ parsedProject M.! m | m <- order]

-- | The cases of a test in the order in which the harness numbers them.
testCases :: [AnnotatedProgram ParserAnn] -> Either String [TestCase]
testCases programs =
    map testCase <$> parsedCases (concat programs)

  where

    testCase c = let loc = caseLocation c in
        TestCase (NC.caseInstance c) (NC.caseName c) (fileOf loc) (locationLine loc)

    fileOf (Position _ start _) = sourceName start
    fileOf _ = ""
