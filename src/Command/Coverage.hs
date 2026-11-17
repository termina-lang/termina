-- | termina coverage: runs the application or the tests built with the
-- coverage, and writes the report from every result that matches the build.
module Command.Coverage (
    coverageCmdArgsParser, coverageCommand, CoverageCmdArgs
) where

import Command.Build (BuildCmdArgs(..), Selection(..), announce, buildDeployment, nothingSelected, selectDeployments, selectedDeployments)
import Command.Provenance (terminaVersion)
import Command.Run (runApplication)
import Command.Test (modeOf, runTests, cannotRun)
import Command.Utils
import Configuration.Configuration
import Configuration.Manifest (Deployment(..), Instrumentation(..), coverageConfigFor)
import Configuration.Platform (Platform(..), checkPlatform)
import Runner.Coverage (coverageArrays, readDumps, readMaps, writeAppResult, writeTestResult)
import Runner.Servers (Server, readServer)

import Control.Monad (forM_, when)
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Options.Applicative as O
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import System.Info (os)

-- | The application, a test or every test
data Target = Application | OneTest String | EveryTest
  deriving Show

data CoverageCmdArgs =
    CoverageCmdArgs
        Target
        (Maybe Platform) -- ^ Only the tests on this platform
        (Maybe ProjectProfile) -- ^ Profile of what runs
        (Maybe String) -- ^ Only this case of the test
        Bool -- ^ Run each case in a process of its own
        (Maybe Integer) -- ^ Seconds after which the application stops
        Bool -- ^ Run the binary of posix-gcc without the coverage under gdb
        (Maybe Server) -- ^ The GDB server of the board
        (Maybe Server) -- ^ The XSDB command server of the PYNQ
        Bool -- ^ Fail instead of fetching the dependencies
        Bool -- ^ Instrument also the modules of the dependencies
        Bool -- ^ Only write the report again
        Bool -- ^ Only build with the coverage
        (Maybe FilePath) -- ^ The folder of the flags dumped outside the command
    deriving Show

coverageCmdArgsParser :: O.Parser CoverageCmdArgs
coverageCmdArgsParser = CoverageCmdArgs
    <$> targetParser
    <*> O.optional (O.option (O.maybeReader checkPlatform)
          ( O.long "platform" <> O.metavar "PLATFORM"
          <> O.help "Only the tests on this platform"))
    <*> O.optional (O.option (O.eitherReader readProfile)
          ( O.long "profile" <> O.metavar "debug|release"
          <> O.help "Profile of what runs (default: the one of the application, debug for the tests of a library)"))
    <*> O.optional (O.strOption
          ( O.long "case" <> O.metavar "INSTANCE.CASE"
          <> O.help "Only this case of the test"))
    <*> O.switch (O.long "isolated"
        <> O.help "Run each case of the tests in a process of its own")
    <*> O.optional (O.option O.auto
          ( O.long "timeout" <> O.metavar "SECONDS"
          <> O.help "Stop the application after SECONDS"))
    <*> O.switch (O.long "with-gdb"
        <> O.help "On posix-gcc, run also the binary without coverage under gdb")
    <*> O.optional (O.option (O.eitherReader (readServer "GDB server"))
          ( O.long "gdb" <> O.metavar "HOST[:PORT]"
          <> O.help "The host of the GDB servers of the boards, or the host and the port of the server of a single board (default: localhost, or host.docker.internal in the container)"))
    <*> O.optional (O.option (O.eitherReader (readServer "XSDB server"))
          ( O.long "xsdb" <> O.metavar "HOST[:PORT]"
          <> O.help "The XSDB command server that prepares the PYNQ (default: the host of the GDB server)"))
    <*> O.switch (O.long "offline"
        <> O.help "Fail instead of fetching when termina.lock does not match termina.yaml")
    <*> O.switch (O.long "include-deps"
        <> O.help "Instrument also the modules of the dependencies")
    <*> O.switch (O.long "report-only"
        <> O.help "Write the report again from the results in output/coverage/runs, without building nor running")
    <*> O.switch (O.long "build-only"
        <> O.help "Build with coverage in output/coverage, without running, with the GDB scripts that clear and dump the flags")
    <*> O.optional (O.strOption
          ( O.long "record" <> O.metavar "FOLDER"
          <> O.help "Take the flags that the dump.gdb of the build dumped to FOLDER as a run, without building nor running"))

  where

    targetParser =
          (OneTest <$> O.strOption (O.long "test" <> O.metavar "NAME" <> O.help "One test, on every platform of its list"))
      O.<|> O.flag' EveryTest (O.long "tests" <> O.help "Every test")
      O.<|> pure Application

-- | Runs the application or the tests with the coverage, builds them, or writes the report again
coverageCommand :: CoverageCmdArgs -> IO ()
coverageCommand (CoverageCmdArgs target mPlatform mProfile mCase isolated seconds withGdb mServer mXsdb offline withDeps reportOnly buildOnly mRecord)
    | reportOnly = refuse "--report-only" (given ++ [ "--build-only" | buildOnly ] ++ [ "--record" | isJust mRecord ] ++ [ "--test or --tests" | notApplication target ])
        >> report
    | Just folder <- mRecord = refuse "--record" (filter (`notElem` ["--platform", "--profile"]) given ++ [ "--build-only" | buildOnly ] ++ [ "--tests" | isEveryTest target ])
        >> record folder
    | buildOnly = refuse "--build-only" (filter (`notElem` ["--platform", "--profile", "--offline", "--include-deps"]) given)
        >> buildWithScripts
    | otherwise = refuseMachO >> case target of
        Application -> do
            refuse "the application" (filter (`elem` ["--platform", "--case", "--isolated"]) given)
            runApplication instrumentation mProfile offline seconds withGdb mServer mXsdb
        _ -> do
            refuse "the tests" (filter (== "--timeout") given)
            either cannotRun (runTests (testName target) mPlatform mProfile withGdb mServer mXsdb offline instrumentation)
                (modeOf (testName target) mCase isolated Nothing offline)

  where

    instrumentation = if withDeps then CoverageWithDeps else Coverage

    -- | The names of the options given
    given = [ option | (True, option) <-
        [ (isJust mPlatform, "--platform"), (isJust mProfile, "--profile"), (isJust mCase, "--case")
        , (isolated, "--isolated"), (isJust seconds, "--timeout"), (withGdb, "--with-gdb")
        , (isJust mServer, "--gdb"), (isJust mXsdb, "--xsdb"), (offline, "--offline"), (withDeps, "--include-deps") ] ]

    refuse what options = case options of
        option : _ -> cannotRun (option ++ " cannot be used with " ++ what)
        [] -> return ()

    notApplication t = case t of { Application -> False; _ -> True }

    isEveryTest t = case t of { EveryTest -> True; _ -> False }

    -- | Builds the selected deployments with coverage and says where the GDB scripts of each are
    buildWithScripts = do
        let args = BuildCmdArgs False False Nothing (selection target) mPlatform mProfile offline instrumentation
        (project, deployments) <- selectedDeployments args
        forM_ deployments $ \deployment -> do
            announce deployments ("Building " ++ deploymentLabel deployment)
            _ <- buildDeployment args project deployment
            putStrLn (infoMessage ("The GDB scripts of " ++ deploymentLabel deployment ++ " are in "
                ++ (outputFolder (deploymentConfig deployment) </> "gdb")))

    -- | Stops on macOS when posix-gcc is among the selected deployments
    refuseMachO = when (os == "darwin") $ do
        manifest <- loadManifest projectRoot >>= either dieWithFirstError return
        deployments <- either cannotRun return (selectDeployments manifest (selection target) mPlatform mProfile)
        when (POSIXGCC `elem` map deploymentPlatform deployments) $
            cannotRun ("termina coverage cannot run " ++ show POSIXGCC ++ " on macOS: run it in the container, or choose a board with --platform")

    -- | A run from the flags dumped to a folder, on the build with the coverage of one platform
    record folder = do
        manifest <- loadManifest projectRoot >>= either dieWithFirstError return
        deployments <- either cannotRun return (selectDeployments manifest (selection target) mPlatform mProfile)
        deployment <- case deployments of
            [d] -> return d
            [] -> cannotRun (nothingSelected manifest (selection target) mPlatform)
            ds -> cannotRun ("--record takes the flags of one platform, and the test has "
                ++ intercalate ", " (map (show . deploymentPlatform) ds) ++ "; choose one with --platform")
        let config = coverageConfigFor instrumentation (deploymentConfig deployment)
        maps <- readMaps (outputFolder config) >>= either cannotRun return
        dumps <- readDumps folder (coverageArrays maps) >>= either cannotRun return
        when (null dumps) $ cannotRun (folder ++ " has no coverage dump of " ++ outputFolder config)
        now <- timestamp
        file <- case target of
            Application -> writeAppResult outputFolderName (show (deploymentPlatform deployment)) now now "recorded" 0 maps dumps
            _ -> writeTestResult outputFolderName (T.unpack (name config)) (T.unpack (platform config)) now now maps dumps
        putStrLn (infoMessage ("The coverage of the record is in " ++ file))
        report

    testName t = case t of { OneTest n -> Just n; _ -> Nothing }

    selection t = case t of { Application -> SelectApp; OneTest n -> SelectTest n; EveryTest -> SelectTests }

    report = do
        let runs = outputFolderName </> "coverage" </> "runs"
        found <- doesDirectoryExist runs
        if not found
            then cannotRun ("there are no coverage results in " ++ runs)
            else writeCoverageReport terminaVersion
