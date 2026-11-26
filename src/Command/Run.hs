-- | termina run: builds the application if needed and runs it on its platform.
module Command.Run (
    runCmdArgsParser, runCommand, RunCmdArgs, runApplication
) where

import Command.Build (BuildCmdArgs(..), Selection(..), buildDeployment, refuseLocalFoldersInRelease, warnLocalFolders)
import Command.Dependencies (Fetching(..), FetchingCommand(..))
import Command.Project (Dependencies(..), loadProject)
import Command.Sources (LoadedProject(..))
import Command.Test (Servers(..), Target(..), Launcher(..), targetOf)
import Command.Provenance (terminaVersion)
import Command.Utils
import Configuration.Configuration
import Configuration.Manifest
import Configuration.Platform (Platform)
import Runner.Coverage (readMaps, coverageArrays, linkedArrays, writeAppResult)
import Runner.Gdb (AppRun(..), runAppGdb)
import Runner.Posix (waitFor)
import Runner.Servers (Server(..), readServer, gdbPortsParser, checkGdbPorts, gdbHost, xsdbHost)

import Control.Exception (AsyncException(UserInterrupt), catch, throwIO)
import Control.Monad (void, when)
import qualified Data.Text as T
import qualified Options.Applicative as O
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>))
import System.Process (createProcess, proc)

data RunCmdArgs =
    RunCmdArgs
        (Maybe ProjectProfile) -- ^ Profile of the application
        Bool -- ^ Fail instead of fetching the dependencies
        (Maybe Integer) -- ^ Seconds after which the run stops
        Bool -- ^ Run the binary of posix-gcc under gdb
        (Maybe Server) -- ^ The GDB server of the board
        [(Platform, Int)] -- ^ The port of the GDB server of some platforms
        (Maybe Server) -- ^ The XSDB command server of the PYNQ
    deriving Show

runCmdArgsParser :: O.Parser RunCmdArgs
runCmdArgsParser = RunCmdArgs
    <$> O.optional (O.option (O.eitherReader readProfile)
          ( O.long "profile" <> O.metavar "debug|release"
          <> O.help "Profile of the application (default: the one of termina.yaml)"))
    <*> O.switch (O.long "offline"
        <> O.help "Fail instead of fetching when termina.lock does not match termina.yaml")
    <*> O.optional (O.option O.auto
          ( O.long "timeout" <> O.metavar "SECONDS"
          <> O.help "Stop the application after SECONDS"))
    <*> O.switch (O.long "with-gdb"
        <> O.help "On posix-gcc, run the binary under gdb")
    <*> O.optional (O.option (O.eitherReader (readServer "GDB server"))
          ( O.long "gdb" <> O.metavar "HOST[:PORT]"
          <> O.help "The GDB server of the board (default: localhost, or host.docker.internal in the container)"))
    <*> gdbPortsParser
    <*> O.optional (O.option (O.eitherReader (readServer "XSDB server"))
          ( O.long "xsdb" <> O.metavar "HOST[:PORT]"
          <> O.help "The XSDB command server that prepares the PYNQ (default: the host of the GDB server)"))

-- | Builds the application if needed and runs it until a Ctrl-C, the timeout or its end
runCommand :: RunCmdArgs -> IO ()
runCommand (RunCmdArgs mProfile offline seconds withGdb mServer gdbPorts mXsdb) =
    runApplication NoCoverage mProfile offline seconds withGdb mServer gdbPorts mXsdb

-- | Runs the application built with its instrumentation
runApplication :: Instrumentation -> Maybe ProjectProfile -> Bool -> Maybe Integer -> Bool -> Maybe Server -> [(Platform, Int)] -> Maybe Server -> IO ()
runApplication instrumentation mProfile offline seconds withGdb mServer gdbPorts mXsdb = do
    ports <- either dieWithError return (checkGdbPorts gdbPorts)
    let withCoverage = instrumentation /= NoCoverage
    refuseLocalFoldersInRelease SelectApp Nothing mProfile
    project <- loadProject (Update (if offline then Offline else Reuse BuildCommand))
    let manifest = loadedManifest project
    plain <- maybe (dieWithError "termina run needs an application, and the project is a library") return
        (appDeploymentIn mProfile manifest)
    let deployment = plain { deploymentConfig = coverageConfigFor instrumentation (deploymentConfig plain) }
        config = deploymentConfig deployment
        plt = deploymentPlatform deployment
        binary = outputFolder config </> "bin" </> T.unpack (name config)
        buildArgs = BuildCmdArgs False False Nothing SelectApp Nothing mProfile offline NoCoverage
    warnLocalFolders project [deployment]
    void (buildDeployment buildArgs project deployment)
    host <- gdbHost mServer
    let servers = Servers (host, mServer >>= \(Server _ port) -> port) ports
                          (xsdbHost host mXsdb, mXsdb >>= \(Server _ port) -> port)
        target = targetOf plt (xsdbInit config) (withGdb || withCoverage) servers
    prepared <- targetPrepare target
    either dieWithError return prepared
    arrays <- if withCoverage
        then either dieWithError (return . coverageArrays) =<< readMaps (outputFolder config)
        else return []
    (linked, unread) <- linkedArrays binary arrays
    maybe (return ()) (putStrLn . warnMessage) unread
    started <- timestamp
    putStrLn (infoMessage ("Running " ++ deploymentLabel deployment ++ "; Ctrl-C stops it"))
    case targetLauncher target of
        AsProcess -> runProcess binary
        UnderGdb gdb gdbTarget -> do
            let folder = outputFolder config </> "run"
            createDirectoryIfMissing True folder
            ran <- runAppGdb gdb binary gdbTarget folder (folder </> "gdb.log") seconds linked
            ended <- timestamp
            case ran of
                Left err -> dieWithError err
                Right run -> do
                    putStrLn (infoMessage ("The application ended (" ++ appEnd run ++ ") after "
                        ++ show (appReboots run) ++ (if appReboots run == 1 then " reboot" else " reboots")))
                    when withCoverage $ do
                        maps <- either dieWithError return =<< readMaps (outputFolder config)
                        file <- writeAppResult outputFolderName (show plt) started ended (appEnd run) (Just (appReboots run))
                            maps (appCoverage run)
                        putStrLn (infoMessage ("The coverage of the run is in " ++ file))
                        writeCoverageReport terminaVersion

  where

    -- | A posix binary without gdb, which a Ctrl-C or the timeout stops
    runProcess binary = do
        (_, _, _, process) <- createProcess (proc binary [])
        ended <- waitFor seconds process `catch` (\e -> if e == UserInterrupt then return Nothing else throwIO e)
        case ended of
            Just (ExitFailure code) -> exitWith (ExitFailure code)
            _ -> return ()
