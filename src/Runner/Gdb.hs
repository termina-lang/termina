-- | The executor of GDB: it drives GDB through GDB/MI, stops the binary of a
-- test at termina__test__init to write the case to run, waits for the end or
-- for an exception, and dumps the header and the area of the record by their
-- symbols. It runs a board through its GDB server, and a posix binary under
-- the native gdb.
module Runner.Gdb (
    GdbTarget(..), GdbEnd(..), GdbRun(..), runGdb
) where

import Runner.Mi

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.List (intercalate)
import System.FilePath ((</>))
import System.IO
import System.Process
import System.Timeout (timeout)

-- | Where the binary runs
data GdbTarget =
    NativeProcess -- ^ a posix binary under the native gdb
    | RemoteServer String [String] -- ^ the address of the GDB server and the commands that load the binary
  deriving (Eq, Show)

-- | Where the run ended
data GdbEnd =
    AtEnd -- ^ at termina__test__end
    | AtReboot -- ^ at termina__exec__reboot, after an exception
    | AtTimeout -- ^ interrupted by the runner at the timeout
    | Crashed String -- ^ stopped by a signal, as a fault of a posix process
  deriving (Eq, Show)

data GdbRun = GdbRun {
    gdbEnd :: GdbEnd,
    gdbRecord :: BS.ByteString, -- ^ the header followed by the area
    gdbBacktrace :: [String] -- ^ the frames at termina__exec__reboot
  } deriving (Eq, Show)

-- | The seconds to reach termina__test__init, which include the load of the binary
startLimit :: Int
startLimit = 120

-- | The seconds that GDB has to stop the target after an interrupt and to answer a command
answerLimit :: Int
answerLimit = 10

-- | A session of GDB: its input, its output and the log of every line it
-- writes; each command waits for its result before the next is sent
data Session = Session Handle Handle Handle

-- | Runs the binary once under GDB, with the folder of the dumps, the file of
-- the log of GDB, the case to select and the timeout from the stop at
-- termina__test__init
runGdb :: FilePath -> FilePath -> GdbTarget -> FilePath -> FilePath -> Integer -> Maybe Integer -> IO (Either String GdbRun)
runGdb gdb binary target folder logFile selectedCase seconds = do
    started <- try (createProcess (proc gdb ["--interpreter=mi3", "-nx", "-q", binary]) {
            std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })
    case started of
        Left err -> return (Left ("could not run " ++ gdb ++ ": " ++ show (err :: IOException)))
        Right (Just input, Just output, Just errors, process) ->
            withFile logFile WriteMode $ \logHandle -> do
                hSetBuffering input LineBuffering
                hClose errors
                let session = Session input output logHandle
                outcome <- drive session
                _ <- try (command session "-gdb-exit") :: IO (Either IOException (Either String [(String, MiValue)]))
                ended <- timeout (answerLimit * 1000000) (waitForProcess process)
                case ended of
                    Just _ -> return ()
                    Nothing -> terminateProcess process >> () <$ waitForProcess process
                return outcome
        Right _ -> return (Left ("could not run " ++ gdb ++ " with its pipes"))

  where

    headerFile = folder </> "header.bin"
    areaFile = folder </> "area.bin"

    drive session = do
        setup <- sequenceAll session $
            ["-gdb-set mi-async on", "-gdb-set confirm off", "-gdb-set pagination off"]
            ++ (case target of
                    -- | The signals that the OSAL of posix uses for its clock and its
                    -- interrupts, and the faults that it turns into exceptions
                    NativeProcess -> ["-gdb-set startup-with-shell off",
                                      console "handle SIGALRM SIGUSR1 SIGUSR2 SIGSEGV SIGBUS SIGFPE SIGILL nostop noprint pass"]
                    RemoteServer address launch ->
                        ["-gdb-set tcp auto-retry off", "-target-select extended-remote " ++ address] ++ map console launch)
            ++ [ "-break-insert " ++ symbol | symbol <- ["termina__test__init", "termina__test__end", "termina__exec__reboot"] ]
            ++ [ case target of
                    NativeProcess -> "-exec-run"
                    RemoteServer _ _ -> "-exec-continue" ]
        case setup of
            Left err -> return (Left err)
            Right () -> do
                first <- awaitStop session (Just startLimit)
                case first of
                    Nothing -> return (Left ("the binary did not reach termina__test__init in " ++ show startLimit ++ " s"))
                    Just stop ->
                        if stopFunction stop /= Just "termina__test__init"
                            then return (Left ("the binary stopped before termina__test__init: " ++ describeStop stop))
                            else do
                                selected <- sequenceAll session [
                                    "-gdb-set var termina__test__header.selected_case = " ++ show selectedCase,
                                    "-exec-continue" ]
                                either (return . Left) (const (finish session)) selected

    -- | The stop after the case was written, the backtrace at an exception, and the dumps
    finish session = do
        stop <- awaitStop session (fromIntegral <$> seconds)
        endOf <- case stop of
            Nothing -> do
                _ <- command session "-exec-interrupt"
                interrupted <- awaitStop session (Just answerLimit)
                return $ maybe (Left "the target did not stop after the interrupt at the timeout") (const (Right AtTimeout)) interrupted
            Just s -> return $ case (stopReason s, stopFunction s) of
                ("breakpoint-hit", Just "termina__test__end") -> Right AtEnd
                ("breakpoint-hit", Just "termina__exec__reboot") -> Right AtReboot
                ("signal-received", _) -> Right (Crashed (maybe "a signal" id (miText "signal-name" (stopResults s))))
                _ -> Left ("the binary ended without reaching termina__test__end: " ++ describeStop s)
        case endOf of
            Left err -> return (Left err)
            Right end -> do
                frames <- if end == AtReboot || isCrash end then backtrace session else return []
                -- | The area is declared without its size in the headers of the OSAL,
                -- which an older GDB takes from the file it is stopped in
                dumped <- sequenceAll session [
                    console ("dump binary memory " ++ headerFile
                             ++ " (char*)&termina__test__header (char*)&termina__test__header+sizeof(termina__test__header)"),
                    console ("dump binary memory " ++ areaFile
                             ++ " (char*)&termina__test__area (char*)&termina__test__area+termina__test__capacity") ]
                case dumped of
                    Left err -> return (Left ("the record could not be dumped: " ++ err))
                    Right () -> do
                        bytes <- BS.append <$> BS.readFile headerFile <*> BS.readFile areaFile
                        return (Right (GdbRun end bytes frames))

    isCrash end =
        case end of
            Crashed _ -> True
            _ -> False

    backtrace session = do
        answer <- command session "-stack-list-frames"
        return $ case answer of
            Right results -> case miField "stack" results of
                Just (MiList frames) -> [ frameText f | MiTuple f <- frames ]
                _ -> []
            Left _ -> []

    frameText f =
        let level = maybe "?" id (miText "level" f)
            function = maybe "??" id (miText "func" f)
            place = case (miText "file" f, miText "line" f) of
                (Just file, Just line) -> " at " ++ file ++ ":" ++ line
                _ -> ""
        in "#" ++ level ++ " " ++ function ++ place

-- | A command of the console through GDB/MI
console :: String -> String
console text = "-interpreter-exec console " ++ miEscape text

-- | The commands one after another, up to the first that fails
sequenceAll :: Session -> [String] -> IO (Either String ())
sequenceAll session commands =
    case commands of
        [] -> return (Right ())
        c : rest -> do
            answer <- command session c
            case answer of
                Left err -> return (Left (c ++ ": " ++ err))
                Right _ -> sequenceAll session rest

-- | Sends a command and waits for its result
command :: Session -> String -> IO (Either String [(String, MiValue)])
command session@(Session input _ _) text = do
    hPutStrLn input text
    answer <- awaitRecord session (Just answerLimit) isResult
    return $ case answer of
        Just (MiResult _ "error" results) -> Left (maybe "error" id (miText "msg" results))
        Just (MiResult _ _ results) -> Right results
        _ -> Left ("no answer in " ++ show answerLimit ++ " s")

  where

    isResult r = case r of
        MiResult _ _ _ -> True
        _ -> False

data Stop = Stop {
    stopReason :: String,
    stopFunction :: Maybe String,
    stopResults :: [(String, MiValue)]
  }

-- | The next stop of the target, if it comes within the limit
awaitStop :: Session -> Maybe Int -> IO (Maybe Stop)
awaitStop session limit = do
    found <- awaitRecord session limit isStop
    return $ case found of
        Just (MiAsync _ _ results) ->
            let frame = case miField "frame" results of
                    Just (MiTuple f) -> miText "func" f
                    _ -> Nothing
            in Just (Stop (maybe "" id (miText "reason" results)) frame results)
        _ -> Nothing

  where

    isStop r = case r of
        MiAsync '*' "stopped" _ -> True
        _ -> False

describeStop :: Stop -> String
describeStop s =
    intercalate ", " ([stopReason s] ++ maybe [] (\f -> ["in " ++ f]) (stopFunction s)
        ++ maybe [] (\c -> ["exit code " ++ c]) (miText "exit-code" (stopResults s)))

-- | Reads the lines of GDB, writing each one to the log, up to the first that
-- the predicate takes, or nothing at the limit or at the end of the output
awaitRecord :: Session -> Maybe Int -> (MiRecord -> Bool) -> IO (Maybe MiRecord)
awaitRecord (Session _ output logHandle) limit wanted =
    let loop = do
            line <- try (hGetLine output) :: IO (Either IOException String)
            case line of
                Left _ -> return Nothing
                Right text -> do
                    hPutStrLn logHandle text
                    let r = parseMiLine text
                    if wanted r then return (Just r) else loop
    in case limit of
        Nothing -> loop
        Just s -> maybe Nothing id <$> timeout (s * 1000000) loop
