-- | The executor of posix: the binary of a test is a process of the host,
-- whose record the runtime maps onto a file that the runner creates.
module Runner.Posix (
    PosixRun(..), runPosix
) where

import Runner.Record (Layout(..), headerSize)

import Control.Exception (IOException, onException, try)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO (IOMode(WriteMode), withFile)
import System.Process
import System.Timeout (timeout)

-- | How the process ended and the bytes of its record
data PosixRun = PosixRun {
    posixExit :: Maybe ExitCode, -- ^ nothing when the runner killed it at the timeout
    posixRecord :: BS.ByteString
  } deriving Show

-- | Runs the binary with its record on a file of the size of the header and the
-- area, zeroed but for the selected case, with its output on a log file
runPosix :: Layout -> FilePath -> FilePath -> FilePath -> Integer -> Integer -> Maybe Integer -> IO (Either String PosixRun)
runPosix layout binary recordFile logFile capacity selectedCase seconds = do
    BS.writeFile recordFile (emptyRecord layout capacity selectedCase)
    environment <- getEnvironment
    let others = filter ((/= "TERMINA_TEST_RECORD") . fst) environment
    started <- try $ withFile logFile WriteMode $ \logHandle -> do
        (_, _, _, process) <- createProcess (proc binary []) {
                env = Just (("TERMINA_TEST_RECORD", recordFile) : others),
                std_in = NoStream,
                std_out = UseHandle logHandle,
                std_err = UseHandle logHandle
            }
        -- | An interrupted runner does not leave the process behind
        ended <- (case seconds of
            Nothing -> Just <$> waitForProcess process
            Just s -> timeout (fromIntegral s * 1000000) (waitForProcess process))
            `onException` (terminateProcess process >> waitForProcess process)
        case ended of
            Just code -> return (Just code)
            Nothing -> terminateProcess process >> waitForProcess process >> return Nothing
    case started of
        Left err -> return (Left ("could not run " ++ binary ++ ": " ++ show (err :: IOException)))
        Right code -> Right . PosixRun code <$> BS.readFile recordFile

-- | The file before the run: zeros, and the case to run in selected_case
emptyRecord :: Layout -> Integer -> Integer -> BS.ByteString
emptyRecord layout capacity selectedCase =
    let size = headerSize layout + fromIntegral capacity
        selected = [ fromIntegral ((selectedCase `shiftR` (8 * i)) .&. 0xFF) | i <- [0 .. 3 :: Int] ]
        ordered = if layoutLittleEndian layout then selected else reverse selected
    in BS.concat [BS.replicate 24 0, BS.pack ordered, BS.replicate (size - 28) 0]
