-- | The preparation of a board through an XSDB command server, which reads one
-- command per line and answers each one with a line that starts with okay or
-- error. The server runs on the host, started from the root of the project,
-- so the paths it is given are relative to the project.
module Runner.Xsdb (prepareBoard, prepareLimit) where

import Control.Exception (IOException, bracket, bracketOnError, try)
import Data.List (stripPrefix)
import Network.Socket
import System.IO
import System.Timeout (timeout)

-- | The seconds the server has to reset the board and to load the bitstream
prepareLimit :: Int
prepareLimit = 60

-- | Sources the script through the server at the host and the port
prepareBoard :: String -> Int -> FilePath -> IO (Either String ())
prepareBoard host port script = do
    result <- try (timeout (prepareLimit * 1000000) (bracket open hClose exchange))
    return $ case result of
        Left err -> Left (show (err :: IOException))
        Right Nothing -> Left ("no answer in " ++ show prepareLimit ++ " s")
        Right (Just answer) -> answer

  where

    open = do
        addresses <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just (show port))
        case addresses of
            [] -> ioError (userError ("no address for " ++ host))
            address : _ ->
                bracketOnError (socket (addrFamily address) Stream defaultProtocol) close $ \sock -> do
                    connect sock (addrAddress address)
                    socketToHandle sock ReadWriteMode

    exchange handle = do
        hSetBuffering handle LineBuffering
        hPutStrLn handle ("source {" ++ script ++ "}")
        answer <- hGetLine handle
        return $ case stripPrefix "okay" answer of
            Just _ -> Right ()
            Nothing -> Left (maybe answer id (stripPrefix "error " answer))
