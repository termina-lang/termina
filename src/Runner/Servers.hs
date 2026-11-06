-- | The servers of the boards that run on the host, started by whoever runs
-- the tests: the GDB server of each board and the XSDB command server of the
-- boards prepared through XSDB. termina test reaches them, and termina
-- platforms gives their addresses to the extension of VS Code.
module Runner.Servers (
    Server(..), readServer, gdbHost, gdbAddress, xsdbAddress
) where

import Configuration.Platform (Platform, gdbServerPort, xsdbServerPort)

import Data.Char (isDigit)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

-- | The host of a server of a board, and its port when it is not the one of the platform
data Server = Server String (Maybe Int)
  deriving (Eq, Show)

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

-- | The host of the GDB servers: the one given, the host of the container, or
-- the machine itself
gdbHost :: Maybe Server -> IO String
gdbHost mServer =
    case mServer of
        Just (Server host _) -> return host
        Nothing -> do
            marked <- lookupEnv "TERMINA_CONTAINER"
            docker <- doesFileExist "/.dockerenv"
            return (if marked /= Nothing || docker then "host.docker.internal" else "localhost")

-- | The address of the GDB server of a board: its host, and the port given or
-- the one of the platform
gdbAddress :: Platform -> (String, Maybe Int) -> String
gdbAddress plt (host, mPort) = host ++ ":" ++ maybe "" show (maybe (gdbServerPort plt) Just mPort)

-- | The address of the XSDB server of a board prepared through XSDB
xsdbAddress :: Platform -> (String, Maybe Int) -> Maybe String
xsdbAddress plt (host, mPort) = (\port -> host ++ ":" ++ show port) <$> maybe (xsdbServerPort plt) Just mPort
