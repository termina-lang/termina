-- | The servers of the boards that run on the host, started by whoever runs
-- the tests: the GDB server of each board and the XSDB command server of the
-- boards prepared through XSDB. termina test reaches them, and termina
-- platforms gives their addresses to the extension of VS Code.
module Runner.Servers (
    Server(..), readServer, readGdbPort, gdbPortsParser, checkGdbPorts, gdbHost, xsdbHost, gdbAddress, xsdbAddress
) where

import Configuration.Platform (Platform, checkPlatform, gdbServerPort, xsdbServerPort)

import Data.Char (isDigit)
import Data.List (nub, (\\))
import qualified Options.Applicative as O
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)

-- | The host of a server of a board, empty for the default one, and its port
-- when it is not the one of the platform
data Server = Server String (Maybe Int)
  deriving (Eq, Show)

-- | A host, a host and a port after the last colon, or a port alone after a colon
readServer :: String -> String -> Either String Server
readServer kind text =
    let (port, rest) = break (== ':') (reverse text) in
    case rest of
        ':' : host ->
            if not (null port) && all isDigit port
                then Right (Server (reverse host) (Just (read (reverse port))))
                else Left ("the " ++ kind ++ " " ++ text ++ " is not HOST, HOST:PORT or :PORT")
        _ -> if null text then Left ("the " ++ kind ++ " is empty") else Right (Server text Nothing)

-- | A platform and the port of its GDB server, after the last colon
readGdbPort :: String -> Either String (Platform, Int)
readGdbPort text =
    let (port, rest) = break (== ':') (reverse text) in
    case rest of
        ':' : name | not (null port) && all isDigit port ->
            case checkPlatform (reverse name) of
                Just plt -> Right (plt, read (reverse port))
                Nothing -> Left ("the platform " ++ reverse name ++ " of --gdb-port does not exist")
        _ -> Left ("the GDB port " ++ text ++ " is not PLATFORM:PORT")

-- | The option --gdb-port, once per platform
gdbPortsParser :: O.Parser [(Platform, Int)]
gdbPortsParser = O.many (O.option (O.eitherReader readGdbPort)
    ( O.long "gdb-port"
    <> O.metavar "PLATFORM:PORT"
    <> O.help "The port of the GDB server of the boards of PLATFORM, instead of the one of the platform; once per platform"))

-- | The ports of --gdb-port, each platform at most once
checkGdbPorts :: [(Platform, Int)] -> Either String [(Platform, Int)]
checkGdbPorts ports =
    case nub (platforms \\ nub platforms) of
        [] -> Right ports
        repeated -> Left ("--gdb-port gives more than one port for " ++ unwords (map show repeated))

    where

        platforms = map fst ports

-- | The host of the GDB servers: the one given, the host of the container, or
-- the machine itself
gdbHost :: Maybe Server -> IO String
gdbHost mServer =
    case mServer of
        Just (Server host _) | not (null host) -> return host
        _ -> do
            marked <- lookupEnv "TERMINA_CONTAINER"
            docker <- doesFileExist "/.dockerenv"
            return (if marked /= Nothing || docker then "host.docker.internal" else "localhost")

-- | The host of the XSDB server: the one given, or the one of the GDB servers
xsdbHost :: String -> Maybe Server -> String
xsdbHost host mXsdb =
    case mXsdb of
        Just (Server h _) | not (null h) -> h
        _ -> host

-- | The address of the GDB server of a board: its host, and the port of
-- --gdb-port for its platform, the port given or the one of the platform
gdbAddress :: Platform -> [(Platform, Int)] -> (String, Maybe Int) -> String
gdbAddress plt ports (host, mPort) =
    host ++ ":" ++ maybe "" show (firstJust [lookup plt ports, mPort, gdbServerPort plt])

    where

        firstJust options = case [ p | Just p <- options ] of
            p : _ -> Just p
            [] -> Nothing

-- | The address of the XSDB server of a board prepared through XSDB, and none
-- for the other platforms
xsdbAddress :: Platform -> (String, Maybe Int) -> Maybe String
xsdbAddress plt (host, mPort) = (\port -> host ++ ":" ++ show (maybe port id mPort)) <$> xsdbServerPort plt
