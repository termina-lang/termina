{-# LANGUAGE OverloadedStrings #-}

-- | termina platforms: the supported platforms and, for the boards, how GDB
-- loads a binary into them, which the extension of VS Code reads to create a
-- project and to start a debugging session.
module Command.Platforms (
    platformsCmdArgsParser, platformsCommand, PlatformsCmdArgs
) where

import Configuration.Platform
import Runner.Servers (Server(..), readServer, gdbHost, gdbAddress, xsdbAddress)

import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (catMaybes)
import qualified Options.Applicative as O

-- | Data type for the "platforms" command arguments
data PlatformsCmdArgs =
    PlatformsCmdArgs
        Bool -- ^ Write the list in JSON
        (Maybe String) -- ^ The host of the GDB servers
        (Maybe Server) -- ^ The XSDB server
    deriving Show

platformsCmdArgsParser :: O.Parser PlatformsCmdArgs
platformsCmdArgsParser = PlatformsCmdArgs
    <$> O.switch (O.long "json"
        <> O.help "Write the list in JSON, with the addresses of the servers of the boards and the commands that load them")
    <*> O.optional (O.strOption
          ( O.long "gdb"
          <> O.metavar "HOST"
          <> O.help "The host of the GDB servers of the boards (default: localhost, or host.docker.internal in the container)"))
    <*> O.optional (O.option (O.eitherReader (readServer "XSDB server"))
          ( O.long "xsdb"
          <> O.metavar "HOST[:PORT]"
          <> O.help "The XSDB command server that prepares the boards prepared through XSDB (default: the host of the GDB servers, at the port of the platform)"))

platformsCommand :: PlatformsCmdArgs -> IO ()
platformsCommand (PlatformsCmdArgs json mGdb mXsdb) = do
    host <- gdbHost (flip Server Nothing <$> mGdb)
    let xsdb = maybe (host, Nothing) (\(Server h port) -> (h, port)) mXsdb
    if json
        then BLC.putStrLn $ encodePretty' defConfig { confCompare = keyOrder keys }
            [ entry host xsdb plt desc | (plt, desc) <- supportedPlatforms ]
        else forM_ supportedPlatforms $ \(plt, desc) -> putStrLn (show plt ++ ": " ++ desc)

  where

    keys = ["name", "description", "gdb", "xsdb", "address", "launch"]

    entry :: String -> (String, Maybe Int) -> Platform -> String -> Value
    entry host xsdb plt desc = object (catMaybes [
        Just ("name" .= show plt),
        Just ("description" .= desc),
        (\commands -> "gdb" .= object [
            "address" .= gdbAddress plt (host, Nothing),
            "launch" .= commands ]) <$> gdbLaunch plt,
        (\address -> "xsdb" .= object [ "address" .= address ]) <$> xsdbAddress plt xsdb
      ])
