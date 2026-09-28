module Main (main) where

import Options.Applicative
import Data.List (intercalate)
import Data.Version (versionBranch)
import Paths_termina (version)
import Command.New
import Command.Build
import Command.Try
import Command.LSP
import Command.Sched

data Command =
    New NewCmdArgs
    | Build BuildCmdArgs
    | Try TryCmdArgs
    | LSP LSPCmdArgs
    | Sched SchedCmdArgs
    deriving Show

newCommandParser :: Parser Command
newCommandParser = New
    <$> newCmdArgsParser

buildCommandParser :: Parser Command
buildCommandParser = Build
    <$> buildCmdArgsParser

schedCommandParser :: Parser Command
schedCommandParser = Sched
    <$> schedCmdArgsParser

tryCommandParser :: Parser Command
tryCommandParser = Try
    <$> tryCmdArgsParser

lspCommandParser :: Parser Command
lspCommandParser = LSP
    <$> lspCmdArgsParser

commandParser :: Parser Command
commandParser = subparser
  ( command "new" (info newCommandParser ( progDesc "Setup a new project" ))
 <> command "build" (info buildCommandParser ( progDesc "Build current project" ))
 <> command "try" (info tryCommandParser ( progDesc "Translate a single file" ))
 <> command "lsp" (info lspCommandParser ( progDesc "Start language server" ))
 <> command "sched" (info schedCommandParser ( progDesc "Generate scheduling models" ))
  )

-- The package version is PVP (X.Y.Z.0); the public one drops the fourth
-- component.
publicVersion :: String
publicVersion = intercalate "." (map show (take 3 (versionBranch version)))

main :: IO ()
main = do
    cmd <- customExecParser (prefs showHelpOnEmpty) $ info (commandParser <**> simpleVersioner ("termina " ++ publicVersion) <**> helper)
        (fullDesc
            <> header "termina: a domain-specific language for real-time critical systems" )
    case cmd of
        New cmdargs -> newCommand cmdargs
        Build cmdargs -> buildCommand cmdargs
        Try cmdargs -> tryCommand cmdargs
        LSP cmdargs -> lspCommand cmdargs
        Sched cmdargs -> schedCommand cmdargs
