module Main (main) where

import Options.Applicative
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

main :: IO ()
main = do
    cmd <- customExecParser (prefs showHelpOnEmpty) $ info (commandParser <**> helper)
        (fullDesc
            <> header "termina: a domain-specific language for real-time critical systems" )
    case cmd of
        New cmdargs -> newCommand cmdargs
        Build cmdargs -> buildCommand cmdargs
        Try cmdargs -> tryCommand cmdargs
        LSP cmdargs -> lspCommand cmdargs
        Sched cmdargs -> schedCommand cmdargs
