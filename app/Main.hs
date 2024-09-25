module Main (main) where

import Options.Applicative
import Command.New
import Command.Build
import Command.Try

data Command =
    New NewCmdArgs
    | Build BuildCmdArgs
    | Try TryCmdArgs
    deriving (Show,Eq)

newCommandParser :: Parser Command
newCommandParser = New
    <$> newCmdArgsParser

buildCommandParser :: Parser Command
buildCommandParser = Build
    <$> buildCmdArgsParser

tryCommandParser :: Parser Command
tryCommandParser = Try
    <$> tryCmdArgsParser

commandParser :: Parser Command
commandParser = subparser
  ( command "new" (info newCommandParser ( progDesc "Setup a new project" ))
 <> command "build" (info buildCommandParser ( progDesc "Build current project" ))
 <> command "try" (info tryCommandParser ( progDesc "Translate a single file" ))
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
