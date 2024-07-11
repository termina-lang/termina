module Main (main) where

import Options.Applicative
import Command.New
import Command.Build

data Command =
    New NewCmdArgs
    | Build BuildCmdArgs
    deriving (Show,Eq)

newCommandParser :: Parser Command
newCommandParser = New
    <$> newCmdArgsParser

buildCommandParser :: Parser Command
buildCommandParser = Build
    <$> buildCmdArgsParser

commandParser :: Parser Command
commandParser = subparser
  ( command "new" (info newCommandParser ( progDesc "Setup a new project" ))
 <> command "build" (info buildCommandParser ( progDesc "Build current project" ))
  )

main :: IO ()
main = do
    cmd <- customExecParser (prefs showHelpOnEmpty) $ info (commandParser <**> helper)
        (fullDesc
            <> header "termina: a domain-specific language for real-time critical systems" )
    case cmd of
        New cmdargs -> newCommand cmdargs
        Build cmdargs -> buildCommand cmdargs
