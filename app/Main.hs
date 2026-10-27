module Main (main) where

import Options.Applicative
import Data.List (intercalate)
import Data.Version (versionBranch)
import Paths_termina (version)
import Command.New
import Command.Add
import Command.Restore
import Command.Update
import Command.Build
import Command.Deliver
import Command.Test
import Command.Try
import Command.LSP
import Command.Sched

data Command =
    New NewCmdArgs
    | Generate BuildCmdArgs
    | Build BuildCmdArgs
    | Deliver DeliverCmdArgs
    | Test TestCmdArgs
    | Try TryCmdArgs
    | LSP LSPCmdArgs
    | Sched SchedCmdArgs
    | Add AddCmdArgs
    | Restore RestoreCmdArgs
    | Update UpdateCmdArgs
    | Verify VerifyCmdArgs
    deriving Show

newCommandParser :: Parser Command
newCommandParser = New
    <$> newCmdArgsParser

addCommandParser :: Parser Command
addCommandParser = Add
    <$> addCmdArgsParser

restoreCommandParser :: Parser Command
restoreCommandParser = Restore
    <$> restoreCmdArgsParser

updateCommandParser :: Parser Command
updateCommandParser = Update
    <$> updateCmdArgsParser

verifyCommandParser :: Parser Command
verifyCommandParser = Verify
    <$> verifyCmdArgsParser

generateCommandParser :: Parser Command
generateCommandParser = Generate
    <$> buildCmdArgsParser

buildCommandParser :: Parser Command
buildCommandParser = Build
    <$> buildCmdArgsParser

deliverCommandParser :: Parser Command
deliverCommandParser = Deliver
    <$> deliverCmdArgsParser

testCommandParser :: Parser Command
testCommandParser = Test
    <$> testCmdArgsParser

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
 <> command "add" (info addCommandParser ( progDesc "Add a dependency to the project or to one of its tests" ))
 <> command "restore" (info restoreCommandParser ( progDesc "Bring back the copies in vendor/ that do not match termina.lock" ))
 <> command "update" (info updateCommandParser ( progDesc "Resolve the tags of termina.yaml again, also those that termina.lock already has" ))
 <> command "verify" (info verifyCommandParser ( progDesc "Check without network that vendor/ and termina.lock match termina.yaml" ))
 <> command "generate" (info generateCommandParser ( progDesc "Generate the C code and the Makefile of the project" ))
 <> command "build" (info buildCommandParser ( progDesc "Generate and compile the project" ))
 <> command "deliver" (info deliverCommandParser ( progDesc "Build the self-contained deliverable of the project" ))
 <> command "test" (info testCommandParser ( progDesc "List the tests of the project" ))
 <> command "try"(info tryCommandParser ( progDesc "Translate a single file" ))
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
        Generate cmdargs -> generateCommand cmdargs
        Build cmdargs -> buildCommand cmdargs
        Deliver cmdargs -> deliverCommand cmdargs
        Test cmdargs -> testCommand cmdargs
        Try cmdargs -> tryCommand cmdargs
        LSP cmdargs -> lspCommand cmdargs
        Sched cmdargs -> schedCommand cmdargs
        Add cmdargs -> addCommand cmdargs
        Restore cmdargs -> restoreCommand cmdargs
        Update cmdargs -> updateCommand cmdargs
        Verify cmdargs -> verifyCommand cmdargs
