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
import Command.Doctor
import Command.Platforms
import Command.Clean
import Command.Utils (enterProject)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)

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
    | Doctor DoctorCmdArgs
    | Platforms PlatformsCmdArgs
    | Clean CleanCmdArgs
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

doctorCommandParser :: Parser Command
doctorCommandParser = Doctor
    <$> doctorCmdArgsParser

platformsCommandParser :: Parser Command
platformsCommandParser = Platforms
    <$> platformsCmdArgsParser

cleanCommandParser :: Parser Command
cleanCommandParser = Clean
    <$> cleanCmdArgsParser

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
  ( withHelp "new" newCommandParser "Setup a new project"
 <> withHelp "add" addCommandParser "Add a dependency to the project or to one of its tests"
 <> withHelp "restore" restoreCommandParser "Bring back the copies in vendor/ that do not match termina.lock"
 <> withHelp "update" updateCommandParser "Resolve the tags of termina.yaml again, also those that termina.lock already has"
 <> withHelp "verify" verifyCommandParser "Check without network that vendor/ and termina.lock match termina.yaml"
 <> withHelp "generate" generateCommandParser "Generate the C code and the Makefile of the project"
 <> withHelp "build" buildCommandParser "Generate and compile the project"
 <> withHelp "deliver" deliverCommandParser "Build the self-contained deliverable of the project"
 <> withHelp "test" testCommandParser "Build and run the tests of the project, or list them"
 <> withHelp "try" tryCommandParser "Translate a single file"
 <> withHelp "lsp" lspCommandParser "Start language server"
 <> withHelp "sched" schedCommandParser "Generate scheduling models"
 <> withHelp "doctor" doctorCommandParser "Report the OSAL, the tools and the toolchain of each platform found"
 <> withHelp "platforms" platformsCommandParser "List the supported platforms, and how GDB loads the boards"
 <> withHelp "clean" cleanCommandParser "Remove output/ and .termina/ of the project"
  )

  where

    -- | A command whose own --help lists its options
    withHelp name parser desc = command name (info (parser <**> helper) (progDesc desc))

-- The package version is PVP (X.Y.Z.0); the public one drops the fourth
-- component.
publicVersion :: String
publicVersion = intercalate "." (map show (take 3 (versionBranch version)))

main :: IO ()
main = do
    -- | The messages go in order with the errors and the output of make
    hSetBuffering stdout LineBuffering
    cmd <- customExecParser (prefs showHelpOnEmpty) $ info (commandParser <**> simpleVersioner ("termina " ++ publicVersion) <**> helper)
        (fullDesc
            <> header "termina: a domain-specific language for real-time critical systems" )
    -- | The commands on a project run from its folder, which they look for
    -- from the current one up. The language server looks for it itself, and
    -- keeps running when there is none.
    case cmd of
        New cmdargs -> newCommand cmdargs
        Generate cmdargs -> enterProject >> generateCommand cmdargs
        Build cmdargs -> enterProject >> buildCommand cmdargs
        Deliver cmdargs -> enterProject >> deliverCommand cmdargs
        Test cmdargs -> enterProject >> testCommand cmdargs
        Try cmdargs -> tryCommand cmdargs
        LSP cmdargs -> lspCommand cmdargs
        Sched cmdargs -> enterProject >> schedCommand cmdargs
        Add cmdargs -> enterProject >> addCommand cmdargs
        Restore cmdargs -> enterProject >> restoreCommand cmdargs
        Update cmdargs -> enterProject >> updateCommand cmdargs
        Verify cmdargs -> enterProject >> verifyCommand cmdargs
        Doctor cmdargs -> doctorCommand cmdargs
        Platforms cmdargs -> platformsCommand cmdargs
        Clean cmdargs -> enterProject >> cleanCommand cmdargs
