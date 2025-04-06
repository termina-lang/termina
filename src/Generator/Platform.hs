
module Generator.Platform where


import Configuration.Configuration
import ControlFlow.Architecture.Types
import Semantic.Types
import qualified Data.Text.IO as TIO

import System.FilePath
import System.Exit
import Command.Utils
import Generator.LanguageC.Printer
import Command.Types
import Configuration.Platform
import Generator.CodeGen.Application.Glue
import Generator.Environment (getPlatformInterruptMap)
import Generator.CodeGen.Application.Makefile
import Generator.Makefile.Printer
import Generator.CodeGen.Application.Config

genPlatformCode :: TerminaConfig -> Platform -> BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
genPlatformCode _ TestPlatform _ _ = return ()
genPlatformCode params plt bbProject progArchitecture = do
  let destinationPath = outputFolder params
      mainFilePath = destinationPath </> "main" <.> "c"
      configFilePath = destinationPath </> "include" </> "config" <.> "h"
  case runGenMainFile params (getPlatformInterruptMap plt) mainFilePath progArchitecture of
    Left err -> die . errorMessage $ show err
    Right cMainFile -> TIO.writeFile mainFilePath $ runCPrinter (profile params == Debug) cMainFile
  case runGenConfigFile params (getPlatformInterruptMap plt) configFilePath progArchitecture of
    Left err -> die . errorMessage $ show err
    Right cConfigFile -> TIO.writeFile configFilePath $ runCPrinter (profile params == Debug) cConfigFile
  case builder params of
    None -> return ()
    Make -> do
      let makeFilePath = destinationPath </> "Makefile"
          makefile = genMakefile params bbProject 
      TIO.writeFile makeFilePath $ runMakefilePrinter makefile