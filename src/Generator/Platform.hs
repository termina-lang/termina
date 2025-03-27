
module Generator.Platform where


import Configuration.Configuration
import ControlFlow.Architecture.Types
import Semantic.Types
import qualified Data.Text.IO as TIO

import System.FilePath
import System.Exit
import Command.Utils
import Generator.LanguageC.Printer
import Generator.Makefile.Printer
import Command.Types
import Configuration.Platform
import Configuration.Platform.RTEMS5NoelSpike as RTEMS5NoelSpike.Config
import Configuration.Platform.RTEMS5LEON3TSIM as RTEMS5LEON3TSIM.Config
import Generator.CodeGen.Application.Glue
import Generator.Environment (getPlatformInterruptMap)

genPlatformCode :: TerminaConfig -> Platform -> BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
genPlatformCode params RTEMS5NoelSpike bbProject progArchitecture = do
  let destinationPath = outputFolder params
      mainFilePath = destinationPath </> "main" <.> "c"
  case runGenMainFile params (getPlatformInterruptMap RTEMS5NoelSpike) mainFilePath progArchitecture of
    Left err -> die . errorMessage $ show err
    Right cMainFile -> TIO.writeFile mainFilePath $ runCPrinter (profile params == Debug) cMainFile
{--  case RTEMS5NoelSpike.Config.builder . rtems5_noel_spike . platformFlags $ params of 
    RTEMS5NoelSpike.Config.None -> return ()
    RTEMS5NoelSpike.Config.Make -> do
      let makeFilePath = destinationPath </> "Makefile"
          makefile = RTEMS5NoelSpike.Makefile.genMakefile params bbProject progArchitecture
      TIO.writeFile makeFilePath $ runMakefilePrinter makefile --}
genPlatformCode params RTEMS5LEON3TSIM bbProject progArchitecture = do
  let destinationPath = outputFolder params
      mainFilePath = destinationPath </> "main" <.> "c"
  case runGenMainFile params (getPlatformInterruptMap RTEMS5LEON3TSIM) mainFilePath progArchitecture of
    Left err -> die . errorMessage $ show err
    Right cMainFile -> TIO.writeFile mainFilePath $ runCPrinter (profile params == Debug) cMainFile
{--  case RTEMS5LEON3TSIM.Config.builder . rtems5_leon3_tsim . platformFlags $ params of 
    RTEMS5LEON3TSIM.Config.None -> return ()
    RTEMS5LEON3TSIM.Config.Make -> do
      let makeFilePath = destinationPath </> "Makefile"
          makefile = RTEMS5LEON3TSIM.Makefile.genMakefile params bbProject progArchitecture
      TIO.writeFile makeFilePath $ runMakefilePrinter makefile --}
genPlatformCode _ TestPlatform _ _ = return ()