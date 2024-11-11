
module Generator.Platform where


import Command.Configuration
import ControlFlow.Architecture.Types
import Semantic.Types
import qualified Data.Text.IO as TIO

import System.FilePath
import Generator.CodeGen.Application.Platform.RTEMS5NoelSpike.Glue
    ( runGenMainFile )
import System.Exit
import Command.Utils
import Generator.LanguageC.Printer
import Generator.Platform.Configuration
import Generator.Platform.RTEMS5NoelSpike
import Generator.Makefile.Printer
import Command.Types
import Generator.CodeGen.Application.Platform.RTEMS5NoelSpike.Makefile

genPlatformCode :: Platform -> TerminaConfig -> BasicBlocksProject -> TerminaProgArch SemanticAnn -> IO ()
genPlatformCode RTEMS5NoelSpike params bbProject progArchitecture = do
  let destinationPath = outputFolder params
      mainFilePath = destinationPath </> "main" <.> "c"
  case runGenMainFile params mainFilePath progArchitecture of
    Left err -> die . errorMessage $ show err
    Right cMainFile -> TIO.writeFile mainFilePath $ runCPrinter (profile params == Debug) cMainFile
  case builder . rtems5_noel_spike . platformFlags $ params of 
    None -> return ()
    Make -> do
      let makeFilePath = destinationPath </> "Makefile"
          makefile = genMakefile params bbProject progArchitecture
      TIO.writeFile makeFilePath $ runMakefilePrinter makefile
genPlatformCode TestPlatform _ _ _ = return ()