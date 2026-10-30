{-# LANGUAGE ScopedTypeVariables #-}

module Generator.CodeGen.Platform where


import Configuration.Configuration
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils (isUnitTest)
import Semantic.Types

import System.FilePath
import System.Exit
import Command.Utils
import Generator.LanguageC.Printer
import Command.Types
import Configuration.Platform
import Generator.CodeGen.Application.Glue
import Generator.CodeGen.Application.Makefile
import Command.Extern (ExternBuild)
import Generator.Makefile.Printer
import Generator.CodeGen.Application.Config
import Generator.TestDescriptor (genTestDescriptor)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE

genPlatformCode :: TerminaConfig -> Platform -> LoweredProject -> TerminaProgArch SemanticAnn -> Maybe ExternBuild -> IO ()
genPlatformCode _ TestPlatform _ _ _ = return ()
genPlatformCode params plt loweredProject progArchitecture mExtern = do
  stamp <- if isTestBuild params
    then case genTestDescriptor params loweredProject progArchitecture of
      Left err -> die . errorMessage $ err
      Right (descriptor, stamp) -> do
        writeIfChanged (destinationPath </> "test" <.> "json") (TE.decodeUtf8 (BL.toStrict descriptor))
        return stamp
    else return 0

  case runGenMainFile params plt mainFile stamp progArchitecture of
    Left err -> die . errorMessage $ show err
    Right cMainFile -> writeIfChanged mainFile $ runCPrinter (lineDirectives params) cMainFile

  case runGenConfigFile params plt configFile progArchitecture of
    Left err -> die . errorMessage $ show err
    Right cConfigFile -> writeIfChanged configFile $ runCPrinter (lineDirectives params) cConfigFile

  writeIfChanged (destinationPath </> "Makefile") $
    runMakefilePrinter (genMakefile params loweredProject mExtern (isUnitTest progArchitecture))

  where

    destinationPath = outputFolder params
    mainFile = destinationPath </> "main" <.> "c"
    configFile = destinationPath </> "include" </> genConfigPathName
