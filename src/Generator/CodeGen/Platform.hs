{-# LANGUAGE ScopedTypeVariables #-}

module Generator.CodeGen.Platform where


import Configuration.Configuration
import ControlFlow.Architecture.Types
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

genPlatformCode :: TerminaConfig -> Platform -> LoweredProject -> TerminaProgArch SemanticAnn -> Maybe ExternBuild -> IO ()
genPlatformCode _ TestPlatform _ _ _ = return ()
genPlatformCode params plt loweredProject progArchitecture mExtern = do
  case runGenMainFile params plt mainFile progArchitecture of
    Left err -> die . errorMessage $ show err
    Right cMainFile -> writeIfChanged mainFile $ runCPrinter (lineDirectives params) cMainFile

  case runGenConfigFile params plt configFile progArchitecture of
    Left err -> die . errorMessage $ show err
    Right cConfigFile -> writeIfChanged configFile $ runCPrinter (lineDirectives params) cConfigFile

  writeIfChanged (destinationPath </> "Makefile") $
    runMakefilePrinter (genMakefile params loweredProject mExtern)

  where

    destinationPath = outputFolder params
    mainFile = destinationPath </> "main" <.> "c"
    configFile = destinationPath </> "include" </> "config" <.> "h"
