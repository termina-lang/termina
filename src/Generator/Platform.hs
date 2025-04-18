{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Exception
import System.IO.Error
import Control.Monad (guard)

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
    Right cConfigFile -> do
      -- If the config file already exists, we need to load it first to check if
      -- it is identical to the one we are generating. If it is not identical, we
      -- need to overwrite it. Otherwise, we can skip the generation.
      -- We first try to load the config file, and if it fails, we just generate it.
      previousConfig <- tryJust (guard . isDoesNotExistError) $ TIO.readFile configFilePath
      case previousConfig of
        Left _ -> TIO.writeFile configFilePath $ runCPrinter (profile params == Debug) cConfigFile
        Right previousContent -> do
          let newContent = runCPrinter (profile params == Debug) cConfigFile
          -- | Here we compare the two contents. If they are the same, we skip the
          -- | generation. If they are different, we overwrite the file.
          -- | We use the `==` operator to compare the two contents, which is not
          -- | the most efficient way to do it, but it is simple and works for our
          -- | purposes. In the future, we might want to use a more efficient way
          -- | to compare the two contents, such as hashing or using a diff library.
          -- | Since the config file is not expected to be very large, this should
          -- | not be a problem for now.
          if previousContent == newContent
            then return ()
            else do
              TIO.writeFile configFilePath newContent
  case builder params of
    None -> return ()
    Make -> do
      let makeFilePath = destinationPath </> "Makefile"
          makefile = genMakefile params bbProject 
      TIO.writeFile makeFilePath $ runMakefilePrinter makefile