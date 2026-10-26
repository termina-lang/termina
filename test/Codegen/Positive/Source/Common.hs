module Codegen.Positive.Source.Common where

import Elaboration (provers, elaborateProgram)
import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Semantic.Environment
import Text.Parsec
import Lowering
import Configuration.Configuration
import Generator.Monadic
import Configuration.Platform
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import Generator.CodeGen.Application.Option

import qualified Data.Set as S

renderHeader :: String -> Text
renderHeader = renderHeaderWith (defaultConfig "test" TestPlatform)

-- | 'renderHeader' type-checking the module as a test build, where test
-- classes are allowed.
renderTestHeader :: String -> Text
renderTestHeader = renderHeaderWith testBuildConfig

renderSource :: String -> Text
renderSource = renderSourceWith (defaultConfig "test" TestPlatform)

-- | 'renderSource' type-checking the module as a test build.
renderTestSource :: String -> Text
renderTestSource = renderSourceWith testBuildConfig

testBuildConfig :: TerminaConfig
testBuildConfig = (defaultConfig "test" TestPlatform) { deploymentKind = TestBuild (pack "test") }

renderHeaderWith :: TerminaConfig -> String -> Text
renderHeaderWith configParams input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast ->
    case runTypeChecking (makeInitialGlobalEnv (Just configParams) TestPlatform []) (typeTerminaModule (S.singleton "test") ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runLowerModule tast of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenHeaderFile configParams TestPlatform "test" [] (fst (elaborateProgram provers bbAST)) emptyMonadicTypes of
              Left err -> pack $ show err
              Right (cHeaderFile, _) -> runCPrinter False cHeaderFile

renderSourceWith :: TerminaConfig -> String -> Text
renderSourceWith configParams input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast ->
    case runTypeChecking (makeInitialGlobalEnv (Just configParams) TestPlatform []) (typeTerminaModule (S.singleton "test") ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runLowerModule tast of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenSourceFile configParams TestPlatform "test" (fst (elaborateProgram provers bbAST)) of
              Left err -> pack $ show err
              Right cSourceFile -> runCPrinter False cSourceFile

renderOption :: MonadicTypes -> Text
renderOption monadicTypes =
  let configParams = defaultConfig "test" TestPlatform  in
  case runGenOptionHeaderFile configParams TestPlatform "test" monadicTypes of
    Left err -> pack $ show err
    Right cOptionsFile -> runCPrinter False cOptionsFile