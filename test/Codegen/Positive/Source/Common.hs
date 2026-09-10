module Codegen.Positive.Source.Common where

import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Semantic.Environment
import Text.Parsec
import ControlFlow.BasicBlocks
import Configuration.Configuration
import Generator.Monadic
import Configuration.Platform
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import Generator.CodeGen.Application.Option

import qualified Data.Set as S

renderHeader :: String -> Text
renderHeader input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    let configParams = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just configParams) TestPlatform []) (typeTerminaModule (S.singleton "test") ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenBBModule tast of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenHeaderFile configParams TestPlatform "test" [] bbAST emptyMonadicTypes of
              Left err -> pack $ show err
              Right (cHeaderFile, _) -> runCPrinter False cHeaderFile

renderSource :: String -> Text
renderSource input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    let configParams = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just configParams) TestPlatform []) (typeTerminaModule (S.singleton "test") ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenBBModule tast of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenSourceFile configParams TestPlatform "test" bbAST of
              Left err -> pack $ show err
              Right cSourceFile -> runCPrinter False cSourceFile

renderOption :: MonadicTypes -> Text
renderOption monadicTypes =
  let configParams = defaultConfig "test" TestPlatform  in
  case runGenOptionHeaderFile configParams TestPlatform "test" monadicTypes of
    Left err -> pack $ show err
    Right cOptionsFile -> runCPrinter False cOptionsFile