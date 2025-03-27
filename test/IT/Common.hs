module IT.Common where

import Data.Text hiding (empty)
import Parser.Parsing
import Semantic.TypeChecking
import Semantic.Environment
import Text.Parsec
import qualified Data.Map as M
import ControlFlow.BasicBlocks
import Configuration.Configuration
import Generator.Option
import Configuration.Platform
import Generator.CodeGen.Module
import Generator.LanguageC.Printer
import Generator.CodeGen.Application.Option
import Generator.Environment

renderHeader :: Bool -> String -> Text
renderHeader includeOptionH input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    let configParams = defaultConfig "test" TestPlatform
        irqMap = getPlatformInterruptMap TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just configParams) []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenBBModule tast of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenHeaderFile configParams irqMap includeOptionH "test" [] bbAST M.empty of
              Left err -> pack $ show err
              Right cHeaderFile -> runCPrinter False cHeaderFile

renderSource :: String -> Text
renderSource input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    let configParams = defaultConfig "test" TestPlatform
        irqMap = getPlatformInterruptMap TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just configParams) []) (typeTerminaModule ast) of
      Left err -> pack $ "Type error: " ++ show err
      Right (tast, _) -> 
        case runGenBBModule tast of
          Left err -> pack $ "Basic blocks error: " ++ show err
          Right bbAST -> 
            case runGenSourceFile configParams irqMap "test" bbAST of
              Left err -> pack $ show err
              Right cSourceFile -> runCPrinter False cSourceFile

renderOption :: OptionMap -> Text
renderOption optionMap =
  let configParams = defaultConfig "test" TestPlatform 
      irqMap = getPlatformInterruptMap TestPlatform in
  case runGenOptionHeaderFile configParams irqMap optionMap of
    Left err -> pack $ show err
    Right cOptionsFile -> runCPrinter False cOptionsFile