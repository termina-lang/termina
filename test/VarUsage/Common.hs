module VarUsage.Common where

import Text.Parsec
import Parser.Parsing
import Data.Text (Text)

import Semantic.TypeChecking
import Semantic.Environment
import Utils.Annotations
import Utils.Errors (ErrorMessage(errorIdent))
import Configuration.Platform
import Configuration.Configuration
import ControlFlow.BasicBlocks
import qualified ControlFlow.VarUsage.Errors as VarUsage
import ControlFlow.VarUsage
import qualified Data.Set as S

-- | Parses, type-checks and lowers a single module named @test@ to basic
-- blocks, then runs the variable-usage (move/borrow) check, returning the
-- error it is expected to raise (or 'Nothing' if usage is well-formed).
runNegativeTestVarUsage :: String -> Maybe VarUsage.Error
runNegativeTestVarUsage input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast ->
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) []) (typeTerminaModule (S.singleton "test") ast) of
      Left err -> error $ "Typing Error: " ++ show err
      Right (typedProgram, _) -> case runGenBBModule typedProgram of
        Left err -> error $ "Basic Blocks Generator Error: " ++ show err
        Right bbProgram -> case runUDAnnotatedProgram bbProgram of
          Just err -> Just $ getError err
          Nothing -> Nothing

-- | The VE-NNN code of the variable-usage error a program raises (or 'Nothing'
-- if usage is well-formed). The code spec asserts this; the detail spec asserts
-- the error constructor itself.
varUsageErrorCode :: String -> Maybe Text
varUsageErrorCode = fmap (errorIdent . annotateError Internal) . runNegativeTestVarUsage
