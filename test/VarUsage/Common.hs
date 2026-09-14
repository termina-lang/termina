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
import qualified ControlFlow.BoxUsage.Errors as BoxUsage
import qualified ControlFlow.VarUsage.Errors as VarUsage
import ControlFlow.BoxUsage (runBoxUsageCheck)
import ControlFlow.VarUsage (runVarUsageCheck)
import qualified Data.Set as S

-- | Parses, type-checks and lowers a single module named @test@ to basic
-- blocks, then runs the box usage (move/borrow) check, returning the error it
-- is expected to raise (or 'Nothing' if usage is well-formed).
runNegativeTestBoxUsage :: String -> Maybe BoxUsage.Error
runNegativeTestBoxUsage input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast ->
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) TestPlatform []) (typeTerminaModule (S.singleton "test") ast) of
      Left err -> error $ "Typing Error: " ++ show err
      Right (typedProgram, _) -> case runGenBBModule typedProgram of
        Left err -> error $ "Basic Blocks Generator Error: " ++ show err
        Right bbProgram -> case runBoxUsageCheck bbProgram of
          Just err -> Just $ getError err
          Nothing -> Nothing

-- | The BE-NNN code of the box usage error a program raises (or 'Nothing' if
-- usage is well-formed). The code spec asserts this; the detail spec asserts
-- the error constructor itself.
boxUsageErrorCode :: String -> Maybe Text
boxUsageErrorCode = fmap (errorIdent . annotateError Internal) . runNegativeTestBoxUsage

-- | Same pipeline as 'runNegativeTestBoxUsage', but running the variable usage
-- check, which owns definite assignment, dead stores and the objects nobody
-- reads.
runNegativeTestVarUsage :: String -> Maybe VarUsage.Error
runNegativeTestVarUsage input = case runP (contents topLevel) "test" "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast ->
    let config = defaultConfig "test" TestPlatform in
    case runTypeChecking (makeInitialGlobalEnv (Just config) TestPlatform []) (typeTerminaModule (S.singleton "test") ast) of
      Left err -> error $ "Typing Error: " ++ show err
      Right (typedProgram, _) -> case runGenBBModule typedProgram of
        Left err -> error $ "Basic Blocks Generator Error: " ++ show err
        Right bbProgram -> case runVarUsageCheck bbProgram of
          Just err -> Just $ getError err
          Nothing -> Nothing

-- | The VE-NNN code of the variable usage error a program raises.
varUsageErrorCode :: String -> Maybe Text
varUsageErrorCode = fmap (errorIdent . annotateError Internal) . runNegativeTestVarUsage
