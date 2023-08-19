module IT.Expression.ArithmeticSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common
import Data.Map
import Parsing
import Semantic.TypeChecking
import Text.Parsec

test0 :: String
test0 = "fn test() {\n" ++
        "    var foo : u16 = 0 : u16;\n" ++
        "    return;\n" ++
        "}"

renderInput :: String -> Text
renderInput input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> pack $ "Type error: " ++ show err
      Right tast -> ppHeaderFile tast
    
{-- case typeCheckRun ast of
    Left err -> error $ "Type Error: " ++ show err
    Right tast -> ppHeaderFile tast --}

spec :: Spec
spec = do
  describe "Pretty printing arithmetic expressions" $ do
    it "Prints function test0" $ do
      renderInput test0 `shouldBe`
        pack "test0"
