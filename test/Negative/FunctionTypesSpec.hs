module Negative.FunctionTypesSpec (spec) where

import Test.Hspec
import Semantic.Errors
import Negative.Common

test0 :: String
test0 = "function foo(param0 : box u8) -> u8 {\n" ++
        "\n" ++
        "    let x : u8 = param0 + 1;\n" ++
        "\n" ++
        "    return x;\n" ++
        "\n" ++
        "}"

test1 :: String
test1 = "function foo(param0 : [u8; 10]) -> u8 {\n" ++
        "\n" ++
        "    let x : u8 = param0[0] + 1;\n" ++
        "\n" ++
        "    return x;\n" ++
        "\n" ++
        "}"

test2 :: String
test2 = "function foo(param0 : u8) -> [u8; 10] {\n" ++
        "\n" ++
        "    var x : [u8; 10] = [0; 10];\n" ++
        "\n" ++
        "    x[0] = param0;\n" ++
        "\n" ++
        "    return x;\n" ++
        "\n" ++
        "}"

spec :: Spec
spec = do
  describe "Function definition" $ do
    it "Defining a box parameter" $ do
     runNegativeTest test0
       `shouldSatisfy`
        isEInvalidParameterType
    it "Defining a fixed size array parameter" $ do
      runNegativeTest test1
        `shouldSatisfy`
          isEInvalidParameterType
    it "Defining a fixed size array return type" $ do
      runNegativeTest test2
        `shouldSatisfy`
          isEInvalidReturnType
  
  where
    isEInvalidParameterType :: Maybe Error -> Bool
    isEInvalidParameterType = 
      \case Just (EInvalidParameterType _) -> True; _ -> False
    
    isEInvalidReturnType :: Maybe Error -> Bool
    isEInvalidReturnType = 
      \case Just (EInvalidReturnType _) -> True; _ -> False