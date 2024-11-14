module Negative.FunctionTypesSpec (spec) where

import Test.Hspec
import Semantic.AST
import Semantic.Errors.Errors
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
        isEInvalidParameterType "param0" (TBoxSubtype TUInt8)
    it "Defining a fixed size array parameter" $ do
      runNegativeTest test1
        `shouldSatisfy`
          isEInvalidParameterType "param0" (TArray TUInt8 (K (TInteger 10 DecRepr)))
    it "Defining a fixed size array return type" $ do
      runNegativeTest test2
        `shouldSatisfy`
          isEInvalidReturnType (TArray TUInt8 (K (TInteger 10 DecRepr)))
  
  where
    isEInvalidParameterType :: Identifier -> TerminaType -> Maybe Error -> Bool
    isEInvalidParameterType ident ts = 
      \case Just (EInvalidParameterType (Parameter ident' ts')) -> ident == ident' && ts == ts'; _ -> False
    
    isEInvalidReturnType :: TerminaType -> Maybe Error -> Bool
    isEInvalidReturnType ts = 
      \case Just (EInvalidReturnType ts') -> ts == ts'; _ -> False