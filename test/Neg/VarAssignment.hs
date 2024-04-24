module Neg.VarAssignment (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Errors
import AST.Parser

runTerminaParser :: String -> Maybe (AnnotatedProgram Annotation)
runTerminaParser = either (const Nothing) Just . parse (contents topLevel) ""

test0 :: String
test0 = "function f (n : u8, x : u8) -> u8 {\n" ++
        "    let ntimes : u8 = n;\n" ++
        "    var y : u8 = 0 : u8;\n" ++
        "\n" ++
        "    for i : u8 in 0 : u8 .. ntimes {\n" ++
        "        x = x * 2 : u8;\n" ++
        "        i = i + 2 : u8;\n" ++
        "        y = i + x;\n" ++
        "    }\n" ++
        "\n" ++
        "    return y;\n" ++
        "\n" ++
        "}"

spec :: Spec
spec = do
  describe "Variable mutability" $ do
    it "Assignment to loop iterator" $
     ((either (Just . semError) (const Nothing) . typeCheckRun) =<< runTerminaParser test0)
       `shouldBe`
        Just (ENotNamedObject "foo")
