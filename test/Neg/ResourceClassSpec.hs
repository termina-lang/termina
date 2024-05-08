module Neg.ResourceClassSpec (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Errors
import AST.Seman

runNegativeTest :: String -> Maybe (Errors Annotation)
runNegativeTest input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> Just $ semError err
      Right _ -> Nothing

test0 :: String
test0 = "resource class id0 {\n" ++
        "    procedure assignment_test1(&priv self, dyn_var0 : dyn u32) {\n" ++
        "        var opt : Option<dyn u32> = None;\n" ++
        "        opt = Some(dyn_var0);\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

spec :: Spec
spec = do
  describe "Resource class definition" $ do
    it "Resource class without provided interfaces" $ do
     runNegativeTest test0
       `shouldSatisfy`
        isEResourceClassNoProvides
  
  where
    isEResourceClassNoProvides :: Maybe (Errors Annotation) -> Bool
    isEResourceClassNoProvides = \case Just (EResourceClassNoProvides "id0") -> True; _ -> False