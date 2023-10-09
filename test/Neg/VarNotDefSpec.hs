module Neg.VarNotDefSpec (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import qualified Semantic.TypeChecking as TypeChecking
import Semantic.Errors as SemanErrors

import Control.Monad

runTerminaParser = either (const Nothing) Just . parse (contents topLevel) ""

test0 :: String
test0 = "fn test0() {\n" ++
        "    foo = foo + 1024 : u16;\n" ++
        "    return;\n" ++
        "}"

spec :: Spec
spec = do
  describe "Error Neg Test" $ do
    it "Undeclared variable" $
     join (fmap ( either (Just . semError) (const Nothing) . TypeChecking.typeCheckRun) (runTerminaParser  test0))
       `shouldBe`
        Just (SemanErrors.ENotNamedVar "foo")
