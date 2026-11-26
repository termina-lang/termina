-- | A module that defines nothing gets no source file.
module Codegen.Positive.EmptySourceSpec (spec) where

import Pipeline.Common (sourceDefinesNothing)

import Test.Hspec

declarations :: String
declarations = unlines
  [ "struct Point {"
  , "    x : u32;"
  , "};"
  , "interface IReader {"
  , "    procedure read(&mut self, p : &mut Point);"
  , "};"
  , "constexpr origin : Point = { x = 0 };" ]

definitions :: String
definitions = declarations ++ unlines
  [ "function first(p : Point) -> u32 {"
  , "    return p.x;"
  , "}" ]

spec :: Spec
spec = describe "Source file of a module" $ do
  it "holds nothing but includes for types, interfaces and constants" $
    sourceDefinesNothing "test" [("test", declarations)] `shouldBe` Right True
  it "holds the definitions of a module with a function" $
    sourceDefinesNothing "test" [("test", definitions)] `shouldBe` Right False
