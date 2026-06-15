-- | Positive parser tests for class members, covering the parameter-count and
-- return-value variations each member kind grammatically allows:
--
--   * method / viewer : 0/1/many params, with and without return type
--   * procedure / interface procedure : 0/1/many params, never a return type
--   * action : 0 or 1 param (single, optional); the parser currently requires a
--     return type (see TODO: actions should be allowed to return nothing)
module Parser.Positive.ClassMembersSpec (spec) where

import Parser.Common (parseWith, ctorName)
import Parser.Parsing
  ( classFieldDefinitionParser, classMethodParser, classProcedureParser
  , classActionParser, classViewerParser, interfaceProcedureParser )

import Test.Hspec

spec :: Spec
spec = describe "Parser: class members" $ do

  it "parses a field" $ ctorOf classFieldDefinitionParser "x : u32;" `shouldBe` "ClassField"

  describe "methods" $ table classMethodParser "ClassMethod"
    [ ("no params, no return",        "method m(&self) { return; }")
    , ("no params, with return",      "method m(&self) -> u32 { return 0 : u32; }")
    , ("one param, with return",      "method m(&self, x : u32) -> u32 { return x; }")
    , ("many params, with return",    "method m(&self, x : u32, y : u32) -> u32 { return x; }")
    , ("many params, no return",      "method m(&mut self, x : u32, y : u32) { return; }")
    ]

  describe "procedures (never a return type)" $ table classProcedureParser "ClassProcedure"
    [ ("no params",   "procedure p(&mut self) { return; }")
    , ("one param",   "procedure p(&mut self, x : u32) { return; }")
    , ("many params", "procedure p(&mut self, x : u32, y : u32) { return; }")
    ]

  describe "interface procedures" $ table interfaceProcedureParser "InterfaceProcedure"
    [ ("no params",   "procedure p(&mut self);")
    , ("one param",   "procedure p(&mut self, x : u32);")
    , ("many params", "procedure p(&mut self, x : u32, y : u32);")
    ]

  describe "actions (0 or 1 param; return type currently required)" $ table classActionParser "ClassAction"
    [ ("no param",  "action a(&priv self) -> u32 { return 0 : u32; }")
    , ("one param", "action a(&priv self, x : u32) -> u32 { return x; }")
    ]

  describe "viewers" $ table classViewerParser "ClassViewer"
    [ ("no params, no return",     "viewer v(&self) { return; }")
    , ("no params, with return",   "viewer v(&self) -> u32 { return 0 : u32; }")
    , ("one param, with return",   "viewer v(&self, x : u32) -> bool { return x == 0 : u32; }")
    , ("many params, with return", "viewer v(&self, x : u32, y : u32) -> bool { return x == y; }")
    ]

  where
    ctorOf p s = either show ctorName (parseWith p s)
    table p expected =
      mapM_ (\(name, src) -> it ("parses a " ++ name) $ ctorOf p src `shouldBe` expected)
