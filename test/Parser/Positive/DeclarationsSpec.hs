-- | Positive parser tests for top-level declarations: functions (with their
-- parameter-count and return-value variations), constants, type definitions and
-- application-level instances (task/resource/handler/channel/emitter).
module Parser.Positive.DeclarationsSpec (spec) where

import Parser.Common (parseWith, ctorName)
import Parser.Parsing
  ( functionParser, constDeclParser, constExprDeclParser
  , taskDeclParser, resourceDeclParser, handlerDeclParser
  , channelDeclParser, emitterDeclParser, globalDeclParser
  , structDefinitionParser, enumDefinitionParser
  , interfaceDefinitionParser, classDefinitionParser )
import Parser.AST (AnnASTElement'(TypeDefinition))

import Test.Hspec

spec :: Spec
spec = describe "Parser: top-level declarations" $ do

  describe "functions (0/1/many params, with and without return type)" $
    mapM_ (\(name, src) -> it ("parses a function with " ++ name) $
              ctorOf functionParser src `shouldBe` "Function")
      [ ("no params, no return",     "function f() { return; }")
      , ("no params, with return",   "function f() -> u32 { return 0 : u32; }")
      , ("one param, with return",   "function f(x : u32) -> u32 { return x; }")
      , ("many params, with return", "function f(x : u32, y : u32) -> u32 { return x; }")
      , ("many params, no return",   "function f(x : u32, y : u32) { return; }")
      ]

  -- NB: each row stores the already-parsed constructor name (a String), not the
  -- parser, so a single list can mix parsers of different result types (Global
  -- vs AnnASTElement).
  describe "constants" $
    rows
      [ ("const", ctorOf constDeclParser "const X : u32 = 1 : u32;", "Const")
      , ("constexpr", ctorOf constExprDeclParser "constexpr N : usize = 4;", "ConstExpr")
      ]

  describe "type definitions (asserting the inner TypeDef node)" $
    rows
      [ ("struct definition", typeDefOf structDefinitionParser "struct S { x : u32; };", "Struct")
      , ("enum definition", typeDefOf enumDefinitionParser "enum E { A, B(u32) };", "Enum")
      , ("interface definition",
          typeDefOf interfaceDefinitionParser "interface I { procedure p(&mut self); };", "Interface")
      , ("class definition",
          typeDefOf classDefinitionParser "resource class C provides I { procedure p(&mut self) { return; } };",
          "Class")
      ]

  describe "application-level instances" $
    rows
      [ ("task instance", ctorOf taskDeclParser "task t : CFoo = { };", "Task")
      , ("resource instance", ctorOf resourceDeclParser "resource r : CFoo = { };", "Resource")
      , ("handler instance", ctorOf handlerDeclParser "handler h : CFoo = { };", "Handler")
      , ("channel instance", ctorOf channelDeclParser "channel c : MsgQueue<u32; 10>;", "Channel")
      , ("emitter instance", ctorOf emitterDeclParser "emitter e : PeriodicTimer = { };", "Emitter")
      , ("global declaration wrapper", ctorOf globalDeclParser "task gd : CFoo = { };", "GlobalDeclaration")
      ]

  where
    ctorOf p s = either show ctorName (parseWith p s)
    typeDefOf p s = case parseWith p s of
      Right (TypeDefinition td _) -> ctorName td
      Right other -> ctorName other
      Left e -> show e
    rows = mapM_ (\(name, got, expected) -> it ("parses a " ++ name) $ got `shouldBe` expected)
