{-# LANGUAGE LambdaCase #-}
-- | Parser-stage error *detail*: asserts the error constructor and payload, not
-- just the PE-NNN code. Complements 'Parser.Negative.CodeSpec'.
module Parser.Negative.DetailSpec (spec) where

import Parser.Common (parserStageError, moduleStageError)
import Parser.Errors (Error(..))
import Utils.Annotations (AnnotatedError(..))
import Text.Parsec.Error (errorMessages, messageString)

import Test.Hspec

modA, modB :: String
modA = unlines ["import pkg.b;", "", "function fa() -> u32 { return 0 : u32; }"]
modB = unlines ["import pkg.a;", "", "function fb() -> u32 { return 0 : u32; }"]

-- | The messages of the parse error of a single module, or Nothing if the
-- module parses or fails for another reason.
parseMessages :: String -> Maybe [String]
parseMessages src =
  case moduleStageError src of
    Just (AnnotatedError (EParseError err) _) -> Just (map messageString (errorMessages err))
    _ -> Nothing

-- | One source per place where a name is declared, each with a name that
-- contains two consecutive underscores.
doubleUnderscore :: [(String, String, String)]
doubleUnderscore =
  [ ("a struct", "struct foo__bar { a : u32; };", identifierMsg "foo__bar")
  , ("a field", "struct Foo { a__b : u32; };", identifierMsg "a__b")
  , ("an enum variant", "enum E { Va__r, Other };", identifierMsg "Va__r")
  , ("a function", "function do__it() -> u32 { return 0 : u32; }", identifierMsg "do__it")
  , ("a parameter", "function f(x__y : u32) -> u32 { return x__y; }", identifierMsg "x__y")
  , ("an ignored parameter", "function f(_x__y : u32) -> u32 { return 0 : u32; }", identifierMsg "_x__y")
  , ("a local", "function f() -> u32 { var t__v : u32 = 0 : u32; return t__v; }", identifierMsg "t__v")
  , ("a method", "resource class C { method m__n(&priv self) { return; } };", identifierMsg "m__n")
  , ("a case argument", "function f(o : Option<u32>) -> u32 { var r : u32 = 0 : u32; match o { case Some(a__b) => { r = a__b; } case None => { r = 1 : u32; } } return r; }", identifierMsg "a__b")
  , ("a module", "import foo__bar.baz;", "Namespace and module names cannot contain two consecutive underscores: foo__bar.baz.")
  ]
  where
    identifierMsg name = "Identifiers cannot contain two consecutive underscores: " ++ name ++ "."

spec :: Spec
spec = describe "Parser stage: error detail" $ do
  it "PE-001 wraps the underlying parse error" $
    moduleStageError "function f() -> { return; }" `shouldSatisfy` \case
      Just (AnnotatedError (EParseError _) _) -> True
      _ -> False
  it "PE-004 carries the detected import cycle" $
    parserStageError [("pkg/a", modA), ("pkg/b", modB)] `shouldSatisfy` \case
      Just (AnnotatedError (EImportedFilesLoop deps) _) -> not (null deps)
      _ -> False
  describe "PE-001 rejects an invalid module path, with that message alone" $ do
    it "when it begins with termina" $
      parseMessages "import termina.types;" `shouldBe` Just ["User-defined modules are not allowed in the termina namespace: termina.types."]
    it "when a name begins with an uppercase letter" $
      parseMessages "import Drivers.uart;" `shouldBe` Just ["Namespace and module names begin with a lowercase letter: Drivers.uart."]
    it "when a name begins with a digit" $
      parseMessages "import drivers.9uart;" `shouldBe` Just ["Namespace and module names begin with a lowercase letter: drivers.9uart."]
    it "when a name contains an uppercase letter" $
      parseMessages "import drivers.uArt;" `shouldBe` Just ["Namespace and module names only contain lowercase letters, digits and underscores: drivers.uArt."]
  describe "PE-001 rejects two consecutive underscores, with that message alone" $
    mapM_ (\(place, src, msg) -> it ("in " ++ place) $
              parseMessages src `shouldBe` Just [msg])
      doubleUnderscore
