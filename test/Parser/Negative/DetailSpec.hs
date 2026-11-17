{-# LANGUAGE LambdaCase #-}
-- | Parser-stage error *detail*: asserts the error constructor and payload, not
-- just the PE-NNN code. Complements 'Parser.Negative.CodeSpec'.
module Parser.Negative.DetailSpec (spec) where

import Parser.Common (parserStageError, moduleStageError)
import Modules.Modules (ModuleDependency(..))
import Parser.Errors (Error(..), parseError)
import Parser.Parsing (parseTerminaModule)
import Parser.Types (DocPlacement(..))
import Utils.Annotations (AnnotatedError(..))
import Text.Parsec (errorPos, sourceLine, sourceColumn)
import Text.Parsec.Error (errorMessages, messageString)

import Test.Hspec

modA, modB :: String
modA = unlines ["import pkg.b;", "", "function fa() -> u32 { return 0 : u32; }"]
modB = unlines ["import pkg.a;", "", "function fb() -> u32 { return 0 : u32; }"]

-- | app/main imports pkg/x, which imports the cycle between pkg/a and pkg/b.
cycleBehindX :: [(String, String)]
cycleBehindX =
  [ ("app/main", unlines ["import pkg.x;", "", "function fm() -> u32 { return 0 : u32; }"])
  , ("pkg/x", unlines ["import pkg.a;", "", "function fx() -> u32 { return 0 : u32; }"])
  , ("pkg/a", modA)
  , ("pkg/b", modB) ]

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

-- | The placement, line and column of a misplaced documentation comment in a
-- module, or Nothing if the module parses or fails for another reason.
misplacedAt :: [String] -> Maybe (DocPlacement, Int, Int)
misplacedAt src =
  case parseTerminaModule "test" "test" (unlines src) of
    Left err | EMisplacedDocComment placement <- parseError err ->
      Just (placement, sourceLine (errorPos err), sourceColumn (errorPos err))
    _ -> Nothing

-- | One module per kind of misplaced documentation comment, with the placement
-- and the position of the comment.
misplaced :: [(String, [String], (DocPlacement, Int, Int))]
misplaced =
  [ ("a block comment before the imports", ["/** Header. */", "import foo.bar;"], (DocBefore, 1, 1))
  , ("a module comment after a declaration", ["function f() { return; }", "//! Late."], (DocModule, 2, 1))
  , ("a comment that no statement follows", ["function f() {", "    return;", "    /// Nothing.", "}"], (DocBefore, 3, 5))
  , ("a comment at the end of the module", ["function f() { return; }", "/// Nothing."], (DocBefore, 2, 1))
  , ("a comment that no case follows", [ "function f(o : Option<u32>) -> u32 {", "    var r : u32 = 0;"
                                       , "    match o {", "        case Some(x) => { r = x; }"
                                       , "        case None => { r = 1; }", "        /// Nothing.", "    }"
                                       , "    return r;", "}" ], (DocBefore, 6, 9))
  , ("a comment inside an expression", ["function f() -> u32 {", "    let x : u32 = /** mid */ 3;", "    return x;", "}"], (DocBefore, 2, 19))
  , ("a comment after with nothing before", ["function f() {", "    ///< After nothing.", "    return;", "}"], (DocAfter, 2, 5))
  , ("a comment before on the side of after", ["struct S {", "    x : u32 /// Wrong side.", "    ;", "};"], (DocBefore, 2, 13))
  , ("a comment after an import", ["import foo.bar; ///< Wrong.", "function f() { return; }"], (DocAfter, 1, 17))
  , ("a comment before a parameter", ["function f(/// The value.", "    v : u32) { return; }"], (DocBefore, 1, 12))
  , ("a comment after on the line below its field", ["struct S {", "    x : u32;", "    ///< Below.", "};"], (DocAfter, 3, 5))
  , ("a second line of a comment after", ["struct S {", "    x : u32; ///< First.", "             ///< Second.", "};"], (DocAfter, 3, 14))
  , ("a comment after a function", ["function f() {", "    return;", "} ///< A function."], (DocAfter, 3, 3))
  , ("a comment after a method", ["resource class C provides I {", "    method m(&priv self) { return; } ///< A method.", "};"], (DocAfter, 2, 38))
  , ("a comment after an interface procedure", ["interface I {", "    procedure p(&mut self); ///< A procedure.", "};"], (DocAfter, 2, 29))
  , ("a comment after the brace of a branch", ["function f(v : u32) {", "    if (v == 0) { ///< First.", "        return;", "    }", "    return;", "}"], (DocAfter, 2, 19))
  ]

spec :: Spec
spec = describe "Parser stage: error detail" $ do
  describe "PE-009 points at the comment" $
    mapM_ (\(what, src, expected) -> it ("for " ++ what) $ misplacedAt src `shouldBe` Just expected) misplaced
  it "PE-001 wraps the underlying parse error" $
    moduleStageError "function f() -> { return; }" `shouldSatisfy` \case
      Just (AnnotatedError (EParseError _) _) -> True
      _ -> False
  it "PE-001 rejects an action with a return type" $
    moduleStageError "task class T { s : sink u32 triggers a; action a(&priv self, x : u32) -> u32 { return x; } };" `shouldSatisfy` \case
      Just (AnnotatedError (EParseError _) _) -> True
      _ -> False
  it "PE-004 carries the detected import cycle" $
    parserStageError [("pkg/a", modA), ("pkg/b", modB)] `shouldSatisfy` \case
      Just (AnnotatedError (EImportedFilesLoop deps) _) -> not (null deps)
      _ -> False
  it "PE-004 gives the modules of the cycle in the order of the imports" $
    (case parserStageError cycleBehindX of
      Just (AnnotatedError (EImportedFilesLoop deps) _) -> Just [ qname | ModuleDependency qname _ <- deps ]
      _ -> Nothing) `shouldBe` Just ["pkg/a", "pkg/b", "pkg/a"]
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
