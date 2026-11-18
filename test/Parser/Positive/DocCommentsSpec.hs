-- | Positive parser tests for documentation comments: every place where the
-- grammar takes one, in both forms, the comments that stay ordinary, and the
-- element whose annotation keeps each comment.
module Parser.Positive.DocCommentsSpec (spec) where

import Data.Foldable (toList)
import Data.List (sortOn)

import Core.AST (TerminaModule'(..), DocComment(..), DocPlacement(..), DocForm(..))
import Parser.Common (parses)
import Parser.Parsing (terminaModuleParser, parseTerminaModule)
import Parser.Types (ParserAnn(..))
import Utils.Annotations (Location(..))
import Text.Parsec (sourceLine)

import Test.Hspec

-- | Does a whole module parse?
accepts :: [String] -> Bool
accepts = parses terminaModuleParser . unlines

-- | The comments of a module, each with the line where the element that keeps
-- it in its annotation starts, its placement and its text, in the order of the
-- lines.
attached :: [String] -> [(Int, DocPlacement, String)]
attached src =
  case parseTerminaModule "test" "test" (unlines src) of
    Right m -> sortOn (\(line, _, _) -> line)
                 [ (sourceLine start, docPlacement d, docText d)
                 | ParserAnn (Position _ start _) (Just d) <- toList m ]
    Left err -> error (show err)

-- | The comment of a module, with its form and its text.
moduleComment :: [String] -> Maybe (DocForm, String)
moduleComment src =
  case parseTerminaModule "test" "test" (unlines src) of
    Right m -> (\d -> (docForm d, docText d)) <$> moduleDoc m
    Left err -> error (show err)

-- | A function whose body is the given lines.
inBody :: [String] -> [String]
inBody body = ["function f(v : u32) -> u32 {", "    var r : u32 = 0;"] ++ body ++ ["    return r;", "}"]

spec :: Spec
spec = describe "Parser: documentation comments" $ do

  describe "at the start of the module" $ do
    it "takes a line comment before the imports" $
      accepts ["//! The module.", "//! Second line.", "", "import foo.bar;", "", "function g() { return; }"]
        `shouldBe` True
    it "takes a block comment before the imports" $
      accepts ["/*!", " * The module.", " */", "", "import foo.bar;"] `shouldBe` True
    it "takes a comment in a module without imports" $
      accepts ["//! The module.", "", "/// A function.", "function g() { return; }"] `shouldBe` True

  describe "before a declaration" $ do
    it "takes one before a function" $
      accepts ["/// Does nothing.", "function g() { return; }"] `shouldBe` True
    it "takes a block comment before a function" $
      accepts ["/**", " * Does nothing.", " *", " * @return Nothing.", " */", "function g() { return; }"]
        `shouldBe` True
    it "takes one before an extern function" $
      accepts ["/// From C.", "extern function crc(d : &[u8; 4]) -> u16;"] `shouldBe` True
    it "takes one before a constant and a constexpr" $
      accepts ["/// A table.", "const t : [u8; 2] = {1, 2};", "/// A size.", "constexpr n : usize = 4;"]
        `shouldBe` True
    it "takes one before a struct, its fields and after them" $
      accepts [ "/// A point.", "struct P {", "    /// Abscissa.", "    x : u32;", "    y : u32; /**< in mm */", "};" ]
        `shouldBe` True
    it "takes one before an enum, its variants and after them, on either side of the comma" $
      accepts [ "/// A mode.", "enum M {", "    /// Off.", "    Off, ///< no power"
              , "    Low ///< some power", "    , On(u8) /**< full power */", "};" ]
        `shouldBe` True
    it "takes one before an interface and its procedures" $
      accepts [ "/// A counter.", "interface I {", "    /// Adds.", "    procedure add(&mut self, by : u32);", "};" ]
        `shouldBe` True
    it "takes one before a class and its members, and after a field" $
      accepts [ "/// A worker.", "task class T {", "    i : in u32 triggers act; ///< data"
              , "    /// Last value.", "    last : u32;"
              , "    /// Reads it.", "    viewer get(&self) -> u32 { return self->last; }"
              , "    /// Reacts.", "    action act(&priv self, v : u32) { self->last = v; return; }", "};" ]
        `shouldBe` True
    it "takes one after a global declaration" $
      accepts ["const a : u32 = 1; ///< one", "const b : u32 = 2; /**< two */"] `shouldBe` True
    it "takes a block comment after an element that goes on below its line" $
      accepts ["struct P {", "    x : u32; /**< Abscissa,", "              *   in mm. */", "};"] `shouldBe` True

  describe "inside a body" $ do
    it "takes one before and after a statement" $
      accepts (inBody ["    /// Doubles it.", "    r = v + v;", "    r = r + 1; ///< no overflow"]) `shouldBe` True
    it "takes one before the first statement and the return" $
      accepts ["function g() {", "    /// Done.", "    return;", "}"] `shouldBe` True
    it "takes one before each branch of an if, on its own line or after the brace" $
      accepts (inBody [ "    /// Picks one.", "    if (v == 0) {", "        r = 1;"
                      , "    }", "    /// The second.", "    else if (v == 1) {", "        r = 2;"
                      , "    } /** The rest. */ else {", "        r = 3;", "    }" ])
        `shouldBe` True
    it "takes one before each case of a match" $
      accepts [ "function g(o : Option<u32>) -> u32 {", "    var r : u32 = 0;", "    /// Unpacks it."
              , "    match o {", "        /// Has one.", "        case Some(x) => { r = x; }"
              , "        /// Has none.", "        case None => { r = 1; }", "    }", "    return r;", "}" ]
        `shouldBe` True

  describe "ordinary comments" $ do
    it "leaves line and block comments anywhere white space goes" $
      accepts (inBody ["    r = v /* inline */ + 1; // trailing", "    /* before */ r = r + 1;"]) `shouldBe` True
    it "takes four slashes, \"/***\" and \"/**/\" as ordinary comments" $
      accepts (inBody ["    //// not documentation", "    r = /**/ v; /*** not documentation */"]) `shouldBe` True
    it "keeps the division operator" $
      accepts (inBody ["    r = v / 2;"]) `shouldBe` True

  describe "the element that keeps the comment" $ do
    it "keeps the text between the delimiters, with the lines of a run joined" $
      attached ["/// Adds two numbers.", "///", "///     indented code", "function g() { return; }"]
        `shouldBe` [(4, DocBefore, " Adds two numbers.\n\n     indented code")]
    it "keeps the text of a block comment as written" $
      attached ["/**", " * Does nothing.", " */", "function g() { return; }"]
        `shouldBe` [(4, DocBefore, "\n * Does nothing.\n ")]
    it "gives the comment of the module to the module" $ do
      moduleComment ["//! The module.", "//! Second line.", "", "import foo.bar;"]
        `shouldBe` Just (LineDoc, " The module.\n Second line.")
      moduleComment ["/*! The module. */", "function g() { return; }"]
        `shouldBe` Just (BlockDoc, " The module. ")
    it "gives a comment to each declaration of the module" $
      attached [ "/// A function.", "function g() { return; }"
               , "/// A struct.", "struct P { x : u32; };"
               , "/// A constant.", "const a : u32 = 1;"
               , "const b : u32 = 2; ///< After a constant." ]
        `shouldBe` [ (2, DocBefore, " A function."), (4, DocBefore, " A struct.")
                   , (6, DocBefore, " A constant."), (7, DocAfter, " After a constant.") ]
    it "gives a comment to each field and member" $
      attached [ "interface I {", "    /// A procedure.", "    procedure add(&mut self);", "};"
               , "struct P {", "    /// Abscissa.", "    x : u32;", "    y : u32; ///< Ordinate.", "};"
               , "task class T {", "    i : in u32 triggers act; ///< A port."
               , "    /// A viewer.", "    viewer get(&self) -> u32 { return 0; }"
               , "    /// An action.", "    action act(&priv self, v : u32) { return; }", "};" ]
        `shouldBe` [ (3, DocBefore, " A procedure."), (7, DocBefore, " Abscissa."), (8, DocAfter, " Ordinate.")
                   , (11, DocAfter, " A port."), (13, DocBefore, " A viewer."), (15, DocBefore, " An action.") ]
    it "gives a comment to each statement" $
      attached (inBody ["    /// Doubles it.", "    r = v + v;", "    r = r + 1; ///< Adds one."])
        `shouldBe` [(4, DocBefore, " Doubles it."), (5, DocAfter, " Adds one.")]
    it "gives a comment to each branch of an if" $
      attached (inBody [ "    /// Picks one.", "    if (v == 0) {", "        r = 1;", "    }"
                       , "    /// The second.", "    else if (v == 1) {", "        r = 2;"
                       , "    } /** The rest. */ else {", "        r = 3;", "    }" ])
        `shouldBe` [(4, DocBefore, " Picks one."), (8, DocBefore, " The second."), (10, DocBefore, " The rest. ")]
    it "gives a comment to each case of a match, the default one included" $
      attached [ "function g(o : Option<u32>) -> u32 {", "    var r : u32 = 0;"
               , "    match o {", "        /// Has one.", "        case Some(x) => { r = x; }"
               , "        /// The rest.", "        case _ => { r = 1; }", "    }", "    return r;", "}" ]
        `shouldBe` [(5, DocBefore, " Has one."), (7, DocBefore, " The rest.")]
