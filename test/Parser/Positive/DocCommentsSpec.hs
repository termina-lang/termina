-- | Positive parser tests for documentation comments: every place where the
-- grammar takes one, in both forms, and the comments that stay ordinary.
module Parser.Positive.DocCommentsSpec (spec) where

import Parser.Common (parses)
import Parser.Parsing (terminaModuleParser)

import Test.Hspec

-- | Does a whole module parse?
accepts :: [String] -> Bool
accepts = parses terminaModuleParser . unlines

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
      accepts [ "/// A point.", "struct P {", "    /// Abscissa.", "    x : u32; ///< in mm", "    y : u32; /**< in mm */", "};" ]
        `shouldBe` True
    it "takes one before an enum, its variants and after them, on either side of the comma" $
      accepts [ "/// A mode.", "enum M {", "    /// Off.", "    Off, ///< no power"
              , "    Low ///< some power", "    , On(u8) /**< full power */", "};" ]
        `shouldBe` True
    it "takes one before an interface and its procedures" $
      accepts [ "/// A counter.", "interface I {", "    /// Adds.", "    procedure add(&mut self, by : u32);", "};" ]
        `shouldBe` True
    it "takes one before a class and its members, and after a field" $
      accepts [ "/// A worker.", "task class T {", "    /// Input.", "    i : in u32 triggers act; ///< data"
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
      accepts (inBody ["    /// Doubles it.", "    r = v + v; ///< no overflow"]) `shouldBe` True
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
