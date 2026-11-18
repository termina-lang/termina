-- | The checks of the documentation comments against the typed AST: a
-- documented parameter that is not in the signature of its element (DE-001).
module Doc.CheckSpec (spec) where

import qualified Data.Text as T

import Pipeline.Common (compileErrorCode, compileErrorMessage)
import Utils.Errors (stripAnsi)

import Test.Hspec

-- | A program with a comment that documents the parameter @x@ before an
-- element.
documenting :: String -> [String] -> String
documenting tag element = unlines (["/**", " * Summary.", " *", " * " ++ tag ++ " The value.", " */"] ++ element)

-- | An interface, and a resource class that provides it with an element.
resource :: [String] -> String
resource element = unlines
  ([ "interface I {", "    procedure p(&mut self, x : u32);", "};", ""
   , "resource class C provides I {"
   , "    procedure p(&mut self, x : u32) {", "        self->m(x);", "        return;", "    }" ]
   ++ element ++ ["};"])

spec :: Spec
spec = describe "Documentation check" $ do

  describe "DE-001: documented parameter not in the signature" $ do
    it "on a function" $
      compileErrorCode (documenting "@param y" ["function f(x : u32) -> u32 {", "    return x;", "}"])
        `shouldBe` Just (T.pack "DE-001")
    it "on an extern function" $
      compileErrorCode (documenting "@param[out] y" ["extern function f(x : &mut u32);"])
        `shouldBe` Just (T.pack "DE-001")
    it "on a procedure of an interface" $
      compileErrorCode (unlines
        [ "interface I {", "    /**", "     * Summary.", "     *", "     * @param y The value.", "     */"
        , "    procedure p(&mut self, x : u32);", "};" ])
        `shouldBe` Just (T.pack "DE-001")
    it "on a method" $
      compileErrorCode (resource
        [ "    /**", "     * Summary.", "     *", "     * @param y The value.", "     */"
        , "    method m(&priv self, x : u32) {", "        return;", "    }" ])
        `shouldBe` Just (T.pack "DE-001")
    it "on self, which is not a parameter of the signature" $
      compileErrorCode (resource
        [ "    /**", "     * Summary.", "     *", "     * @param self The resource.", "     */"
        , "    method m(&priv self, x : u32) {", "        return;", "    }" ])
        `shouldBe` Just (T.pack "DE-001")
    it "names the element and its parameters" $
      fmap stripAnsi (compileErrorMessage (documenting "@param y" ["function f(a : u32, b : u32) -> u32 {", "    return a + b;", "}"]))
        `shouldSatisfy` maybe False (T.isInfixOf (T.pack "f has no parameter y. Its parameters are a and b."))

  describe "accepts the parameters of the signature" $ do
    it "on a function" $
      compileErrorCode (documenting "@param x" ["function f(x : u32) -> u32 {", "    return x;", "}"])
        `shouldBe` Nothing
    it "on a function, with [out]" $
      compileErrorCode (documenting "@param[out] x" ["function f(x : &mut u32) {", "    *x = 0;", "    return;", "}"])
        `shouldBe` Nothing
    it "on a procedure of an interface" $
      compileErrorCode (unlines
        [ "interface I {", "    /**", "     * Summary.", "     *", "     * @param x The value.", "     */"
        , "    procedure p(&mut self, x : u32);", "};" ])
        `shouldBe` Nothing
