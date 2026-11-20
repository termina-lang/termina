-- | Meta-test: every documentation comment that the parser keeps reaches the
-- annotations of the typed AST once. A stage that builds an annotation and
-- forgets the comment loses it; one that copies it to a new element gives it
-- twice. Both change the list of comments of the module.
module Meta.DocCommentsSpec (spec) where

import Data.Foldable (toList)
import Data.List (sort)

import Core.AST (TerminaModule'(..), DocComment(..))
import Parser.Parsing (parseTerminaModule)
import Parser.Types (ParserAnn(..))
import Data.Text (unpack)
import Pipeline.Common (Failure(..), runTypedModule)
import Semantic.Types (sourceOf)

import Test.Hspec

-- | A module with a documentation comment on each kind of element that takes
-- one, in both forms.
documented :: String
documented = unlines
  [ "/// The lower bound."
  , "const low : u32 = 1;"
  , "const high : u32 = 9; ///< The upper bound."
  , ""
  , "/// A point."
  , "struct Point {"
  , "    /// Abscissa."
  , "    x : u32;"
  , "    y : u32; ///< Ordinate."
  , "};"
  , ""
  , "/** A mode. */"
  , "enum Mode {"
  , "    /// Off."
  , "    Off,"
  , "    On(u32) ///< On, at a level."
  , "};"
  , ""
  , "/// A counter."
  , "interface ICounter {"
  , "    /// Adds to the count."
  , "    procedure add(&mut self, by : u32);"
  , "};"
  , ""
  , "/// The counter."
  , "resource class CCounter provides ICounter {"
  , "    count : u32; ///< The count."
  , ""
  , "    /// Adds to the count."
  , "    procedure add(&mut self, by : u32) {"
  , "        /// Adds it."
  , "        self->count = self->count + by;"
  , "        return;"
  , "    }"
  , ""
  , "    /// Clears the count."
  , "    method clear(&priv self) {"
  , "        self->count = 0; ///< Back to zero."
  , "        return;"
  , "    }"
  , "};"
  , ""
  , "interface INothing {"
  , "    procedure nothing(&mut self);"
  , "};"
  , ""
  , "resource class CNothing provides INothing {"
  , "    procedure nothing(&mut self) {"
  , "        return;"
  , "    }"
  , "};"
  , ""
  , "/// An instance without an initializer."
  , "resource nothing : CNothing;"
  , ""
  , "/// The level of a mode."
  , "function level(m : Mode, v : u32) -> u32 {"
  , "    /// The result."
  , "    var r : u32 = 0;"
  , "    /// Unpacks the mode."
  , "    match m {"
  , "        /// No level."
  , "        case Off => { r = 0; }"
  , "        /// Its level."
  , "        case On(l) => { r = l; }"
  , "    }"
  , "    /// Compares it."
  , "    if (v > high) {"
  , "        r = high;"
  , "    }"
  , "    /// The lower one."
  , "    else if (v < low) {"
  , "        r = low;"
  , "    }"
  , "    /** The rest. */"
  , "    else {"
  , "        r = r + v;"
  , "    }"
  , "    return r;"
  , "}"
  ]

-- | The comments of a list of annotations, each with its location, in order.
comments :: [ParserAnn] -> [(String, String)]
comments anns = sort [ (show (docLocation d), docText d) | ParserAnn _ (Just d) <- anns ]

spec :: Spec
spec = describe "Meta: documentation comments" $
  it "every comment the parser keeps reaches the typed AST once" $ do
    parsed <- either (fail . show) (return . concatMap toList . frags)
      (parseTerminaModule "test" "test" documented)
    typed <- either (fail . unpack . failMessage) (return . concatMap toList)
      (runTypedModule "test" [("test", documented)])
    length (comments parsed) `shouldBe` 25
    comments (map sourceOf typed) `shouldBe` comments parsed
