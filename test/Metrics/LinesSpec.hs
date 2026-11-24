-- | The classification of the lines of a source file.
module Metrics.LinesSpec (spec) where

import Metrics.Lines

import Test.Hspec

classes :: String -> [LineClass]
classes = map lineClass . classifyLines

code, eol, comment, doc, blank :: LineInfo
code = LineInfo Code False False
eol = LineInfo Code True False
comment = LineInfo Comment False False
doc = LineInfo Comment False True
blank = LineInfo Blank False False

spec :: Spec
spec = do
  describe "Line classes" $ do
    it "A line with code is code" $
      classifyLines "x = 1;\n" `shouldBe` [code]
    it "A line with only white space is blank" $
      classifyLines "  \t\n" `shouldBe` [blank]
    it "A line comment alone is a comment line" $
      classifyLines "  // note\n" `shouldBe` [comment]
    it "An empty line comment is a comment line" $
      classifyLines "//\n" `shouldBe` [comment]
    it "A line with code and a comment is code with an end-of-line comment" $
      classifyLines "x = 1; // note\n" `shouldBe` [eol]
    it "A block comment closed before code on the same line counts the line as code" $
      classifyLines "/* note */ x = 1;\n" `shouldBe` [eol]
    it "A blank line inside a block comment is a comment line" $
      classifyLines "/* a\n\n   b */\n" `shouldBe` [comment, comment, comment]
    it "Code after the end of a block comment on a later line is code" $
      classifyLines "/* a\n */ x = 1;\n" `shouldBe` [comment, eol]
    it "Block comments do not nest" $
      classes "/* a /* b */ x = 1; */\n" `shouldBe` [Code]

  describe "Documentation comments" $ do
    it "Each line of a run of line documentation comments is a documentation line" $
      classifyLines "/// a\n/// b\n" `shouldBe` [doc, doc]
    it "A module documentation comment is a documentation line" $
      classifyLines "//! a\n" `shouldBe` [doc]
    it "Every line of a block documentation comment is a documentation line" $
      classifyLines "/** a\n\n   b */\n" `shouldBe` [doc, doc, doc]
    it "A documentation comment after an element is an end-of-line comment" $
      classifyLines "x : u8; ///< a\n" `shouldBe` [eol]
    it "Four slashes open an ordinary comment" $
      classifyLines "//// a\n" `shouldBe` [comment]
    it "An empty block comment is an ordinary comment" $
      classifyLines "/**/\n" `shouldBe` [comment]

  describe "Literals" $ do
    it "A line comment opener inside a string is code" $
      classifyLines "s = \"a // b\";\n" `shouldBe` [code]
    it "A block comment opener inside a string is code" $
      classifyLines "s = \"/*\";\nx = 1;\n" `shouldBe` [code, code]
    it "An escaped quote does not end a string" $
      classifyLines "s = \"a \\\" // b\";\n" `shouldBe` [code]
    it "A slash in a character literal does not open a comment" $
      classifyLines "c = '/'; d = '/';\n" `shouldBe` [code]
    it "An escaped quote does not end a character literal" $
      classifyLines "c = '\\''; // a\n" `shouldBe` [eol]

  describe "Ends of the text" $ do
    it "An empty text has no lines" $
      classifyLines "" `shouldBe` []
    it "A final newline does not start a line" $
      length (classifyLines "a\n\n") `shouldBe` 2
    it "A last line without a newline is counted" $
      classifyLines "a\nb" `shouldBe` [code, code]
    it "A carriage return is white space" $
      classifyLines "a\r\n\r\n" `shouldBe` [code, blank]

  describe "End of an element" $ do
    it "Skips the blank and comment lines before the end position" $
      lastCodeLine (codeStarts "f {\n}\n\n// c\ng\n") 5 1 `shouldBe` 2
    it "Skips the indentation of the line of the end position" $
      lastCodeLine (codeStarts "f {\n}\n    g\n") 3 5 `shouldBe` 2
    it "Counts a tab as Parsec does" $
      lastCodeLine (codeStarts "f {\n}\n\tg\n") 3 9 `shouldBe` 2
    it "Keeps the line of the end position when code precedes it" $
      lastCodeLine (codeStarts "f { } g\n") 1 7 `shouldBe` 1
    it "Skips a comment before the next element on its line" $
      lastCodeLine (codeStarts "f {\n}\n/* c */ g\n") 3 9 `shouldBe` 2

  describe "The example module" $ do
    it "Counts the lines of the module" $ do
      ls <- scanLines <$> readFile "test/Metrics/examples/sensors.fin"
      totalCounts ls `shouldBe` LineCounts 53 41 5 7 1 2
    it "Counts the lines of a range" $ do
      ls <- scanLines <$> readFile "test/Metrics/examples/sensors.fin"
      countRange ls 15 27 `shouldBe` LineCounts 13 11 1 1 0 1
      countRange ls 36 52 `shouldBe` LineCounts 17 17 0 0 1 0
    it "Computes the comment ratios" $ do
      ls <- scanLines <$> readFile "test/Metrics/examples/sensors.fin"
      fmap (round . (* 1000)) (commentRatio (totalCounts ls)) `shouldBe` Just (130 :: Int)
      fmap (round . (* 1000)) (ecssCommentRatio (totalCounts ls)) `shouldBe` Just (65 :: Int)
