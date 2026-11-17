-- | The columns of the positions that the parser gives count characters, also
-- on a line with tabs, where Parsec moves a tab to the next column 1 + 8k.
module Parser.Positive.PositionsSpec (spec) where

import Parser.AST
import Parser.Parsing (parseTerminaModule, characterColumn)
import Utils.Annotations (Location(..))
import Text.Parsec (errorPos, sourceLine, sourceColumn)
import Text.Parsec.Pos (newPos)

import Test.Hspec

-- | The line and column of the start and the end of the return statement that
-- is the body of the only function of a module.
returnAt :: [String] -> Maybe ((Int, Int), (Int, Int))
returnAt src =
  case parseTerminaModule "test" "test" (unlines src) of
    Right (Termina _ [Function _ _ _ (Block [ReturnStmt _ (Position _ start end)] _) _ _]) ->
      Just ((sourceLine start, sourceColumn start), (sourceLine end, sourceColumn end))
    _ -> Nothing

-- | The line and column of the error of a module that does not parse.
errorAt :: [String] -> Maybe (Int, Int)
errorAt src =
  case parseTerminaModule "test" "test" (unlines src) of
    Left err -> Just (sourceLine (errorPos err), sourceColumn (errorPos err))
    Right _ -> Nothing

-- | The character column of a column of Parsec on the only line of a text.
column :: String -> Int -> Int
column line = sourceColumn . characterColumn line . newPos "test" 1

spec :: Spec
spec = describe "Parser: positions" $ do

  describe "the character column of a column of Parsec" $ do
    it "is the same on a line without tabs" $
      column "return y;" 8 `shouldBe` 8
    it "counts a tab at the start of a line as one character" $
      column "\treturn y;" 16 `shouldBe` 9
    it "counts each of two tabs as one character" $
      column "\t\treturn y;" 24 `shouldBe` 10
    it "counts a tab after other characters as one character" $
      column "ab\tc" 9 `shouldBe` 4
    it "gives the column after the last character for the end of the line" $
      column "\tab" 11 `shouldBe` 4

  describe "a module indented with tabs" $ do
    it "gives the statements their character columns" $
      returnAt ["function f() {", "\treturn;", "}"] `shouldBe` Just ((2, 2), (2, 8))
    it "leaves the columns of a module without tabs as they are" $
      returnAt ["function f() {", "    return;", "}"] `shouldBe` Just ((2, 5), (2, 11))
    it "gives the error its character column" $
      errorAt ["function f() -> u32 {", "\treturn 1 +;", "}"] `shouldBe` Just (2, 12)
