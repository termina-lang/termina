{-# LANGUAGE LambdaCase #-}
-- | Positive parser tests for literal constants ('constantParser'): integers
-- (signs, hex), floats (decimal and scientific), booleans, chars and null.
module Parser.Positive.LiteralsSpec (spec) where

import Parser.Common (parseWith)
import Parser.Parsing (constantParser)
import Parser.AST
import Parser.Types (ParserAnn)
import Core.AST

import Test.Hspec

-- | The literal a constant fragment parses to (unwrapping the annotation).
lit :: String -> Maybe (Const ParserAnn)
lit s = case parseWith constantParser s of
  Right (Constant c _) -> Just c
  _ -> Nothing

spec :: Spec
spec = describe "Parser: literal constants" $ do

  describe "integers" $ do
    it "parses a decimal integer" $
      lit "5" `shouldSatisfy` \case Just (I (TInteger 5 DecRepr) _) -> True; _ -> False
    it "parses an explicitly negative integer" $
      lit "-5" `shouldSatisfy` \case Just (I (TInteger (-5) DecRepr) _) -> True; _ -> False
    it "parses an explicitly positive integer" $
      lit "+5" `shouldSatisfy` \case Just (I (TInteger 5 DecRepr) _) -> True; _ -> False
    it "parses a hexadecimal integer" $
      lit "0xFF" `shouldSatisfy` \case Just (I (TInteger 255 HexRepr) _) -> True; _ -> False
    it "parses a typed integer literal" $
      lit "5 : u16" `shouldSatisfy` \case Just (I (TInteger 5 _) (Just _)) -> True; _ -> False

  describe "floats" $ do
    it "parses a decimal float" $
      lit "1.5" `shouldSatisfy` \case Just (F (TFloat _ FPDecimal) _) -> True; _ -> False
    it "parses scientific notation" $
      lit "1e3" `shouldSatisfy` \case Just (F (TFloat _ FPScientific) _) -> True; _ -> False
    it "parses scientific notation with a fractional part" $
      lit "1.5e3" `shouldSatisfy` \case Just (F (TFloat _ FPScientific) _) -> True; _ -> False
    it "parses scientific notation with a negative exponent" $
      lit "1e-3" `shouldSatisfy` \case Just (F (TFloat _ FPScientific) _) -> True; _ -> False
    it "parses scientific notation with a positive exponent sign" $
      lit "1e+3" `shouldSatisfy` \case Just (F (TFloat _ FPScientific) _) -> True; _ -> False

  describe "other literals" $ do
    it "parses true" $
      lit "true" `shouldSatisfy` \case Just (B True) -> True; _ -> False
    it "parses false" $
      lit "false" `shouldSatisfy` \case Just (B False) -> True; _ -> False
    it "parses a char literal" $
      lit "'a'" `shouldSatisfy` \case Just (C 'a') -> True; _ -> False
    it "parses the null literal" $
      lit "null" `shouldSatisfy` \case Just Null -> True; _ -> False
