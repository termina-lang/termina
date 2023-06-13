module UT.PPrinter.Expression.ConstantSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Data.Text
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common



renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression

spec :: Spec
spec = do
  describe "Pretty printing constant expressions" $ do
    it "Prints the constant 0xFF of type u8" $ do
      renderExpression uint8Const `shouldBe`
        pack "(uint8_t)8"
    it "Prints the constant 1024 of type u16" $ do
      renderExpression uint16Const `shouldBe`
        pack "(uint16_t)1024"
    it "Prints the constant 0xFFFF0000 of type u32" $ do
      renderExpression uint32Const `shouldBe`
        pack "(uint32_t)4294901760"
    it "Prints the constant 1800000000 of type u64" $ do
      renderExpression uint64Const `shouldBe`
        pack "(uint64_t)1800000000"
    it "Prints the constant -128 of type i8" $ do
      renderExpression int8Const `shouldBe`
        pack "(int8_t)-128"
    it "Prints the constant 1024 of type i16" $ do
      renderExpression int16Const `shouldBe`
        pack "(int16_t)1024"
    it "Prints the constant -1024 of type i32" $ do
      renderExpression int32Const `shouldBe`
        pack "(int32_t)-1024"
    it "Prints the constant -3000000000 of type i64" $ do
      renderExpression int64Const `shouldBe`
        pack "(int64_t)-3000000000"
    it "Prints the constant 'a' of type char" $ do
      renderExpression charConst `shouldBe`
        pack "'a'"
    it "Prints the constant true of type bool" $ do
      renderExpression trueBool `shouldBe`
        pack "1"
    it "Prints the constant false of type bool" $ do
      renderExpression falseBool `shouldBe`
        pack "0"  