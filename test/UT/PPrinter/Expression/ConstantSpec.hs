module UT.PPrinter.Expression.ConstantSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Data.Text
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common



renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppRootExpression

spec :: Spec
spec = do
  describe "Pretty printing constant expressions" $ do
    it "Prints the constant 0xFF of type u8" $ do
      renderExpression uint8Const0x8 `shouldBe`
        pack "(uint8_t)8"
    it "Prints the constant 1024 of type u16" $ do
      renderExpression uint16Const1024 `shouldBe`
        pack "(uint16_t)1024"
    it "Prints the constant 0xFFFF0000 of type u32" $ do
      renderExpression uint32Const0xFFFF0000 `shouldBe`
        pack "(uint32_t)4294901760"
    it "Prints the constant 1800000000 of type u64" $ do
      renderExpression uint64Const1800000000 `shouldBe`
        pack "(uint64_t)1800000000"
    it "Prints the constant -128 of type i8" $ do
      renderExpression int8ConstMinux128 `shouldBe`
        pack "(int8_t)-128"
    it "Prints the constant 1024 of type i16" $ do
      renderExpression int16Const1024 `shouldBe`
        pack "(int16_t)1024"
    it "Prints the constant -1024 of type i32" $ do
      renderExpression int32ConstMinux1024 `shouldBe`
        pack "(int32_t)-1024"
    it "Prints the constant -3000000000 of type i64" $ do
      renderExpression int64ConstMinux3000000000 `shouldBe`
        pack "(int64_t)-3000000000"
    it "Prints the constant 'a' of type char" $ do
      renderExpression charConsta `shouldBe`
        pack "'a'"
    it "Prints the constant true of type bool" $ do
      renderExpression trueBool `shouldBe`
        pack "1"
    it "Prints the constant false of type bool" $ do
      renderExpression falseBool `shouldBe`
        pack "0"  