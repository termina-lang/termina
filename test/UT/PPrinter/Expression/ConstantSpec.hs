module UT.PPrinter.Expression.ConstantSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

uint8Const0x8, uint16Const1024, uint32Const0xFFFF0000,
  uint64Const1800000000, int8ConstMinux128, int16Const1024,
  int32ConstMinux1024, int64ConstMinux3000000000,
  charConsta, trueBool, falseBool :: Expression SemanticAnns
uint8Const0x8 = Constant (I UInt8 0x08) uint8SemAnn
uint16Const1024 = Constant (I UInt16 1024) uint16SemAnn
uint32Const0xFFFF0000 = Constant (I UInt32 0xFFFF0000) uint32SemAnn
uint64Const1800000000 = Constant (I UInt64 1800000000) uint64SemAnn
int8ConstMinux128 = Constant (I Int8 (-128)) int8SemAnn
int16Const1024 = Constant (I Int16 1024) int16SemAnn
int32ConstMinux1024 = Constant (I Int32 (-1024)) int32SemAnn
int64ConstMinux3000000000 = Constant (I Int64 (-3000000000)) int64SemAnn
charConsta = Constant (C 'a') charSemAnn
trueBool = Constant (B True) boolSemAnn
falseBool = Constant (B False) boolSemAnn

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression empty

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