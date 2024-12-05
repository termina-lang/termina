module UT.PPrinter.Expression.ConstantSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

uint8Const0x8, uint16Const1024, uint32Const0xFFFF0000,
  uint64Const1800000000, int8ConstMinux128, int16Const1024,
  int32ConstMinux1024, int64ConstMinux3000000000,
  charConsta, trueBool, falseBool :: Expression SemanticAnn
uint8Const0x8 = Constant (I (TInteger 0x08 HexRepr) (Just TUInt8)) uint8ExprSemAnn
uint16Const1024 = Constant (I (TInteger 1024 DecRepr) (Just TUInt16)) uint16ExprSemAnn
uint32Const0xFFFF0000 = Constant (I (TInteger 0xFFFF0000 HexRepr) (Just TUInt32)) uint32ExprSemAnn
uint64Const1800000000 = Constant (I (TInteger 1800000000 DecRepr) (Just TUInt64)) uint64ExprSemAnn
int8ConstMinux128 = Constant (I (TInteger (-128) DecRepr) (Just TInt8)) int8ExprSemAnn
int16Const1024 = Constant (I (TInteger 1024 DecRepr) (Just TInt16)) int16ExprSemAnn
int32ConstMinux1024 = Constant (I (TInteger (-1024) DecRepr) (Just TInt32)) int32ExprSemAnn
int64ConstMinux3000000000 = Constant (I (TInteger (-3000000000) DecRepr) (Just TInt64)) int64ExprSemAnn
charConsta = Constant (C 'a') charExprSemAnn
trueBool = Constant (B True) boolExprSemAnn
falseBool = Constant (B False) boolExprSemAnn

spec :: Spec
spec = do
  describe "Pretty printing constant expressions" $ do
    it "Prints the constant 0x8 of type u8" $ do
      renderExpression uint8Const0x8 `shouldBe`
        pack "0x8U"
    it "Prints the constant 1024 of type u16" $ do
      renderExpression uint16Const1024 `shouldBe`
        pack "1024U"
    it "Prints the constant 0xFFFF0000 of type u32" $ do
      renderExpression uint32Const0xFFFF0000 `shouldBe`
        pack "0xFFFF0000U"
    it "Prints the constant 1800000000 of type u64" $ do
      renderExpression uint64Const1800000000 `shouldBe`
        pack "UINT64_C(1800000000)"
    it "Prints the constant -128 of type i8" $ do
      renderExpression int8ConstMinux128 `shouldBe`
        pack "-(128L)"
    it "Prints the constant 1024 of type i16" $ do
      renderExpression int16Const1024 `shouldBe`
        pack "1024L"
    it "Prints the constant -1024 of type i32" $ do
      renderExpression int32ConstMinux1024 `shouldBe`
        pack "-(1024L)"
    it "Prints the constant -3000000000 of type i64" $ do
      renderExpression int64ConstMinux3000000000 `shouldBe`
        pack "-INT64_C(3000000000)"
    it "Prints the constant 'a' of type char" $ do
      renderExpression charConsta `shouldBe`
        pack "'a'"
    it "Prints the constant true of type bool" $ do
      renderExpression trueBool `shouldBe`
        pack "1"
    it "Prints the constant false of type bool" $ do
      renderExpression falseBool `shouldBe`
        pack "0"  