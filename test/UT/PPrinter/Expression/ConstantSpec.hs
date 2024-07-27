module UT.PPrinter.Expression.ConstantSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Control.Monad.Reader
import Generator.CodeGen.Expression
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

uint8Const0x8, uint16Const1024, uint32Const0xFFFF0000,
  uint64Const1800000000, int8ConstMinux128, int16Const1024,
  int32ConstMinux1024, int64ConstMinux3000000000,
  charConsta, trueBool, falseBool :: Expression SemanticAnn
uint8Const0x8 = Constant (I (TInteger 0x08 HexRepr) (Just UInt8)) uint8SemAnn
uint16Const1024 = Constant (I (TInteger 1024 DecRepr) (Just UInt16)) uint16SemAnn
uint32Const0xFFFF0000 = Constant (I (TInteger 0xFFFF0000 HexRepr) (Just UInt32)) uint32SemAnn
uint64Const1800000000 = Constant (I (TInteger 1800000000 DecRepr) (Just UInt64)) uint64SemAnn
int8ConstMinux128 = Constant (I (TInteger (-128) DecRepr) (Just Int8)) int8SemAnn
int16Const1024 = Constant (I (TInteger 1024 DecRepr) (Just Int16)) int16SemAnn
int32ConstMinux1024 = Constant (I (TInteger (-1024) DecRepr) (Just Int32)) int32SemAnn
int64ConstMinux3000000000 = Constant (I (TInteger (-3000000000) DecRepr) (Just Int64)) int64SemAnn
charConsta = Constant (C 'a') charSemAnn
trueBool = Constant (B True) boolSemAnn
falseBool = Constant (B False) boolSemAnn

renderExpression :: Expression SemanticAnn -> Text
renderExpression expr = 
  case runReaderT (genExpression expr) empty of
    Left err -> pack $ show err
    Right cExpr -> render $ runReader (pprint cExpr) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing constant expressions" $ do
    it "Prints the constant 0x8 of type u8" $ do
      renderExpression uint8Const0x8 `shouldBe`
        pack "0x8"
    it "Prints the constant 1024 of type u16" $ do
      renderExpression uint16Const1024 `shouldBe`
        pack "1024"
    it "Prints the constant 0xFFFF0000 of type u32" $ do
      renderExpression uint32Const0xFFFF0000 `shouldBe`
        pack "0xFFFF0000"
    it "Prints the constant 1800000000 of type u64" $ do
      renderExpression uint64Const1800000000 `shouldBe`
        pack "1800000000"
    it "Prints the constant -128 of type i8" $ do
      renderExpression int8ConstMinux128 `shouldBe`
        pack "-128"
    it "Prints the constant 1024 of type i16" $ do
      renderExpression int16Const1024 `shouldBe`
        pack "1024"
    it "Prints the constant -1024 of type i32" $ do
      renderExpression int32ConstMinux1024 `shouldBe`
        pack "-1024"
    it "Prints the constant -3000000000 of type i64" $ do
      renderExpression int64ConstMinux3000000000 `shouldBe`
        pack "-3000000000"
    it "Prints the constant 'a' of type char" $ do
      renderExpression charConsta `shouldBe`
        pack "'a'"
    it "Prints the constant true of type bool" $ do
      renderExpression trueBool `shouldBe`
        pack "1"
    it "Prints the constant false of type bool" $ do
      renderExpression falseBool `shouldBe`
        pack "0"  