module UT.PPrinter.Expression.ConstantSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Control.Monad.Reader
import Generator.Expression
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

uint8Const0x8, uint16Const1024, uint32Const0xFFFF0000,
  uint64Const1800000000, int8ConstMinux128, int16Const1024,
  int32ConstMinux1024, int64ConstMinux3000000000,
  charConsta, trueBool, falseBool :: Expression SemanticAnns
uint8Const0x8 = Constant (I UInt8 (TInteger 0x08 HexRepr)) uint8SemAnn
uint16Const1024 = Constant (I UInt16 (TInteger 1024 DecRepr)) uint16SemAnn
uint32Const0xFFFF0000 = Constant (I UInt32 (TInteger 0xFFFF0000 HexRepr)) uint32SemAnn
uint64Const1800000000 = Constant (I UInt64 (TInteger 1800000000 DecRepr)) uint64SemAnn
int8ConstMinux128 = Constant (I Int8 (TInteger (-128) DecRepr)) int8SemAnn
int16Const1024 = Constant (I Int16 (TInteger 1024 DecRepr)) int16SemAnn
int32ConstMinux1024 = Constant (I Int32 (TInteger (-1024) DecRepr)) int32SemAnn
int64ConstMinux3000000000 = Constant (I Int64 (TInteger (-3000000000) DecRepr)) int64SemAnn
charConsta = Constant (C 'a') charSemAnn
trueBool = Constant (B True) boolSemAnn
falseBool = Constant (B False) boolSemAnn

renderExpression :: Expression SemanticAnns -> Text
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
        pack "0xffff0000"
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