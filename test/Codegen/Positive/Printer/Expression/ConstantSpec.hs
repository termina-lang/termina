module Codegen.Positive.Printer.Expression.ConstantSpec (spec) where

import Codegen.Positive.Printer.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

uint8Const0x8, uint16Const1024, uint32Const0xFFFF0000,
  uint64Const1800000000, int8ConstMinux128, int16Const1024,
  int32ConstMinux1024, int64ConstMinux3000000000,
  charConsta, charConstHigh, charConstEsc, charConstLow, charConstQuote,
  charConstBackslash, charConstNewline, charConstNull,
  trueBool, falseBool :: Expression SemanticAnn
uint8Const0x8 = Constant (I (TInteger 0x08 HexRepr) (Just TUInt8)) uint8ExprSemAnn
uint16Const1024 = Constant (I (TInteger 1024 DecRepr) (Just TUInt16)) uint16ExprSemAnn
uint32Const0xFFFF0000 = Constant (I (TInteger 0xFFFF0000 HexRepr) (Just TUInt32)) uint32ExprSemAnn
uint64Const1800000000 = Constant (I (TInteger 1800000000 DecRepr) (Just TUInt64)) uint64ExprSemAnn
int8ConstMinux128 = Constant (I (TInteger (-128) DecRepr) (Just TInt8)) int8ExprSemAnn
int16Const1024 = Constant (I (TInteger 1024 DecRepr) (Just TInt16)) int16ExprSemAnn
int32ConstMinux1024 = Constant (I (TInteger (-1024) DecRepr) (Just TInt32)) int32ExprSemAnn
int64ConstMinux3000000000 = Constant (I (TInteger (-3000000000) DecRepr) (Just TInt64)) int64ExprSemAnn
charConsta = Constant (C 'a') charExprSemAnn
charConstHigh = Constant (C '\xC8') charExprSemAnn         -- a high byte (200); printer-level defensive case (source char literals are ASCII-capped by SE-217)
charConstEsc = Constant (C '\ESC') charExprSemAnn          -- a control character (ESC, 0x1B)
charConstLow = Constant (C '\x05') charExprSemAnn          -- a low value (0x05): exercises zero-padding of the hex escape to two digits
charConstQuote = Constant (C '\'') charExprSemAnn
charConstBackslash = Constant (C '\\') charExprSemAnn
charConstNewline = Constant (C '\n') charExprSemAnn
charConstNull = Constant (C '\0') charExprSemAnn
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
    it "Prints a non-ASCII char (200) as a terminated hex escape, not a wrong octal" $ do
      renderExpression charConstHigh `shouldBe`
        pack "'\\xC8'"
    it "Prints a control char (ESC) as a hex escape, not a C-invalid named escape" $ do
      renderExpression charConstEsc `shouldBe`
        pack "'\\x1B'"
    it "Zero-pads a short hex escape to two digits" $ do
      renderExpression charConstLow `shouldBe`
        pack "'\\x05'"
    it "Escapes a single quote in a char constant" $ do
      renderExpression charConstQuote `shouldBe`
        pack "'\\''"
    it "Escapes a backslash in a char constant" $ do
      renderExpression charConstBackslash `shouldBe`
        pack "'\\\\'"
    it "Prints a newline using the C simple escape" $ do
      renderExpression charConstNewline `shouldBe`
        pack "'\\n'"
    it "Prints NUL as the '\\0' escape" $ do
      renderExpression charConstNull `shouldBe`
        pack "'\\0'"
    it "Prints the constant true of type bool" $ do
      renderExpression trueBool `shouldBe`
        pack "true"
    it "Prints the constant false of type bool" $ do
      renderExpression falseBool `shouldBe`
        pack "false"  