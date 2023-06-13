module UT.PPrinter.Expression.BitwiseSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Data.Text
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

var0 :: Expression SemanticAnns
var0 = Variable "var0" (SemAnn undefined uint16TS)

var1 :: Expression SemanticAnns
var1 = Variable "var1" (SemAnn undefined (DynamicSubtype uint16TS))

var0LeftShiftConstant :: Expression SemanticAnns
var0LeftShiftConstant = BinOp BitwiseLeftShift var0 uint8Const (SemAnn undefined uint16TS)

constantLeftShiftVar0 :: Expression SemanticAnns
constantLeftShiftVar0 = BinOp BitwiseLeftShift uint8Const var0 (SemAnn undefined uint16TS)

var1LeftShiftConstant :: Expression SemanticAnns
var1LeftShiftConstant = BinOp BitwiseLeftShift var1 uint8Const (SemAnn undefined uint16TS)

constantLeftShiftVar1 :: Expression SemanticAnns
constantLeftShiftVar1 = BinOp BitwiseLeftShift uint8Const var1 (SemAnn undefined uint16TS)

var0LeftShiftVar1 :: Expression SemanticAnns
var0LeftShiftVar1 = BinOp BitwiseLeftShift var0 var1 (SemAnn undefined uint16TS)

var0LeftShiftVar1LeftShiftConstant :: Expression SemanticAnns
var0LeftShiftVar1LeftShiftConstant = BinOp BitwiseLeftShift var0LeftShiftVar1 uint8Const (SemAnn undefined uint16TS)

var0RightShiftConstant :: Expression SemanticAnns
var0RightShiftConstant = BinOp BitwiseRightShift var0 uint8Const (SemAnn undefined uint16TS)

constantRightShiftVar0 :: Expression SemanticAnns
constantRightShiftVar0 = BinOp BitwiseRightShift uint8Const var0 (SemAnn undefined uint16TS)

var0BitwiseAndConstant :: Expression SemanticAnns
var0BitwiseAndConstant = BinOp BitwiseAnd var0 uint16Const (SemAnn undefined uint16TS)

constantBitwiseAndVar0 :: Expression SemanticAnns
constantBitwiseAndVar0 = BinOp BitwiseAnd uint16Const var0 (SemAnn undefined uint16TS)

var0BitwiseAndVar1 :: Expression SemanticAnns
var0BitwiseAndVar1 = BinOp BitwiseAnd var0 var1 (SemAnn undefined uint16TS)

var1BitwiseOrconstant :: Expression SemanticAnns
var1BitwiseOrconstant = BinOp BitwiseOr var1 uint16Const (SemAnn undefined uint16TS)

var0BitwiseOrVar1 :: Expression SemanticAnns
var0BitwiseOrVar1 = BinOp BitwiseOr var0 var1 (SemAnn undefined uint16TS)

var1BitwiseXorconstant :: Expression SemanticAnns
var1BitwiseXorconstant = BinOp BitwiseXor var1 uint16Const (SemAnn undefined uint16TS)

var0BitwiseXorVar1 :: Expression SemanticAnns
var0BitwiseXorVar1 = BinOp BitwiseXor var0 var1 (SemAnn undefined uint16TS)

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression

spec :: Spec
spec = do
  describe "Pretty printing variable expression" $ do
    it "Prints the expression: var0 << 8 : u8" $ do
      renderExpression var0LeftShiftConstant `shouldBe`
        pack "var0 << (uint8_t)8"
    it "Prints the expression: var1 << 8 : u8" $ do
      renderExpression var1LeftShiftConstant `shouldBe`
        pack "*((uint16_t *)var1.datum) << (uint8_t)8"
    it "Prints the expression: 8 : u8 << var0" $ do
      renderExpression constantLeftShiftVar0 `shouldBe`
        pack "(uint8_t)8 << var0"
    it "Prints the expression: 8 : u8 << var1" $ do
      renderExpression constantLeftShiftVar1 `shouldBe`
        pack "(uint8_t)8 << *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 << var1 : u16" $ do
      renderExpression var0LeftShiftVar1 `shouldBe`
        pack "var0 << *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 << var1 << 8 : u8" $ do
      renderExpression var0LeftShiftVar1LeftShiftConstant `shouldBe`
        pack "var0 << *((uint16_t *)var1.datum) << (uint8_t)8"
    it "Prints the expression: var0 >> 8 : u8" $ do
      renderExpression var0RightShiftConstant `shouldBe`
        pack "var0 >> (uint8_t)8"
    it "Prints the expression: 1024 : u16 >> var0" $ do
      renderExpression constantRightShiftVar0 `shouldBe`
        pack "(uint8_t)8 >> var0"
    it "Prints the expression: var0 & 1024 : u16" $ do
      renderExpression var0BitwiseAndConstant `shouldBe`
        pack "var0 & (uint16_t)1024"
    it "Prints the expression: 1024 : u16 & var0" $ do
      renderExpression constantBitwiseAndVar0 `shouldBe`
        pack "(uint16_t)1024 & var0"
    it "Prints the expression: var0 & var1 : u16" $ do
      renderExpression var0BitwiseAndVar1 `shouldBe`
        pack "var0 & *((uint16_t *)var1.datum)"
    it "Prints the expression: var1 | 1024 : u16" $ do
      renderExpression var1BitwiseOrconstant `shouldBe`
        pack "*((uint16_t *)var1.datum) | (uint16_t)1024"
    it "Prints the expression: var0 | var1 : u16" $ do
      renderExpression var0BitwiseOrVar1 `shouldBe`
        pack "var0 | *((uint16_t *)var1.datum)"
    it "Prints the expression: var1 ^ 1024 : u16" $ do
      renderExpression var1BitwiseXorconstant `shouldBe`
        pack "*((uint16_t *)var1.datum) ^ (uint16_t)1024"
    it "Prints the expression: var0 ^ var1 : u16" $ do
      renderExpression var0BitwiseXorVar1 `shouldBe`
        pack "var0 ^ *((uint16_t *)var1.datum)"
        
    