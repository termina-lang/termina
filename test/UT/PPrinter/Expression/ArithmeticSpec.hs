module UT.PPrinter.Expression.ArithmeticSpec (spec) where

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

var0PlusConstant :: Expression SemanticAnns
var0PlusConstant = BinOp Addition var0 uint16Const (SemAnn undefined uint16TS)

constantPlusVar0 :: Expression SemanticAnns
constantPlusVar0 = BinOp Addition uint16Const var0 (SemAnn undefined uint16TS)

var1PlusConstant :: Expression SemanticAnns
var1PlusConstant = BinOp Addition var1 uint16Const (SemAnn undefined uint16TS)

constantPlusVar1 :: Expression SemanticAnns
constantPlusVar1 = BinOp Addition uint16Const var1 (SemAnn undefined uint16TS)

var0PlusVar1 :: Expression SemanticAnns
var0PlusVar1 = BinOp Addition var0 var1 (SemAnn undefined uint16TS)

var0PlusVar1PlusConstant :: Expression SemanticAnns
var0PlusVar1PlusConstant = BinOp Addition var0PlusVar1 uint16Const (SemAnn undefined uint16TS)

var0MinusConstant :: Expression SemanticAnns
var0MinusConstant = BinOp Subtraction var0 uint16Const (SemAnn undefined uint16TS)

constantMinusVar0 :: Expression SemanticAnns
constantMinusVar0 = BinOp Subtraction uint16Const var0 (SemAnn undefined uint16TS)

var0MultConstant :: Expression SemanticAnns
var0MultConstant = BinOp Multiplication var0 uint16Const (SemAnn undefined uint16TS)

constantMultVar0 :: Expression SemanticAnns
constantMultVar0 = BinOp Multiplication uint16Const var0 (SemAnn undefined uint16TS)

var0MultVar1 :: Expression SemanticAnns
var0MultVar1 = BinOp Multiplication var0 var1 (SemAnn undefined uint16TS)

var1Divconstant :: Expression SemanticAnns
var1Divconstant = BinOp Division var1 uint16Const (SemAnn undefined uint16TS)

var0DivVar1 :: Expression SemanticAnns
var0DivVar1 = BinOp Division var0 var1 (SemAnn undefined uint16TS)

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppRootExpression

spec :: Spec
spec = do
  describe "Pretty printing variable expression" $ do
    it "Prints the expression: var0 + 1024 : u16" $ do
      renderExpression var0PlusConstant `shouldBe`
        pack "var0 + (uint16_t)1024"
    it "Prints the expression: var1 + 1024 : u16" $ do
      renderExpression var1PlusConstant `shouldBe`
        pack "*((uint16_t *)var1.datum) + (uint16_t)1024"
    it "Prints the expression: 1024 : u16 + var0" $ do
      renderExpression constantPlusVar0 `shouldBe`
        pack "(uint16_t)1024 + var0"
    it "Prints the expression: 1024 : u16 + var1" $ do
      renderExpression constantPlusVar1 `shouldBe`
        pack "(uint16_t)1024 + *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 + var1 : u16" $ do
      renderExpression var0PlusVar1 `shouldBe`
        pack "var0 + *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 + var1 + 1024 : u16" $ do
      renderExpression var0PlusVar1PlusConstant `shouldBe`
        pack "var0 + *((uint16_t *)var1.datum) + (uint16_t)1024"
    it "Prints the expression: var0 - 1024 : u16" $ do
      renderExpression var0MinusConstant `shouldBe`
        pack "var0 - (uint16_t)1024"
    it "Prints the expression: 1024 : u16 - var0" $ do
      renderExpression constantMinusVar0 `shouldBe`
        pack "(uint16_t)1024 - var0"
    it "Prints the expression: var0 * 1024 : u16" $ do
      renderExpression var0MultConstant `shouldBe`
        pack "var0 * (uint16_t)1024"
    it "Prints the expression: 1024 : u16 * var0" $ do
      renderExpression constantMultVar0 `shouldBe`
        pack "(uint16_t)1024 * var0"
    it "Prints the expression: var0 * var1 : u16" $ do
      renderExpression var0MultVar1 `shouldBe`
        pack "var0 * *((uint16_t *)var1.datum)"
    it "Prints the expression: var1 / 1024 : u16" $ do
      renderExpression var1Divconstant `shouldBe`
        pack "*((uint16_t *)var1.datum) / (uint16_t)1024"
    it "Prints the expression: var0 / var1 : u16" $ do
      renderExpression var0DivVar1 `shouldBe`
        pack "var0 / *((uint16_t *)var1.datum)"
        
    