module UT.PPrinter.Expression.RelationalSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Data.Text
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

var0, var1, var2, var3, var4, var5 :: Expression SemanticAnns
var0 = Variable "var0" (SemAnn undefined uint16TS)
var1 = Variable "var1" (SemAnn undefined (DynamicSubtype uint16TS))
var2 = Variable "var2" (SemAnn undefined tmDescriptorTS)
var3 = Variable "var3" (SemAnn undefined tmDescriptorTS)
var4 = Variable "var4" (SemAnn undefined dynamicTMDescriptorTS)
var5 = Variable "var5" (SemAnn undefined dynamicTMDescriptorTS)

var0EqConstant, constantEqVar0, var1EqConstant, 
  constantEqVar1, var0EqVar1 :: Expression SemanticAnns
var0EqConstant = BinOp RelationalEqual var0 uint16Const (SemAnn undefined Bool)
constantEqVar0 = BinOp RelationalEqual uint16Const var0 (SemAnn undefined Bool)
var1EqConstant = BinOp RelationalEqual var1 uint16Const (SemAnn undefined Bool)
constantEqVar1 = BinOp RelationalEqual uint16Const var1 (SemAnn undefined Bool)
var0EqVar1 = BinOp RelationalEqual var0 var1 (SemAnn undefined Bool)

var2EqVar3, var2EqVar4, var4EqVar2, var4EqVar5 :: Expression SemanticAnns
var2EqVar3 = BinOp RelationalEqual var2 var3 (SemAnn undefined Bool)
var2EqVar4 = BinOp RelationalEqual var2 var4 (SemAnn undefined Bool)
var4EqVar2 = BinOp RelationalEqual var4 var2 (SemAnn undefined Bool)
var4EqVar5 = BinOp RelationalEqual var4 var5 (SemAnn undefined Bool)

var0NeqConstant, constantNeqVar0, var1NeqConstant, 
  constantNeqVar1, var0NeqVar1 :: Expression SemanticAnns
var0NeqConstant = BinOp RelationalNotEqual var0 uint16Const (SemAnn undefined Bool)
constantNeqVar0 = BinOp RelationalNotEqual uint16Const var0 (SemAnn undefined Bool)
var1NeqConstant = BinOp RelationalNotEqual var1 uint16Const (SemAnn undefined Bool)
constantNeqVar1 = BinOp RelationalNotEqual uint16Const var1 (SemAnn undefined Bool)
var0NeqVar1 = BinOp RelationalNotEqual var0 var1 (SemAnn undefined Bool)

var2NeqVar3, var2NeqVar4, var4NeqVar2, var4NeqVar5 :: Expression SemanticAnns
var2NeqVar3 = BinOp RelationalNotEqual var2 var3 (SemAnn undefined Bool)
var2NeqVar4 = BinOp RelationalNotEqual var2 var4 (SemAnn undefined Bool)
var4NeqVar2 = BinOp RelationalNotEqual var4 var2 (SemAnn undefined Bool)
var4NeqVar5 = BinOp RelationalNotEqual var4 var5 (SemAnn undefined Bool)

var0GTConstant, constantGTVar0, var1GTConstant, 
  constantGTVar1, var0GTVar1 :: Expression SemanticAnns
var0GTConstant = BinOp RelationalGT var0 uint16Const (SemAnn undefined Bool)
constantGTVar0 = BinOp RelationalGT uint16Const var0 (SemAnn undefined Bool)
var1GTConstant = BinOp RelationalGT var1 uint16Const (SemAnn undefined Bool)
constantGTVar1 = BinOp RelationalGT uint16Const var1 (SemAnn undefined Bool)
var0GTVar1 = BinOp RelationalGT var0 var1 (SemAnn undefined Bool)

var0GTEConstant, constantGTEVar0, var1GTEConstant, 
  constantGTEVar1, var0GTEVar1 :: Expression SemanticAnns
var0GTEConstant = BinOp RelationalGTE var0 uint16Const (SemAnn undefined Bool)
constantGTEVar0 = BinOp RelationalGTE uint16Const var0 (SemAnn undefined Bool)
var1GTEConstant = BinOp RelationalGTE var1 uint16Const (SemAnn undefined Bool)
constantGTEVar1 = BinOp RelationalGTE uint16Const var1 (SemAnn undefined Bool)
var0GTEVar1 = BinOp RelationalGTE var0 var1 (SemAnn undefined Bool)

var0LTConstant, constantLTVar0, var1LTConstant, 
  constantLTVar1, var0LTVar1 :: Expression SemanticAnns
var0LTConstant = BinOp RelationalLT var0 uint16Const (SemAnn undefined Bool)
constantLTVar0 = BinOp RelationalLT uint16Const var0 (SemAnn undefined Bool)
var1LTConstant = BinOp RelationalLT var1 uint16Const (SemAnn undefined Bool)
constantLTVar1 = BinOp RelationalLT uint16Const var1 (SemAnn undefined Bool)
var0LTVar1 = BinOp RelationalLT var0 var1 (SemAnn undefined Bool)

var0LTEConstant, constantLTEVar0, var1LTEConstant, 
  constantLTEVar1, var0LTEVar1 :: Expression SemanticAnns
var0LTEConstant = BinOp RelationalLTE var0 uint16Const (SemAnn undefined Bool)
constantLTEVar0 = BinOp RelationalLTE uint16Const var0 (SemAnn undefined Bool)
var1LTEConstant = BinOp RelationalLTE var1 uint16Const (SemAnn undefined Bool)
constantLTEVar1 = BinOp RelationalLTE uint16Const var1 (SemAnn undefined Bool)
var0LTEVar1 = BinOp RelationalLTE var0 var1 (SemAnn undefined Bool)

logicalAndConst, logicalAndExpr :: Expression SemanticAnns
logicalAndConst = BinOp LogicalAnd trueBool falseBool (SemAnn undefined Bool)
logicalAndExpr = BinOp LogicalAnd var0EqVar1 var4NeqVar5 (SemAnn undefined Bool)

logicalOrConst, logicalOrExpr :: Expression SemanticAnns
logicalOrConst = BinOp LogicalOr falseBool trueBool (SemAnn undefined Bool)
logicalOrExpr = BinOp LogicalOr var1LTConstant var2EqVar3 (SemAnn undefined Bool)

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppRootExpression

spec :: Spec
spec = do
  describe "Pretty printing equality expressions" $ do
    it "Prints the expression: var0 == 1024 : u16" $ do
      renderExpression var0EqConstant `shouldBe`
        pack "var0 == (uint16_t)1024"
    it "Prints the expression: var1 == 1024 : u16 (var1 : 'dyn u16)" $ do
      renderExpression var1EqConstant `shouldBe`
        pack "*((uint16_t *)var1.datum) == (uint16_t)1024"
    it "Prints the expression: 1024 : u16 == var0" $ do
      renderExpression constantEqVar0 `shouldBe`
        pack "(uint16_t)1024 == var0"
    it "Prints the expression: 1024 : u16 == var1 (var1 : 'dyn u16)" $ do
      renderExpression constantEqVar1 `shouldBe`
        pack "(uint16_t)1024 == *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 == var1 (var1 : 'dyn u16)" $ do
      renderExpression var0EqVar1 `shouldBe`
        pack "var0 == *((uint16_t *)var1.datum)"
    it "Prints the expression: var2 == var3 (struct == struct)" $ do
      renderExpression var2EqVar3 `shouldBe`
        pack "(__TMDescriptor__eq(&var2, &var3) == 1)"
    it "Prints the expression: var2 == var4 (struct == 'dyn struct)" $ do
      renderExpression var2EqVar4 `shouldBe`
        pack "(__TMDescriptor__eq(&var2, (TMDescriptor *)var4.datum) == 1)"
    it "Prints the expression: var4 == var2 ('dyn struct == struct)" $ do
      renderExpression var4EqVar2 `shouldBe`
        pack "(__TMDescriptor__eq((TMDescriptor *)var4.datum, &var2) == 1)"
    it "Prints the expression: var4 == var5 ('dyn struct == 'dyn struct)" $ do
      renderExpression var4EqVar5 `shouldBe`
        pack "(__TMDescriptor__eq((TMDescriptor *)var4.datum,\n                    (TMDescriptor *)var5.datum) == 1)"
  describe "Pretty printing not-equality expressions" $ do
    it "Prints the expression: var0 != 1024 : u16" $ do
      renderExpression var0NeqConstant `shouldBe`
        pack "var0 != (uint16_t)1024"
    it "Prints the expression: var1 != 1024 : u16" $ do
      renderExpression var1NeqConstant `shouldBe`
        pack "*((uint16_t *)var1.datum) != (uint16_t)1024"
    it "Prints the expression: 1024 : u16 != var0" $ do
      renderExpression constantNeqVar0 `shouldBe`
        pack "(uint16_t)1024 != var0"
    it "Prints the expression: 1024 : u16 != var1" $ do
      renderExpression constantNeqVar1 `shouldBe`
        pack "(uint16_t)1024 != *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 != var1 : u16" $ do
      renderExpression var0NeqVar1 `shouldBe`
        pack "var0 != *((uint16_t *)var1.datum)"
    it "Prints the expression: var2 != var3 (struct == struct)" $ do
      renderExpression var2NeqVar3 `shouldBe`
        pack "(__TMDescriptor__eq(&var2, &var3) == 0)"
    it "Prints the expression: var2 != var4 (struct == 'dyn struct)" $ do
      renderExpression var2NeqVar4 `shouldBe`
        pack "(__TMDescriptor__eq(&var2, (TMDescriptor *)var4.datum) == 0)"
    it "Prints the expression: var4 != var2 ('dyn struct == struct)" $ do
      renderExpression var4NeqVar2 `shouldBe`
        pack "(__TMDescriptor__eq((TMDescriptor *)var4.datum, &var2) == 0)"
    it "Prints the expression: var4 != var5 ('dyn struct == 'dyn struct)" $ do
      renderExpression var4NeqVar5 `shouldBe`
        pack "(__TMDescriptor__eq((TMDescriptor *)var4.datum,\n                    (TMDescriptor *)var5.datum) == 0)"
  describe "Pretty printing greater than expressions" $ do
    it "Prints the expression: var0 > 1024 : u16" $ do
      renderExpression var0GTConstant `shouldBe`
        pack "var0 > (uint16_t)1024"
    it "Prints the expression: var1 > 1024 : u16 (var1 : 'dyn u16)" $ do
      renderExpression var1GTConstant `shouldBe`
        pack "*((uint16_t *)var1.datum) > (uint16_t)1024"
    it "Prints the expression: 1024 : u16 > var0" $ do
      renderExpression constantGTVar0 `shouldBe`
        pack "(uint16_t)1024 > var0"
    it "Prints the expression: 1024 : u16 > var1 (var1 : 'dyn u16)" $ do
      renderExpression constantGTVar1 `shouldBe`
        pack "(uint16_t)1024 > *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 > var1 : u16 (var1 : 'dyn u16)" $ do
      renderExpression var0GTVar1 `shouldBe`
        pack "var0 > *((uint16_t *)var1.datum)"
  describe "Pretty printing greater than or equal expressions" $ do
    it "Prints the expression: var0 >= 1024 : u16" $ do
      renderExpression var0GTEConstant `shouldBe`
        pack "var0 >= (uint16_t)1024"
    it "Prints the expression: var1 >= 1024 : u16 (var1 : 'dyn u16)" $ do
      renderExpression var1GTEConstant `shouldBe`
        pack "*((uint16_t *)var1.datum) >= (uint16_t)1024"
    it "Prints the expression: 1024 : u16 >= var0" $ do
      renderExpression constantGTEVar0 `shouldBe`
        pack "(uint16_t)1024 >= var0"
    it "Prints the expression: 1024 : u16 >= var1 (var1 : 'dyn u16)" $ do
      renderExpression constantGTEVar1 `shouldBe`
        pack "(uint16_t)1024 >= *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 >= var1 : u16 (var1 : 'dyn u16)" $ do
      renderExpression var0GTEVar1 `shouldBe`
        pack "var0 >= *((uint16_t *)var1.datum)"
  describe "Pretty printing less than expressions" $ do
    it "Prints the expression: var0 < 1024 : u16" $ do
      renderExpression var0LTConstant `shouldBe`
        pack "var0 < (uint16_t)1024"
    it "Prints the expression: var1 < 1024 : u16 (var1 : 'dyn u16)" $ do
      renderExpression var1LTConstant `shouldBe`
        pack "*((uint16_t *)var1.datum) < (uint16_t)1024"
    it "Prints the expression: 1024 : u16 < var0" $ do
      renderExpression constantLTVar0 `shouldBe`
        pack "(uint16_t)1024 < var0"
    it "Prints the expression: 1024 : u16 < var1 (var1 : 'dyn u16)" $ do
      renderExpression constantLTVar1 `shouldBe`
        pack "(uint16_t)1024 < *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 < var1 : u16 (var1 : 'dyn u16)" $ do
      renderExpression var0LTVar1 `shouldBe`
        pack "var0 < *((uint16_t *)var1.datum)"
  describe "Pretty printing less than expressions" $ do
    it "Prints the expression: var0 < 1024 : u16" $ do
      renderExpression var0LTConstant `shouldBe`
        pack "var0 < (uint16_t)1024"
    it "Prints the expression: var1 < 1024 : u16 (var1 : 'dyn u16)" $ do
      renderExpression var1LTConstant `shouldBe`
        pack "*((uint16_t *)var1.datum) < (uint16_t)1024"
    it "Prints the expression: 1024 : u16 < var0" $ do
      renderExpression constantLTVar0 `shouldBe`
        pack "(uint16_t)1024 < var0"
    it "Prints the expression: 1024 : u16 < var1 (var1 : 'dyn u16)" $ do
      renderExpression constantLTVar1 `shouldBe`
        pack "(uint16_t)1024 < *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 < var1 : u16 (var1 : 'dyn u16)" $ do
      renderExpression var0LTVar1 `shouldBe`
        pack "var0 < *((uint16_t *)var1.datum)"
  describe "Pretty printing less than or equal expressions" $ do
    it "Prints the expression: var0 <= 1024 : u16" $ do
      renderExpression var0LTEConstant `shouldBe`
        pack "var0 <= (uint16_t)1024"
    it "Prints the expression: var1 <= 1024 : u16" $ do
      renderExpression var1LTEConstant `shouldBe`
        pack "*((uint16_t *)var1.datum) <= (uint16_t)1024"
    it "Prints the expression: 1024 : u16 <= var0" $ do
      renderExpression constantLTEVar0 `shouldBe`
        pack "(uint16_t)1024 <= var0"
    it "Prints the expression: 1024 : u16 <= var1" $ do
      renderExpression constantLTEVar1 `shouldBe`
        pack "(uint16_t)1024 <= *((uint16_t *)var1.datum)"
    it "Prints the expression: var0 <= var1 : u16" $ do
      renderExpression var0LTEVar1 `shouldBe`
        pack "var0 <= *((uint16_t *)var1.datum)"
  describe "Pretty printing logical and expressions" $ do
    it "Prints the expression: true && false" $ do
      renderExpression logicalAndConst `shouldBe`
        pack "1 && 0"
    it "Prints the expression: var0 == var1 && var4 != var5" $ do
      renderExpression logicalAndExpr `shouldBe`
        pack (
          "var0 == *((uint16_t *)var1.datum) && (__TMDescriptor__eq((TMDescriptor *)var4.datum,\n" ++
          "                                                         (TMDescriptor *)var5.datum) == 0)")
  describe "Pretty printing logical or expressions" $ do
    it "Prints the expression: false || true" $ do
      renderExpression logicalOrConst `shouldBe`
        pack "0 || 1"
    it "Prints the expression: var1 < 1024 || var2 == var3" $ do
      renderExpression logicalOrExpr `shouldBe`
        pack (
          "*((uint16_t *)var1.datum) < (uint16_t)1024 || (__TMDescriptor__eq(&var2,\n" ++ 
          "                                                                  &var3) == 1)")
  