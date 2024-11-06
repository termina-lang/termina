module UT.PPrinter.Expression.RelationalSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

uint16Const1024 :: Expression SemanticAnn
uint16Const1024 = Constant (I (TInteger 1024 DecRepr) (Just TUInt16)) uint16ExprSemAnn

var0, var1 :: Object SemanticAnn
var0 = Variable "var0" (objSemAnn Mutable TUInt16)
var1 = Variable "var1" boxUInt16SemAnn

unboxVar1 :: Object SemanticAnn
unboxVar1 = Unbox var1 (objSemAnn Mutable TUInt16)

trueBool, falseBool :: Expression SemanticAnn
trueBool = Constant (B True) boolExprSemAnn
falseBool = Constant (B False) boolExprSemAnn

var0EqConstant, constantEqVar0, var1EqConstant, 
  constantEqVar1, var0EqVar1 :: Expression SemanticAnn
var0EqConstant = BinOp RelationalEqual (AccessObject var0) uint16Const1024 boolExprSemAnn
constantEqVar0 = BinOp RelationalEqual uint16Const1024 (AccessObject var0) boolExprSemAnn
var1EqConstant = BinOp RelationalEqual (AccessObject unboxVar1) uint16Const1024 boolExprSemAnn
constantEqVar1 = BinOp RelationalEqual uint16Const1024 (AccessObject unboxVar1) boolExprSemAnn
var0EqVar1 = BinOp RelationalEqual (AccessObject var0) (AccessObject unboxVar1) boolExprSemAnn

var0NeqConstant, constantNeqVar0, var1NeqConstant, 
  constantNeqVar1, var0NeqVar1 :: Expression SemanticAnn
var0NeqConstant = BinOp RelationalNotEqual (AccessObject var0) uint16Const1024 boolExprSemAnn
constantNeqVar0 = BinOp RelationalNotEqual uint16Const1024 (AccessObject var0) boolExprSemAnn
var1NeqConstant = BinOp RelationalNotEqual (AccessObject unboxVar1) uint16Const1024 boolExprSemAnn
constantNeqVar1 = BinOp RelationalNotEqual uint16Const1024 (AccessObject unboxVar1) boolExprSemAnn
var0NeqVar1 = BinOp RelationalNotEqual (AccessObject var0) (AccessObject unboxVar1) boolExprSemAnn

var0GTConstant, constantGTVar0, var1GTConstant, 
  constantGTVar1, var0GTVar1 :: Expression SemanticAnn
var0GTConstant = BinOp RelationalGT (AccessObject (var0)) uint16Const1024 boolExprSemAnn
constantGTVar0 = BinOp RelationalGT uint16Const1024 (AccessObject (var0)) boolExprSemAnn
var1GTConstant = BinOp RelationalGT (AccessObject (unboxVar1)) uint16Const1024 boolExprSemAnn
constantGTVar1 = BinOp RelationalGT uint16Const1024 (AccessObject (unboxVar1)) boolExprSemAnn
var0GTVar1 = BinOp RelationalGT (AccessObject (var0)) (AccessObject (unboxVar1)) boolExprSemAnn

var0GTEConstant, constantGTEVar0, var1GTEConstant, 
  constantGTEVar1, var0GTEVar1 :: Expression SemanticAnn
var0GTEConstant = BinOp RelationalGTE (AccessObject (var0)) uint16Const1024 boolExprSemAnn
constantGTEVar0 = BinOp RelationalGTE uint16Const1024 (AccessObject (var0)) boolExprSemAnn
var1GTEConstant = BinOp RelationalGTE (AccessObject (unboxVar1)) uint16Const1024 boolExprSemAnn
constantGTEVar1 = BinOp RelationalGTE uint16Const1024 (AccessObject (unboxVar1)) boolExprSemAnn
var0GTEVar1 = BinOp RelationalGTE (AccessObject (var0)) (AccessObject (unboxVar1)) boolExprSemAnn

var0LTConstant, constantLTVar0, var1LTConstant, 
  constantLTVar1, var0LTVar1 :: Expression SemanticAnn
var0LTConstant = BinOp RelationalLT (AccessObject (var0)) uint16Const1024 boolExprSemAnn
constantLTVar0 = BinOp RelationalLT uint16Const1024 (AccessObject (var0)) boolExprSemAnn
var1LTConstant = BinOp RelationalLT (AccessObject (unboxVar1)) uint16Const1024 boolExprSemAnn
constantLTVar1 = BinOp RelationalLT uint16Const1024 (AccessObject (unboxVar1)) boolExprSemAnn
var0LTVar1 = BinOp RelationalLT (AccessObject (var0)) (AccessObject (unboxVar1)) boolExprSemAnn

var0LTEConstant, constantLTEVar0, var1LTEConstant, 
  constantLTEVar1, var0LTEVar1 :: Expression SemanticAnn
var0LTEConstant = BinOp RelationalLTE (AccessObject (var0)) uint16Const1024 boolExprSemAnn
constantLTEVar0 = BinOp RelationalLTE uint16Const1024 (AccessObject (var0)) boolExprSemAnn
var1LTEConstant = BinOp RelationalLTE (AccessObject (unboxVar1)) uint16Const1024 boolExprSemAnn
constantLTEVar1 = BinOp RelationalLTE uint16Const1024 (AccessObject (unboxVar1)) boolExprSemAnn
var0LTEVar1 = BinOp RelationalLTE (AccessObject (var0)) (AccessObject (unboxVar1)) boolExprSemAnn

logicalAndConst, logicalAndExpr :: Expression SemanticAnn
logicalAndConst = BinOp LogicalAnd trueBool falseBool boolExprSemAnn
logicalAndExpr = BinOp LogicalAnd var0EqVar1 var0LTEConstant boolExprSemAnn

logicalOrConst, logicalOrExpr :: Expression SemanticAnn
logicalOrConst = BinOp LogicalOr falseBool trueBool boolExprSemAnn
logicalOrExpr = BinOp LogicalOr var1LTConstant var0LTEConstant boolExprSemAnn

spec :: Spec
spec = do
  describe "Pretty printing equality expressions" $ do
    it "Prints the expression: var0 == 1024 : u16" $ do
      renderExpression var0EqConstant `shouldBe`
        pack "var0 == 1024"
    it "Prints the expression: var1 == 1024 : u16 (var1 : 'box u16)" $ do
      renderExpression var1EqConstant `shouldBe`
        pack "*(uint16_t *)var1.data == 1024"
    it "Prints the expression: 1024 : u16 == var0" $ do
      renderExpression constantEqVar0 `shouldBe`
        pack "1024 == var0"
    it "Prints the expression: 1024 : u16 == var1 (var1 : 'box u16)" $ do
      renderExpression constantEqVar1 `shouldBe`
        pack "1024 == *(uint16_t *)var1.data"
    it "Prints the expression: var0 == var1 (var1 : 'box u16)" $ do
      renderExpression var0EqVar1 `shouldBe`
        pack "var0 == *(uint16_t *)var1.data"
  describe "Pretty printing not-equality expressions" $ do
    it "Prints the expression: var0 != 1024 : u16" $ do
      renderExpression var0NeqConstant `shouldBe`
        pack "var0 != 1024"
    it "Prints the expression: var1 != 1024 : u16" $ do
      renderExpression var1NeqConstant `shouldBe`
        pack "*(uint16_t *)var1.data != 1024"
    it "Prints the expression: 1024 : u16 != var0" $ do
      renderExpression constantNeqVar0 `shouldBe`
        pack "1024 != var0"
    it "Prints the expression: 1024 : u16 != var1" $ do
      renderExpression constantNeqVar1 `shouldBe`
        pack "1024 != *(uint16_t *)var1.data"
    it "Prints the expression: var0 != var1 : u16" $ do
      renderExpression var0NeqVar1 `shouldBe`
        pack "var0 != *(uint16_t *)var1.data"
  describe "Pretty printing greater than expressions" $ do
    it "Prints the expression: var0 > 1024 : u16" $ do
      renderExpression var0GTConstant `shouldBe`
        pack "var0 > 1024"
    it "Prints the expression: var1 > 1024 : u16 (var1 : 'box u16)" $ do
      renderExpression var1GTConstant `shouldBe`
        pack "*(uint16_t *)var1.data > 1024"
    it "Prints the expression: 1024 : u16 > var0" $ do
      renderExpression constantGTVar0 `shouldBe`
        pack "1024 > var0"
    it "Prints the expression: 1024 : u16 > var1 (var1 : 'box u16)" $ do
      renderExpression constantGTVar1 `shouldBe`
        pack "1024 > *(uint16_t *)var1.data"
    it "Prints the expression: var0 > var1 : u16 (var1 : 'box u16)" $ do
      renderExpression var0GTVar1 `shouldBe`
        pack "var0 > *(uint16_t *)var1.data"
  describe "Pretty printing greater than or equal expressions" $ do
    it "Prints the expression: var0 >= 1024 : u16" $ do
      renderExpression var0GTEConstant `shouldBe`
        pack "var0 >= 1024"
    it "Prints the expression: var1 >= 1024 : u16 (var1 : 'box u16)" $ do
      renderExpression var1GTEConstant `shouldBe`
        pack "*(uint16_t *)var1.data >= 1024"
    it "Prints the expression: 1024 : u16 >= var0" $ do
      renderExpression constantGTEVar0 `shouldBe`
        pack "1024 >= var0"
    it "Prints the expression: 1024 : u16 >= var1 (var1 : 'box u16)" $ do
      renderExpression constantGTEVar1 `shouldBe`
        pack "1024 >= *(uint16_t *)var1.data"
    it "Prints the expression: var0 >= var1 : u16 (var1 : 'box u16)" $ do
      renderExpression var0GTEVar1 `shouldBe`
        pack "var0 >= *(uint16_t *)var1.data"
  describe "Pretty printing less than expressions" $ do
    it "Prints the expression: var0 < 1024 : u16" $ do
      renderExpression var0LTConstant `shouldBe`
        pack "var0 < 1024"
    it "Prints the expression: var1 < 1024 : u16 (var1 : 'box u16)" $ do
      renderExpression var1LTConstant `shouldBe`
        pack "*(uint16_t *)var1.data < 1024"
    it "Prints the expression: 1024 : u16 < var0" $ do
      renderExpression constantLTVar0 `shouldBe`
        pack "1024 < var0"
    it "Prints the expression: 1024 : u16 < var1 (var1 : 'box u16)" $ do
      renderExpression constantLTVar1 `shouldBe`
        pack "1024 < *(uint16_t *)var1.data"
    it "Prints the expression: var0 < var1 : u16 (var1 : 'box u16)" $ do
      renderExpression var0LTVar1 `shouldBe`
        pack "var0 < *(uint16_t *)var1.data"
  describe "Pretty printing less than expressions" $ do
    it "Prints the expression: var0 < 1024 : u16" $ do
      renderExpression var0LTConstant `shouldBe`
        pack "var0 < 1024"
    it "Prints the expression: var1 < 1024 : u16 (var1 : 'box u16)" $ do
      renderExpression var1LTConstant `shouldBe`
        pack "*(uint16_t *)var1.data < 1024"
    it "Prints the expression: 1024 : u16 < var0" $ do
      renderExpression constantLTVar0 `shouldBe`
        pack "1024 < var0"
    it "Prints the expression: 1024 : u16 < var1 (var1 : 'box u16)" $ do
      renderExpression constantLTVar1 `shouldBe`
        pack "1024 < *(uint16_t *)var1.data"
    it "Prints the expression: var0 < var1 : u16 (var1 : 'box u16)" $ do
      renderExpression var0LTVar1 `shouldBe`
        pack "var0 < *(uint16_t *)var1.data"
  describe "Pretty printing less than or equal expressions" $ do
    it "Prints the expression: var0 <= 1024 : u16" $ do
      renderExpression var0LTEConstant `shouldBe`
        pack "var0 <= 1024"
    it "Prints the expression: var1 <= 1024 : u16" $ do
      renderExpression var1LTEConstant `shouldBe`
        pack "*(uint16_t *)var1.data <= 1024"
    it "Prints the expression: 1024 : u16 <= var0" $ do
      renderExpression constantLTEVar0 `shouldBe`
        pack "1024 <= var0"
    it "Prints the expression: 1024 : u16 <= var1" $ do
      renderExpression constantLTEVar1 `shouldBe`
        pack "1024 <= *(uint16_t *)var1.data"
    it "Prints the expression: var0 <= var1 : u16" $ do
      renderExpression var0LTEVar1 `shouldBe`
        pack "var0 <= *(uint16_t *)var1.data"
  describe "Pretty printing logical and expressions" $ do
    it "Prints the expression: true && false" $ do
      renderExpression logicalAndConst `shouldBe`
        pack "1 && 0"
    it "Prints the expression: var0 == var1 && var4 != var5" $ do
      renderExpression logicalAndExpr `shouldBe`
        pack "var0 == *(uint16_t *)var1.data && var0 <= 1024"
  describe "Pretty printing logical or expressions" $ do
    it "Prints the expression: false || true" $ do
      renderExpression logicalOrConst `shouldBe`
        pack "0 || 1"
    it "Prints the expression: var1 < 1024 || var2 == var3" $ do
      renderExpression logicalOrExpr `shouldBe`
        pack "*(uint16_t *)var1.data < 1024 || var0 <= 1024"
