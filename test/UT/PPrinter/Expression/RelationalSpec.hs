module UT.PPrinter.Expression.RelationalSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

uint16Const1024 :: Expression SemanticAnns
uint16Const1024 = Constant (I UInt16 1024) uint16SemAnn

var0, var1 :: Object' Expression SemanticAnns
var0 = Variable "var0" uint16SemAnn
var1 = Variable "var1" dynUInt16SemAnn

undynVar1 :: Object' Expression SemanticAnns
undynVar1 = Undyn var1 uint16SemAnn

trueBool, falseBool :: Expression SemanticAnns
trueBool = Constant (B True) boolSemAnn
falseBool = Constant (B False) boolSemAnn

var0EqConstant, constantEqVar0, var1EqConstant, 
  constantEqVar1, var0EqVar1 :: Expression SemanticAnns
var0EqConstant = BinOp RelationalEqual (AccessObject (RHS var0)) uint16Const1024 boolSemAnn
constantEqVar0 = BinOp RelationalEqual uint16Const1024 (AccessObject (RHS var0)) boolSemAnn
var1EqConstant = BinOp RelationalEqual (AccessObject (RHS undynVar1)) uint16Const1024 boolSemAnn
constantEqVar1 = BinOp RelationalEqual uint16Const1024 (AccessObject (RHS undynVar1)) boolSemAnn
var0EqVar1 = BinOp RelationalEqual (AccessObject (RHS var0)) (AccessObject (RHS undynVar1)) boolSemAnn

var0NeqConstant, constantNeqVar0, var1NeqConstant, 
  constantNeqVar1, var0NeqVar1 :: Expression SemanticAnns
var0NeqConstant = BinOp RelationalNotEqual (AccessObject (RHS var0)) uint16Const1024 boolSemAnn
constantNeqVar0 = BinOp RelationalNotEqual uint16Const1024 (AccessObject (RHS var0)) boolSemAnn
var1NeqConstant = BinOp RelationalNotEqual (AccessObject (RHS undynVar1)) uint16Const1024 boolSemAnn
constantNeqVar1 = BinOp RelationalNotEqual uint16Const1024 (AccessObject (RHS undynVar1)) boolSemAnn
var0NeqVar1 = BinOp RelationalNotEqual (AccessObject (RHS var0)) (AccessObject (RHS undynVar1)) boolSemAnn

var0GTConstant, constantGTVar0, var1GTConstant, 
  constantGTVar1, var0GTVar1 :: Expression SemanticAnns
var0GTConstant = BinOp RelationalGT (AccessObject (RHS var0)) uint16Const1024 boolSemAnn
constantGTVar0 = BinOp RelationalGT uint16Const1024 (AccessObject (RHS var0)) boolSemAnn
var1GTConstant = BinOp RelationalGT (AccessObject (RHS undynVar1)) uint16Const1024 boolSemAnn
constantGTVar1 = BinOp RelationalGT uint16Const1024 (AccessObject (RHS undynVar1)) boolSemAnn
var0GTVar1 = BinOp RelationalGT (AccessObject (RHS var0)) (AccessObject (RHS undynVar1)) boolSemAnn

var0GTEConstant, constantGTEVar0, var1GTEConstant, 
  constantGTEVar1, var0GTEVar1 :: Expression SemanticAnns
var0GTEConstant = BinOp RelationalGTE (AccessObject (RHS var0)) uint16Const1024 boolSemAnn
constantGTEVar0 = BinOp RelationalGTE uint16Const1024 (AccessObject (RHS var0)) boolSemAnn
var1GTEConstant = BinOp RelationalGTE (AccessObject (RHS undynVar1)) uint16Const1024 boolSemAnn
constantGTEVar1 = BinOp RelationalGTE uint16Const1024 (AccessObject (RHS undynVar1)) boolSemAnn
var0GTEVar1 = BinOp RelationalGTE (AccessObject (RHS var0)) (AccessObject (RHS undynVar1)) boolSemAnn

var0LTConstant, constantLTVar0, var1LTConstant, 
  constantLTVar1, var0LTVar1 :: Expression SemanticAnns
var0LTConstant = BinOp RelationalLT (AccessObject (RHS var0)) uint16Const1024 boolSemAnn
constantLTVar0 = BinOp RelationalLT uint16Const1024 (AccessObject (RHS var0)) boolSemAnn
var1LTConstant = BinOp RelationalLT (AccessObject (RHS undynVar1)) uint16Const1024 boolSemAnn
constantLTVar1 = BinOp RelationalLT uint16Const1024 (AccessObject (RHS undynVar1)) boolSemAnn
var0LTVar1 = BinOp RelationalLT (AccessObject (RHS var0)) (AccessObject (RHS undynVar1)) boolSemAnn

var0LTEConstant, constantLTEVar0, var1LTEConstant, 
  constantLTEVar1, var0LTEVar1 :: Expression SemanticAnns
var0LTEConstant = BinOp RelationalLTE (AccessObject (RHS var0)) uint16Const1024 boolSemAnn
constantLTEVar0 = BinOp RelationalLTE uint16Const1024 (AccessObject (RHS var0)) boolSemAnn
var1LTEConstant = BinOp RelationalLTE (AccessObject (RHS undynVar1)) uint16Const1024 boolSemAnn
constantLTEVar1 = BinOp RelationalLTE uint16Const1024 (AccessObject (RHS undynVar1)) boolSemAnn
var0LTEVar1 = BinOp RelationalLTE (AccessObject (RHS var0)) (AccessObject (RHS undynVar1)) boolSemAnn

logicalAndConst, logicalAndExpr :: Expression SemanticAnns
logicalAndConst = BinOp LogicalAnd trueBool falseBool boolSemAnn
logicalAndExpr = BinOp LogicalAnd var0EqVar1 var0LTEConstant boolSemAnn

logicalOrConst, logicalOrExpr :: Expression SemanticAnns
logicalOrConst = BinOp LogicalOr falseBool trueBool boolSemAnn
logicalOrExpr = BinOp LogicalOr var1LTConstant var0LTEConstant boolSemAnn

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression empty

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
        pack "var0 == *((uint16_t *)var1.datum) && var0 <= (uint16_t)1024"
  describe "Pretty printing logical or expressions" $ do
    it "Prints the expression: false || true" $ do
      renderExpression logicalOrConst `shouldBe`
        pack "0 || 1"
    it "Prints the expression: var1 < 1024 || var2 == var3" $ do
      renderExpression logicalOrExpr `shouldBe`
        pack "*((uint16_t *)var1.datum) < (uint16_t)1024 || var0 <= (uint16_t)1024"
  