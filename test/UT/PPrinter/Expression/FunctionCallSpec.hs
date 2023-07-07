module UT.PPrinter.Expression.FunctionCallSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

var0, var1, pVar :: Expression SemanticAnns
-- | var0 : u16
var0 = Variable "var0" uint16SemAnn
-- | dynVar1 : 'dyn u16
var1 = Variable "var1" dynUInt16SemAnn
-- | p_var : &u16
pVar = Variable "p_var" refUInt16SemAnn

undynVar1 :: Expression SemanticAnns
undynVar1 = Undyn var1 uint16SemAnn

referenceVar0, referenceVar1 :: Expression SemanticAnns
-- | &var0 : &u16
referenceVar0 = ReferenceExpression var0 refUInt16SemAnn
-- | &var1 : &u16
referenceVar1 = ReferenceExpression var1 refUInt16SemAnn

dereferencepVar, derefRefDynVar1 :: Expression SemanticAnns
-- | *p_var : u16
dereferencepVar = DereferenceExpression pVar uint16SemAnn
-- | *&var1 : u16
derefRefDynVar1 = DereferenceExpression referenceVar1 uint16SemAnn

vector0, dynVector0 :: Expression SemanticAnns
vector0 = Variable "vector0" (vectorSemAnn UInt32 (I UInt32 10))
dynVector0 = Variable "dynVector0" (dynVectorSemAnn UInt32 (I UInt32 10))

pVector1, pDynVector1 :: Expression SemanticAnns
pVector1 = Variable "p_vector1" (refVectorSemAnn UInt32 (I UInt32 10))
pDynVector1 = Variable "p_dynVector1" (refVectorSemAnn UInt32 (I UInt32 10))

referenceVector0, referenceDynVector0 :: Expression SemanticAnns
referenceVector0 = ReferenceExpression vector0 (refVectorSemAnn UInt32 (I UInt32 10))
referenceDynVector0 = ReferenceExpression dynVector0 (refVectorSemAnn UInt32 (I UInt32 10))

uint16Const :: Expression SemanticAnns
uint16Const = Constant (I UInt16 1024) uint16SemAnn

var0PlusConstant, var1PlusConstant :: Expression SemanticAnns
var0PlusConstant = BinOp Addition var0 uint16Const uint16SemAnn
var1PlusConstant = BinOp Addition undynVar1 uint16Const uint16SemAnn

dereferencepVarPlusConstant :: Expression SemanticAnns
dereferencepVarPlusConstant = BinOp Addition dereferencepVar uint16Const uint16SemAnn

var0PlusVar1, dereferencepVar2PlusderefRefVar1 :: Expression SemanticAnns
var0PlusVar1 = BinOp Addition var0 undynVar1 uint16SemAnn
dereferencepVar2PlusderefRefVar1 = BinOp Addition dereferencepVar derefRefDynVar1 uint16SemAnn

functionCallSingleVar0, functionCallSingleDynVar1,
  functionCallSinglepVar :: Expression SemanticAnns
functionCallSingleVar0 = FunctionExpression "foo" [var0] unitSemAnn
functionCallSingleDynVar1 = FunctionExpression "foo" [var1] unitSemAnn
functionCallSinglepVar = FunctionExpression "foo" [pVar] unitSemAnn

functionCallSingleRefVar0, functionCallSingleRefDynVar1,
  functionCallSingleDerefpVar :: Expression SemanticAnns
functionCallSingleRefVar0 = FunctionExpression "foo" [referenceVar0] unitSemAnn
functionCallSingleRefDynVar1 = FunctionExpression "foo" [referenceVar1] unitSemAnn
functionCallSingleDerefpVar = FunctionExpression "foo" [dereferencepVar] unitSemAnn

functionCallSingleVar0PlusConstant, functionCallSingleDynVar1PlusConstant,
  functionCallSingleVar0PlusVar1,
  functionCallSingleDerefpVarPlusConstant,
  functionCallSingleDerefpVarPlusDerefRefVar1 :: Expression SemanticAnns
functionCallSingleVar0PlusConstant = FunctionExpression "foo" [var0PlusConstant] unitSemAnn
functionCallSingleDynVar1PlusConstant = FunctionExpression "foo" [var1PlusConstant] unitSemAnn
functionCallSingleVar0PlusVar1 = FunctionExpression "foo" [var0PlusVar1] unitSemAnn
functionCallSingleDerefpVarPlusConstant = FunctionExpression "foo" [dereferencepVarPlusConstant] unitSemAnn
functionCallSingleDerefpVarPlusDerefRefVar1 = FunctionExpression "foo" [dereferencepVar2PlusderefRefVar1] unitSemAnn

functionCallSingleVector0, functionCallSingleDynVector0,
  functionCallSinglepVector1, functionCallSinglepDynVector1 :: Expression SemanticAnns
functionCallSingleVector0 = FunctionExpression "foo" [vector0] unitSemAnn
functionCallSingleDynVector0 = FunctionExpression "foo" [dynVector0] unitSemAnn
functionCallSinglepVector1 = FunctionExpression "foo" [pVector1] unitSemAnn
functionCallSinglepDynVector1 = FunctionExpression "foo" [pDynVector1] unitSemAnn

functionCallSingleRefVector0, functionCallSingleRefDynVector0 :: Expression SemanticAnns
functionCallSingleRefVector0 = FunctionExpression "foo" [referenceVector0] unitSemAnn
functionCallSingleRefDynVector0 = FunctionExpression "foo" [referenceDynVector0] unitSemAnn

call2Parameters, call3Parameters, call4Parameters :: Expression SemanticAnns
call2Parameters = FunctionExpression "foo2" [var0PlusConstant, var0PlusVar1] unitSemAnn
call3Parameters = FunctionExpression "foo3" [vector0, referenceVar0, var1PlusConstant] unitSemAnn
call4Parameters = FunctionExpression "foo4" [dynVector0, referenceVar1, functionCallSingleVar0, call2Parameters] unitSemAnn

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression empty

spec :: Spec
spec = do
  describe "Pretty printing function call expressions with one parameter" $ do
    it "Prints the expression: foo(var0)" $ do
      renderExpression functionCallSingleVar0 `shouldBe`
        pack "foo(var0)"
    it "Prints the expression: foo(var1)" $ do
      renderExpression functionCallSingleDynVar1 `shouldBe`
        pack "foo(var1)"
    it "Prints the expression: foo(p_var)" $ do
      renderExpression functionCallSinglepVar `shouldBe`
        pack "foo(p_var)"
    it "Prints the expression: foo(&var0)" $ do
      renderExpression functionCallSingleRefVar0 `shouldBe` 
        pack "foo(&var0)"
    it "Prints the expression: foo(&var1)" $ do
      renderExpression functionCallSingleRefDynVar1 `shouldBe`
        pack "foo((uint16_t *)var1.datum)"
    it "Prints the expression: foo(*p_var)" $ do
      renderExpression functionCallSingleDerefpVar `shouldBe`
        pack "foo(*(p_var))"
    it "Prints the expression: foo(var0 + 1024 : u16)" $ do
      renderExpression functionCallSingleVar0PlusConstant `shouldBe`
        pack "foo(var0 + (uint16_t)1024)"
    it "Prints the expression: foo(var1 + 1024 : u16)" $ do
      renderExpression functionCallSingleDynVar1PlusConstant `shouldBe`
        pack "foo(*((uint16_t *)var1.datum) + (uint16_t)1024)"
    it "Prints the expression: foo(var0 + dynVar1)" $ do
      renderExpression functionCallSingleVar0PlusVar1 `shouldBe`
        pack "foo(var0 + *((uint16_t *)var1.datum))"
    it "Prints the expression: foo(*p_var + 1024 : u16)" $ do
      renderExpression functionCallSingleDerefpVarPlusConstant `shouldBe`
        pack "foo(*(p_var) + (uint16_t)1024)"
    it "Prints the expression: foo(*p_var + *p_dynVar3)" $ do
      renderExpression functionCallSingleDerefpVarPlusDerefRefVar1 `shouldBe`
        pack "foo(*(p_var) + *((uint16_t *)var1.datum))"
    it "Prints the expression: foo(vector0)" $ do
      renderExpression functionCallSingleVector0 `shouldBe`
        pack "foo(vector0)"
    it "Prints the expression: foo(&vector0)" $ do
      renderExpression functionCallSingleRefVector0 `shouldBe`
        pack "foo(vector0)"
    it "Prints the expression: foo(dynVector0)" $ do
      renderExpression functionCallSingleDynVector0 `shouldBe`
        pack "foo(dynVector0)"
    it "Prints the expression: foo(&dynVector0)" $ do
      renderExpression functionCallSingleRefDynVector0 `shouldBe`
        pack "foo((uint32_t *)dynVector0.datum)"
    it "Prints the expression: foo(p_vector1)" $ do
      renderExpression functionCallSinglepVector1 `shouldBe`
        pack "foo(p_vector1)"
    it "Prints the expression: foo(p_dynVector1)" $ do
      renderExpression functionCallSinglepDynVector1 `shouldBe`
        pack "foo(p_dynVector1)"
  describe "Pretty printing function call expressions with multiple parameters" $ do
    it "Prints the expression: foo2(var0 + (uint16_t)1024, var0 + var1)" $ do
      renderExpression call2Parameters `shouldBe`
        pack "foo2(var0 + (uint16_t)1024, var0 + *((uint16_t *)var1.datum))"
    it "Prints the expression: foo3(vector0, &var0, var1 + 1024 : u16)" $ do
      renderExpression call3Parameters `shouldBe`
        pack "foo3(vector0, &var0, *((uint16_t *)var1.datum) + (uint16_t)1024)"
    it "Prints the expression: foo4(dynVector0, &dynVar1, foo(var0), foo2(var0 + (uint16_t)1024, var0 + dynVar1))" $ do
      renderExpression call4Parameters `shouldBe`
        pack (
          "foo4(dynVector0, (uint16_t *)var1.datum, foo(var0),\n" ++
          "     foo2(var0 + (uint16_t)1024, var0 + *((uint16_t *)var1.datum)))")