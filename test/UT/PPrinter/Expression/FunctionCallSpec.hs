module UT.PPrinter.Expression.FunctionCallSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Data.Text
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

var0, dynVar1, pVar2, pDynVar3 :: Expression SemanticAnns
var0 = Variable "var0" (SemAnn undefined uint16TS)
dynVar1 = Variable "dynVar1" (SemAnn undefined (DynamicSubtype uint16TS))
pVar2 = Variable "p_var2" (SemAnn undefined (Reference uint16TS))
pDynVar3 = Variable "p_dynVar3" (SemAnn undefined (Reference (DynamicSubtype uint16TS)))

referenceVar0, referenceDynVar1 :: Expression SemanticAnns
referenceVar0 = ReferenceExpression var0 (SemAnn undefined (Reference uint16TS))
referenceDynVar1 = ReferenceExpression dynVar1 (SemAnn undefined (Reference (DynamicSubtype uint16TS)))

dereferencepVar2, dereferencepDynVar3 :: Expression SemanticAnns
dereferencepVar2 = DereferenceExpression pVar2 (SemAnn undefined uint16TS)
dereferencepDynVar3 = DereferenceExpression pDynVar3 (SemAnn undefined (DynamicSubtype uint16TS))

vector0, dynVector0 :: Expression SemanticAnns
vector0 = Variable "vector0" (SemAnn undefined vectorTS)
dynVector0 = Variable "dynVector0" (SemAnn undefined (DynamicSubtype vectorTS))

pVector1, pDynVector1 :: Expression SemanticAnns
pVector1 = Variable "p_vector1" (SemAnn undefined (Reference vectorTS))
pDynVector1 = Variable "p_dynVector1" (SemAnn undefined (DynamicSubtype vectorTS))

referenceVector0, referenceDynVector0 :: Expression SemanticAnns
referenceVector0 = ReferenceExpression vector0 (SemAnn undefined (Reference (DynamicSubtype vectorTS)))
referenceDynVector0 = ReferenceExpression dynVector0 (SemAnn undefined (Reference (DynamicSubtype vectorTS)))

var0PlusConstant, dynVar1PlusConstant :: Expression SemanticAnns
var0PlusConstant = BinOp Addition var0 uint16Const1024 (SemAnn undefined uint16TS)
dynVar1PlusConstant = BinOp Addition dynVar1 uint16Const1024 (SemAnn undefined uint16TS)

dereferencepVar2PlusConstant, dereferencepDynVar3PlusConstant :: Expression SemanticAnns
dereferencepVar2PlusConstant = BinOp Addition dereferencepVar2 uint16Const1024 (SemAnn undefined uint16TS)
dereferencepDynVar3PlusConstant = BinOp Addition dereferencepDynVar3 uint16Const1024 (SemAnn undefined uint16TS)

var0PlusVar1, dereferencepVar2PlusdereferecepDynVar3 :: Expression SemanticAnns
var0PlusVar1 = BinOp Addition var0 dynVar1 (SemAnn undefined uint16TS)
dereferencepVar2PlusdereferecepDynVar3 = BinOp Addition dereferencepVar2 dereferencepDynVar3 (SemAnn undefined uint16TS)

functionCallSingleVar0, functionCallSingleDynVar1,
  functionCallSinglepVar2, functionCallSinglepDynVar3 :: Expression SemanticAnns
functionCallSingleVar0 = FunctionExpression "foo" [var0] (SemAnn undefined Unit)
functionCallSingleDynVar1 = FunctionExpression "foo" [dynVar1] (SemAnn undefined Unit)
functionCallSinglepVar2 = FunctionExpression "foo" [pVar2] (SemAnn undefined Unit)
functionCallSinglepDynVar3 = FunctionExpression "foo" [pDynVar3] (SemAnn undefined Unit)

functionCallSingleRefVar0, functionCallSingleRefDynVar1,
  functionCallSingleDerefVar0 :: Expression SemanticAnns
functionCallSingleRefVar0 = FunctionExpression "foo" [referenceVar0] (SemAnn undefined Unit)
functionCallSingleRefDynVar1 = FunctionExpression "foo" [referenceDynVar1] (SemAnn undefined Unit)
functionCallSingleDerefVar0 = FunctionExpression "foo" [dereferencepVar2] (SemAnn undefined Unit)

functionCallSingleVar0PlusConstant, functionCallSingleDynVar1PlusConstant,
  functionCallSingleVar0PlusVar1,
  functionCallSingleDerefpVar2PlusConstant, functionCallSingleDerefpDynVar3PlusConstant,
  functionCallSingleDerefpVar2PlusDerefpDynVar3 :: Expression SemanticAnns
functionCallSingleVar0PlusConstant = FunctionExpression "foo" [var0PlusConstant] (SemAnn undefined Unit)
functionCallSingleDynVar1PlusConstant = FunctionExpression "foo" [dynVar1PlusConstant] (SemAnn undefined Unit)
functionCallSingleVar0PlusVar1 = FunctionExpression "foo" [var0PlusVar1] (SemAnn undefined Unit)
functionCallSingleDerefpVar2PlusConstant = FunctionExpression "foo" [dereferencepVar2PlusConstant] (SemAnn undefined Unit)
functionCallSingleDerefpDynVar3PlusConstant = FunctionExpression "foo" [dereferencepDynVar3PlusConstant] (SemAnn undefined Unit)
functionCallSingleDerefpVar2PlusDerefpDynVar3 = FunctionExpression "foo" [dereferencepVar2PlusdereferecepDynVar3] (SemAnn undefined Unit)

functionCallSingleVector0, functionCallSingleDynVector0,
  functionCallSinglepVector1, functionCallSinglepDynVector1 :: Expression SemanticAnns
functionCallSingleVector0 = FunctionExpression "foo" [vector0] (SemAnn undefined Unit)
functionCallSingleDynVector0 = FunctionExpression "foo" [dynVector0] (SemAnn undefined Unit)
functionCallSinglepVector1 = FunctionExpression "foo" [pVector1] (SemAnn undefined Unit)
functionCallSinglepDynVector1 = FunctionExpression "foo" [pDynVector1] (SemAnn undefined Unit)

functionCallSingleRefVector0, functionCallSingleRefDynVector0 :: Expression SemanticAnns
functionCallSingleRefVector0 = FunctionExpression "foo" [referenceVector0] (SemAnn undefined Unit)
functionCallSingleRefDynVector0 = FunctionExpression "foo" [referenceDynVector0] (SemAnn undefined Unit)

call2Parameters, call3Parameters, call4Parameters :: Expression SemanticAnns
call2Parameters = FunctionExpression "foo2" [var0PlusConstant, var0PlusVar1] (SemAnn undefined Unit)
call3Parameters = FunctionExpression "foo3" [vector0, referenceVar0, dynVar1PlusConstant] (SemAnn undefined Unit)
call4Parameters = FunctionExpression "foo4" [dynVector0, referenceDynVar1, functionCallSingleVar0, call2Parameters] (SemAnn undefined Unit)

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppRootExpression

spec :: Spec
spec = do
  describe "Pretty printing function call expressions with one parameter" $ do
    it "Prints the expression: foo(var0)" $ do
      renderExpression functionCallSingleVar0 `shouldBe`
        pack "foo(var0)"
    it "Prints the expression: foo(dynVar1)" $ do
      renderExpression functionCallSingleDynVar1 `shouldBe`
        pack "foo(dynVar1)"
    it "Prints the expression: foo(p_var2)" $ do
      renderExpression functionCallSinglepVar2 `shouldBe`
        pack "foo(p_var2)"
    it "Prints the expression: foo(p_dynVar3)" $ do
      renderExpression functionCallSinglepDynVar3 `shouldBe`
        pack "foo(p_dynVar3)"
    it "Prints the expression: foo(&var0)" $ do
      renderExpression functionCallSingleRefVar0 `shouldBe` 
        pack "foo(&var0)"
    it "Prints the expression: foo(&dynVar1)" $ do
      renderExpression functionCallSingleRefDynVar1 `shouldBe`
        pack "foo((uint16_t *)dynVar1.datum)"
    it "Prints the expression: foo(*p_var2)" $ do
      renderExpression functionCallSingleDerefVar0 `shouldBe`
        pack "foo(*(p_var2))"
    it "Prints the expression: foo(var0 + 1024 : u16)" $ do
      renderExpression functionCallSingleVar0PlusConstant `shouldBe`
        pack "foo(var0 + (uint16_t)1024)"
    it "Prints the expression: foo(dynVar1 + 1024 : u16)" $ do
      renderExpression functionCallSingleDynVar1PlusConstant `shouldBe`
        pack "foo(*((uint16_t *)dynVar1.datum) + (uint16_t)1024)"
    it "Prints the expression: foo(var0 + dynVar1)" $ do
      renderExpression functionCallSingleVar0PlusVar1 `shouldBe`
        pack "foo(var0 + *((uint16_t *)dynVar1.datum))"
    it "Prints the expression: foo(*p_var2 + 1024 : u16)" $ do
      renderExpression functionCallSingleDerefpVar2PlusConstant `shouldBe`
        pack "foo(*(p_var2) + (uint16_t)1024)"
    it "Prints the expression: foo(*p_var3 + 1024 : u16)" $ do
      renderExpression functionCallSingleDerefpDynVar3PlusConstant `shouldBe`
        pack "foo(*(p_dynVar3) + (uint16_t)1024)"
    it "Prints the expression: foo(*p_var2 + *p_dynVar3)" $ do
      renderExpression functionCallSingleDerefpVar2PlusDerefpDynVar3 `shouldBe`
        pack "foo(*(p_var2) + *(p_dynVar3))"
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
    it "Prints the expression: foo2(var0 + (uint16_t)1024, var0 + dynVar1)" $ do
      renderExpression call2Parameters `shouldBe`
        pack "foo2(var0 + (uint16_t)1024, var0 + *((uint16_t *)dynVar1.datum))"
    it "Prints the expression: foo3(vector0, &var0, dynVar1 + 1024 : u16)" $ do
      renderExpression call3Parameters `shouldBe`
        pack "foo3(vector0, &var0, *((uint16_t *)dynVar1.datum) + (uint16_t)1024)"
    it "Prints the expression: foo4(dynVector0, &dynVar1, foo(var0), foo2(var0 + (uint16_t)1024, var0 + dynVar1))" $ do
      renderExpression call4Parameters `shouldBe`
        pack (
          "foo4(dynVector0, (uint16_t *)dynVar1.datum, foo(var0),\n" ++
          "     foo2(var0 + (uint16_t)1024, var0 + *((uint16_t *)dynVar1.datum)))")