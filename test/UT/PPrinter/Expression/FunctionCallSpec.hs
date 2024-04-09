module UT.PPrinter.Expression.FunctionCallSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Control.Monad.Reader
import Generator.Expression
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

var0, var1, pVar :: Object SemanticAnns
-- | var0 : u16
var0 = Variable "var0" (objSemAnn Mutable UInt16)
-- | dynVar1 : 'dyn u16
var1 = Variable "var1" dynUInt16SemAnn
-- | p_var : &u16
pVar = Variable "p_var" refUInt16SemAnn

undynVar1 :: Expression SemanticAnns
undynVar1 = AccessObject (Undyn var1 (objSemAnn Mutable UInt16))

referenceVar0, referenceVar1 :: Expression SemanticAnns
-- | &mut var0 : &u16
referenceVar0 = ReferenceExpression Mutable var0 refUInt16SemAnn
-- | &mut var1 : &u16
referenceVar1 = ReferenceExpression Mutable var1 refUInt16SemAnn

dereferencepVar :: Object SemanticAnns
-- | *p_var : u16
dereferencepVar = Dereference pVar (objSemAnn Mutable UInt16)

vector0, dynVector0 :: Object SemanticAnns
vector0 = Variable "vector0" (vectorSemAnn Mutable UInt32 (K 10))
dynVector0 = Variable "dynVector0" (dynVectorSemAnn UInt32 (K 10))

pVector1 :: Expression SemanticAnns
pVector1 = AccessObject (Variable "p_vector1" (refVectorSemAnn UInt32 (K 10)))

referenceVector0 :: Expression SemanticAnns
referenceVector0 = ReferenceExpression Mutable vector0 (refVectorSemAnn UInt32 (K 10))

uint16Const :: Expression SemanticAnns
uint16Const = Constant (I UInt16 1024) uint16SemAnn

var0PlusConstant, var1PlusConstant :: Expression SemanticAnns
var0PlusConstant = BinOp Addition (AccessObject var0) uint16Const uint16SemAnn
var1PlusConstant = BinOp Addition undynVar1 uint16Const uint16SemAnn

dereferencepVarPlusConstant :: Expression SemanticAnns
dereferencepVarPlusConstant = BinOp Addition (AccessObject dereferencepVar) uint16Const uint16SemAnn

var0PlusVar1, dereferencepVar2PlusVar1 :: Expression SemanticAnns
var0PlusVar1 = BinOp Addition (AccessObject var0) undynVar1 uint16SemAnn
dereferencepVar2PlusVar1 = BinOp Addition (AccessObject dereferencepVar) undynVar1 (objSemAnn Mutable UInt16)

functionCallSingleVar0, functionCallSingleDynVar1,
  functionCallSinglepVar :: Expression SemanticAnns
functionCallSingleVar0 = FunctionExpression "foo" [AccessObject var0] (funSemAnn [Parameter "param0" UInt16] UInt16)
functionCallSingleDynVar1 = FunctionExpression "foo" [AccessObject var1] (funSemAnn [Parameter "param0" (DynamicSubtype UInt16)] Unit)
functionCallSinglepVar = FunctionExpression "foo" [AccessObject pVar] (funSemAnn [Parameter "param0" (Reference Mutable UInt16)] Unit)

functionCallSingleRefVar0, functionCallSingleRefDynVar1,
  functionCallSingleDerefpVar :: Expression SemanticAnns
functionCallSingleRefVar0 = FunctionExpression "foo" [referenceVar0] (funSemAnn [Parameter "param0" (Reference Mutable UInt16)] Unit)
functionCallSingleRefDynVar1 = FunctionExpression "foo" [referenceVar1] (funSemAnn [Parameter "param0" (Reference Mutable UInt16)] Unit)
functionCallSingleDerefpVar = FunctionExpression "foo" [AccessObject dereferencepVar] (funSemAnn [Parameter "param0" UInt16] Unit)

functionCallRetArray :: Expression SemanticAnns
functionCallRetArray = FunctionExpression "foo" [] (funSemAnn [] (Vector UInt32 (K 10)))

functionCallSingleVar0PlusConstant, functionCallSingleDynVar1PlusConstant,
  functionCallSingleVar0PlusVar1,
  functionCallSingleDerefpVarPlusConstant,
  functionCallSingleDerefpVarPlusDerefRefVar1 :: Expression SemanticAnns
functionCallSingleVar0PlusConstant = FunctionExpression "foo" [var0PlusConstant] (funSemAnn [Parameter "param0" UInt16] Unit)
functionCallSingleDynVar1PlusConstant = FunctionExpression "foo" [var1PlusConstant] (funSemAnn [Parameter "param0" UInt16] Unit)
functionCallSingleVar0PlusVar1 = FunctionExpression "foo" [var0PlusVar1] (funSemAnn [Parameter "param0" UInt16] Unit)
functionCallSingleDerefpVarPlusConstant = FunctionExpression "foo" [dereferencepVarPlusConstant] (funSemAnn [Parameter "param0" UInt16] Unit)
functionCallSingleDerefpVarPlusDerefRefVar1 = FunctionExpression "foo" [dereferencepVar2PlusVar1] (funSemAnn [Parameter "param0" UInt16] Unit)

functionCallSingleVector0, functionCallSingleDynVector0,
  functionCallSinglepVector1 :: Expression SemanticAnns
functionCallSingleVector0 = FunctionExpression "foo" [AccessObject vector0] (funSemAnn [Parameter "param0" (Vector UInt32 (K 10))] Unit)
functionCallSingleDynVector0 = FunctionExpression "foo" [AccessObject dynVector0] (funSemAnn [Parameter "param0" (DynamicSubtype (Vector UInt32 (K 10)))] Unit)
functionCallSinglepVector1 = FunctionExpression "foo" [pVector1] (funSemAnn [Parameter "param0" (Reference Mutable (Vector UInt32 (K 10)))] Unit)

functionCallSingleRefVector0 :: Expression SemanticAnns
functionCallSingleRefVector0 = FunctionExpression "foo" [referenceVector0] (funSemAnn [Parameter "param0" (Reference Mutable (Vector UInt32 (K 10)))] Unit)

call2Parameters, call3Parameters, call4Parameters :: Expression SemanticAnns
call2Parameters = FunctionExpression "foo2" [var0PlusConstant, var0PlusVar1] (funSemAnn [Parameter "param0" UInt16, Parameter "param1" UInt16] UInt16)
call3Parameters = FunctionExpression "foo3"
  [
    referenceVar0,
    AccessObject vector0,
    var1PlusConstant
  ] (funSemAnn [
    Parameter "param0" (Reference Mutable UInt16),
    Parameter "param1" (Vector UInt32 (K 10)),
    Parameter "param2" UInt16
    ] Unit)
call4Parameters = FunctionExpression "foo4"
  [
    AccessObject dynVector0,
    referenceVar1,
    functionCallSingleVar0,
    call2Parameters
  ] (funSemAnn [
    Parameter "param0" (DynamicSubtype (Vector UInt32 (K 10))),
    Parameter "param1" (Reference Mutable UInt16),
    Parameter "param2" UInt16,
    Parameter "param3" UInt16
    ] Unit)

renderExpression :: Expression SemanticAnns -> Text
renderExpression expr = 
  case runReaderT (genExpression expr) empty of
    Left err -> pack $ show err
    Right cExpr -> render $ runReader (pprint cExpr) (CPrinterConfig False False)

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
        pack "foo((uint16_t *)var1.data)"
    it "Prints the expression: foo(*p_var)" $ do
      renderExpression functionCallSingleDerefpVar `shouldBe`
        pack "foo(*p_var)"
    it "Prints the expression: foo(var0 + 1024 : u16)" $ do
      renderExpression functionCallSingleVar0PlusConstant `shouldBe`
        pack "foo(var0 + 1024)"
    it "Prints the expression: foo(var1 + 1024 : u16)" $ do
      renderExpression functionCallSingleDynVar1PlusConstant `shouldBe`
        pack "foo(*(uint16_t *)var1.data + 1024)"
    it "Prints the expression: foo(var0 + dynVar1)" $ do
      renderExpression functionCallSingleVar0PlusVar1 `shouldBe`
        pack "foo(var0 + *(uint16_t *)var1.data)"
    it "Prints the expression: foo(*p_var + 1024 : u16)" $ do
      renderExpression functionCallSingleDerefpVarPlusConstant `shouldBe`
        pack "foo(*p_var + 1024)"
    it "Prints the expression: foo(*p_var + *p_dynVar3)" $ do
      renderExpression functionCallSingleDerefpVarPlusDerefRefVar1 `shouldBe`
        pack "foo(*p_var + *(uint16_t *)var1.data)"
    it "Prints the expression: foo(vector0)" $ do
      renderExpression functionCallSingleVector0 `shouldBe`
        pack "foo(*(__wrapper_uint32__10_t *)vector0)"
    it "Prints the expression: foo(&vector0)" $ do
      renderExpression functionCallSingleRefVector0 `shouldBe`
        pack "foo(vector0)"
    it "Prints the expression: foo(dynVector0)" $ do
      renderExpression functionCallSingleDynVector0 `shouldBe`
        pack "foo(dynVector0)"
    it "Prints the expression: foo(p_vector1)" $ do
      renderExpression functionCallSinglepVector1 `shouldBe`
        pack "foo(p_vector1)"
  describe "Pretty printing function call expressions with multiple parameters" $ do
    it "Prints the expression: foo2(var0 + 1024, var0 + var1)" $ do
      renderExpression call2Parameters `shouldBe`
        pack "foo2(var0 + 1024, var0 + *(uint16_t *)var1.data)"
    it "Prints the expression: foo3(vector0, &var0, var1 + 1024 : u16)" $ do
      renderExpression call3Parameters `shouldBe`
        pack ("foo3(&var0, *(__wrapper_uint32__10_t *)vector0, " ++
              "*(uint16_t *)var1.data + 1024)")
    it "Prints the expression: foo4(dynVector0, &dynVar1, foo(var0), foo2(var0 + 1024, var0 + dynVar1))" $ do
      renderExpression call4Parameters `shouldBe`
        pack (
          "foo4(dynVector0, (uint16_t *)var1.data, foo(var0),\n     " ++
          "foo2(var0 + 1024, var0 + *(uint16_t *)var1.data))")
  describe "Pretty printing functions returning and receiving arrays" $ do
    it "Prints the expression foo() with a function returning an array" $ do
      renderExpression functionCallRetArray `shouldBe`
        pack "foo().array"
