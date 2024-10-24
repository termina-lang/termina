module UT.PPrinter.Expression.FunctionCallSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Types
import Control.Monad.Reader
import Generator.CodeGen.Expression
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

var0, var1, pVar :: Object SemanticAnn
-- | var0 : u16
var0 = Variable "var0" (objSemAnn Mutable TUInt16)
-- | boxVar1 : 'box u16
var1 = Variable "var1" boxUInt16SemAnn
-- | p_var : &u16
pVar = Variable "p_var" refUInt16SemAnn

unboxVar1 :: Expression SemanticAnn
unboxVar1 = AccessObject (Unbox var1 (objSemAnn Mutable TUInt16))

referenceVar0, referenceVar1 :: Expression SemanticAnn
-- | &mut var0 : &u16
referenceVar0 = ReferenceExpression Mutable var0 refUInt16SemAnn
-- | &mut var1 : &u16
referenceVar1 = ReferenceExpression Mutable var1 refUInt16SemAnn

dereferencepVar :: Object SemanticAnn
-- | *p_var : u16
dereferencepVar = Dereference pVar (objSemAnn Mutable TUInt16)

array0, boxArray0 :: Object SemanticAnn
array0 = Variable "array0" (arrayObjSemAnn Mutable TUInt32 (K (TInteger 10 DecRepr)))
boxArray0 = Variable "boxArray0" (boxArrayObjSemAnn TUInt32 (K (TInteger 10 DecRepr)))

pArray1 :: Expression SemanticAnn
pArray1 = AccessObject (Variable "p_array1" (refArraySemAnn TUInt32 (K (TInteger 10 DecRepr))))

referenceArray0 :: Expression SemanticAnn
referenceArray0 = ReferenceExpression Mutable array0 (refArraySemAnn TUInt32 (K (TInteger 10 DecRepr)))

uint16Const :: Expression SemanticAnn
uint16Const = Constant (I (TInteger 1024 DecRepr) (Just TUInt16)) uint16ExprSemAnn

var0PlusConstant, var1PlusConstant :: Expression SemanticAnn
var0PlusConstant = BinOp Addition (AccessObject var0) uint16Const uint16ExprSemAnn
var1PlusConstant = BinOp Addition unboxVar1 uint16Const uint16ExprSemAnn

dereferencepVarPlusConstant :: Expression SemanticAnn
dereferencepVarPlusConstant = BinOp Addition (AccessObject dereferencepVar) uint16Const uint16ExprSemAnn

var0PlusVar1, dereferencepVar2PlusVar1 :: Expression SemanticAnn
var0PlusVar1 = BinOp Addition (AccessObject var0) unboxVar1 uint16ExprSemAnn
dereferencepVar2PlusVar1 = BinOp Addition (AccessObject dereferencepVar) unboxVar1 uint16ExprSemAnn

functionCallSingleVar0, functionCallSingleBoxVar1,
  functionCallSinglepVar :: Expression SemanticAnn
functionCallSingleVar0 = FunctionCall "foo" [AccessObject var0] (funSemAnn [TUInt16] TUInt16)
functionCallSingleBoxVar1 = FunctionCall "foo" [AccessObject var1] (funSemAnn [TBoxSubtype TUInt16] TUnit)
functionCallSinglepVar = FunctionCall "foo" [AccessObject pVar] (funSemAnn [TReference Mutable TUInt16] TUnit)

functionCallSingleRefVar0, functionCallSingleRefBoxVar1,
  functionCallSingleDerefpVar :: Expression SemanticAnn
functionCallSingleRefVar0 = FunctionCall "foo" [referenceVar0] (funSemAnn [TReference Mutable TUInt16] TUnit)
functionCallSingleRefBoxVar1 = FunctionCall "foo" [referenceVar1] (funSemAnn [TReference Mutable TUInt16] TUnit)
functionCallSingleDerefpVar = FunctionCall "foo" [AccessObject dereferencepVar] (funSemAnn [TInt16] TUnit)

functionCallSingleVar0PlusConstant, functionCallSingleBoxVar1PlusConstant,
  functionCallSingleVar0PlusVar1,
  functionCallSingleDerefpVarPlusConstant,
  functionCallSingleDerefpVarPlusDerefRefVar1 :: Expression SemanticAnn
functionCallSingleVar0PlusConstant = FunctionCall "foo" [var0PlusConstant] (funSemAnn [TUInt16] TUnit)
functionCallSingleBoxVar1PlusConstant = FunctionCall "foo" [var1PlusConstant] (funSemAnn [TUInt16] TUnit)
functionCallSingleVar0PlusVar1 = FunctionCall "foo" [var0PlusVar1] (funSemAnn [TUInt16] TUnit)
functionCallSingleDerefpVarPlusConstant = FunctionCall "foo" [dereferencepVarPlusConstant] (funSemAnn [TUInt16] TUnit)
functionCallSingleDerefpVarPlusDerefRefVar1 = FunctionCall "foo" [dereferencepVar2PlusVar1] (funSemAnn [TUInt16] TUnit)

functionCallSingleBoxArray0, functionCallSinglepArray1 :: Expression SemanticAnn
functionCallSingleBoxArray0 = FunctionCall "foo" [AccessObject boxArray0] (funSemAnn [TBoxSubtype (TArray TUInt32 (K (TInteger 10 DecRepr)))] TUnit)
functionCallSinglepArray1 = FunctionCall "foo" [pArray1] (funSemAnn [TReference Mutable (TArray TUInt32 (K (TInteger 10 DecRepr)))] TUnit)

functionCallSingleRefArray0 :: Expression SemanticAnn
functionCallSingleRefArray0 = FunctionCall "foo" [referenceArray0] (funSemAnn [TReference Mutable (TArray TUInt32 (K (TInteger 10 DecRepr)))] TUnit)

call2Parameters, call3Parameters :: Expression SemanticAnn
call2Parameters = FunctionCall "foo2" [var0PlusConstant, var0PlusVar1] (funSemAnn [TUInt16, TUInt16] TUInt16)
call3Parameters = FunctionCall "foo4"
  [
    AccessObject boxArray0,
    referenceVar1,
    functionCallSingleVar0,
    call2Parameters
  ] (funSemAnn [
      TBoxSubtype (TArray TUInt32 (K (TInteger 10 DecRepr))),
      TReference Mutable TUInt16,
      TUInt16,
      TUInt16
    ] TUnit)

renderExpression :: Expression SemanticAnn -> Text
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
      renderExpression functionCallSingleBoxVar1 `shouldBe`
        pack "foo(var1)"
    it "Prints the expression: foo(p_var)" $ do
      renderExpression functionCallSinglepVar `shouldBe`
        pack "foo(p_var)"
    it "Prints the expression: foo(&var0)" $ do
      renderExpression functionCallSingleRefVar0 `shouldBe`
        pack "foo(&var0)"
    it "Prints the expression: foo(&var1)" $ do
      renderExpression functionCallSingleRefBoxVar1 `shouldBe`
        pack "foo((uint16_t *)var1.data)"
    it "Prints the expression: foo(*p_var)" $ do
      renderExpression functionCallSingleDerefpVar `shouldBe`
        pack "foo(*p_var)"
    it "Prints the expression: foo(var0 + 1024 : u16)" $ do
      renderExpression functionCallSingleVar0PlusConstant `shouldBe`
        pack "foo(var0 + 1024)"
    it "Prints the expression: foo(var1 + 1024 : u16)" $ do
      renderExpression functionCallSingleBoxVar1PlusConstant `shouldBe`
        pack "foo(*(uint16_t *)var1.data + 1024)"
    it "Prints the expression: foo(var0 + boxVar1)" $ do
      renderExpression functionCallSingleVar0PlusVar1 `shouldBe`
        pack "foo(var0 + *(uint16_t *)var1.data)"
    it "Prints the expression: foo(*p_var + 1024 : u16)" $ do
      renderExpression functionCallSingleDerefpVarPlusConstant `shouldBe`
        pack "foo(*p_var + 1024)"
    it "Prints the expression: foo(*p_var + *p_boxVar3)" $ do
      renderExpression functionCallSingleDerefpVarPlusDerefRefVar1 `shouldBe`
        pack "foo(*p_var + *(uint16_t *)var1.data)"
    it "Prints the expression: foo(&array0)" $ do
      renderExpression functionCallSingleRefArray0 `shouldBe`
        pack "foo(array0)"
    it "Prints the expression: foo(boxArray0)" $ do
      renderExpression functionCallSingleBoxArray0 `shouldBe`
        pack "foo(boxArray0)"
    it "Prints the expression: foo(p_array1)" $ do
      renderExpression functionCallSinglepArray1 `shouldBe`
        pack "foo(p_array1)"
  describe "Pretty printing function call expressions with multiple parameters" $ do
    it "Prints the expression: foo2(var0 + 1024, var0 + var1)" $ do
      renderExpression call2Parameters `shouldBe`
        pack "foo2(var0 + 1024, var0 + *(uint16_t *)var1.data)"
    it "Prints the expression: foo4(boxArray0, &boxVar1, foo(var0), foo2(var0 + 1024, var0 + boxVar1))" $ do
      renderExpression call3Parameters `shouldBe`
        pack (
          "foo4(boxArray0, (uint16_t *)var1.data, foo(var0),\n     " ++
          "foo2(var0 + 1024, var0 + *(uint16_t *)var1.data))")
