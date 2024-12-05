module UT.PPrinter.Expression.ArrayIndexSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

arrayObjAnn, boxArrayObjAnn, twoDymArrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable TUInt32 (K (TInteger 10 DecRepr))
boxArrayObjAnn = boxArrayObjSemAnn TUInt32 (K (TInteger 10 DecRepr))
twoDymArrayObjAnn = twoDymArrayObjSemAnn Mutable TInt64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

refArrayAnn :: SemanticAnn
refArrayAnn = refSemAnn (TArray TUInt32 (K (TInteger 10 DecRepr)))

var0, array0, array1 :: Object SemanticAnn
var0 = Variable "var0" (objSemAnn Mutable TUInt16)
array0 = Variable "array0" arrayObjAnn
array1 = Variable "array1" twoDymArrayObjAnn

pArray0 :: Object SemanticAnn
pArray0 = Variable "p_array0" refArrayAnn

usizeIndex3, usizeIndex4, usizeConst0x8 :: Expression SemanticAnn
usizeIndex3 = Constant (I (TInteger 3 DecRepr) (Just TUSize)) usizeExprSemAnn
usizeIndex4 = Constant (I (TInteger 4 DecRepr) (Just TUSize)) usizeExprSemAnn
usizeConst0x8 = Constant (I (TInteger 8 DecRepr) (Just TUSize)) usizeExprSemAnn

boxArray0 :: Object SemanticAnn
boxArray0 = Variable "box_array0" boxArrayObjAnn

array0IndexConstant, array0IndexVar0 :: Expression SemanticAnn
array0IndexConstant = AccessObject (ArrayIndexExpression array0 usizeConst0x8 (objSemAnn Mutable TUInt32))
array0IndexVar0 = AccessObject (ArrayIndexExpression array0 (AccessObject var0) (objSemAnn Mutable TUInt32))

boxArray0IndexConstant, boxArray0IndexVar0 :: Expression SemanticAnn
boxArray0IndexConstant = AccessObject (ArrayIndexExpression (Unbox boxArray0 arrayObjAnn) usizeConst0x8 (objSemAnn Mutable TUInt32))
boxArray0IndexVar0 = AccessObject (ArrayIndexExpression (Unbox boxArray0 arrayObjAnn) (AccessObject var0) (objSemAnn Mutable TUInt32))

array1IndexFirstDym :: Object SemanticAnn
array1IndexFirstDym = ArrayIndexExpression array1 usizeIndex3 (arrayObjSemAnn Mutable TInt64 (K (TInteger 5 DecRepr)))

array1IndexExpression :: Expression SemanticAnn
array1IndexExpression = AccessObject (ArrayIndexExpression array1IndexFirstDym usizeIndex4 (objSemAnn Mutable TInt64))

derefpArray0 :: Object SemanticAnn
derefpArray0 = Dereference pArray0 arrayObjAnn

derefpArray0IndexConstant, derefpArray0IndexVar0 :: Expression SemanticAnn
derefpArray0IndexConstant = AccessObject (ArrayIndexExpression derefpArray0 usizeIndex3 (objSemAnn Mutable TUInt32))
derefpArray0IndexVar0 = AccessObject (ArrayIndexExpression derefpArray0 (AccessObject var0) (objSemAnn Mutable TUInt32))

spec :: Spec
spec = do
  describe "Pretty printing array index expressions" $ do
    it "Prints the expression: array[0x08 : u8]" $ do
      renderExpression array0IndexConstant `shouldBe`
        pack "array0[8U]"
    it "Prints the expression: array[var0]" $ do
      renderExpression array0IndexVar0 `shouldBe`
        pack "array0[var0]"
    it "Prints the expression: array1[3 : u32][4 : u32]" $ do
      renderExpression array1IndexExpression `shouldBe`
        pack "array1[3U][4U]"
    it "Prints the expression: box_array0[0x08 : u8]" $ do
      renderExpression boxArray0IndexConstant `shouldBe`
        pack "((uint32_t *)box_array0.data)[8U]"
    it "Prints the expression: box_array0[var0]" $ do
      renderExpression boxArray0IndexVar0 `shouldBe`
        pack "((uint32_t *)box_array0.data)[var0]"
    it "Prints the expression: *array0[3 : u32]" $ do
      renderExpression derefpArray0IndexConstant `shouldBe`
        pack "p_array0[3U]"
    it "Prints the expression: *array0[var0]" $ do
      renderExpression derefpArray0IndexVar0 `shouldBe`
        pack "p_array0[var0]" 
