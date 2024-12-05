module UT.PPrinter.Expression.VariableSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

arrayObjAnn, twoDymArrayObjAnn, boxTwoDymArrayObjAnn, boxThreeDymArrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable TUInt32 (K (TInteger 10 DecRepr))
twoDymArrayObjAnn = twoDymArrayObjSemAnn Mutable TInt64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))
boxTwoDymArrayObjAnn = boxTwoDymArrayObjSemAnn TInt64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))
boxThreeDymArrayObjAnn = boxThreeDymArrayObjSemAnn TChar (K (TInteger 40 DecRepr)) (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

var0, array0, array1 :: Object SemanticAnn
var0 = Variable "var0" (objSemAnn Mutable TUInt16)
array0 = Variable "array0" arrayObjAnn
array1 = Variable "array1" twoDymArrayObjAnn

boxVar0, boxArray1, boxArray2 :: Object SemanticAnn
boxVar0 = Variable "box_var0" boxUInt16SemAnn
boxArray1 = Variable "box_array1" boxTwoDymArrayObjAnn
boxArray2 = Variable "box_array2" boxThreeDymArrayObjAnn

spec :: Spec
spec = do
  describe "Pretty printing variable expression" $ do
    it "Prints the variable var0 : u16" $ do
      renderExpression (AccessObject var0) `shouldBe`
        pack "var0"
    it "Prints the variable array0 : [u32; 10 : u32]" $ do
      renderExpression (AccessObject array0) `shouldBe`
        pack "array0"
    it "Prints the variable array1 : [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject array1) `shouldBe`
        pack "array1"
    it "Prints the variable box_var0 : 'box u16" $ do
      renderExpression (AccessObject boxVar0) `shouldBe`
        pack "box_var0"
    it "Prints the unboxed variable box_var0 : 'box u16" $ do
      renderExpression (AccessObject (Unbox boxVar0 (objSemAnn Mutable TUInt16))) `shouldBe`
        pack "*(uint16_t *)box_var0.data"
    it "Prints the variable box_array1 : 'box [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject boxArray1) `shouldBe`
        pack "box_array1"
    it "Prints the unboxed variable box_array1 : 'box [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject (Unbox boxArray1 twoDymArrayObjAnn)) `shouldBe`
        pack "(int64_t (*)[5U])box_array1.data"
    it "Prints the unboxed variable box_array2 : [[[char; 40 : u32]; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject (Unbox boxArray2 boxThreeDymArrayObjAnn)) `shouldBe`
        pack "(char (*)[5U][40U])box_array2.data"
