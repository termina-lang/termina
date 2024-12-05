module UT.PPrinter.Expression.ReferenceSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

arrayObjAnn, boxArrayObjAnn, twoDymArrayObjAnn, boxTwoDymArrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable TUInt32 (K (TInteger 10 DecRepr))
boxArrayObjAnn = boxArrayObjSemAnn TUInt32 (K (TInteger 10 DecRepr))
twoDymArrayObjAnn = twoDymArrayObjSemAnn Mutable TInt64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))
boxTwoDymArrayObjAnn = boxTwoDymArrayObjSemAnn TInt64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

refArrayAnn, refTwoDymArrayAnn :: SemanticAnn
refArrayAnn = refArraySemAnn TUInt32 (K (TInteger 10 DecRepr))
refTwoDymArrayAnn = refTwoDymArraySemAnn TInt64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

var0, array0, array1 :: Object SemanticAnn
var0 = Variable "var0" (objSemAnn Mutable TUInt16)
array0 = Variable "array0" arrayObjAnn
array1 = Variable "array1" twoDymArrayObjAnn

boxVar0, boxArray0, boxArray1 :: Object SemanticAnn
boxVar0 = Variable "box_var0" boxUInt16SemAnn
boxArray0 = Variable "box_array0" boxArrayObjAnn
boxArray1 = Variable "box_array1" boxTwoDymArrayObjAnn

pVar0, pArray0, pArray1 :: Object SemanticAnn
pVar0 = Variable "p_var0" refUInt16SemAnn
pArray0 = Variable "p_array0" refArrayAnn
pArray1 = Variable "p_array1" refTwoDymArrayAnn

refVar0expr, refArray0expr, refArray1expr :: Expression SemanticAnn
refVar0expr = ReferenceExpression Mutable var0 refUInt16SemAnn
refArray0expr = ReferenceExpression Mutable array0 refArrayAnn
refArray1expr = ReferenceExpression Mutable array1 refTwoDymArrayAnn

refBoxVar0expr, refBoxArray0expr, refBoxArray1expr :: Expression SemanticAnn
refBoxVar0expr = ReferenceExpression Mutable boxVar0 refUInt16SemAnn
refBoxArray0expr = ReferenceExpression Mutable boxArray0 refArrayAnn
refBoxArray1expr = ReferenceExpression Mutable boxArray1 refTwoDymArrayAnn

derefpVar0, derefpArray0, derefpArray1 :: Expression SemanticAnn
derefpVar0 = AccessObject (Dereference pVar0 (objSemAnn Mutable TUInt16)) -- | *p_var0 |
derefpArray0 = AccessObject (Dereference pArray0 arrayObjAnn) -- | *p_array0 |
derefpArray1 = AccessObject (Dereference pArray1 twoDymArrayObjAnn) -- | *p_array1 |
 
spec :: Spec
spec = do
  describe "Pretty printing reference expressions" $ do
    it "Prints the expression: &var0" $ do
      renderExpression refVar0expr `shouldBe`
        pack "&var0"
    it "Prints the expression: &array0" $ do
      renderExpression refArray0expr `shouldBe`
        pack "array0"
    it "Prints the expression: &array1" $ do
      renderExpression refArray1expr `shouldBe`
        pack "array1"
    it "Prints the expression: &box_var0" $ do
      renderExpression refBoxVar0expr `shouldBe`
        pack "(uint16_t *)box_var0.data"
    it "Prints the expression: &box_array0" $ do
      renderExpression refBoxArray0expr `shouldBe`
        pack "(uint32_t *)box_array0.data"
    it "Prints the expression: &box_array1" $ do
      renderExpression refBoxArray1expr `shouldBe`
        pack "(int64_t (*)[5U])box_array1.data"
  describe "Pretty printing dereference expressions" $ do
    it "Prints the expression: *p_var0" $ do
      renderExpression derefpVar0 `shouldBe`
        pack "*p_var0"
    it "Prints the expression: *p_array0" $ do
      renderExpression derefpArray0 `shouldBe`
        pack "p_array0"
    it "Prints the expression: *p_array1" $ do
      renderExpression derefpArray1 `shouldBe`
        pack "p_array1"
