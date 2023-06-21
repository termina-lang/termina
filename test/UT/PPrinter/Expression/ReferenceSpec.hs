module UT.PPrinter.Expression.ReferenceSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Data.Text
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

var0, vector0, vector1 :: Expression SemanticAnns
var0 = Variable "var0" (SemAnn undefined uint16TS)
vector0 = Variable "vector0" (SemAnn undefined vectorTS)
vector1 = Variable "vector1" (SemAnn undefined twoDimVectorTS)

dynVar0, dynVector0, dynVector1 :: Expression SemanticAnns
dynVar0 = Variable "dyn_var0" (SemAnn undefined (DynamicSubtype uint16TS))
dynVector0 = Variable "dyn_vector0" (SemAnn undefined (DynamicSubtype vectorTS))
dynVector1 = Variable "dyn_vector1" (SemAnn undefined (DynamicSubtype twoDimVectorTS))

pVar0expr, pVector0expr, pVector1expr :: Expression SemanticAnns
pVar0expr = ReferenceExpression var0 (SemAnn undefined (Reference uint16TS))
pVector0expr = ReferenceExpression vector0 (SemAnn undefined (Reference vectorTS))
pVector1expr = ReferenceExpression vector1 (SemAnn undefined (Reference twoDimVectorTS))

pDynVar0expr, pDynVector0expr, pDynVector1expr :: Expression SemanticAnns
pDynVar0expr = ReferenceExpression dynVar0 (SemAnn undefined (Reference (DynamicSubtype uint16TS)))
pDynVector0expr = ReferenceExpression dynVector0 (SemAnn undefined (Reference (DynamicSubtype vectorTS)))
pDynVector1expr = ReferenceExpression dynVector1 (SemAnn undefined (Reference (DynamicSubtype twoDimVectorTS)))

derefpVar0, derefpVector0, derefpVector1 :: Expression SemanticAnns
derefpVar0 = DereferenceExpression pVar0expr (SemAnn undefined uint16TS)
derefpVector0 = DereferenceExpression pVector0expr (SemAnn undefined vectorTS)
derefpVector1 = DereferenceExpression pVector1expr (SemAnn undefined twoDimVectorTS)

derefpDynVar0expr, derefpDynVector0expr, derefpDynVector1expr :: Expression SemanticAnns
derefpDynVar0expr = DereferenceExpression pDynVar0expr (SemAnn undefined (DynamicSubtype uint16TS))
derefpDynVector0expr = DereferenceExpression pDynVector0expr (SemAnn undefined (DynamicSubtype vectorTS))
derefpDynVector1expr = DereferenceExpression pDynVector1expr (SemAnn undefined (DynamicSubtype twoDimVectorTS))

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression

spec :: Spec
spec = do
  describe "Pretty printing reference expressions" $ do
    it "Prints the expression: &var0" $ do
      renderExpression pVar0expr `shouldBe`
        pack "&var0"
    it "Prints the expression: &vector0" $ do
      renderExpression pVector0expr `shouldBe`
        pack "vector0"
    it "Prints the expression: &vector1" $ do
      renderExpression pVector1expr `shouldBe`
        pack "vector1"
    it "Prints the expression: &dyn_var0" $ do
      renderExpression pDynVar0expr `shouldBe`
        pack "(uint16_t *)dyn_var0.datum"
    it "Prints the expression: &dyn_vector0" $ do
      renderExpression pDynVector0expr `shouldBe`
        pack "(uint32_t *)dyn_vector0.datum"
    it "Prints the expression: &dyn_vector1" $ do
      renderExpression pDynVector1expr `shouldBe`
        pack "(int64_t (*)[10])dyn_vector1.datum"
  describe "Pretty printing dereference expressions" $ do
    it "Prints the expression: *(&var0)" $ do
      renderExpression derefpVar0 `shouldBe`
        pack "*(&var0)"
    it "Prints the expression: *(&vector0)" $ do
      renderExpression derefpVector0 `shouldBe`
        pack "vector0"
    it "Prints the expression: *(&vector1)" $ do
      renderExpression derefpVector1 `shouldBe`
        pack "vector1"
    it "Prints the expression: *&dyn_var0" $ do
      renderExpression derefpDynVar0expr `shouldBe`
        pack "*((uint16_t *)dyn_var0.datum)"
    it "Prints the expression: *&dyn_vector0" $ do
      renderExpression derefpDynVector0expr `shouldBe`
        pack "((uint32_t *)dyn_vector0.datum)"
    it "Prints the expression: *&dyn_vector1" $ do
      renderExpression derefpDynVector1expr `shouldBe`
        pack "((int64_t (*)[10])dyn_vector1.datum)"

