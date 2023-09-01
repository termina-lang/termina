module UT.PPrinter.Expression.ReferenceSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import PPrinter.Expression
import UT.PPrinter.Expression.Common
import Semantic.Monad

vectorAnn, dynVectorAnn, twoDymVectorAnn, dynTwoDymVectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn UInt32 (I UInt32 10)
dynVectorAnn = dynVectorSemAnn UInt32 (I UInt32 10)
twoDymVectorAnn = twoDymVectorSemAnn Int64 (I UInt32 5) (I UInt32 10)
dynTwoDymVectorAnn = dynTwoDymVectorSemAnn Int64 (I UInt32 5) (I UInt32 10)

refVectorAnn, refTwoDymVectorAnn :: SemanticAnns
refVectorAnn = refVectorSemAnn UInt32 (I UInt32 10)
refTwoDymVectorAnn = refTwoDymVectorSemAnn Int64 (I UInt32 5) (I UInt32 10)

var0, vector0, vector1 :: Object SemanticAnns
var0 = Variable "var0" uint16SemAnn
vector0 = Variable "vector0" vectorAnn
vector1 = Variable "vector1" twoDymVectorAnn

dynVar0, dynVector0, dynVector1 :: Object' Expression SemanticAnns
dynVar0 = Variable "dyn_var0" dynUInt16SemAnn
dynVector0 = Variable "dyn_vector0" dynVectorAnn
dynVector1 = Variable "dyn_vector1" dynTwoDymVectorAnn

pVar0expr, pVector0expr, pVector1expr :: Expression SemanticAnns
pVar0expr = ReferenceExpression ( var0) refUInt16SemAnn
pVector0expr = ReferenceExpression ( vector0) refVectorAnn
pVector1expr = ReferenceExpression ( vector1) refTwoDymVectorAnn

pDynVar0expr, pDynVector0expr, pDynVector1expr :: Expression SemanticAnns
pDynVar0expr = ReferenceExpression ( dynVar0) refUInt16SemAnn
pDynVector0expr = ReferenceExpression ( dynVector0) refVectorAnn
pDynVector1expr = ReferenceExpression ( dynVector1) refTwoDymVectorAnn

derefpVar0, derefpVector0, derefpVector1 :: Expression SemanticAnns
derefpVar0 = AccessObject (Dereference (IdentifierExpression pVar0expr refUInt16SemAnn) uint16SemAnn)
derefpVector0 = AccessObject (Dereference (IdentifierExpression pVector0expr refVectorAnn) vectorAnn)
derefpVector1 = (AccessObject (Dereference (IdentifierExpression pVector1expr refTwoDymVectorAnn) twoDymVectorAnn))

derefpDynVar0expr, derefpDynVector0expr, derefpDynVector1expr :: Expression SemanticAnns
derefpDynVar0expr = (AccessObject ((Dereference (IdentifierExpression pDynVar0expr refUInt16SemAnn) uint16SemAnn)))
derefpDynVector0expr = (AccessObject ((Dereference (IdentifierExpression pDynVector0expr refVectorAnn) vectorAnn)))
derefpDynVector1expr = (AccessObject ((Dereference (IdentifierExpression pDynVector1expr refTwoDymVectorAnn) twoDymVectorAnn)))

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression empty
 
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
        pack "(int64_t (*)[5])dyn_vector1.datum"
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
        pack "(uint32_t *)dyn_vector0.datum"
    it "Prints the expression: *&dyn_vector1" $ do
      renderExpression derefpDynVector1expr `shouldBe`
        pack "(int64_t (*)[5])dyn_vector1.datum"

