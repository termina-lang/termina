module UT.PPrinter.Expression.ReferenceSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import PPrinter.Expression
import UT.PPrinter.Expression.Common
import Semantic.Monad

vectorAnn, dynVectorAnn, twoDymVectorAnn, dynTwoDymVectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn Mutable UInt32 (K 10)
dynVectorAnn = dynVectorSemAnn UInt32 (K 10)
twoDymVectorAnn = twoDymVectorSemAnn Mutable Int64 (K 5) (K 10)
dynTwoDymVectorAnn = dynTwoDymVectorSemAnn Int64 (K 5) (K 10)

refVectorAnn, refTwoDymVectorAnn :: SemanticAnns
refVectorAnn = refVectorSemAnn UInt32 (K 10)
refTwoDymVectorAnn = refTwoDymVectorSemAnn Int64 (K 5) (K 10)

var0, vector0, vector1 :: Object SemanticAnns
var0 = Variable "var0" (objSemAnn Mutable UInt16)
vector0 = Variable "vector0" vectorAnn
vector1 = Variable "vector1" twoDymVectorAnn

dynVar0, dynVector0, dynVector1 :: Object SemanticAnns
dynVar0 = Variable "dyn_var0" dynUInt16SemAnn
dynVector0 = Variable "dyn_vector0" dynVectorAnn
dynVector1 = Variable "dyn_vector1" dynTwoDymVectorAnn

pVar0, pVector0, pVector1 :: Object SemanticAnns
pVar0 = Variable "p_var0" refUInt16SemAnn
pVector0 = Variable "p_vector0" refVectorAnn
pVector1 = Variable "p_vector1" refTwoDymVectorAnn

refVar0expr, refVector0expr, refVector1expr :: Expression SemanticAnns
refVar0expr = ReferenceExpression Mutable var0 refUInt16SemAnn
refVector0expr = ReferenceExpression Mutable vector0 refVectorAnn
refVector1expr = ReferenceExpression Mutable vector1 refTwoDymVectorAnn

refDynVar0expr, refDynVector0expr, refDynVector1expr :: Expression SemanticAnns
refDynVar0expr = ReferenceExpression Mutable dynVar0 refUInt16SemAnn
refDynVector0expr = ReferenceExpression Mutable dynVector0 refVectorAnn
refDynVector1expr = ReferenceExpression Mutable dynVector1 refTwoDymVectorAnn

derefpVar0, derefpVector0, derefpVector1 :: Expression SemanticAnns
derefpVar0 = AccessObject (Dereference pVar0 (objSemAnn Mutable UInt16)) -- | *p_var0 |
derefpVector0 = AccessObject (Dereference pVector0 vectorAnn) -- | *p_vector0 |
derefpVector1 = AccessObject (Dereference pVector1 twoDymVectorAnn) -- | *p_vector1 |

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression empty
 
spec :: Spec
spec = do
  describe "Pretty printing reference expressions" $ do
    it "Prints the expression: &var0" $ do
      renderExpression refVar0expr `shouldBe`
        pack "&var0"
    it "Prints the expression: &vector0" $ do
      renderExpression refVector0expr `shouldBe`
        pack "vector0"
    it "Prints the expression: &vector1" $ do
      renderExpression refVector1expr `shouldBe`
        pack "vector1"
    it "Prints the expression: &dyn_var0" $ do
      renderExpression refDynVar0expr `shouldBe`
        pack "(uint16_t *)(dyn_var0.data)"
    it "Prints the expression: &dyn_vector0" $ do
      renderExpression refDynVector0expr `shouldBe`
        pack "(uint32_t *)(dyn_vector0.data)"
    it "Prints the expression: &dyn_vector1" $ do
      renderExpression refDynVector1expr `shouldBe`
        pack "(int64_t (*)[5])(dyn_vector1.data)"
  describe "Pretty printing dereference expressions" $ do
    it "Prints the expression: *p_var0" $ do
      renderExpression derefpVar0 `shouldBe`
        pack "*p_var0"
    it "Prints the expression: *p_vector0" $ do
      renderExpression derefpVector0 `shouldBe`
        pack "p_vector0"
    it "Prints the expression: *p_vector1" $ do
      renderExpression derefpVector1 `shouldBe`
        pack "p_vector1"
