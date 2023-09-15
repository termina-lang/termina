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

dynVar0, dynVector0, dynVector1 :: Object SemanticAnns
dynVar0 = Variable "dyn_var0" dynUInt16SemAnn
dynVector0 = Variable "dyn_vector0" dynVectorAnn
dynVector1 = Variable "dyn_vector1" dynTwoDymVectorAnn

pVar0, pVector0, pVector1 :: Object SemanticAnns
pVar0 = Variable "p_var0" refUInt16SemAnn
pVector0 = Variable "p_vector0" refVectorAnn
pVector1 = Variable "p_vector1" refTwoDymVectorAnn

refVar0expr, refVector0expr, refVector1expr :: Expression SemanticAnns
refVar0expr = ReferenceExpression var0 refUInt16SemAnn
refVector0expr = ReferenceExpression vector0 refVectorAnn
refVector1expr = ReferenceExpression vector1 refTwoDymVectorAnn

refDynVar0expr, refDynVector0expr, refDynVector1expr :: Expression SemanticAnns
refDynVar0expr = ReferenceExpression dynVar0 refUInt16SemAnn
refDynVector0expr = ReferenceExpression dynVector0 refVectorAnn
refDynVector1expr = ReferenceExpression dynVector1 refTwoDymVectorAnn

derefpVar0, derefpVector0, derefpVector1 :: Expression SemanticAnns
derefpVar0 = AccessObject (Dereference pVar0 uint16SemAnn) -- | *p_var0 |
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
        pack "(uint16_t *)dyn_var0.data"
    it "Prints the expression: &dyn_vector0" $ do
      renderExpression refDynVector0expr `shouldBe`
        pack "(uint32_t *)dyn_vector0.data"
    it "Prints the expression: &dyn_vector1" $ do
      renderExpression refDynVector1expr `shouldBe`
        pack "(int64_t (*)[5])dyn_vector1.data"
  describe "Pretty printing dereference expressions" $ do
    it "Prints the expression: *p_var0" $ do
      renderExpression derefpVar0 `shouldBe`
        pack "*(p_var0)"
    it "Prints the expression: *p_vector0" $ do
      renderExpression derefpVector0 `shouldBe`
        pack "p_vector0"
    it "Prints the expression: *p_vector1" $ do
      renderExpression derefpVector1 `shouldBe`
        pack "p_vector1"
