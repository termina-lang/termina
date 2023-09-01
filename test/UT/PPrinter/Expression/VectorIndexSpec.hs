module UT.PPrinter.Expression.VectorIndexSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

vectorAnn, dynVectorAnn, twoDymVectorAnn, dynTwoDymVectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn UInt32 (I UInt32 10)
dynVectorAnn = dynVectorSemAnn UInt32 (I UInt32 10)
twoDymVectorAnn = twoDymVectorSemAnn Int64 (I UInt32 5) (I UInt32 10)
dynTwoDymVectorAnn = dynTwoDymVectorSemAnn Int64 (I UInt32 5) (I UInt32 10)

refVectorAnn, refTwoDymVectorAnn :: SemanticAnns
refVectorAnn = refSemAnn (Vector UInt32 (KC (I UInt32 10)))
refTwoDymVectorAnn = refSemAnn (Vector (Vector Int64 (KC (I UInt32 5))) (KC (I UInt32 10)))

var0, vector0, vector1 :: Object SemanticAnns
var0 = Variable "var0" uint16SemAnn
vector0 = Variable "vector0" vectorAnn
vector1 = Variable "vector1" twoDymVectorAnn

uint32Index3, uint32Index4, uint8Const0x8 :: Expression SemanticAnns
uint32Index3 = Constant (I UInt32 3) uint32SemAnn
uint32Index4 = Constant (I UInt32 4) uint32SemAnn
uint8Const0x8 = Constant (I UInt8 8) uint8SemAnn

dynVector0, dynVector1 :: Object SemanticAnns
dynVector0 = Variable "dyn_vector0" dynVectorAnn
dynVector1 = Variable "dyn_vector1" dynTwoDymVectorAnn

vector0IndexConstant, vector0IndexVar0 :: Expression SemanticAnns
vector0IndexConstant = (AccessObject ((VectorIndexExpression vector0 uint8Const0x8 uint32SemAnn)))
vector0IndexVar0 = (AccessObject ((VectorIndexExpression vector0 (AccessObject (var0)) uint32SemAnn)))

dynVector0IndexConstant, dynVector0IndexVar0 :: Expression SemanticAnns
dynVector0IndexConstant = (AccessObject ((VectorIndexExpression (Undyn dynVector0 vectorAnn) uint8Const0x8 uint32SemAnn)))
dynVector0IndexVar0 = (AccessObject ((VectorIndexExpression (Undyn dynVector0 vectorAnn) (AccessObject (var0)) uint32SemAnn)))

vector1IndexFirstDym :: Object SemanticAnns
vector1IndexFirstDym = VectorIndexExpression vector1 uint32Index3 (vectorSemAnn Int64 (I UInt32 5))

vector1IndexExpression :: Expression SemanticAnns
vector1IndexExpression = (AccessObject ((VectorIndexExpression vector1IndexFirstDym uint32Index4 int64SemAnn)))

pVector0expr :: Expression SemanticAnns
pVector0expr = ReferenceExpression ( vector0) refVectorAnn

pDynVector0expr, pDynVector1expr :: Expression SemanticAnns
pDynVector0expr = ReferenceExpression ( dynVector0) refVectorAnn
pDynVector1expr = ReferenceExpression ( dynVector1) refTwoDymVectorAnn

derefpVector0 :: Object SemanticAnns
derefpVector0 = Dereference (IdentifierExpression pVector0expr refVectorAnn) vectorAnn

derefpVector0IndexConstant, derefpVector0IndexVar0 :: Expression SemanticAnns
derefpVector0IndexConstant = (AccessObject ((VectorIndexExpression derefpVector0 uint32Index3 uint32SemAnn)))
derefpVector0IndexVar0 = (AccessObject ((VectorIndexExpression derefpVector0 (AccessObject (var0)) uint32SemAnn)))

derefpDynVector0expr, derefpDynVector1expr :: Object SemanticAnns
derefpDynVector0expr = Dereference (IdentifierExpression pDynVector0expr refVectorAnn) vectorAnn
derefpDynVector1expr = Dereference (IdentifierExpression pDynVector1expr refTwoDymVectorAnn) twoDymVectorAnn

derefpDynVector0IndexExpression :: Expression SemanticAnns
derefpDynVector0IndexExpression = (AccessObject ((VectorIndexExpression derefpDynVector0expr uint32Index3 uint32SemAnn)))

derefpDynVector1FirstDym, derefpDynVector1IndexExpression :: Expression SemanticAnns
derefpDynVector1FirstDym = (AccessObject ((VectorIndexExpression derefpDynVector1expr uint32Index3 (vectorSemAnn Int64 (I UInt32 5)))))
derefpDynVector1IndexExpression = (AccessObject ((VectorIndexExpression (IdentifierExpression derefpDynVector1FirstDym (vectorSemAnn Int64 (I UInt32 5))) uint32Index4 int64SemAnn)))

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression empty

spec :: Spec
spec = do
  describe "Pretty printing vector index expressions" $ do
    it "Prints the expression: vector[0x08 : u8]" $ do
      renderExpression vector0IndexConstant `shouldBe`
        pack "vector0[(uint8_t)8]"
    it "Prints the expression: vector[var0]" $ do
      renderExpression vector0IndexVar0 `shouldBe`
        pack "vector0[var0]"
    it "Prints the expression: vector1[3 : u32][4 : u32]" $ do
      renderExpression vector1IndexExpression `shouldBe`
        pack "vector1[(uint32_t)3][(uint32_t)4]"
    it "Prints the expression: dyn_vector0[0x08 : u8]" $ do
      renderExpression dynVector0IndexConstant `shouldBe`
        pack "((uint32_t *)dyn_vector0.datum)[(uint8_t)8]"
    it "Prints the expression: dyn_vector0[var0]" $ do
      renderExpression dynVector0IndexVar0 `shouldBe`
        pack "((uint32_t *)dyn_vector0.datum)[var0]"
    it "Prints the expression: *vector0[3 : u32]" $ do
      renderExpression derefpVector0IndexConstant `shouldBe`
        pack "vector0[(uint32_t)3]"
    it "Prints the expression: *vector0[var0]" $ do
      renderExpression derefpVector0IndexVar0 `shouldBe`
        pack "vector0[var0]" 
    it "Prints the expression: *dyn_vector0[3 : u32]" $ do
      renderExpression derefpDynVector0IndexExpression `shouldBe`
        pack "(uint32_t *)dyn_vector0.datum[(uint32_t)3]"
    it "Prints the expression: *vector1[3 : u32][4 : u32]" $ do
      renderExpression derefpDynVector1IndexExpression `shouldBe`
        pack "(int64_t (*)[5])dyn_vector1.datum[(uint32_t)3][(uint32_t)4]"
