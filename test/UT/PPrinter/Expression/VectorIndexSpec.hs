module UT.PPrinter.Expression.VectorIndexSpec (spec) where

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

uint32Index3, uint32Index4 :: Expression SemanticAnns
uint32Index3 = Constant (I UInt32 3) (SemAnn undefined UInt32)
uint32Index4 = Constant (I UInt32 4) (SemAnn undefined UInt32)

dynVector0, dynVector1 :: Expression SemanticAnns
dynVector0 = Variable "dyn_vector0" (SemAnn undefined (DynamicSubtype vectorTS))
dynVector1 = Variable "dyn_vector1" (SemAnn undefined (DynamicSubtype twoDimVectorTS))

vector0IndexConstant, vector0IndexVar0 :: Expression SemanticAnns
vector0IndexConstant = VectorIndexExpression vector0 uint8Const (SemAnn undefined uint32TS)
vector0IndexVar0 = VectorIndexExpression vector0 var0 (SemAnn undefined uint32TS)

dynVector0IndexConstant, dynVector0IndexVar0 :: Expression SemanticAnns
dynVector0IndexConstant = VectorIndexExpression dynVector0 uint8Const (SemAnn undefined uint32TS)
dynVector0IndexVar0 = VectorIndexExpression dynVector0 var0 (SemAnn undefined uint32TS)

vector1IndexFirstDym, vector1IndexExpression :: Expression SemanticAnns
vector1IndexFirstDym = VectorIndexExpression vector1 uint32Index3 (SemAnn undefined (Vector Int64 (KC (I UInt32 5))))
vector1IndexExpression = VectorIndexExpression vector1IndexFirstDym uint32Index4 (SemAnn undefined Int64)

pVector0expr :: Expression SemanticAnns
pVector0expr = ReferenceExpression vector0 (SemAnn undefined (Reference vectorTS))

pDynVector0expr, pDynVector1expr :: Expression SemanticAnns
pDynVector0expr = ReferenceExpression dynVector0 (SemAnn undefined (Reference (DynamicSubtype vectorTS)))
pDynVector1expr = ReferenceExpression dynVector1 (SemAnn undefined (Reference (DynamicSubtype twoDimVectorTS)))

derefpVector0 :: Expression SemanticAnns
derefpVector0 = DereferenceExpression pVector0expr (SemAnn undefined vectorTS)

derefpVector0IndexConstant, derefpVector0IndexVar0 :: Expression SemanticAnns
derefpVector0IndexConstant = VectorIndexExpression derefpVector0 uint32Index3 (SemAnn undefined uint32TS)
derefpVector0IndexVar0 = VectorIndexExpression derefpVector0 var0 (SemAnn undefined uint32TS)

derefpDynVector0expr, derefpDynVector1expr :: Expression SemanticAnns
derefpDynVector0expr = DereferenceExpression pDynVector0expr (SemAnn undefined (DynamicSubtype vectorTS))
derefpDynVector1expr = DereferenceExpression pDynVector1expr (SemAnn undefined (DynamicSubtype twoDimVectorTS))

derefpDynVector0IndexExpression :: Expression SemanticAnns
derefpDynVector0IndexExpression = VectorIndexExpression derefpDynVector0expr uint32Index3 (SemAnn undefined uint32TS)

derefpDynVector1FirstDym, derefpDynVector1IndexExpression :: Expression SemanticAnns
derefpDynVector1FirstDym = VectorIndexExpression derefpDynVector1expr uint32Index3 (SemAnn undefined (Vector Int64 (KC (I UInt32 5))))
derefpDynVector1IndexExpression = VectorIndexExpression derefpDynVector1FirstDym uint32Index4 (SemAnn undefined Int64)

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppRootExpression

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
        pack "((uint32_t *)dyn_vector0.datum)[(uint32_t)3]"
    it "Prints the expression: *vector1[3 : u32][4 : u32]" $ do
      renderExpression derefpDynVector1IndexExpression `shouldBe`
        pack "((int64_t (*)[10])dyn_vector1.datum)[(uint32_t)3][(uint32_t)4]"