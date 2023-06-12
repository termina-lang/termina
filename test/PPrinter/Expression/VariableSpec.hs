module PPrinter.Expression.VariableSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Data.Text
import Semantic.Monad
import PPrinter.Expression

integerTS, vectorTS, twoDimVectorTS, threeDimVectorTS :: TypeSpecifier
integerTS = UInt16
vectorTS = Vector UInt32 (KC (I UInt32 10))
twoDimVectorTS = Vector (Vector Int64 (KC (I UInt32 5))) (KC (I UInt32 10))
threeDimVectorTS = Vector (Vector (Vector Char (KC (I UInt32 40))) (KC (I UInt32 5))) (KC (I UInt32 10))


integerVariable :: Expression SemanticAnns
integerVariable = Variable "id0" (SemAnn undefined integerTS)

vectorVariable :: Expression SemanticAnns
vectorVariable = Variable "id0" (SemAnn undefined vectorTS)

twoDimensionVectorVariable :: Expression SemanticAnns
twoDimensionVectorVariable = Variable "id0" (SemAnn undefined twoDimVectorTS)

dynamicIntegerVariable :: Expression SemanticAnns
dynamicIntegerVariable = Variable "id0" (SemAnn undefined (DynamicSubtype integerTS))

dynamicVectorVariable :: Expression SemanticAnns
dynamicVectorVariable = Variable "id0" (SemAnn undefined (DynamicSubtype vectorTS))

dynamicTwoDimensionVectorVariable :: Expression SemanticAnns
dynamicTwoDimensionVectorVariable = Variable "id0" (SemAnn undefined (DynamicSubtype twoDimVectorTS))

dynamicThreeDimensionVectorVariable :: Expression SemanticAnns
dynamicThreeDimensionVectorVariable = Variable "id0" (SemAnn undefined (DynamicSubtype threeDimVectorTS))

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression

spec :: Spec
spec = do
  describe "Pretty printing variable expression" $ do
    it "Prints the name of a variable of type u16" $ do
      renderExpression integerVariable `shouldBe`
        pack "id0"
    it "Prints the name of a variable of type [u32; 10 : u32]" $ do
      renderExpression vectorVariable `shouldBe`
        pack "id0"
    it "Prints the name of a variable of type [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression twoDimensionVectorVariable `shouldBe`
        pack "id0"
    it "Prints the name of a 'dyn variable of type u16" $ do
      renderExpression dynamicIntegerVariable `shouldBe`
        pack "*((uint16_t *)id0.datum)"
    it "Prints the name of a 'dyn variable of type [u32; 10 : u32]" $ do
      renderExpression dynamicVectorVariable `shouldBe`
        pack "((uint32_t *)id0.datum)"
    it "Prints the name of a 'dyn variable of type [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression dynamicTwoDimensionVectorVariable `shouldBe`
        pack "((int64_t (*)[10])id0.datum)"
    it "Prints the name of a 'dyn variable of type [[[char; 40 : u32]; 5 : u32]; 10 : u32]" $ do
      renderExpression dynamicThreeDimensionVectorVariable `shouldBe`
        pack "((char (*)[5][10])id0.datum)"