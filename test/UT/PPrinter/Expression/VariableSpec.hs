module UT.PPrinter.Expression.VariableSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Data.Text
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

integerVariable, vectorVariable, twoDimensionVectorVariable :: Expression SemanticAnns
integerVariable = Variable "id0" (SemAnn undefined uint16TS)
vectorVariable = Variable "id0" (SemAnn undefined vectorTS)
twoDimensionVectorVariable = Variable "id0" (SemAnn undefined twoDimVectorTS)

dynamicIntegerVariable, dynamicVectorVariable :: Expression SemanticAnns
dynamicIntegerVariable = Variable "id0" (SemAnn undefined (DynamicSubtype uint16TS))
dynamicVectorVariable = Variable "id0" (SemAnn undefined (DynamicSubtype vectorTS))

dynamicTwoDimensionVectorVariable, dynamicThreeDimensionVectorVariable :: Expression SemanticAnns
dynamicTwoDimensionVectorVariable = Variable "id0" (SemAnn undefined (DynamicSubtype twoDimVectorTS))
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
    it "Prints the name of a variable of type [[i64; 5 : u32]; 10 : u32] between parenthesis" $ do
      renderExpression (ParensExpression twoDimensionVectorVariable (SemAnn undefined twoDimVectorTS)) `shouldBe`
        pack "(id0)"
    it "Prints the name of a 'dyn variable of type u16" $ do
      renderExpression dynamicIntegerVariable `shouldBe`
        pack "*((uint16_t *)id0.datum)"
    it "Prints the name of a 'dyn variable of type [u32; 10 : u32] between parenthesis" $ do
      renderExpression (ParensExpression dynamicVectorVariable (SemAnn undefined (DynamicSubtype vectorTS))) `shouldBe`
        pack "(((uint32_t *)id0.datum))"
    it "Prints the name of a 'dyn variable of type [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression dynamicTwoDimensionVectorVariable `shouldBe`
        pack "((int64_t (*)[10])id0.datum)"
    it "Prints the name of a 'dyn variable of type [[[char; 40 : u32]; 5 : u32]; 10 : u32]" $ do
      renderExpression dynamicThreeDimensionVectorVariable `shouldBe`
        pack "((char (*)[5][10])id0.datum)"