module UT.PPrinter.Expression.VariableSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

vectorAnn, twoDymVectorAnn, dynTwoDymVectorAnn, dynThreeDymVectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn Mutable UInt32 (I UInt32 10)
twoDymVectorAnn = twoDymVectorSemAnn Mutable Int64 (I UInt32 5) (I UInt32 10)
dynTwoDymVectorAnn = dynTwoDymVectorSemAnn Int64 (I UInt32 5) (I UInt32 10)
dynThreeDymVectorAnn = dynThreeDymVectorSemAnn Char (I UInt32 40) (I UInt32 5) (I UInt32 10)

var0, vector0, vector1 :: Object SemanticAnns
var0 = Variable "var0" (objSemAnn Mutable UInt16)
vector0 = Variable "vector0" vectorAnn
vector1 = Variable "vector1" twoDymVectorAnn

dynVar0, dynVector1, dynVector2 :: Object SemanticAnns
dynVar0 = Variable "dyn_var0" dynUInt16SemAnn
dynVector1 = Variable "dyn_vector1" dynTwoDymVectorAnn
dynVector2 = Variable "dyn_vector2" dynThreeDymVectorAnn


renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression empty

spec :: Spec
spec = do
  describe "Pretty printing variable expression" $ do
    it "Prints the variable var0 : u16" $ do
      renderExpression (AccessObject var0) `shouldBe`
        pack "var0"
    it "Prints the variable vector0 : [u32; 10 : u32]" $ do
      renderExpression (AccessObject vector0) `shouldBe`
        pack "vector0"
    it "Prints the variable vector1 : [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject vector1) `shouldBe`
        pack "vector1"
    it "Prints the variable dyn_var0 : 'dyn u16" $ do
      renderExpression (AccessObject dynVar0) `shouldBe`
        pack "dyn_var0"
    it "Prints the undyned variable dyn_var0 : 'dyn u16" $ do
      renderExpression (AccessObject (Undyn dynVar0 (objSemAnn Mutable UInt16))) `shouldBe`
        pack "*((uint16_t *)dyn_var0.data)"
    it "Prints the variable dyn_vector1 : 'dyn [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject dynVector1) `shouldBe`
        pack "dyn_vector1"
    it "Prints the undyned variable dyn_vector1 : 'dyn [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject (Undyn dynVector1 twoDymVectorAnn)) `shouldBe`
        pack "((int64_t (*)[5])dyn_vector1.data)"
    it "Prints the undyned variable dyn_vector2 : [[[char; 40 : u32]; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject (Undyn dynVector2 dynThreeDymVectorAnn)) `shouldBe`
        pack "((char (*)[5][40])dyn_vector2.data)"
