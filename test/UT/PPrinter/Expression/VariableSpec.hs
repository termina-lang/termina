module UT.PPrinter.Expression.VariableSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Control.Monad.Reader
import Generator.CodeGen.Expression
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

vectorAnn, twoDymArrayAnn, boxTwoDymArrayAnn, boxThreeDymArrayAnn :: SemanticAnn
vectorAnn = vectorSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))
twoDymArrayAnn = twoDymArraySemAnn Mutable Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))
boxTwoDymArrayAnn = boxTwoDymArraySemAnn Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))
boxThreeDymArrayAnn = boxThreeDymArraySemAnn Char (K (TInteger 40 DecRepr)) (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

var0, vector0, vector1 :: Object SemanticAnn
var0 = Variable "var0" (objSemAnn Mutable UInt16)
vector0 = Variable "vector0" vectorAnn
vector1 = Variable "vector1" twoDymArrayAnn

boxVar0, boxArray1, boxArray2 :: Object SemanticAnn
boxVar0 = Variable "box_var0" boxUInt16SemAnn
boxArray1 = Variable "box_vector1" boxTwoDymArrayAnn
boxArray2 = Variable "box_vector2" boxThreeDymArrayAnn

renderExpression :: Expression SemanticAnn -> Text
renderExpression expr = 
  case runReaderT (genExpression expr) empty of
    Left err -> pack $ show err
    Right cExpr -> render $ runReader (pprint cExpr) (CPrinterConfig False False)

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
    it "Prints the variable box_var0 : 'box u16" $ do
      renderExpression (AccessObject boxVar0) `shouldBe`
        pack "box_var0"
    it "Prints the unboxed variable box_var0 : 'box u16" $ do
      renderExpression (AccessObject (Unbox boxVar0 (objSemAnn Mutable UInt16))) `shouldBe`
        pack "*(uint16_t *)box_var0.data"
    it "Prints the variable box_vector1 : 'box [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject boxArray1) `shouldBe`
        pack "box_vector1"
    it "Prints the unboxed variable box_vector1 : 'box [[i64; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject (Unbox boxArray1 twoDymArrayAnn)) `shouldBe`
        pack "(int64_t (*)[5])box_vector1.data"
    it "Prints the unboxed variable box_vector2 : [[[char; 40 : u32]; 5 : u32]; 10 : u32]" $ do
      renderExpression (AccessObject (Unbox boxArray2 boxThreeDymArrayAnn)) `shouldBe`
        pack "(char (*)[5][40])box_vector2.data"
