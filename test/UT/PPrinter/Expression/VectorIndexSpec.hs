module UT.PPrinter.Expression.VectorIndexSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Control.Monad.Reader
import Generator.CGenerator
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

vectorAnn, dynVectorAnn, twoDymVectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn Mutable UInt32 (K 10)
dynVectorAnn = dynVectorSemAnn UInt32 (K 10)
twoDymVectorAnn = twoDymVectorSemAnn Mutable Int64 (K 5) (K 10)

refVectorAnn :: SemanticAnns
refVectorAnn = refSemAnn (Vector UInt32 (K 10))

var0, vector0, vector1 :: Object SemanticAnns
var0 = Variable "var0" (objSemAnn Mutable UInt16)
vector0 = Variable "vector0" vectorAnn
vector1 = Variable "vector1" twoDymVectorAnn

pVector0 :: Object SemanticAnns
pVector0 = Variable "p_vector0" refVectorAnn

usizeIndex3, usizeIndex4, usizeConst0x8 :: Expression SemanticAnns
usizeIndex3 = Constant (I USize 3) usizeSemAnn
usizeIndex4 = Constant (I USize 4) usizeSemAnn
usizeConst0x8 = Constant (I USize 8) usizeSemAnn

dynVector0 :: Object SemanticAnns
dynVector0 = Variable "dyn_vector0" dynVectorAnn

vector0IndexConstant, vector0IndexVar0 :: Expression SemanticAnns
vector0IndexConstant = AccessObject (VectorIndexExpression vector0 usizeConst0x8 (objSemAnn Mutable UInt32))
vector0IndexVar0 = AccessObject (VectorIndexExpression vector0 (AccessObject var0) (objSemAnn Mutable UInt32))

dynVector0IndexConstant, dynVector0IndexVar0 :: Expression SemanticAnns
dynVector0IndexConstant = AccessObject (VectorIndexExpression (Undyn dynVector0 vectorAnn) usizeConst0x8 (objSemAnn Mutable UInt32))
dynVector0IndexVar0 = AccessObject (VectorIndexExpression (Undyn dynVector0 vectorAnn) (AccessObject var0) (objSemAnn Mutable UInt32))

vector1IndexFirstDym :: Object SemanticAnns
vector1IndexFirstDym = VectorIndexExpression vector1 usizeIndex3 (vectorSemAnn Mutable Int64 (K 5))

vector1IndexExpression :: Expression SemanticAnns
vector1IndexExpression = AccessObject (VectorIndexExpression vector1IndexFirstDym usizeIndex4 (objSemAnn Mutable Int64))

derefpVector0 :: Object SemanticAnns
derefpVector0 = Dereference pVector0 vectorAnn

derefpVector0IndexConstant, derefpVector0IndexVar0 :: Expression SemanticAnns
derefpVector0IndexConstant = AccessObject (VectorIndexExpression derefpVector0 usizeIndex3 (objSemAnn Mutable UInt32))
derefpVector0IndexVar0 = AccessObject (VectorIndexExpression derefpVector0 (AccessObject var0) (objSemAnn Mutable UInt32))

renderExpression :: Expression SemanticAnns -> Text
renderExpression expr = 
  case runReaderT (genExpression expr) empty of
    Left err -> pack $ show err
    Right cExpr -> render $ runReader (pprint cExpr) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing vector index expressions" $ do
    it "Prints the expression: vector[0x08 : u8]" $ do
      renderExpression vector0IndexConstant `shouldBe`
        pack "vector0[8]"
    it "Prints the expression: vector[var0]" $ do
      renderExpression vector0IndexVar0 `shouldBe`
        pack "vector0[var0]"
    it "Prints the expression: vector1[3 : u32][4 : u32]" $ do
      renderExpression vector1IndexExpression `shouldBe`
        pack "vector1[3][4]"
    it "Prints the expression: dyn_vector0[0x08 : u8]" $ do
      renderExpression dynVector0IndexConstant `shouldBe`
        pack "((uint32_t *)dyn_vector0.data)[8]"
    it "Prints the expression: dyn_vector0[var0]" $ do
      renderExpression dynVector0IndexVar0 `shouldBe`
        pack "((uint32_t *)dyn_vector0.data)[var0]"
    it "Prints the expression: *vector0[3 : u32]" $ do
      renderExpression derefpVector0IndexConstant `shouldBe`
        pack "p_vector0[3]"
    it "Prints the expression: *vector0[var0]" $ do
      renderExpression derefpVector0IndexVar0 `shouldBe`
        pack "p_vector0[var0]" 
