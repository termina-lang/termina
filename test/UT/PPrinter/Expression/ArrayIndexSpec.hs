module UT.PPrinter.Expression.ArrayIndexSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Control.Monad.Reader
import Generator.CodeGen.Expression
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

vectorAnn, boxArrayAnn, twoDymArrayAnn :: SemanticAnn
vectorAnn = vectorSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))
boxArrayAnn = boxArraySemAnn UInt32 (K (TInteger 10 DecRepr))
twoDymArrayAnn = twoDymArraySemAnn Mutable Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

refArrayAnn :: SemanticAnn
refArrayAnn = refSemAnn (Array UInt32 (K (TInteger 10 DecRepr)))

var0, vector0, vector1 :: Object SemanticAnn
var0 = Variable "var0" (objSemAnn Mutable UInt16)
vector0 = Variable "vector0" vectorAnn
vector1 = Variable "vector1" twoDymArrayAnn

pArray0 :: Object SemanticAnn
pArray0 = Variable "p_vector0" refArrayAnn

usizeIndex3, usizeIndex4, usizeConst0x8 :: Expression SemanticAnn
usizeIndex3 = Constant (I (TInteger 3 DecRepr) (Just USize)) usizeSemAnn
usizeIndex4 = Constant (I (TInteger 4 DecRepr) (Just USize)) usizeSemAnn
usizeConst0x8 = Constant (I (TInteger 8 DecRepr) (Just USize)) usizeSemAnn

boxArray0 :: Object SemanticAnn
boxArray0 = Variable "box_vector0" boxArrayAnn

vector0IndexConstant, vector0IndexVar0 :: Expression SemanticAnn
vector0IndexConstant = AccessObject (ArrayIndexExpression vector0 usizeConst0x8 (objSemAnn Mutable UInt32))
vector0IndexVar0 = AccessObject (ArrayIndexExpression vector0 (AccessObject var0) (objSemAnn Mutable UInt32))

boxArray0IndexConstant, boxArray0IndexVar0 :: Expression SemanticAnn
boxArray0IndexConstant = AccessObject (ArrayIndexExpression (Unbox boxArray0 vectorAnn) usizeConst0x8 (objSemAnn Mutable UInt32))
boxArray0IndexVar0 = AccessObject (ArrayIndexExpression (Unbox boxArray0 vectorAnn) (AccessObject var0) (objSemAnn Mutable UInt32))

vector1IndexFirstDym :: Object SemanticAnn
vector1IndexFirstDym = ArrayIndexExpression vector1 usizeIndex3 (vectorSemAnn Mutable Int64 (K (TInteger 5 DecRepr)))

vector1IndexExpression :: Expression SemanticAnn
vector1IndexExpression = AccessObject (ArrayIndexExpression vector1IndexFirstDym usizeIndex4 (objSemAnn Mutable Int64))

derefpArray0 :: Object SemanticAnn
derefpArray0 = Dereference pArray0 vectorAnn

derefpArray0IndexConstant, derefpArray0IndexVar0 :: Expression SemanticAnn
derefpArray0IndexConstant = AccessObject (ArrayIndexExpression derefpArray0 usizeIndex3 (objSemAnn Mutable UInt32))
derefpArray0IndexVar0 = AccessObject (ArrayIndexExpression derefpArray0 (AccessObject var0) (objSemAnn Mutable UInt32))

renderExpression :: Expression SemanticAnn -> Text
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
    it "Prints the expression: box_vector0[0x08 : u8]" $ do
      renderExpression boxArray0IndexConstant `shouldBe`
        pack "((uint32_t *)box_vector0.data)[8]"
    it "Prints the expression: box_vector0[var0]" $ do
      renderExpression boxArray0IndexVar0 `shouldBe`
        pack "((uint32_t *)box_vector0.data)[var0]"
    it "Prints the expression: *vector0[3 : u32]" $ do
      renderExpression derefpArray0IndexConstant `shouldBe`
        pack "p_vector0[3]"
    it "Prints the expression: *vector0[var0]" $ do
      renderExpression derefpArray0IndexVar0 `shouldBe`
        pack "p_vector0[var0]" 
