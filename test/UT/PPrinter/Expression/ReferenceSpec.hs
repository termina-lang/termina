module UT.PPrinter.Expression.ReferenceSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Control.Monad.Reader
import Generator.CodeGen.Expression
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common
import Semantic.Monad

vectorObjAnn, boxArrayObjAnn, twoDymArrayObjAnn, boxTwoDymArrayObjAnn :: SemanticAnn
vectorObjAnn = vectorObjSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))
boxArrayObjAnn = boxArrayObjSemAnn UInt32 (K (TInteger 10 DecRepr))
twoDymArrayObjAnn = twoDymArrayObjSemAnn Mutable Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))
boxTwoDymArrayObjAnn = boxTwoDymArrayObjSemAnn Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

refArrayAnn, refTwoDymArrayAnn :: SemanticAnn
refArrayAnn = refArraySemAnn UInt32 (K (TInteger 10 DecRepr))
refTwoDymArrayAnn = refTwoDymArraySemAnn Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

var0, vector0, vector1 :: Object SemanticAnn
var0 = Variable "var0" (objSemAnn Mutable UInt16)
vector0 = Variable "vector0" vectorObjAnn
vector1 = Variable "vector1" twoDymArrayObjAnn

boxVar0, boxArray0, boxArray1 :: Object SemanticAnn
boxVar0 = Variable "box_var0" boxUInt16SemAnn
boxArray0 = Variable "box_vector0" boxArrayObjAnn
boxArray1 = Variable "box_vector1" boxTwoDymArrayObjAnn

pVar0, pArray0, pArray1 :: Object SemanticAnn
pVar0 = Variable "p_var0" refUInt16SemAnn
pArray0 = Variable "p_vector0" refArrayAnn
pArray1 = Variable "p_vector1" refTwoDymArrayAnn

refVar0expr, refArray0expr, refArray1expr :: Expression SemanticAnn
refVar0expr = ReferenceExpression Mutable var0 refUInt16SemAnn
refArray0expr = ReferenceExpression Mutable vector0 refArrayAnn
refArray1expr = ReferenceExpression Mutable vector1 refTwoDymArrayAnn

refBoxVar0expr, refBoxArray0expr, refBoxArray1expr :: Expression SemanticAnn
refBoxVar0expr = ReferenceExpression Mutable boxVar0 refUInt16SemAnn
refBoxArray0expr = ReferenceExpression Mutable boxArray0 refArrayAnn
refBoxArray1expr = ReferenceExpression Mutable boxArray1 refTwoDymArrayAnn

derefpVar0, derefpArray0, derefpArray1 :: Expression SemanticAnn
derefpVar0 = AccessObject (Dereference pVar0 (objSemAnn Mutable UInt16)) -- | *p_var0 |
derefpArray0 = AccessObject (Dereference pArray0 vectorObjAnn) -- | *p_vector0 |
derefpArray1 = AccessObject (Dereference pArray1 twoDymArrayObjAnn) -- | *p_vector1 |

renderExpression :: Expression SemanticAnn -> Text
renderExpression expr = 
  case runReaderT (genExpression expr) empty of
    Left err -> pack $ show err
    Right cExpr -> render $ runReader (pprint cExpr) (CPrinterConfig False False)
 
spec :: Spec
spec = do
  describe "Pretty printing reference expressions" $ do
    it "Prints the expression: &var0" $ do
      renderExpression refVar0expr `shouldBe`
        pack "&var0"
    it "Prints the expression: &vector0" $ do
      renderExpression refArray0expr `shouldBe`
        pack "vector0"
    it "Prints the expression: &vector1" $ do
      renderExpression refArray1expr `shouldBe`
        pack "vector1"
    it "Prints the expression: &box_var0" $ do
      renderExpression refBoxVar0expr `shouldBe`
        pack "(uint16_t *)box_var0.data"
    it "Prints the expression: &box_vector0" $ do
      renderExpression refBoxArray0expr `shouldBe`
        pack "(uint32_t *)box_vector0.data"
    it "Prints the expression: &box_vector1" $ do
      renderExpression refBoxArray1expr `shouldBe`
        pack "(int64_t (*)[5])box_vector1.data"
  describe "Pretty printing dereference expressions" $ do
    it "Prints the expression: *p_var0" $ do
      renderExpression derefpVar0 `shouldBe`
        pack "*p_var0"
    it "Prints the expression: *p_vector0" $ do
      renderExpression derefpArray0 `shouldBe`
        pack "p_vector0"
    it "Prints the expression: *p_vector1" $ do
      renderExpression derefpArray1 `shouldBe`
        pack "p_vector1"
