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

vectorAnn, dynArrayAnn, twoDymArrayAnn, dynTwoDymArrayAnn :: SemanticAnns
vectorAnn = vectorSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))
dynArrayAnn = dynArraySemAnn UInt32 (K (TInteger 10 DecRepr))
twoDymArrayAnn = twoDymArraySemAnn Mutable Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))
dynTwoDymArrayAnn = dynTwoDymArraySemAnn Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

refArrayAnn, refTwoDymArrayAnn :: SemanticAnns
refArrayAnn = refArraySemAnn UInt32 (K (TInteger 10 DecRepr))
refTwoDymArrayAnn = refTwoDymArraySemAnn Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

var0, vector0, vector1 :: Object SemanticAnns
var0 = Variable "var0" (objSemAnn Mutable UInt16)
vector0 = Variable "vector0" vectorAnn
vector1 = Variable "vector1" twoDymArrayAnn

dynVar0, dynArray0, dynArray1 :: Object SemanticAnns
dynVar0 = Variable "dyn_var0" dynUInt16SemAnn
dynArray0 = Variable "dyn_vector0" dynArrayAnn
dynArray1 = Variable "dyn_vector1" dynTwoDymArrayAnn

pVar0, pArray0, pArray1 :: Object SemanticAnns
pVar0 = Variable "p_var0" refUInt16SemAnn
pArray0 = Variable "p_vector0" refArrayAnn
pArray1 = Variable "p_vector1" refTwoDymArrayAnn

refVar0expr, refArray0expr, refArray1expr :: Expression SemanticAnns
refVar0expr = ReferenceExpression Mutable var0 refUInt16SemAnn
refArray0expr = ReferenceExpression Mutable vector0 refArrayAnn
refArray1expr = ReferenceExpression Mutable vector1 refTwoDymArrayAnn

refDynVar0expr, refDynArray0expr, refDynArray1expr :: Expression SemanticAnns
refDynVar0expr = ReferenceExpression Mutable dynVar0 refUInt16SemAnn
refDynArray0expr = ReferenceExpression Mutable dynArray0 refArrayAnn
refDynArray1expr = ReferenceExpression Mutable dynArray1 refTwoDymArrayAnn

derefpVar0, derefpArray0, derefpArray1 :: Expression SemanticAnns
derefpVar0 = AccessObject (Dereference pVar0 (objSemAnn Mutable UInt16)) -- | *p_var0 |
derefpArray0 = AccessObject (Dereference pArray0 vectorAnn) -- | *p_vector0 |
derefpArray1 = AccessObject (Dereference pArray1 twoDymArrayAnn) -- | *p_vector1 |

renderExpression :: Expression SemanticAnns -> Text
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
    it "Prints the expression: &dyn_var0" $ do
      renderExpression refDynVar0expr `shouldBe`
        pack "(uint16_t *)dyn_var0.data"
    it "Prints the expression: &dyn_vector0" $ do
      renderExpression refDynArray0expr `shouldBe`
        pack "(uint32_t *)dyn_vector0.data"
    it "Prints the expression: &dyn_vector1" $ do
      renderExpression refDynArray1expr `shouldBe`
        pack "(int64_t (*)[5])dyn_vector1.data"
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
