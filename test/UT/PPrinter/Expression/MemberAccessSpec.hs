module UT.PPrinter.Expression.MemberAccessSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text hiding (empty)
import Data.Map
import Control.Monad.Reader
import Generator.CodeGen.Expression
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common
import Semantic.Types

tmDescriptor0, tmDescriptor1 :: Object SemanticAnn
tmDescriptor0 = Variable "tm_descriptor0" (structObjSemAnn Mutable "TMDescriptor")
tmDescriptor1 = Variable "tm_descriptor1" (boxStructTypeSemAnn "TMDescriptor")

pTMDescriptor0 :: Object SemanticAnn
pTMDescriptor0 = Variable "p_tm_descriptor0" (refStructSemAnn "TMDescriptor")

unboxTMDescriptor1 :: Object SemanticAnn
unboxTMDescriptor1 = Unbox tmDescriptor1 (structObjSemAnn Mutable "TMDescriptor")

tmDescriptor0field0, tmDescriptor1field0 :: Expression SemanticAnn
tmDescriptor0field0 = AccessObject (MemberAccess tmDescriptor0 "field0" (objSemAnn Mutable TUInt32))
tmDescriptor1field0 = AccessObject (MemberAccess unboxTMDescriptor1 "field0" (objSemAnn Mutable TUInt32))

pTMDescriptor0field0 :: Expression SemanticAnn
pTMDescriptor0field0 = AccessObject (DereferenceMemberAccess pTMDescriptor0 "field0" (objSemAnn Mutable TUInt32))

renderExpression :: Expression SemanticAnn -> Text
renderExpression expr = 
  case runReaderT (genExpression expr) empty of
    Left err -> pack $ show err
    Right cExpr -> render $ runReader (pprint cExpr) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing member access expressions" $ do
    it "Prints the expression: tm_descriptor0.field0" $ do
      renderExpression tmDescriptor0field0 `shouldBe`
        pack "tm_descriptor0.field0"
    it "Prints the expression: tm_descriptor1.field0" $ do
      renderExpression tmDescriptor1field0 `shouldBe`
        pack "(*(TMDescriptor *)tm_descriptor1.data).field0"
    it "Prints the expression: p_tm_descriptor0->field0" $ do
      renderExpression pTMDescriptor0field0 `shouldBe`
        pack "p_tm_descriptor0->field0"
