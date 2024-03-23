module UT.PPrinter.Expression.MemberAccessSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Control.Monad.Reader
import Generator.Expression
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common
import Semantic.Monad

tmDescriptor0, tmDescriptor1 :: Object SemanticAnns
tmDescriptor0 = Variable "tm_descriptor0" (definedTypeSemAnn Mutable "TMDescriptor")
tmDescriptor1 = Variable "tm_descriptor1" (dynDefinedTypeSemAnn "TMDescriptor")

pTMDescriptor0 :: Object SemanticAnns
pTMDescriptor0 = Variable "p_tm_descriptor0" (refDefinedTypeSemAnn "TMDescriptor")

undynTMDescriptor1 :: Object SemanticAnns
undynTMDescriptor1 = Undyn tmDescriptor1 (definedTypeSemAnn Mutable "TMDescriptor")

tmDescriptor0field0, tmDescriptor1field0 :: Expression SemanticAnns
tmDescriptor0field0 = AccessObject (MemberAccess tmDescriptor0 "field0" (objSemAnn Mutable UInt32))
tmDescriptor1field0 = AccessObject (MemberAccess undynTMDescriptor1 "field0" (objSemAnn Mutable UInt32))

pTMDescriptor0field0 :: Expression SemanticAnns
pTMDescriptor0field0 = AccessObject (DereferenceMemberAccess pTMDescriptor0 "field0" (objSemAnn Mutable UInt32))

renderExpression :: Expression SemanticAnns -> Text
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
