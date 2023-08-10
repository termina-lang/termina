module UT.PPrinter.Expression.MemberAccessSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import PPrinter.Expression
import UT.PPrinter.Expression.Common
import Semantic.Monad

tmDescriptor0, tmDescriptor1 :: Object' Expression SemanticAnns
tmDescriptor0 = Variable "tm_descriptor0" (definedTypeSemAnn "TMDescriptor")
tmDescriptor1 = Variable "tm_descriptor1" (dynDefinedTypeSemAnn "TMDescriptor")

undynTMDescriptor1 :: Object' Expression SemanticAnns
undynTMDescriptor1 = Undyn tmDescriptor1 (definedTypeSemAnn "TMDescriptor")

tmDescriptor0field0, tmDescriptor1field0 :: Expression SemanticAnns
tmDescriptor0field0 = AccessObject (RHS (MemberAccess tmDescriptor0 "field0" uint32SemAnn))
tmDescriptor1field0 = AccessObject (RHS (MemberAccess undynTMDescriptor1 "field0" uint32SemAnn))

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression empty

spec :: Spec
spec = do
  describe "Pretty printing member access expressions" $ do
    it "Prints the expression: tm_descriptor0.field0" $ do
      renderExpression tmDescriptor0field0 `shouldBe`
        pack "tm_descriptor0.field0"
    it "Prints the expression: tm_descriptor1.field0" $ do
      renderExpression tmDescriptor1field0 `shouldBe`
        pack "*((TMDescriptor *)tm_descriptor1.datum).field0"
