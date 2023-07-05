module UT.PPrinter.Expression.CastSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common


castUInt32toUInt8, castUInt32toUInt16 :: Expression SemanticAnns
castUInt32toUInt8 = Casting (Constant (I UInt32 0xFFFF0000) uint32SemAnn) UInt8 uint8SemAnn
castUInt32toUInt16 = Casting (Constant (I UInt32 0xFFFF0000) uint32SemAnn) UInt16 uint16SemAnn

var0 :: Expression SemanticAnns
var0 = Variable "var0" uint32SemAnn

castVar0toUInt8, castVar0toUInt32 :: Expression SemanticAnns
castVar0toUInt8 = Casting var0 UInt8 uint8SemAnn
castVar0toUInt32 = Casting var0 UInt32 uint32SemAnn

tmDescriptor0, field0 :: Expression SemanticAnns
tmDescriptor0 = Variable "tm_descriptor0" (definedTypeSemAnn "TMDescriptor")
field0 = Variable "field0" uint32SemAnn

tmDescriptor0field0 :: Expression SemanticAnns
tmDescriptor0field0 = BinOp MemberAccess tmDescriptor0 field0 uint32SemAnn

castTMDescriptor0field0toUInt8 :: Expression SemanticAnns
castTMDescriptor0field0toUInt8 = Casting tmDescriptor0field0 UInt8 uint8SemAnn

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression

spec :: Spec
spec = do
  describe "Pretty printing casting expressions" $ do
    it "Prints the expression ((0xFFFF0000 : u32) as u8)" $ do
      renderExpression castUInt32toUInt8 `shouldBe`
        pack "(uint8_t)4294901760"
    it "Prints the expression ((0xFFFF0000 : u32) as u16)" $ do
      renderExpression castUInt32toUInt16 `shouldBe`
        pack "(uint16_t)4294901760"
    it "Prints the expression (var0 as u8)" $ do
      renderExpression castVar0toUInt8 `shouldBe`
        pack "(uint8_t)var0"
    it "Prints the expression (var0 as u32)" $ do
      renderExpression castVar0toUInt32 `shouldBe`
        pack "(uint32_t)var0"
    it "Prints the expression (tm_descriptor0.field0 as u8)" $ do
      renderExpression castTMDescriptor0field0toUInt8 `shouldBe`
        pack "(uint8_t)tm_descriptor0.field0"
