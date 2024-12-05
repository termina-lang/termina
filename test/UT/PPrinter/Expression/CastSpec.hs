module UT.PPrinter.Expression.CastSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Data.Text hiding (empty)
import Semantic.Types
import Semantic.AST

castUInt32toUInt8, castUInt32toUInt16 :: Expression SemanticAnn
castUInt32toUInt8 = Casting (Constant (I (TInteger 0xFFFF0000 HexRepr) (Just TUInt32)) uint32ExprSemAnn) TUInt8 uint8ExprSemAnn
castUInt32toUInt16 = Casting (Constant (I (TInteger 0xFFFF0000 HexRepr) (Just TUInt32)) uint32ExprSemAnn) TUInt16 uint16ExprSemAnn

var0 :: Expression SemanticAnn
var0 = AccessObject (Variable "var0" (objSemAnn Mutable TUInt32))

castVar0toUInt8, castVar0toUInt32 :: Expression SemanticAnn
castVar0toUInt8 = Casting var0 TUInt8 uint8ExprSemAnn
castVar0toUInt32 = Casting var0 TUInt32 uint32ExprSemAnn

tmDescriptor0 :: Object SemanticAnn
tmDescriptor0 = Variable "tm_descriptor0" (structObjSemAnn Mutable "TMDescriptor")

tmDescriptor0field0 :: Object SemanticAnn
tmDescriptor0field0 = MemberAccess tmDescriptor0 "field0" (objSemAnn Mutable TUInt32)

castTMDescriptor0field0toUInt8 :: Expression SemanticAnn
castTMDescriptor0field0toUInt8 = Casting (AccessObject tmDescriptor0field0) TUInt8 uint8ExprSemAnn

spec :: Spec
spec = do
  describe "Pretty printing casting expressions" $ do
    it "Prints the expression ((0xFFFF0000 : u32) as u8)" $ do
      renderExpression castUInt32toUInt8 `shouldBe`
        pack "(uint8_t)0xFFFF0000U"
    it "Prints the expression ((0xFFFF0000 : u32) as u16)" $ do
      renderExpression castUInt32toUInt16 `shouldBe`
        pack "(uint16_t)0xFFFF0000U"
    it "Prints the expression (var0 as u8)" $ do
      renderExpression castVar0toUInt8 `shouldBe`
        pack "(uint8_t)var0"
    it "Prints the expression (var0 as u32)" $ do
      renderExpression castVar0toUInt32 `shouldBe`
        pack "(uint32_t)var0"
    it "Prints the expression (tm_descriptor0.field0 as u8)" $ do
      renderExpression castTMDescriptor0field0toUInt8 `shouldBe`
        pack "(uint8_t)tm_descriptor0.field0"
