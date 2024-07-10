module UT.PPrinter.Expression.BitwiseSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Control.Monad.Reader
import Generator.CodeGen.Expression
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

var0 :: Expression SemanticAnns
-- | var0 : u16
var0 = AccessObject (Variable "var0" (objSemAnn Mutable UInt16))

undynVar1 :: Expression SemanticAnns
undynVar1 = AccessObject (Undyn (Variable "var1" dynUInt16SemAnn) (objSemAnn Mutable UInt16))

constUInt8, constUInt16 :: Expression SemanticAnns
constUInt8 = Constant (I (TInteger 0x08 HexRepr) (Just UInt8)) uint8SemAnn
constUInt16 = Constant (I (TInteger 1024 DecRepr) (Just UInt16)) uint16SemAnn

var0LeftShiftConstant :: Expression SemanticAnns
var0LeftShiftConstant = BinOp BitwiseLeftShift var0 constUInt8 uint16SemAnn

constantLeftShiftVar0 :: Expression SemanticAnns
constantLeftShiftVar0 = BinOp BitwiseLeftShift constUInt8 var0 uint16SemAnn

var1LeftShiftConstant :: Expression SemanticAnns
var1LeftShiftConstant = BinOp BitwiseLeftShift undynVar1 constUInt8 uint16SemAnn

constantLeftShiftVar1 :: Expression SemanticAnns
constantLeftShiftVar1 = BinOp BitwiseLeftShift constUInt8 undynVar1 uint16SemAnn

var0LeftShiftVar1 :: Expression SemanticAnns
var0LeftShiftVar1 = BinOp BitwiseLeftShift var0 undynVar1 uint16SemAnn

var0LeftShiftVar1LeftShiftConstant :: Expression SemanticAnns
var0LeftShiftVar1LeftShiftConstant = BinOp BitwiseLeftShift var0LeftShiftVar1 constUInt8 uint16SemAnn

var0RightShiftConstant :: Expression SemanticAnns
var0RightShiftConstant = BinOp BitwiseRightShift var0 constUInt8 uint16SemAnn

constantRightShiftVar0 :: Expression SemanticAnns
constantRightShiftVar0 = BinOp BitwiseRightShift constUInt8 var0 uint16SemAnn

var0BitwiseAndConstant :: Expression SemanticAnns
var0BitwiseAndConstant = BinOp BitwiseAnd var0 constUInt16 uint16SemAnn

constantBitwiseAndVar0 :: Expression SemanticAnns
constantBitwiseAndVar0 = BinOp BitwiseAnd constUInt16 var0 uint16SemAnn

var0BitwiseAndVar1 :: Expression SemanticAnns
var0BitwiseAndVar1 = BinOp BitwiseAnd var0 undynVar1 uint16SemAnn

var1BitwiseOrconstant :: Expression SemanticAnns
var1BitwiseOrconstant = BinOp BitwiseOr undynVar1 constUInt16 uint16SemAnn

var0BitwiseOrVar1 :: Expression SemanticAnns
var0BitwiseOrVar1 = BinOp BitwiseOr var0 undynVar1 uint16SemAnn

var1BitwiseXorconstant :: Expression SemanticAnns
var1BitwiseXorconstant = BinOp BitwiseXor undynVar1 constUInt16 uint16SemAnn

var0BitwiseXorVar1 :: Expression SemanticAnns
var0BitwiseXorVar1 = BinOp BitwiseXor var0 undynVar1 uint16SemAnn

renderExpression :: Expression SemanticAnns -> Text
renderExpression expr = 
  case runReaderT (genExpression expr) empty of
    Left err -> pack $ show err
    Right cExpr -> render $ runReader (pprint cExpr) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing variable expression" $ do
    it "Prints the expression: var0 << 0x8 : u8" $ do
      renderExpression var0LeftShiftConstant `shouldBe`
        pack "var0 << 0x8"
    it "Prints the expression: var1 << 0x8 : u8" $ do
      renderExpression var1LeftShiftConstant `shouldBe`
        pack "*(uint16_t *)var1.data << 0x8"
    it "Prints the expression: 8 : u8 << var0" $ do
      renderExpression constantLeftShiftVar0 `shouldBe`
        pack "0x8 << var0"
    it "Prints the expression: 8 : u8 << var1" $ do
      renderExpression constantLeftShiftVar1 `shouldBe`
        pack "0x8 << *(uint16_t *)var1.data"
    it "Prints the expression: var0 << var1 : u16" $ do
      renderExpression var0LeftShiftVar1 `shouldBe`
        pack "var0 << *(uint16_t *)var1.data"
    it "Prints the expression: var0 << var1 << 0x8 : u8" $ do
      renderExpression var0LeftShiftVar1LeftShiftConstant `shouldBe`
        pack "(uint16_t)(var0 << *(uint16_t *)var1.data) << 0x8"
    it "Prints the expression: var0 >> 0x8 : u8" $ do
      renderExpression var0RightShiftConstant `shouldBe`
        pack "var0 >> 0x8"
    it "Prints the expression: 0x8 >> var0" $ do
      renderExpression constantRightShiftVar0 `shouldBe`
        pack "0x8 >> var0"
    it "Prints the expression: var0 & 1024 : u16" $ do
      renderExpression var0BitwiseAndConstant `shouldBe`
        pack "var0 & 1024"
    it "Prints the expression: 1024 : u16 & var0" $ do
      renderExpression constantBitwiseAndVar0 `shouldBe`
        pack "1024 & var0"
    it "Prints the expression: var0 & var1 : u16" $ do
      renderExpression var0BitwiseAndVar1 `shouldBe`
        pack "var0 & *(uint16_t *)var1.data"
    it "Prints the expression: var1 | 1024 : u16" $ do
      renderExpression var1BitwiseOrconstant `shouldBe`
        pack "*(uint16_t *)var1.data | 1024"
    it "Prints the expression: var0 | var1 : u16" $ do
      renderExpression var0BitwiseOrVar1 `shouldBe`
        pack "var0 | *(uint16_t *)var1.data"
    it "Prints the expression: var1 ^ 1024 : u16" $ do
      renderExpression var1BitwiseXorconstant `shouldBe`
        pack "*(uint16_t *)var1.data ^ 1024"
    it "Prints the expression: var0 ^ var1 : u16" $ do
      renderExpression var0BitwiseXorVar1 `shouldBe`
        pack "var0 ^ *(uint16_t *)var1.data"
        
    