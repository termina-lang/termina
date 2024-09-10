module UT.PPrinter.Expression.BitwiseSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Control.Monad.Reader
import Generator.CCCodeGen.Expression
import Generator.LanguageC.CompCertCPrinter
import UT.PPrinter.Expression.Common

var0 :: Expression SemanticAnn
-- | var0 : u16
var0 = AccessObject (Variable "var0" (objSemAnn Mutable UInt16))

unboxVar1 :: Expression SemanticAnn
unboxVar1 = AccessObject (Unbox (Variable "var1" boxUInt16SemAnn) (objSemAnn Mutable UInt16))

constUInt8, constUInt16 :: Expression SemanticAnn
constUInt8 = Constant (I (TInteger 0x08 HexRepr) (Just UInt8)) uint8SemAnn
constUInt16 = Constant (I (TInteger 1024 DecRepr) (Just UInt16)) uint16SemAnn

var0LeftShiftConstant :: Expression SemanticAnn
var0LeftShiftConstant = BinOp BitwiseLeftShift var0 constUInt8 uint16SemAnn

constantLeftShiftVar0 :: Expression SemanticAnn
constantLeftShiftVar0 = BinOp BitwiseLeftShift constUInt8 var0 uint16SemAnn

var1LeftShiftConstant :: Expression SemanticAnn
var1LeftShiftConstant = BinOp BitwiseLeftShift unboxVar1 constUInt8 uint16SemAnn

constantLeftShiftVar1 :: Expression SemanticAnn
constantLeftShiftVar1 = BinOp BitwiseLeftShift constUInt8 unboxVar1 uint16SemAnn

var0LeftShiftVar1 :: Expression SemanticAnn
var0LeftShiftVar1 = BinOp BitwiseLeftShift var0 unboxVar1 uint16SemAnn

var0LeftShiftVar1LeftShiftConstant :: Expression SemanticAnn
var0LeftShiftVar1LeftShiftConstant = BinOp BitwiseLeftShift var0LeftShiftVar1 constUInt8 uint16SemAnn

var0RightShiftConstant :: Expression SemanticAnn
var0RightShiftConstant = BinOp BitwiseRightShift var0 constUInt8 uint16SemAnn

constantRightShiftVar0 :: Expression SemanticAnn
constantRightShiftVar0 = BinOp BitwiseRightShift constUInt8 var0 uint16SemAnn

var0BitwiseAndConstant :: Expression SemanticAnn
var0BitwiseAndConstant = BinOp BitwiseAnd var0 constUInt16 uint16SemAnn

constantBitwiseAndVar0 :: Expression SemanticAnn
constantBitwiseAndVar0 = BinOp BitwiseAnd constUInt16 var0 uint16SemAnn

var0BitwiseAndVar1 :: Expression SemanticAnn
var0BitwiseAndVar1 = BinOp BitwiseAnd var0 unboxVar1 uint16SemAnn

var1BitwiseOrconstant :: Expression SemanticAnn
var1BitwiseOrconstant = BinOp BitwiseOr unboxVar1 constUInt16 uint16SemAnn

var0BitwiseOrVar1 :: Expression SemanticAnn
var0BitwiseOrVar1 = BinOp BitwiseOr var0 unboxVar1 uint16SemAnn

var1BitwiseXorconstant :: Expression SemanticAnn
var1BitwiseXorconstant = BinOp BitwiseXor unboxVar1 constUInt16 uint16SemAnn

var0BitwiseXorVar1 :: Expression SemanticAnn
var0BitwiseXorVar1 = BinOp BitwiseXor var0 unboxVar1 uint16SemAnn

renderExpression :: Expression SemanticAnn -> Text
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
        
    