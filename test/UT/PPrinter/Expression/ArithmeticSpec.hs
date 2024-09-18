module UT.PPrinter.Expression.ArithmeticSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Semantic.Monad
import UT.PPrinter.Expression.Common
import Data.Map
import Control.Monad.Reader
import Generator.CodeGen.Expression
import Generator.LanguageC.Printer

var0 :: Expression SemanticAnn
-- | var0 : u16
var0 = AccessObject (Variable "var0" (objSemAnn Mutable UInt16))

unboxVar1 :: Expression SemanticAnn
unboxVar1 = AccessObject (Unbox (Variable "var1" boxUInt16SemAnn) (objSemAnn Mutable UInt16))

constUInt16 :: Expression SemanticAnn
-- | 1024 : u16
constUInt16 = Constant (I (TInteger 1024 DecRepr) (Just UInt16)) uint16ExprSemAnn

var0PlusConstant :: Expression SemanticAnn
-- | var0 + 1024 : u16
var0PlusConstant = BinOp Addition var0 constUInt16 uint16ExprSemAnn

constantPlusVar0 :: Expression SemanticAnn
-- | 1024 : u16 + var0
constantPlusVar0 = BinOp Addition constUInt16 var0 uint16ExprSemAnn

var1PlusConstant :: Expression SemanticAnn
-- | var1 + 1024 : u16
var1PlusConstant = BinOp Addition unboxVar1 constUInt16 uint16ExprSemAnn

constantPlusVar1 :: Expression SemanticAnn
constantPlusVar1 = BinOp Addition constUInt16 unboxVar1 uint16ExprSemAnn

var0PlusVar1 :: Expression SemanticAnn
var0PlusVar1 = BinOp Addition var0 unboxVar1 uint16ExprSemAnn

var0PlusVar1PlusConstant :: Expression SemanticAnn
var0PlusVar1PlusConstant = BinOp Addition var0PlusVar1 constUInt16 uint16ExprSemAnn

var0MinusConstant :: Expression SemanticAnn
var0MinusConstant = BinOp Subtraction var0 constUInt16 uint16ExprSemAnn

constantMinusVar0 :: Expression SemanticAnn
constantMinusVar0 = BinOp Subtraction constUInt16 var0 uint16ExprSemAnn

var0MultConstant :: Expression SemanticAnn
var0MultConstant = BinOp Multiplication var0 constUInt16 uint16ExprSemAnn

constantMultVar0 :: Expression SemanticAnn
constantMultVar0 = BinOp Multiplication constUInt16 var0 uint16ExprSemAnn

var0MultVar1 :: Expression SemanticAnn
var0MultVar1 = BinOp Multiplication var0 unboxVar1 uint16ExprSemAnn

var1DivConstant :: Expression SemanticAnn
var1DivConstant = BinOp Division unboxVar1 constUInt16 uint16ExprSemAnn

var0DivVar1 :: Expression SemanticAnn
var0DivVar1 = BinOp Division var0 unboxVar1 uint16ExprSemAnn

var1ModConstant :: Expression SemanticAnn
var1ModConstant = BinOp Modulo unboxVar1 constUInt16 uint16ExprSemAnn

var0ModVar1 :: Expression SemanticAnn
var0ModVar1 = BinOp Modulo var0 unboxVar1 uint16ExprSemAnn

renderExpression :: Expression SemanticAnn -> Text
renderExpression expr = 
  case runReaderT (genExpression expr) empty of
    Left err -> pack $ show err
    Right cExpr -> render $ runReader (pprint cExpr) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing variable expression" $ do
    it "Prints the expression: var0 + 1024 : u16" $ do
      renderExpression var0PlusConstant `shouldBe`
        pack "var0 + 1024"
    it "Prints the expression: var1 + 1024 : u16" $ do
      renderExpression var1PlusConstant `shouldBe`
        pack "*(uint16_t *)var1.data + 1024"
    it "Prints the expression: 1024 : u16 + var0" $ do
      renderExpression constantPlusVar0 `shouldBe`
        pack "1024 + var0"
    it "Prints the expression: 1024 : u16 + var1" $ do
      renderExpression constantPlusVar1 `shouldBe`
        pack "1024 + *(uint16_t *)var1.data"
    it "Prints the expression: var0 + var1 : u16" $ do
      renderExpression var0PlusVar1 `shouldBe`
        pack "var0 + *(uint16_t *)var1.data"
    it "Prints the expression: var0 + var1 + 1024 : u16" $ do
      renderExpression var0PlusVar1PlusConstant `shouldBe`
        pack "(uint16_t)(var0 + *(uint16_t *)var1.data) + 1024"
    it "Prints the expression: var0 - 1024 : u16" $ do
      renderExpression var0MinusConstant `shouldBe`
        pack "var0 - 1024"
    it "Prints the expression: 1024 : u16 - var0" $ do
      renderExpression constantMinusVar0 `shouldBe`
        pack "1024 - var0"
    it "Prints the expression: var0 * 1024 : u16" $ do
      renderExpression var0MultConstant `shouldBe`
        pack "var0 * 1024"
    it "Prints the expression: 1024 : u16 * var0" $ do
      renderExpression constantMultVar0 `shouldBe`
        pack "1024 * var0"
    it "Prints the expression: var0 * var1 : u16" $ do
      renderExpression var0MultVar1 `shouldBe`
        pack "var0 * *(uint16_t *)var1.data"
    it "Prints the expression: var1 / 1024 : u16" $ do
      renderExpression var1DivConstant `shouldBe`
        pack "*(uint16_t *)var1.data / 1024"
    it "Prints the expression: var0 / var1 : u16" $ do
      renderExpression var0DivVar1 `shouldBe`
        pack "var0 / *(uint16_t *)var1.data"
    it "Prints the expression: var1 % 1024 : u16" $ do
      renderExpression var1ModConstant `shouldBe`
        pack "*(uint16_t *)var1.data % 1024"
    it "Prints the expression: var0 % var1 : u16" $ do
      renderExpression var0ModVar1 `shouldBe`
        pack "var0 % *(uint16_t *)var1.data"

