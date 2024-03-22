module UT.PPrinter.Expression.ArithmeticSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Semantic.Monad
import UT.PPrinter.Expression.Common
import Data.Map
import Control.Monad.Reader
import Generator.CGenerator
import Generator.LanguageC.Printer


var0 :: Expression SemanticAnns
-- | var0 : u16
var0 = AccessObject (Variable "var0" (objSemAnn Mutable UInt16))

undynVar1 :: Expression SemanticAnns
undynVar1 = AccessObject (Undyn (Variable "var1" dynUInt16SemAnn) (objSemAnn Mutable UInt16))

constUInt16 :: Expression SemanticAnns
-- | 1024 : u16
constUInt16 = Constant (I UInt16 1024) uint16SemAnn

var0PlusConstant :: Expression SemanticAnns
-- | var0 + 1024 : u16
var0PlusConstant = BinOp Addition var0 constUInt16 uint16SemAnn

constantPlusVar0 :: Expression SemanticAnns
-- | 1024 : u16 + var0
constantPlusVar0 = BinOp Addition constUInt16 var0 uint16SemAnn

var1PlusConstant :: Expression SemanticAnns
-- | var1 + 1024 : u16
var1PlusConstant = BinOp Addition undynVar1 constUInt16 uint16SemAnn

constantPlusVar1 :: Expression SemanticAnns
constantPlusVar1 = BinOp Addition constUInt16 undynVar1 uint16SemAnn

var0PlusVar1 :: Expression SemanticAnns
var0PlusVar1 = BinOp Addition var0 undynVar1 uint16SemAnn

var0PlusVar1PlusConstant :: Expression SemanticAnns
var0PlusVar1PlusConstant = BinOp Addition var0PlusVar1 constUInt16 uint16SemAnn

var0MinusConstant :: Expression SemanticAnns
var0MinusConstant = BinOp Subtraction var0 constUInt16 uint16SemAnn

constantMinusVar0 :: Expression SemanticAnns
constantMinusVar0 = BinOp Subtraction constUInt16 var0 uint16SemAnn

var0MultConstant :: Expression SemanticAnns
var0MultConstant = BinOp Multiplication var0 constUInt16 uint16SemAnn

constantMultVar0 :: Expression SemanticAnns
constantMultVar0 = BinOp Multiplication constUInt16 var0 uint16SemAnn

var0MultVar1 :: Expression SemanticAnns
var0MultVar1 = BinOp Multiplication var0 undynVar1 uint16SemAnn

var1DivConstant :: Expression SemanticAnns
var1DivConstant = BinOp Division undynVar1 constUInt16 uint16SemAnn

var0DivVar1 :: Expression SemanticAnns
var0DivVar1 = BinOp Division var0 undynVar1 uint16SemAnn

var1ModConstant :: Expression SemanticAnns
var1ModConstant = BinOp Modulo undynVar1 constUInt16 uint16SemAnn

var0ModVar1 :: Expression SemanticAnns
var0ModVar1 = BinOp Modulo var0 undynVar1 uint16SemAnn

renderExpression :: Expression SemanticAnns -> Text
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

