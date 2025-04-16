module UT.PPrinter.Statement.ForLoopSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

arrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable TUInt32 (buildConstExprTUSize 10)

array0 :: Object SemanticAnn
array0 = Variable "array0" arrayObjAnn

total, i :: Expression SemanticAnn
total = AccessObject (Variable "total" (objSemAnn Mutable TUInt32))
i = AccessObject (Variable "i" (objSemAnn Mutable TUSize))

array0IndexI:: Expression SemanticAnn
array0IndexI = AccessObject (ArrayIndexExpression array0 i (objSemAnn Mutable TUInt32))

forLoopBody :: Block SemanticAnn
forLoopBody = Block [AssignmentStmt (Variable "total" (objSemAnn Mutable TUInt32)) (BinOp Addition total array0IndexI uint32ExprSemAnn) stmtSemAnn] stmtSemAnn

breakCond :: Expression SemanticAnn
breakCond = BinOp RelationalNotEqual i (Constant (I (TInteger 5 DecRepr) (Just TUSize)) uint32ExprSemAnn) boolExprSemAnn

forLoop0 :: Statement SemanticAnn
forLoop0 = ForLoopStmt "i" TUSize (Constant (I (TInteger 0 DecRepr) (Just TUSize)) uint32ExprSemAnn) (Constant (I (TInteger 10 DecRepr) (Just TUSize)) uint32ExprSemAnn) Nothing forLoopBody stmtSemAnn

forLoop1 :: Statement SemanticAnn
forLoop1 = ForLoopStmt "i" TUSize (Constant (I (TInteger 0 DecRepr) (Just TUSize)) uint32ExprSemAnn) (Constant (I (TInteger 10 DecRepr) (Just TUSize)) uint32ExprSemAnn) (Just breakCond) forLoopBody stmtSemAnn

spec :: Spec
spec = do
  describe "Pretty for-loop statements" $ do
    it "Prints a for loop statement without break condition" $ do
      renderStatement forLoop0 `shouldBe`
        pack (
          "\nfor (size_t i = 0U; i < 10U; i = i + 1U) {\n" ++
          "    \n" ++
          "    total = total + array0[__termina_array__index(10U, i)];\n" ++
          "\n" ++
          "}")
    it "Prints a for loop statement with break condition" $ do
      renderStatement forLoop1 `shouldBe`
        pack (
          "\nfor (size_t i = 0U; i < 10U && i != 5U; i = i + 1U) {\n" ++
          "    \n" ++
          "    total = total + array0[__termina_array__index(10U, i)];\n" ++
          "\n" ++
          "}")
