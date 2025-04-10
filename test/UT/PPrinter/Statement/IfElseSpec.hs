module UT.PPrinter.Statement.IfElseSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

optionBoxUInt32TS :: TerminaType SemanticAnn
optionBoxUInt32TS = TOption (TBoxSubtype TUInt32)

arrayTS :: TerminaType SemanticAnn
arrayTS = TArray TUInt32 (buildConstExprTUSize 10)

optionBoxUInt32ExprSemAnn :: SemanticAnn
optionBoxUInt32ExprSemAnn = optionBoxExprSemAnn TUInt32

arrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable TUInt32 (buildConstExprTUSize 10)

array0 :: Expression SemanticAnn
array0 = AccessObject (Variable "array0" arrayObjAnn)

array1 :: Statement SemanticAnn
array1 = Declaration "array1" Mutable arrayTS array0 stmtSemAnn

foo0 :: Expression SemanticAnn
foo0 = AccessObject (Variable "foo0" (objSemAnn Mutable TUInt32))

uint32Const0, uint32Const0xFFFF0000 :: Expression SemanticAnn
uint32Const0 = Constant (I (TInteger 0 DecRepr) (Just TUInt32)) uint32ExprSemAnn
uint32Const0xFFFF0000 = Constant (I (TInteger 4294901760 DecRepr) (Just TUInt32)) uint32ExprSemAnn

constToFoo0 :: Statement SemanticAnn
constToFoo0 = AssignmentStmt (Variable "foo0" (objSemAnn Mutable TUInt32)) uint32Const0 stmtSemAnn

boxVar0 :: Expression SemanticAnn
boxVar0 = AccessObject (Variable "box_var0" boxUInt32SemAnn)

option0, option1 :: Statement SemanticAnn
option0 = Declaration "option0" Mutable optionBoxUInt32TS (OptionVariantInitializer (Some boxVar0) optionBoxUInt32ExprSemAnn) stmtSemAnn
option1 = Declaration "option1" Mutable optionBoxUInt32TS (OptionVariantInitializer None optionBoxUInt32ExprSemAnn) stmtSemAnn

twoDeclarations :: Block SemanticAnn
twoDeclarations = Block [array1, option0] stmtSemAnn

oneAssignment :: Block SemanticAnn
oneAssignment = Block [constToFoo0] stmtSemAnn

oneDeclaration :: Block SemanticAnn
oneDeclaration = Block [option1] stmtSemAnn

cond0, cond1 :: Expression SemanticAnn
cond0 = BinOp RelationalEqual foo0 uint32Const0 boolExprSemAnn
cond1 = BinOp RelationalNotEqual foo0 uint32Const0xFFFF0000 boolExprSemAnn

singleIf :: Statement SemanticAnn
singleIf = IfElseStmt cond0 twoDeclarations [] Nothing stmtSemAnn

ifElse :: Statement SemanticAnn
ifElse = IfElseStmt cond1 twoDeclarations [] (Just oneDeclaration) stmtSemAnn

elseIf :: ElseIf SemanticAnn
elseIf = ElseIf cond0 oneAssignment stmtSemAnn

ifElseIf :: Statement SemanticAnn
ifElseIf = IfElseStmt cond1 twoDeclarations [elseIf] (Just oneDeclaration) stmtSemAnn

spec :: Spec
spec = do
  describe "Pretty printing if statements" $ do
    it "Prints a single if statement" $ do
      renderStatement singleIf `shouldBe`
        pack (
          "\nif (foo0 == 0U) {\n" ++
          "    \n" ++
          "    uint32_t array1[10U];\n" ++
          "    for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "        array1[__i0] = array0[__i0];\n" ++
          "    }\n" ++
          "\n" ++
          "    __option_box_t option0;\n" ++
          "    option0.__variant = Some;\n" ++
          "    option0.Some.__0 = box_var0;\n" ++
          "\n" ++
          "}")
    it "Prints an if-else statement" $ do
      renderStatement ifElse `shouldBe`
        pack (
          "\nif (foo0 != 4294901760U) {\n" ++
          "    \n" ++
          "    uint32_t array1[10U];\n" ++
          "    for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "        array1[__i0] = array0[__i0];\n" ++
          "    }\n" ++
          "\n" ++
          "    __option_box_t option0;\n" ++
          "    option0.__variant = Some;\n" ++
          "    option0.Some.__0 = box_var0;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    __option_box_t option1;\n" ++
          "    option1.__variant = None;\n" ++
          "\n" ++
          "}")
    it "Prints an if-else-if-else statement" $ do
      renderStatement ifElseIf `shouldBe`
        pack (
          "\nif (foo0 != 4294901760U) {\n" ++
          "    \n" ++
          "    uint32_t array1[10U];\n" ++
          "    for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "        array1[__i0] = array0[__i0];\n" ++
          "    }\n" ++
          "\n" ++
          "    __option_box_t option0;\n" ++
          "    option0.__variant = Some;\n" ++
          "    option0.Some.__0 = box_var0;\n" ++
          "\n" ++
          "} else if (foo0 == 0U) {\n" ++
          "    \n" ++
          "    foo0 = 0U;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    __option_box_t option1;\n" ++
          "    option1.__variant = None;\n" ++
          "\n" ++
          "}")
