module UT.PPrinter.Statement.MatchSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

import Utils.Annotations

optionBoxUInt32ObjSemAnn :: SemanticAnn
optionBoxUInt32ObjSemAnn = optionBoxObjSemAnn Mutable TUInt32

arrayObjAnn, boxArrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable TUInt32 (buildConstExprTUSize 10)
boxArrayObjAnn = boxArrayObjSemAnn TUInt32 (buildConstExprTUSize 10)

param0, param1 :: Object SemanticAnn
param0 = Variable "param0" boxUInt32SemAnn
param1 = Variable "param1" boxArrayObjAnn

uint32Const0 :: Expression SemanticAnn
uint32Const0 = Constant (I (TInteger 0 DecRepr) (Just TUInt32)) uint32ExprSemAnn

usizeConst0x8 :: Expression SemanticAnn
usizeConst0x8 = Constant (I (TInteger 8 DecRepr) (Just TUSize)) (simpleTySemAnn (TConstSubtype TUSize))

optionVar :: Expression SemanticAnn
optionVar = AccessObject (Variable "option_var" optionBoxUInt32ObjSemAnn)

array0IndexConstant :: Expression SemanticAnn
array0IndexConstant = AccessObject (ArrayIndexExpression (Unbox param1 arrayObjAnn) usizeConst0x8 (objSemAnn Mutable TUInt32))

foo1 :: Object SemanticAnn
foo1 = Variable "foo1" (objSemAnn Mutable TUInt32)

param0ToFoo1, constToFoo1 :: Statement SemanticAnn
param0ToFoo1 = AssignmentStmt foo1 (AccessObject (Unbox param0 (objSemAnn Mutable TUInt32))) stmtSemAnn
constToFoo1 = AssignmentStmt foo1 uint32Const0 stmtSemAnn

matchCaseSome0 :: MatchCase SemanticAnn
matchCaseSome0 = MatchCase "Some" ["param0"] (Block [param0ToFoo1] stmtSemAnn) (matchCaseSemAnn [TBoxSubtype TUInt32])

param1ToFoo1 :: Statement SemanticAnn
param1ToFoo1 = AssignmentStmt foo1 array0IndexConstant stmtSemAnn

matchCaseSome1 :: MatchCase SemanticAnn
matchCaseSome1 = MatchCase "Some" ["param1"] (Block [param1ToFoo1] stmtSemAnn) (matchCaseSemAnn [TBoxSubtype (TArray TUInt32 (buildConstExprTUSize 10))])

matchCaseNone :: MatchCase SemanticAnn
matchCaseNone = MatchCase "None" [] (Block [constToFoo1] stmtSemAnn) (matchCaseSemAnn [])

-- | A match statement with two cases. In Termina syntax:
-- match option_var {
--   Some(param0) => {
--     foo1 = param0;
--   }
--   None => {
--     foo1 = 0U;
--   }
-- }
matchOption0 :: Statement SemanticAnn 
matchOption0 = MatchStmt optionVar [matchCaseSome0, matchCaseNone] Nothing stmtSemAnn

matchOption1 :: Statement SemanticAnn
matchOption1 = MatchStmt optionVar [matchCaseNone, matchCaseSome1] Nothing stmtSemAnn

getInteger :: Expression SemanticAnn
getInteger = FunctionCall "get_integer" [] (SemanticAnn (ETy (AppType [] (TOption (TBoxSubtype TUInt32)))) Internal)

matchOption2 :: Statement SemanticAnn
matchOption2 = MatchStmt getInteger [matchCaseSome0, matchCaseNone] Nothing stmtSemAnn

spec :: Spec
spec = do
  describe "Pretty printing match statements" $ do
    it "Prints a match option statement" $ do
      renderStatement matchOption0 `shouldBe`
        pack (
          "\nif (option_var.__variant == Some) {\n" ++
          "    \n" ++
          "    __termina_box_t param0 = option_var.Some.__0;\n" ++
          "\n" ++
          "    foo1 = *(uint32_t *)param0.data;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    foo1 = 0U;\n" ++
          "\n" ++
          "}")
    it "Prints a match option array statement" $ do
      renderStatement matchOption1 `shouldBe`
        pack (
          "\nif (option_var.__variant == None) {\n" ++
          "    \n" ++
          "    foo1 = 0U;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    __termina_box_t param1 = option_var.Some.__0;\n" ++
          "\n" ++
          "    foo1 = ((uint32_t *)param1.data)[8U];\n" ++
          "\n" ++
          "}")
    it "Prints a match option statement with a complex expression" $ do
      renderStatement matchOption2 `shouldBe`
        pack (
          "\n{\n" ++
          "    \n" ++
          "    __option_box_t __match = get_integer();\n" ++
          "\n" ++
          "    if (__match.__variant == Some) {\n" ++
          "        \n" ++
          "        __termina_box_t param0 = __match.Some.__0;\n" ++
          "\n" ++
          "        foo1 = *(uint32_t *)param0.data;\n" ++
          "\n" ++
          "    } else {\n" ++
          "        \n" ++
          "        foo1 = 0U;\n" ++
          "\n" ++
          "    }\n" ++
          "\n" ++
          "}")
