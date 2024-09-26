module UT.PPrinter.Statement.MatchSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Types

import Prettyprinter
import Control.Monad.Reader
import Generator.CodeGen.Statement
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common
import Utils.Annotations
import ControlFlow.Common
import Control.Monad.Except

optionBoxUInt32ObjSemAnn :: SemanticAnn
optionBoxUInt32ObjSemAnn = optionBoxObjSemAnn Mutable UInt32

arrayObjAnn, boxArrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))
boxArrayObjAnn = boxArrayObjSemAnn UInt32 (K (TInteger 10 DecRepr))

param0, param1 :: Object SemanticAnn
param0 = Variable "param0" boxUInt32SemAnn
param1 = Variable "param1" boxArrayObjAnn

uint32Const0 :: Expression SemanticAnn
uint32Const0 = Constant (I (TInteger 0 DecRepr) (Just UInt32)) uint32ExprSemAnn

usizeConst0x8 :: Expression SemanticAnn
usizeConst0x8 = Constant (I (TInteger 8 DecRepr) (Just USize)) usizeExprSemAnn

optionVar :: Expression SemanticAnn
optionVar = AccessObject (Variable "option_var" optionBoxUInt32ObjSemAnn)

array0IndexConstant :: Expression SemanticAnn
array0IndexConstant = AccessObject (ArrayIndexExpression (Unbox param1 arrayObjAnn) usizeConst0x8 (objSemAnn Mutable UInt32))

foo1 :: Object SemanticAnn
foo1 = Variable "foo1" (objSemAnn Mutable UInt32)

param0ToFoo1, constToFoo1 :: Statement SemanticAnn
param0ToFoo1 = AssignmentStmt foo1 (AccessObject (Unbox param0 (objSemAnn Mutable UInt32))) stmtSemAnn
constToFoo1 = AssignmentStmt foo1 uint32Const0 stmtSemAnn

matchCaseSome0 :: MatchCase SemanticAnn
matchCaseSome0 = MatchCase "Some" ["param0"] [param0ToFoo1] (matchCaseSemAnn [BoxSubtype UInt32])

param1ToFoo1 :: Statement SemanticAnn
param1ToFoo1 = AssignmentStmt foo1 array0IndexConstant stmtSemAnn

matchCaseSome1 :: MatchCase SemanticAnn
matchCaseSome1 = MatchCase "Some" ["param1"] [param1ToFoo1] (matchCaseSemAnn [BoxSubtype (Array UInt32 (K (TInteger 10 DecRepr)))])

matchCaseNone :: MatchCase SemanticAnn
matchCaseNone = MatchCase "None" [] [constToFoo1] (matchCaseSemAnn [])

-- | A match statement with two cases. In Termina syntax:
-- match option_var {
--   Some(param0) => {
--     foo1 = param0;
--   }
--   None => {
--     foo1 = 0;
--   }
-- }
matchOption0 :: Statement SemanticAnn 
matchOption0 = MatchStmt optionVar [matchCaseSome0, matchCaseNone] stmtSemAnn

matchOption1 :: Statement SemanticAnn
matchOption1 = MatchStmt optionVar [matchCaseNone, matchCaseSome1] stmtSemAnn

getInteger :: Expression SemanticAnn
getInteger = FunctionCall "get_integer" [] (Located (ETy (AppType [] (Option (BoxSubtype UInt32)))) Internal)

matchOption2 :: Statement SemanticAnn
matchOption2 = MatchStmt getInteger [matchCaseSome0, matchCaseNone] stmtSemAnn

renderStatement :: Statement SemanticAnn -> Text
renderStatement stmt = 
  case runExcept (genBBlocks [] [stmt]) of
    Left err -> pack $ show err
    Right bBlocks ->
      case runReaderT (Prelude.concat <$> mapM genBlocks bBlocks) empty of
        Left err -> pack $ show err
        Right cStmts -> render $ vsep $ runReader (mapM pprint cStmts) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing match statements" $ do
    it "Prints a match option statement" $ do
      renderStatement matchOption0 `shouldBe`
        pack (
          "\nif (option_var.__variant == Some) {\n" ++
          "    \n" ++
          "    __option_box_params_t __Some = option_var.Some;\n" ++
          "\n" ++
          "    foo1 = *(uint32_t *)__Some.__0.data;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    foo1 = 0;\n" ++
          "\n" ++
          "}")
    it "Prints a match option array statement" $ do
      renderStatement matchOption1 `shouldBe`
        pack (
          "\nif (option_var.__variant == None) {\n" ++
          "    \n" ++
          "    foo1 = 0;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    __option_box_params_t __Some = option_var.Some;\n" ++
          "\n" ++
          "    foo1 = ((uint32_t *)__Some.__0.data)[8];\n" ++
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
          "        foo1 = *(uint32_t *)__match.Some.__0.data;\n" ++
          "\n" ++
          "    } else {\n" ++
          "        \n" ++
          "        foo1 = 0;\n" ++
          "\n" ++
          "    }\n" ++
          "\n" ++
          "}")
