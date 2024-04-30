module UT.PPrinter.Statement.MatchSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad

import Prettyprinter
import Control.Monad.Reader
import Generator.Statement
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

optionDynUInt32SemAnn :: SemanticAnns
optionDynUInt32SemAnn = optionDynSemAnn Mutable UInt32

vectorAnn, dynArrayAnn :: SemanticAnns
vectorAnn = vectorSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))
dynArrayAnn = dynArraySemAnn UInt32 (K (TInteger 10 DecRepr))

param0, param1 :: Object SemanticAnns
param0 = Variable "param0" dynUInt32SemAnn
param1 = Variable "param1" dynArrayAnn

uint32Const0 :: Expression SemanticAnns
uint32Const0 = Constant (I (TInteger 0 DecRepr) (Just UInt32)) uint32SemAnn

usizeConst0x8 :: Expression SemanticAnns
usizeConst0x8 = Constant (I (TInteger 8 DecRepr) (Just USize)) usizeSemAnn

optionVar :: Expression SemanticAnns
optionVar = AccessObject (Variable "option_var" optionDynUInt32SemAnn)

vector0IndexConstant :: Expression SemanticAnns
vector0IndexConstant = AccessObject (ArrayIndexExpression (Undyn param1 vectorAnn) usizeConst0x8 (objSemAnn Mutable UInt32))

foo1 :: Object SemanticAnns
foo1 = Variable "foo1" (objSemAnn Mutable UInt32)

param0ToFoo1, constToFoo1 :: Statement SemanticAnns
param0ToFoo1 = AssignmentStmt foo1 (AccessObject (Undyn param0 (objSemAnn Mutable UInt32))) stmtSemAnn
constToFoo1 = AssignmentStmt foo1 uint32Const0 stmtSemAnn

matchCaseSome0 :: MatchCase SemanticAnns
matchCaseSome0 = MatchCase "Some" ["param0"] [param0ToFoo1] stmtSemAnn

param1ToFoo1 :: Statement SemanticAnns
param1ToFoo1 = AssignmentStmt foo1 vector0IndexConstant stmtSemAnn

matchCaseSome1 :: MatchCase SemanticAnns
matchCaseSome1 = MatchCase "Some" ["param1"] [param1ToFoo1] stmtSemAnn

matchCaseNone :: MatchCase SemanticAnns
matchCaseNone = MatchCase "None" [] [constToFoo1] stmtSemAnn

-- | A match statement with two cases. In Termina syntax:
-- match option_var {
--   Some(param0) => {
--     foo1 = param0;
--   }
--   None => {
--     foo1 = 0;
--   }
-- }
matchOption0 :: Statement SemanticAnns 
matchOption0 = MatchStmt optionVar [matchCaseSome0, matchCaseNone] stmtSemAnn

matchOption1 :: Statement SemanticAnns
matchOption1 = MatchStmt optionVar [matchCaseNone, matchCaseSome1] stmtSemAnn

getInteger :: Expression SemanticAnns
getInteger = FunctionExpression "get_integer" [] [] (SemAnn undefined (ETy (AppType [] [] (Option (DynamicSubtype UInt32)))))

matchOption2 :: Statement SemanticAnns
matchOption2 = MatchStmt getInteger [matchCaseSome0, matchCaseNone] stmtSemAnn

renderStatement :: Statement SemanticAnns -> Text
renderStatement stmt = 
  case runReaderT (genBlockItem stmt) empty of
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
          "    __option_dyn_params_t __Some = option_var.Some;\n" ++
          "\n" ++
          "    foo1 = *(uint32_t *)__Some.__0.data;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    foo1 = 0;\n" ++
          "\n" ++
          "}")
    it "Prints a match option vector statement" $ do
      renderStatement matchOption1 `shouldBe`
        pack (
          "\nif (option_var.__variant == None) {\n" ++
          "    \n" ++
          "    foo1 = 0;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    __option_dyn_params_t __Some = option_var.Some;\n" ++
          "\n" ++
          "    foo1 = ((uint32_t *)__Some.__0.data)[8];\n" ++
          "\n" ++
          "}")
    it "Prints a match option statement with a complex expression" $ do
      renderStatement matchOption2 `shouldBe`
        pack (
          "\n{\n" ++
          "    \n" ++
          "    __option_dyn_t __match = get_integer();\n" ++
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
