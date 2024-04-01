module UT.PPrinter.Statement.IfElseSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Prettyprinter
import Control.Monad.Reader
import Generator.Statement
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

optionDynUInt32TS :: TypeSpecifier
optionDynUInt32TS = Option (DynamicSubtype UInt32)

vectorTS :: TypeSpecifier
vectorTS = Vector UInt32 (K 10)

optionDynUInt32SemAnn :: SemanticAnns
optionDynUInt32SemAnn = optionDynSemAnn Mutable UInt32

vectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn Mutable UInt32 (K 10)

vector0 :: Expression SemanticAnns
vector0 = AccessObject (Variable "vector0" vectorAnn)

vector1 :: Statement SemanticAnns
vector1 = Declaration "vector1" Mutable vectorTS vector0 stmtSemAnn

foo0 :: Expression SemanticAnns
foo0 = AccessObject (Variable "foo0" (objSemAnn Mutable UInt32))

uint32Const0, uint32Const0xFFFF0000 :: Expression SemanticAnns
uint32Const0 = Constant (I UInt32 0) uint32SemAnn
uint32Const0xFFFF0000 = Constant (I UInt32 4294901760) uint32SemAnn

constToFoo0 :: Statement SemanticAnns
constToFoo0 = AssignmentStmt (Variable "foo0" (objSemAnn Mutable UInt32)) uint32Const0 stmtSemAnn

dynVar0 :: Expression SemanticAnns
dynVar0 = AccessObject (Variable "dyn_var0" dynUInt32SemAnn)

option0, option1 :: Statement SemanticAnns
option0 = Declaration "option0" Mutable optionDynUInt32TS (OptionVariantExpression (Some dynVar0) optionDynUInt32SemAnn) stmtSemAnn
option1 = Declaration "option1" Mutable optionDynUInt32TS (OptionVariantExpression None optionDynUInt32SemAnn) stmtSemAnn

twoDeclarations :: [Statement SemanticAnns]
twoDeclarations = [vector1, option0]

oneAssignment :: [Statement SemanticAnns]
oneAssignment = [constToFoo0]

oneDeclaration :: [Statement SemanticAnns]
oneDeclaration = [option1]

cond0, cond1 :: Expression SemanticAnns
cond0 = BinOp RelationalEqual foo0 uint32Const0 boolSemAnn
cond1 = BinOp RelationalNotEqual foo0 uint32Const0xFFFF0000 boolSemAnn

singleIf :: Statement SemanticAnns
singleIf = IfElseStmt cond0 twoDeclarations [] [] stmtSemAnn

ifElse :: Statement SemanticAnns
ifElse = IfElseStmt cond1 twoDeclarations [] oneDeclaration stmtSemAnn

elseIf :: ElseIf SemanticAnns
elseIf = ElseIf cond0 oneAssignment stmtSemAnn

ifElseIf :: Statement SemanticAnns
ifElseIf = IfElseStmt cond1 twoDeclarations [elseIf] oneDeclaration stmtSemAnn

renderStatement :: Statement SemanticAnns -> Text
renderStatement stmt = 
  case runReaderT (genBlockItem stmt) empty of
    Left err -> pack $ show err
    Right cStmts -> render $ vsep $ runReader (mapM pprint cStmts) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing if statements" $ do
    it "Prints a single if statement" $ do
      renderStatement singleIf `shouldBe`
        pack (
          "\nif (foo0 == 0) {\n" ++
          "    \n" ++
          "    uint32_t vector1[10];\n" ++
          "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "        vector1[__i0] = vector0[__i0];\n" ++
          "    }\n" ++
          "\n" ++
          "    __option_dyn_t option0;\n" ++
          "    option0.__variant = Some;\n" ++
          "    option0.Some.__0 = dyn_var0;\n" ++
          "\n" ++
          "}")
    it "Prints an if-else statement" $ do
      renderStatement ifElse `shouldBe`
        pack (
          "\nif (foo0 != 4294901760) {\n" ++
          "    \n" ++
          "    uint32_t vector1[10];\n" ++
          "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "        vector1[__i0] = vector0[__i0];\n" ++
          "    }\n" ++
          "\n" ++
          "    __option_dyn_t option0;\n" ++
          "    option0.__variant = Some;\n" ++
          "    option0.Some.__0 = dyn_var0;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    __option_dyn_t option1;\n" ++
          "    option1.__variant = None;\n" ++
          "\n" ++
          "}")
    it "Prints an if-else-if-else statement" $ do
      renderStatement ifElseIf `shouldBe`
        pack (
          "\nif (foo0 != 4294901760) {\n" ++
          "    \n" ++
          "    uint32_t vector1[10];\n" ++
          "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "        vector1[__i0] = vector0[__i0];\n" ++
          "    }\n" ++
          "\n" ++
          "    __option_dyn_t option0;\n" ++
          "    option0.__variant = Some;\n" ++
          "    option0.Some.__0 = dyn_var0;\n" ++
          "\n" ++
          "} else if (foo0 == 0) {\n" ++
          "    \n" ++
          "    foo0 = 0;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    __option_dyn_t option1;\n" ++
          "    option1.__variant = None;\n" ++
          "\n" ++
          "}")
