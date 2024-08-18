module UT.PPrinter.Statement.IfElseSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Prettyprinter
import Control.Monad.Reader
import Generator.CodeGen.Statement
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

optionBoxUInt32TS :: TypeSpecifier
optionBoxUInt32TS = Option (BoxSubtype UInt32)

vectorTS :: TypeSpecifier
vectorTS = Array UInt32 (K (TInteger 10 DecRepr))

optionBoxUInt32SemAnn :: SemanticAnn
optionBoxUInt32SemAnn = optionBoxSemAnn Mutable UInt32

vectorAnn :: SemanticAnn
vectorAnn = vectorSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))

vector0 :: Expression SemanticAnn
vector0 = AccessObject (Variable "vector0" vectorAnn)

vector1 :: Statement SemanticAnn
vector1 = Declaration "vector1" Mutable vectorTS vector0 stmtSemAnn

foo0 :: Expression SemanticAnn
foo0 = AccessObject (Variable "foo0" (objSemAnn Mutable UInt32))

uint32Const0, uint32Const0xFFFF0000 :: Expression SemanticAnn
uint32Const0 = Constant (I (TInteger 0 DecRepr) (Just UInt32)) uint32SemAnn
uint32Const0xFFFF0000 = Constant (I (TInteger 4294901760 DecRepr) (Just UInt32)) uint32SemAnn

constToFoo0 :: Statement SemanticAnn
constToFoo0 = AssignmentStmt (Variable "foo0" (objSemAnn Mutable UInt32)) uint32Const0 stmtSemAnn

boxVar0 :: Expression SemanticAnn
boxVar0 = AccessObject (Variable "box_var0" boxUInt32SemAnn)

option0, option1 :: Statement SemanticAnn
option0 = Declaration "option0" Mutable optionBoxUInt32TS (OptionVariantInitializer (Some boxVar0) optionBoxUInt32SemAnn) stmtSemAnn
option1 = Declaration "option1" Mutable optionBoxUInt32TS (OptionVariantInitializer None optionBoxUInt32SemAnn) stmtSemAnn

twoDeclarations :: [Statement SemanticAnn]
twoDeclarations = [vector1, option0]

oneAssignment :: [Statement SemanticAnn]
oneAssignment = [constToFoo0]

oneDeclaration :: [Statement SemanticAnn]
oneDeclaration = [option1]

cond0, cond1 :: Expression SemanticAnn
cond0 = BinOp RelationalEqual foo0 uint32Const0 boolSemAnn
cond1 = BinOp RelationalNotEqual foo0 uint32Const0xFFFF0000 boolSemAnn

singleIf :: Statement SemanticAnn
singleIf = IfElseStmt cond0 twoDeclarations [] Nothing stmtSemAnn

ifElse :: Statement SemanticAnn
ifElse = IfElseStmt cond1 twoDeclarations [] (Just oneDeclaration) stmtSemAnn

elseIf :: ElseIf SemanticAnn
elseIf = ElseIf cond0 oneAssignment stmtSemAnn

ifElseIf :: Statement SemanticAnn
ifElseIf = IfElseStmt cond1 twoDeclarations [elseIf] (Just oneDeclaration) stmtSemAnn

renderStatement :: Statement SemanticAnn -> Text
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
          "    __option_box_t option0;\n" ++
          "    option0.__variant = Some;\n" ++
          "    option0.Some.__0 = box_var0;\n" ++
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
          "\nif (foo0 != 4294901760) {\n" ++
          "    \n" ++
          "    uint32_t vector1[10];\n" ++
          "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "        vector1[__i0] = vector0[__i0];\n" ++
          "    }\n" ++
          "\n" ++
          "    __option_box_t option0;\n" ++
          "    option0.__variant = Some;\n" ++
          "    option0.Some.__0 = box_var0;\n" ++
          "\n" ++
          "} else if (foo0 == 0) {\n" ++
          "    \n" ++
          "    foo0 = 0;\n" ++
          "\n" ++
          "} else {\n" ++
          "    \n" ++
          "    __option_box_t option1;\n" ++
          "    option1.__variant = None;\n" ++
          "\n" ++
          "}")
