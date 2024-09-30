module UT.PPrinter.Statement.IfElseSpec (spec) where

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
import ControlFlow.BasicBlocks
import Control.Monad.Except

optionBoxUInt32TS :: TerminaType
optionBoxUInt32TS = Option (BoxSubtype UInt32)

arrayTS :: TerminaType
arrayTS = Array UInt32 (K (TInteger 10 DecRepr))

optionBoxUInt32ExprSemAnn :: SemanticAnn
optionBoxUInt32ExprSemAnn = optionBoxExprSemAnn UInt32

arrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))

array0 :: Expression SemanticAnn
array0 = AccessObject (Variable "array0" arrayObjAnn)

array1 :: Statement SemanticAnn
array1 = Declaration "array1" Mutable arrayTS array0 stmtSemAnn

foo0 :: Expression SemanticAnn
foo0 = AccessObject (Variable "foo0" (objSemAnn Mutable UInt32))

uint32Const0, uint32Const0xFFFF0000 :: Expression SemanticAnn
uint32Const0 = Constant (I (TInteger 0 DecRepr) (Just UInt32)) uint32ExprSemAnn
uint32Const0xFFFF0000 = Constant (I (TInteger 4294901760 DecRepr) (Just UInt32)) uint32ExprSemAnn

constToFoo0 :: Statement SemanticAnn
constToFoo0 = AssignmentStmt (Variable "foo0" (objSemAnn Mutable UInt32)) uint32Const0 stmtSemAnn

boxVar0 :: Expression SemanticAnn
boxVar0 = AccessObject (Variable "box_var0" boxUInt32SemAnn)

option0, option1 :: Statement SemanticAnn
option0 = Declaration "option0" Mutable optionBoxUInt32TS (OptionVariantInitializer (Some boxVar0) optionBoxUInt32ExprSemAnn) stmtSemAnn
option1 = Declaration "option1" Mutable optionBoxUInt32TS (OptionVariantInitializer None optionBoxUInt32ExprSemAnn) stmtSemAnn

twoDeclarations :: [Statement SemanticAnn]
twoDeclarations = [array1, option0]

oneAssignment :: [Statement SemanticAnn]
oneAssignment = [constToFoo0]

oneDeclaration :: [Statement SemanticAnn]
oneDeclaration = [option1]

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
  describe "Pretty printing if statements" $ do
    it "Prints a single if statement" $ do
      renderStatement singleIf `shouldBe`
        pack (
          "\nif (foo0 == 0) {\n" ++
          "    \n" ++
          "    uint32_t array1[10];\n" ++
          "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
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
          "\nif (foo0 != 4294901760) {\n" ++
          "    \n" ++
          "    uint32_t array1[10];\n" ++
          "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
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
          "\nif (foo0 != 4294901760) {\n" ++
          "    \n" ++
          "    uint32_t array1[10];\n" ++
          "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "        array1[__i0] = array0[__i0];\n" ++
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
