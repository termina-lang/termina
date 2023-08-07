module UT.PPrinter.Statement.IfElseSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Statement
import UT.PPrinter.Expression.Common

optionDynUInt32TS :: TypeSpecifier
optionDynUInt32TS = Option (DynamicSubtype UInt32)

vectorTS :: TypeSpecifier
vectorTS = Vector UInt32 (KC (I UInt32 10))

optionDynUInt32SemAnn :: SemanticAnns
optionDynUInt32SemAnn = optionDynSemAnn UInt32

vectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn UInt32 (I UInt32 10)

vector0 :: Expression SemanticAnns
vector0 = AccessObject (RHS (Variable "vector0" vectorAnn))

vector1 :: Statement SemanticAnns
vector1 = Declaration "vector1" vectorTS vector0 undefined

foo0 :: Expression SemanticAnns
foo0 = AccessObject (RHS (Variable "foo0" uint32SemAnn))

uint32Const0, uint32Const0xFFFF0000 :: Expression SemanticAnns
uint32Const0 = Constant (I UInt32 0) uint32SemAnn
uint32Const0xFFFF0000 = Constant (I UInt32 4294901760) uint32SemAnn

constToFoo0 :: Statement SemanticAnns
constToFoo0 = AssignmentStmt (LHS (Variable "foo0" uint32SemAnn)) uint32Const0 undefined

dynVar0 :: Expression SemanticAnns
dynVar0 = AccessObject (RHS (Variable "dyn_var0" dynUInt32SemAnn))

option0, option1 :: Statement SemanticAnns
option0 = Declaration "option0" optionDynUInt32TS (OptionVariantExpression (Some dynVar0) optionDynUInt32SemAnn) undefined
option1 = Declaration "option1" optionDynUInt32TS (OptionVariantExpression None optionDynUInt32SemAnn) undefined

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
singleIf = IfElseStmt cond0 twoDeclarations [] [] undefined

ifElse :: Statement SemanticAnns
ifElse = IfElseStmt cond1 twoDeclarations [] oneDeclaration undefined

elseIf :: ElseIf SemanticAnns
elseIf = ElseIf cond0 oneAssignment undefined

ifElseIf :: Statement SemanticAnns
ifElseIf = IfElseStmt cond1 twoDeclarations [elseIf] oneDeclaration undefined

renderStatement :: Statement SemanticAnns -> Text
renderStatement = render . ppStatement empty

spec :: Spec
spec = do
  describe "Pretty printing if statements" $ do
    it "Prints a single if statement" $ do
      renderStatement singleIf `shouldBe`
        pack (
          "if (foo0 == (uint32_t)0) {\n" ++
          "\n" ++
          "    uint32_t vector1[10];\n" ++
          "\n" ++
          "    {\n" ++
          "        for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "            vector1[__i0] = vector0[__i0];\n" ++
          "        }\n" ++
          "    }\n" ++
          "\n" ++
          "    __Option_dyn_t option0;\n" ++
          "\n" ++
          "    {\n" ++
          "        option0.__variant = Some;\n" ++
          "        option0.__Some.__0 = dyn_var0;\n" ++
          "    }\n" ++
          "\n" ++
          "}")
    it "Prints an if-else statement" $ do
      renderStatement ifElse `shouldBe`
        pack (
          "if (foo0 != (uint32_t)4294901760) {\n" ++
          "\n" ++
          "    uint32_t vector1[10];\n" ++
          "\n" ++
          "    {\n" ++
          "        for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "            vector1[__i0] = vector0[__i0];\n" ++
          "        }\n" ++
          "    }\n" ++
          "\n" ++
          "    __Option_dyn_t option0;\n" ++
          "\n" ++
          "    {\n" ++
          "        option0.__variant = Some;\n" ++
          "        option0.__Some.__0 = dyn_var0;\n" ++
          "    }\n" ++
          "\n" ++
          "} else {\n" ++
          "\n" ++
          "    __Option_dyn_t option1;\n" ++
          "\n" ++
          "    {\n" ++
          "        option1.__variant = None;\n" ++
          "    }\n" ++
          "\n" ++
          "}")
    it "Prints an if-else-if-else statement" $ do
      renderStatement ifElseIf `shouldBe`
        pack (
          "if (foo0 != (uint32_t)4294901760) {\n" ++
          "\n" ++
          "    uint32_t vector1[10];\n" ++
          "\n" ++
          "    {\n" ++
          "        for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "            vector1[__i0] = vector0[__i0];\n" ++
          "        }\n" ++
          "    }\n" ++
          "\n" ++
          "    __Option_dyn_t option0;\n" ++
          "\n" ++
          "    {\n" ++
          "        option0.__variant = Some;\n" ++
          "        option0.__Some.__0 = dyn_var0;\n" ++
          "    }\n" ++
          "\n" ++
          "} else if (foo0 == (uint32_t)0) {\n" ++
          "\n" ++
          "    foo0 = (uint32_t)0;\n" ++
          "\n" ++
          "} else {\n" ++
          "\n" ++
          "    __Option_dyn_t option1;\n" ++
          "\n" ++
          "    {\n" ++
          "        option1.__variant = None;\n" ++
          "    }\n" ++
          "\n" ++
          "}")
