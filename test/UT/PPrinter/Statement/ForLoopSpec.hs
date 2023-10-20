module UT.PPrinter.Statement.ForLoopSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Statement
import UT.PPrinter.Expression.Common

vectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn Mutable UInt32 (K 10)

vector0 :: Object SemanticAnns
vector0 = Variable "vector0" vectorAnn

total, i :: Expression SemanticAnns
total = AccessObject (Variable "total" (objSemAnn Mutable UInt32))
i = AccessObject (Variable "i" (objSemAnn Mutable USize))

vector0IndexI:: Expression SemanticAnns
vector0IndexI = AccessObject (VectorIndexExpression vector0 i (objSemAnn Mutable UInt32))

forLoopBody :: [Statement SemanticAnns]
forLoopBody = [AssignmentStmt (Variable "total" (objSemAnn Mutable UInt32)) (BinOp Addition total vector0IndexI uint32SemAnn) undefined]

breakCond :: Expression SemanticAnns
breakCond = BinOp RelationalNotEqual i (Constant (I USize 5) uint32SemAnn) boolSemAnn

forLoop0 :: Statement SemanticAnns
forLoop0 = ForLoopStmt "i" USize (Constant (I USize 0) uint32SemAnn) (Constant (I USize 10) uint32SemAnn) Nothing forLoopBody undefined

forLoop1 :: Statement SemanticAnns
forLoop1 = ForLoopStmt "i" USize (Constant (I USize 0) uint32SemAnn) (Constant (I USize 10) uint32SemAnn) (Just breakCond) forLoopBody undefined

renderStatement :: Statement SemanticAnns -> Text
renderStatement = render . ppStatement empty

spec :: Spec
spec = do
  describe "Pretty for-loop statements" $ do
    it "Prints a for loop statement without break condition" $ do
      renderStatement forLoop0 `shouldBe`
        pack (
          "{\n" ++
          "    size_t __start = 0;\n" ++
          "    size_t __end = 10;\n" ++
          "\n" ++
          "    for (size_t i = __start; i < __end; i = i + 1) {\n" ++
          "        \n" ++
          "        total = total + vector0[i];\n" ++
          "\n" ++
          "    }\n" ++
          "}")
    it "Prints a for loop statement with break condition" $ do
      renderStatement forLoop1 `shouldBe`
        pack (
          "{\n" ++
          "    size_t __start = 0;\n" ++
          "    size_t __end = 10;\n" ++
          "\n" ++
          "    for (size_t i = __start; i < __end && (i != 5); i = i + 1) {\n" ++
          "        \n" ++
          "        total = total + vector0[i];\n" ++
          "\n" ++
          "    }\n" ++
          "}")
