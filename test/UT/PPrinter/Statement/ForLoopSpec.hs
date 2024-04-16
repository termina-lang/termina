module UT.PPrinter.Statement.ForLoopSpec (spec) where

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

vectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))

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
breakCond = BinOp RelationalNotEqual i (Constant (I USize (TInteger 5 DecRepr)) uint32SemAnn) boolSemAnn

forLoop0 :: Statement SemanticAnns
forLoop0 = ForLoopStmt "i" USize (Constant (I USize (TInteger 0 DecRepr)) uint32SemAnn) (Constant (I USize (TInteger 10 DecRepr)) uint32SemAnn) Nothing forLoopBody undefined

forLoop1 :: Statement SemanticAnns
forLoop1 = ForLoopStmt "i" USize (Constant (I USize (TInteger 0 DecRepr)) uint32SemAnn) (Constant (I USize (TInteger 10 DecRepr)) uint32SemAnn) (Just breakCond) forLoopBody undefined

renderStatement :: Statement SemanticAnns -> Text
renderStatement stmt = 
  case runReaderT (genBlockItem stmt) empty of
    Left err -> pack $ show err
    Right cStmts -> render $ vsep $ runReader (mapM pprint cStmts) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty for-loop statements" $ do
    it "Prints a for loop statement without break condition" $ do
      renderStatement forLoop0 `shouldBe`
        pack (
          "\nfor (size_t i = 0; i < 10; i = i + 1) {\n" ++
          "    \n" ++
          "    total = total + vector0[i];\n" ++
          "\n" ++
          "}")
    it "Prints a for loop statement with break condition" $ do
      renderStatement forLoop1 `shouldBe`
        pack (
          "\nfor (size_t i = 0; i < 10 && i != 5; i = i + 1) {\n" ++
          "    \n" ++
          "    total = total + vector0[i];\n" ++
          "\n" ++
          "}")
