module UT.PPrinter.Statement.ForLoopSpec (spec) where

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
import ControlFlow.Common
import Control.Monad.Except

vectorObjAnn :: SemanticAnn
vectorObjAnn = vectorObjSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))

vector0 :: Object SemanticAnn
vector0 = Variable "vector0" vectorObjAnn

total, i :: Expression SemanticAnn
total = AccessObject (Variable "total" (objSemAnn Mutable UInt32))
i = AccessObject (Variable "i" (objSemAnn Mutable USize))

vector0IndexI:: Expression SemanticAnn
vector0IndexI = AccessObject (ArrayIndexExpression vector0 i (objSemAnn Mutable UInt32))

forLoopBody :: [Statement SemanticAnn]
forLoopBody = [AssignmentStmt (Variable "total" (objSemAnn Mutable UInt32)) (BinOp Addition total vector0IndexI uint32ExprSemAnn) stmtSemAnn]

breakCond :: Expression SemanticAnn
breakCond = BinOp RelationalNotEqual i (Constant (I (TInteger 5 DecRepr) (Just USize)) uint32ExprSemAnn) boolExprSemAnn

forLoop0 :: Statement SemanticAnn
forLoop0 = ForLoopStmt "i" USize (Constant (I (TInteger 0 DecRepr) (Just USize)) uint32ExprSemAnn) (Constant (I (TInteger 10 DecRepr) (Just USize)) uint32ExprSemAnn) Nothing forLoopBody stmtSemAnn

forLoop1 :: Statement SemanticAnn
forLoop1 = ForLoopStmt "i" USize (Constant (I (TInteger 0 DecRepr) (Just USize)) uint32ExprSemAnn) (Constant (I (TInteger 10 DecRepr) (Just USize)) uint32ExprSemAnn) (Just breakCond) forLoopBody stmtSemAnn

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
