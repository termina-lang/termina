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
import ControlFlow.BasicBlocks
import Control.Monad.Except

arrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable TUInt32 (K (TInteger 10 DecRepr))

array0 :: Object SemanticAnn
array0 = Variable "array0" arrayObjAnn

total, i :: Expression SemanticAnn
total = AccessObject (Variable "total" (objSemAnn Mutable TUInt32))
i = AccessObject (Variable "i" (objSemAnn Mutable TUSize))

array0IndexI:: Expression SemanticAnn
array0IndexI = AccessObject (ArrayIndexExpression array0 i (objSemAnn Mutable TUInt32))

forLoopBody :: Block SemanticAnn
forLoopBody = Block [AssignmentStmt (Variable "total" (objSemAnn Mutable TUInt32)) (BinOp Addition total array0IndexI uint32ExprSemAnn) stmtSemAnn] stmtSemAnn

breakCond :: Expression SemanticAnn
breakCond = BinOp RelationalNotEqual i (Constant (I (TInteger 5 DecRepr) (Just TUSize)) uint32ExprSemAnn) boolExprSemAnn

forLoop0 :: Statement SemanticAnn
forLoop0 = ForLoopStmt "i" TUSize (Constant (I (TInteger 0 DecRepr) (Just TUSize)) uint32ExprSemAnn) (Constant (I (TInteger 10 DecRepr) (Just TUSize)) uint32ExprSemAnn) Nothing forLoopBody stmtSemAnn

forLoop1 :: Statement SemanticAnn
forLoop1 = ForLoopStmt "i" TUSize (Constant (I (TInteger 0 DecRepr) (Just TUSize)) uint32ExprSemAnn) (Constant (I (TInteger 10 DecRepr) (Just TUSize)) uint32ExprSemAnn) (Just breakCond) forLoopBody stmtSemAnn

renderStatement :: Statement SemanticAnn -> Text
renderStatement stmt = 
  case runExcept (genBBlocks [] [stmt]) of
    Left err -> pack $ show err
    Right bBlocks ->
      case runReader (runExceptT (Prelude.concat <$> mapM genBlocks bBlocks)) empty of
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
          "    total = total + array0[i];\n" ++
          "\n" ++
          "}")
    it "Prints a for loop statement with break condition" $ do
      renderStatement forLoop1 `shouldBe`
        pack (
          "\nfor (size_t i = 0; i < 10 && i != 5; i = i + 1) {\n" ++
          "    \n" ++
          "    total = total + array0[i];\n" ++
          "\n" ++
          "}")
