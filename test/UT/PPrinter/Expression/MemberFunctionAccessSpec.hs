module UT.PPrinter.Expression.MemberFunctionAccessSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text hiding (empty)
import Data.Map
import Control.Monad.Reader
import Generator.CodeGen.Statement
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common
import Semantic.Types
import ControlFlow.BasicBlocks
import Control.Monad.Except
import Prettyprinter

self, tmChannel, tmPool, bar0, bar1 :: Object SemanticAnn
self = Variable "self" (refDefinedTypeSemAnn "Resource")
tmChannel = Variable "tm_channel" (msgQueueSemAnn (DefinedType "TMDescriptor"))
tmPool = Variable "tm_pool" (poolSemAnn UInt32)
bar0 = Variable "bar0" (objSemAnn Mutable UInt16)
bar1 = Variable "bar1" boxUInt16SemAnn

refObj :: Expression SemanticAnn
refObj = (ReferenceExpression Mutable (Variable "memory" (objSemAnn Mutable (Option (BoxSubtype UInt32)))) (refSemAnn (Option (BoxSubtype UInt32))))

tmPoolAlloc :: Expression SemanticAnn
tmPoolAlloc = MemberFunctionCall tmPool "alloc" [refObj] (funSemAnn [] Unit)

selfDereference :: Object SemanticAnn
selfDereference = Dereference self (definedTypeObjSemAnn Mutable "Resource")

tmChannelsend, selfFoo0 :: Expression SemanticAnn
tmChannelsend = MemberFunctionCall tmChannel "send" [AccessObject bar0] (funSemAnn [UInt16] Unit)
selfFoo0 = MemberFunctionCall selfDereference "foo0" [AccessObject bar0, AccessObject bar1] (funSemAnn [UInt16, UInt16] Unit)

tmChannelSendStmt, selfFoo0Stmt, tmPoolAllocStmt :: Statement SemanticAnn
tmChannelSendStmt = SingleExpStmt tmChannelsend stmtSemAnn
selfFoo0Stmt = SingleExpStmt selfFoo0 stmtSemAnn
tmPoolAllocStmt = SingleExpStmt tmPoolAlloc stmtSemAnn

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
  describe "Pretty printing method call expressions" $ do
    it "Prints the expression: tm_pool.alloc()" $ do
      renderStatement tmPoolAllocStmt `shouldBe`
        pack "\n__termina_pool__alloc(tm_pool, &memory);"
    it "Prints the expression: tm_channel.foo0(bar0)" $ do
      renderStatement tmChannelSendStmt `shouldBe`
        pack "\n__termina_msg_queue__send(tm_channel, (void *)&bar0);"
    it "Prints the expression: (*self).foo0(bar0, bar1)" $ do
      renderStatement selfFoo0Stmt `shouldBe`
        pack "\nResource__foo0(self, bar0, bar1);"
