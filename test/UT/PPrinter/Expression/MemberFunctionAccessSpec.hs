module UT.PPrinter.Expression.MemberFunctionAccessSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

self, tmChannel, tmPool, bar0, bar1 :: Object SemanticAnn
self = Variable "self" (refGlobalResourceSemAnn "Resource")
tmChannel = Variable "tm_channel" (msgQueueSemAnn (TStruct "TMDescriptor"))
tmPool = Variable "tm_pool" (poolSemAnn TUInt32)
bar0 = Variable "bar0" (objSemAnn Mutable TUInt16)
bar1 = Variable "bar1" boxUInt16SemAnn

refObj :: Expression SemanticAnn
refObj = ReferenceExpression Mutable (Variable "memory" (objSemAnn Mutable (TOption (TBoxSubtype TUInt32)))) (refSemAnn (TOption (TBoxSubtype TUInt32)))

tmPoolAlloc :: Expression SemanticAnn
tmPoolAlloc = MemberFunctionCall tmPool "alloc" [refObj] (funSemAnn [] TUnit)

selfDereference :: Object SemanticAnn
selfDereference = Dereference self (resourceObjSemAnn Mutable "Resource")

tmChannelsend, selfFoo0 :: Expression SemanticAnn
tmChannelsend = MemberFunctionCall tmChannel "send" [AccessObject bar0] (funSemAnn [TUInt16] TUnit)
selfFoo0 = MemberFunctionCall selfDereference "foo0" [AccessObject bar0, AccessObject bar1] (funSemAnn [TUInt16, TUInt16] TUnit)

tmChannelSendStmt, selfFoo0Stmt, tmPoolAllocStmt :: Statement SemanticAnn
tmChannelSendStmt = SingleExpStmt tmChannelsend stmtSemAnn
selfFoo0Stmt = SingleExpStmt selfFoo0 stmtSemAnn
tmPoolAllocStmt = SingleExpStmt tmPoolAlloc stmtSemAnn

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
