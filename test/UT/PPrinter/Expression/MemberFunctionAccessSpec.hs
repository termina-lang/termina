module UT.PPrinter.Expression.MemberFunctionAccessSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

self, tmChannel, bar0, bar1 :: Object SemanticAnn
self = Variable "self" (refGlobalResourceSemAnn "Resource")
tmChannel = Variable "tm_channel" (msgQueueSemAnn (TStruct "TMDescriptor"))
bar0 = Variable "bar0" (objSemAnn Mutable TUInt16)
bar1 = Variable "bar1" boxUInt16SemAnn

selfDereference :: Object SemanticAnn
selfDereference = Dereference self (resourceObjSemAnn Mutable "Resource")

tmChannelsend, selfFoo0 :: Expression SemanticAnn
tmChannelsend = MemberFunctionCall tmChannel "send" [AccessObject bar0] (funSemAnn [TUInt16] TUnit)
selfFoo0 = MemberFunctionCall selfDereference "foo0" [AccessObject bar0, AccessObject bar1] (funSemAnn [TUInt16, TUInt16] TUnit)

tmChannelSendStmt, selfFoo0Stmt :: Statement SemanticAnn
tmChannelSendStmt = SingleExpStmt tmChannelsend stmtSemAnn
selfFoo0Stmt = SingleExpStmt selfFoo0 stmtSemAnn

spec :: Spec
spec = do
  describe "Pretty printing method call expressions" $ do
    it "Prints the expression: tm_channel.foo0(bar0)" $ do
      renderStatement tmChannelSendStmt `shouldBe`
        pack "\n__termina_out_port__send(tm_channel, (void *)&bar0);"
    it "Prints the expression: (*self).foo0(bar0, bar1)" $ do
      renderStatement selfFoo0Stmt `shouldBe`
        pack "\nResource__foo0(self, bar0, bar1);"
