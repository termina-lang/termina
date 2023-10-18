module UT.PPrinter.Expression.MemberFunctionAccessSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import PPrinter.Expression
import UT.PPrinter.Expression.Common
import Semantic.Monad

self, tmChannel, tmPool, bar0, bar1 :: Object SemanticAnns
self = Variable "self" (refDefinedTypeSemAnn "Resource")
tmChannel = Variable "tm_channel" (msgQueueSemAnn (DefinedType "TMDescriptor") 10)
tmPool = Variable "tm_pool" (poolSemAnn UInt32 10)
bar0 = Variable "bar0" (objSemAnn Mutable UInt16)
bar1 = Variable "bar1" dynUInt16SemAnn

tmPoolAlloc :: Expression SemanticAnns
tmPoolAlloc = MemberFunctionAccess tmPool "alloc" [] unitSemAnn

tmChannelsend, selfFoo0 :: Expression SemanticAnns
tmChannelsend = MemberFunctionAccess tmChannel "send" [AccessObject bar0] unitSemAnn
selfFoo0 = MemberFunctionAccess self "foo0" [AccessObject bar0, AccessObject bar1] unitSemAnn

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression empty

spec :: Spec
spec = do
  describe "Pretty printing method call expressions" $ do
    it "Prints the expression: tm_pool.alloc()" $ do
      renderExpression tmPoolAlloc `shouldBe`
        pack "__termina_pool_alloc(tm_pool)"
    it "Prints the expression: tm_channel.foo0(bar0)" $ do
      renderExpression tmChannelsend `shouldBe`
        pack "__termina_msg_queue_send(tm_channel, bar0)"
    it "Prints the expression: resource.foo0(bar0, bar1)" $ do
      renderExpression selfFoo0 `shouldBe`
        pack "__Resource_foo0(self, bar0, bar1)"
