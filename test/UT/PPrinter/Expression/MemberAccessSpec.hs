module UT.PPrinter.Expression.MemberAccessSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Data.Text
import Semantic.Monad
import PPrinter.Expression
import UT.PPrinter.Expression.Common

tmDescriptor0, tmDescriptor1, resource0, tmChannel, tmPool, 
  bar0, bar1, field0, alloc, send, foo0 :: Expression SemanticAnns
tmDescriptor0 = Variable "tm_descriptor0" (SemAnn undefined (DefinedType "TMDescriptor"))
tmDescriptor1 = Variable "tm_descriptor1" (SemAnn undefined (DynamicSubtype (DefinedType "TMDescriptor")))
resource0 = Variable "resource0" (SemAnn undefined (DefinedType "Resource"))
tmChannel = Variable "tm_channel" (SemAnn undefined (MsgQueue (DefinedType "TMDescriptor") (K 10)))
tmPool = Variable "tm_pool" (SemAnn undefined (Pool UInt32 (K 10)))
bar0 = Variable "bar0" (SemAnn undefined uint16TS)
bar1 = Variable "bar1" (SemAnn undefined (DynamicSubtype uint16TS))
field0 = Variable "field0" (SemAnn undefined uint32TS)
alloc = FunctionExpression "alloc" [] (SemAnn undefined Unit)
send = FunctionExpression "send" [bar0] (SemAnn undefined UInt32)
foo0 = FunctionExpression "foo0" [bar0, bar1] (SemAnn undefined Unit)

tmDescriptor0field0, tmDescriptor1field0 :: Expression SemanticAnns
tmDescriptor0field0 = BinOp MemberAccess tmDescriptor0 field0 (SemAnn undefined uint32TS)
tmDescriptor1field0 = BinOp MemberAccess tmDescriptor1 field0 (SemAnn undefined uint32TS)

tmPoolAlloc :: Expression SemanticAnns
tmPoolAlloc = BinOp MemberAccess tmPool alloc (SemAnn undefined uint32TS)

tmChannelsend, resource0foo0 :: Expression SemanticAnns
tmChannelsend = BinOp MemberAccess tmChannel send (SemAnn undefined uint32TS)
resource0foo0 = BinOp MemberAccess resource0 foo0 (SemAnn undefined uint32TS)

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppRootExpression

spec :: Spec
spec = do
  describe "Pretty printing member access expressions" $ do
    it "Prints the expression: tm_descriptor0.field0" $ do
      renderExpression tmDescriptor0field0 `shouldBe`
        pack "tm_descriptor0.field0"
    it "Prints the expression: tm_descriptor1.field0" $ do
      renderExpression tmDescriptor1field0 `shouldBe`
        pack "*((TMDescriptor *)tm_descriptor1.datum).field0"
  describe "Pretty printing method call expressions" $ do
    it "Prints the expression: tm_pool.alloc()" $ do
      renderExpression tmPoolAlloc `shouldBe`
        pack "__pool_alloc(&tm_pool)"
    it "Prints the expression: tm_channel.foo0(bar0)" $ do
      renderExpression tmChannelsend `shouldBe`
        pack "__msg_queue_send(&tm_channel, bar0)"
    it "Prints the expression: resource.foo0(bar0, bar1)" $ do
      renderExpression resource0foo0 `shouldBe`
        pack "__Resource_foo0(&resource0, bar0, bar1)"