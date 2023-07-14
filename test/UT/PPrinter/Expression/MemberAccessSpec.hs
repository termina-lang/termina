module UT.PPrinter.Expression.MemberAccessSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import PPrinter.Expression
import UT.PPrinter.Expression.Common
import Semantic.Monad

tmDescriptor0, tmDescriptor1, resource0, tmChannel, tmPool, 
  bar0, bar1, field0, alloc, send, foo0 :: Expression SemanticAnns
tmDescriptor0 = Variable "tm_descriptor0" (definedTypeSemAnn "TMDescriptor")
tmDescriptor1 = Variable "tm_descriptor1" (dynDefinedTypeSemAnn "TMDescriptor")
resource0 = Variable "resource0" (definedTypeSemAnn "Resource")
tmChannel = Variable "tm_channel" (msgQueueSemAnn (DefinedType "TMDescriptor") 10)
tmPool = Variable "tm_pool" (poolSemAnn UInt32 10)
bar0 = Variable "bar0" uint16SemAnn
bar1 = Variable "bar1" dynUInt16SemAnn
field0 = Variable "field0" uint32SemAnn
alloc = FunctionExpression "alloc" [] unitSemAnn
send = FunctionExpression "send" [bar0] uint32SemAnn
foo0 = FunctionExpression "foo0" [bar0, bar1] unitSemAnn

undynTMDescriptor1 :: Expression SemanticAnns
undynTMDescriptor1 = Undyn tmDescriptor1 (definedTypeSemAnn "TMDescriptor")

tmDescriptor0field0, tmDescriptor1field0 :: Expression SemanticAnns
tmDescriptor0field0 = BinOp MemberAccess tmDescriptor0 field0 uint32SemAnn
tmDescriptor1field0 = BinOp MemberAccess undynTMDescriptor1 field0 uint32SemAnn

tmPoolAlloc :: Expression SemanticAnns
tmPoolAlloc = BinOp MemberAccess tmPool alloc uint32SemAnn

tmChannelsend, resource0foo0 :: Expression SemanticAnns
tmChannelsend = BinOp MemberAccess tmChannel send uint32SemAnn
resource0foo0 = BinOp MemberAccess resource0 foo0 uint32SemAnn

renderExpression :: Expression SemanticAnns -> Text
renderExpression = render . ppExpression empty

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