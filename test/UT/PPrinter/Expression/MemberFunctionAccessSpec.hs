module UT.PPrinter.Expression.MemberFunctionAccessSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Control.Monad.Reader
import Generator.CGenerator
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common
import Semantic.Monad

self, tmChannel, tmPool, bar0, bar1 :: Object SemanticAnns
self = Variable "self" (refDefinedTypeSemAnn "Resource")
tmChannel = Variable "tm_channel" (msgQueueSemAnn (DefinedType "TMDescriptor"))
tmPool = Variable "tm_pool" (poolSemAnn UInt32)
bar0 = Variable "bar0" (objSemAnn Mutable UInt16)
bar1 = Variable "bar1" dynUInt16SemAnn

tmPoolAlloc :: Expression SemanticAnns
tmPoolAlloc = MemberFunctionAccess tmPool "alloc" [] unitSemAnn

selfDereference :: Object SemanticAnns
selfDereference = Dereference self (definedTypeSemAnn Private "Resource")

tmChannelsend, selfFoo0 :: Expression SemanticAnns
tmChannelsend = MemberFunctionAccess tmChannel "send" [AccessObject bar0] unitSemAnn
selfFoo0 = MemberFunctionAccess selfDereference "foo0" [AccessObject bar0, AccessObject bar1] unitSemAnn

renderExpression :: Expression SemanticAnns -> Text
renderExpression expr = 
  case runReaderT (genExpression expr) empty of
    Left err -> pack $ show err
    Right cExpr -> render $ runReader (pprint cExpr) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing method call expressions" $ do
    it "Prints the expression: tm_pool.alloc()" $ do
      renderExpression tmPoolAlloc `shouldBe`
        pack "__termina__pool__alloc(tm_pool)"
    it "Prints the expression: tm_channel.foo0(bar0)" $ do
      renderExpression tmChannelsend `shouldBe`
        pack "__termina__msg_queue__send(tm_channel, (void *)&bar0)"
    it "Prints the expression: (*self).foo0(bar0, bar1)" $ do
      renderExpression selfFoo0 `shouldBe`
        pack "Resource__foo0(self, bar0, bar1)"
