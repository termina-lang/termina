module UT.PPrinter.Statement.FreeSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Statement
import UT.PPrinter.Expression.Common

dynVectorAnn :: SemanticAnns
dynVectorAnn = dynVectorSemAnn UInt32 (I UInt32 10)

dynVar0, dynVector0 :: Object SemanticAnns
dynVar0 = Variable "dyn_var0" dynUInt32SemAnn
dynVector0 = Variable "dyn_vector0" dynVectorAnn

freeVar0, freeVector0 :: Statement SemanticAnns
freeVar0 = Free dynVar0 undefined
freeVector0 = Free dynVector0 undefined

renderStatement :: Statement SemanticAnns -> Text
renderStatement = render . ppStatement empty

spec :: Spec
spec = do
  describe "Pretty printing free statements" $ do
    it "Prints the statement free(dyn_var0);" $ do
      renderStatement freeVar0 `shouldBe`
        pack "__termina_pool_free(dyn_var0);"
    it "Prints the statement free(dyn_vector0);" $ do
      renderStatement freeVector0 `shouldBe`
        pack "__termina_pool_free(dyn_vector0);"
