-- | An option-box built in the argument of a procedure call moves its box.
module VarUsage.Negative.OptionBoxArgumentSpec (spec) where

import Test.Hspec
import Semantic.AST
import ControlFlow.BoxUsage.Errors
import qualified ControlFlow.BoxUsage.Errors as BE
import VarUsage.Common

-- | A resource procedure that receives a box parameter @data@ and hands an
-- option-box to a port that takes one by value. The body is spliced before the
-- final @return@.
resource :: String -> String
resource body =
  "interface ISink {\n" ++
  "    procedure consume(&mut self, data : Option<box u32>);\n" ++
  "};\n" ++
  "interface IR {\n" ++
  "    procedure proc0(&mut self, _data : box u32);\n" ++
  "};\n" ++
  "resource class CR provides IR {\n" ++
  "    data_pool : access Allocator<u32>;\n" ++
  "    snk : access ISink;\n" ++
  "    procedure proc0(&mut self, data : box u32) {\n" ++
  body ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

isMovedTwice :: Identifier -> Maybe BE.Error -> Bool
isMovedTwice ident = \case
  Just (EBoxMovedTwice ident' _) -> ident == ident'
  _ -> False

isNotMoved :: Identifier -> Maybe BE.Error -> Bool
isNotMoved ident = \case
  Just (EBoxNotMoved ident') -> ident == ident'
  _ -> False

spec :: Spec
spec = describe "Box usage: option-box built in an argument" $ do

  it "moves the box of Some into the procedure" $
    runNegativeTestBoxUsage (resource "        self->snk.consume(Some(data));\n")
      `shouldSatisfy` null

  it "rejects freeing the box after moving it in Some" $
    runNegativeTestBoxUsage (resource
      ("        self->snk.consume(Some(data));\n" ++
       "        self->data_pool.free(data);\n"))
      `shouldSatisfy` isMovedTwice "data"

  it "does not move a box with None" $
    runNegativeTestBoxUsage (resource "        self->snk.consume(None);\n")
      `shouldSatisfy` isNotMoved "data"

  it "accepts None and then freeing the box" $
    runNegativeTestBoxUsage (resource
      ("        self->snk.consume(None);\n" ++
       "        self->data_pool.free(data);\n"))
      `shouldSatisfy` null
