-- | A discarded fixed-location read is cast to void in C.
module Codegen.Positive.Source.Statement.DiscardSpec (spec) where

import Pipeline.Common (runFullBuild)

import Test.Hspec
import Data.Text (isInfixOf, pack)

discards :: String
discards =
  "struct Regs {\n" ++
  "    data : u32;\n" ++
  "};\n" ++
  "interface IFifo {\n" ++
  "    procedure flush(&mut self, i : usize);\n" ++
  "};\n" ++
  "resource class CFifo provides IFifo {\n" ++
  "    regs : loc Regs;\n" ++
  "    table : loc [u32; 4];\n" ++
  "    procedure flush(&mut self, i : usize) {\n" ++
  "        _ = self->regs.data;\n" ++
  "        _ = self->table[i];\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

spec :: Spec
spec = describe "Discard statement" $ do
  it "casts the read of a field of a fixed-location struct to void" $
    runFullBuild discards `shouldSatisfy` isInfixOf (pack "(void)self->regs->data;")
  it "checks the index of a discarded element" $
    runFullBuild discards `shouldSatisfy`
      isInfixOf (pack "(void)self->table[termina__check__array_index(4U, i)];")
