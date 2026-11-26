-- | An atomic load leaves the value in the object its argument refers to,
-- whether the argument is a reference to a variable or a reference parameter.
module Codegen.Positive.Source.Statement.AtomicLoadSpec (spec) where

import Pipeline.Common (runFullBuild)

import Test.Hspec
import Data.Text (isInfixOf, pack)

atomicLoads :: String
atomicLoads =
  "interface IReader {\n" ++
  "    procedure get(&mut self, dst : &mut u32);\n" ++
  "    procedure get_at(&mut self, dst : &mut u32);\n" ++
  "};\n" ++
  "resource class CReader provides IReader {\n" ++
  "    value : access AtomicAccess<u32>;\n" ++
  "    values : access AtomicArrayAccess<u32, 4>;\n" ++
  "    procedure get(&mut self, dst : &mut u32) {\n" ++
  "        var x : u32 = 0;\n" ++
  "        self->value.load(&mut x);\n" ++
  "        self->value.load(dst);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "    procedure get_at(&mut self, dst : &mut u32) {\n" ++
  "        self->values.load_index(2, dst);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

spec :: Spec
spec = describe "Atomic loads" $ do
  it "stores the value in a variable" $
    runFullBuild atomicLoads `shouldSatisfy`
      isInfixOf (pack "x = atomic_load(self->value)")
  it "stores the value through a reference parameter" $
    runFullBuild atomicLoads `shouldSatisfy`
      isInfixOf (pack "*dst = atomic_load(self->value)")
  it "stores an element through a reference parameter" $
    runFullBuild atomicLoads `shouldSatisfy`
      isInfixOf (pack "*dst = atomic_load(&self->values[2U])")
