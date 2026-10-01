-- | The index of an atomic access to an element of an array goes through the
-- bounds check, as that of any other array access, unless the provers show
-- that it falls inside the array.
module Codegen.Positive.Source.Statement.AtomicIndexSpec (spec) where

import Pipeline.Common (runFullBuild)

import Test.Hspec
import Data.Text (isInfixOf, pack)

atomicAccesses :: String
atomicAccesses =
  "interface IPool {\n" ++
  "    procedure touch(&mut self, i : usize);\n" ++
  "};\n" ++
  "resource class CPool provides IPool {\n" ++
  "    pool : access AtomicArrayAccess<u32; 8>;\n" ++
  "    procedure touch(&mut self, i : usize) {\n" ++
  "        var v : u32 = 0;\n" ++
  "        self->pool.load_index(i, &mut v);\n" ++
  "        if i < 8 {\n" ++
  "            self->pool.store_index(i, v + 1);\n" ++
  "        }\n" ++
  "        self->pool.store_index(3, v);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

spec :: Spec
spec = describe "Atomic accesses to array elements" $ do
  it "checks a variable index" $
    runFullBuild atomicAccesses `shouldSatisfy`
      isInfixOf (pack "atomic_load(&self->pool[termina__check__array_index(8U, i)])")
  it "leaves unchecked an index that a guard keeps inside the array" $
    runFullBuild atomicAccesses `shouldSatisfy`
      isInfixOf (pack "atomic_store(&self->pool[i], v + 1U)")
  it "leaves unchecked a constant index" $
    runFullBuild atomicAccesses `shouldSatisfy`
      isInfixOf (pack "atomic_store(&self->pool[3U], v)")
