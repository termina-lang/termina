-- | An atomic addition leaves the previous value in the object its last
-- argument refers to, and its index, on an array, goes through the bounds
-- check as that of any other atomic access.
module Codegen.Positive.Source.Statement.AtomicFetchAddSpec (spec) where

import Pipeline.Common (runFullBuild)

import Test.Hspec
import Data.Text (isInfixOf, pack)

atomicAdditions :: String
atomicAdditions =
  "interface ICounter {\n" ++
  "    procedure next(&mut self, id : &mut u16);\n" ++
  "    procedure count(&mut self, i : usize);\n" ++
  "};\n" ++
  "resource class CCounter provides ICounter {\n" ++
  "    counter : access AtomicAccess<u16>;\n" ++
  "    counts : access AtomicArrayAccess<u32, 8>;\n" ++
  "    procedure next(&mut self, id : &mut u16) {\n" ++
  "        self->counter.fetch_add(1, id);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "    procedure count(&mut self, i : usize) {\n" ++
  "        var prev : u32 = 0;\n" ++
  "        self->counts.fetch_add_index(i, 2, &mut prev);\n" ++
  "        self->counts.fetch_add_index(3, prev, &mut prev);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

spec :: Spec
spec = describe "Atomic additions" $ do
  it "stores the previous value through a reference parameter" $
    runFullBuild atomicAdditions `shouldSatisfy`
      isInfixOf (pack "*id = atomic_fetch_add(self->counter, 1U)")
  it "checks a variable index" $
    runFullBuild atomicAdditions `shouldSatisfy`
      isInfixOf (pack "prev = atomic_fetch_add(&self->counts[termina__check__array_index(8U, i)],\n                            2U)")
  it "leaves unchecked a constant index" $
    runFullBuild atomicAdditions `shouldSatisfy`
      isInfixOf (pack "prev = atomic_fetch_add(&self->counts[3U], prev)")
