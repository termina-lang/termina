{-# LANGUAGE LambdaCase #-}
-- | Constant-folding negative tests, *detail* flavour: assert the value carried
-- by the error (the overflowing constant, the out-of-bounds index), not just
-- the CFE-NNN code.
module ConstFolding.Negative.DetailSpec (spec) where

import ConstFolding.Common (constFoldError)
import ControlFlow.ConstFolding.Errors (Error(..))
import Utils.Annotations (AnnotatedError(..))

import Test.Hspec

-- CFE-004: 256 does not fit in a u8.
overflow :: String
overflow = "function f() -> u8 {\n    return 256 : u16 as u8;\n}"

-- CFE-007: slicing [0 .. 5] out of a 4-element array.
sliceOutOfBounds :: String
sliceOutOfBounds =
  "function take2(_data : &[u8; 2]) {\n    return;\n}\n" ++
  "const lo : usize = 0;\n" ++
  "const hi : usize = 5;\n" ++
  "function f() {\n" ++
  "    var a : [u8; 4] = [0 : u8; 4];\n" ++
  "    take2(&a[lo .. hi]);\n" ++
  "    return;\n" ++
  "}"

-- CFE-011: storing at index 10 of an atomic array of 8 elements.
atomicIndexOutOfBounds :: String
atomicIndexOutOfBounds =
  "interface IPool {\n    procedure touch(&mut self);\n};\n" ++
  "resource class CPool provides IPool {\n" ++
  "    pool : access AtomicArrayAccess<u32; 8>;\n" ++
  "    procedure touch(&mut self) {\n" ++
  "        self->pool.store_index(10, 0);\n" ++
  "        return;\n" ++
  "    }\n" ++
  "};\n"

spec :: Spec
spec = describe "ConstFolding: error detail (carried value)" $ do
  it "CFE-011 carries the size of the atomic array and the index, in that order" $
    constFoldError atomicIndexOutOfBounds `shouldSatisfy` \case
      Just (AnnotatedError (EAtomicArrayIndexOutOfBounds size index) _) -> size == 8 && index == 10
      _ -> False
  it "CFE-004 carries the overflowing constant" $
    constFoldError overflow `shouldSatisfy` \case
      Just (AnnotatedError (EConstIntegerOverflow 256 _) _) -> True
      _ -> False
  it "CFE-007 carries the out-of-bounds slice bounds" $
    constFoldError sliceOutOfBounds `shouldSatisfy` \case
      Just (AnnotatedError (EArraySliceOutOfBounds size upper) _) -> size == 4 && upper == 5
      _ -> False
