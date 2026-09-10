{-# LANGUAGE LambdaCase #-}
-- | Constant-folding negative tests, *detail* flavour: assert the value carried
-- by the error (the overflowing constant, the out-of-bounds index), not just
-- the CPE-NNN code.
module ConstFolding.Negative.DetailSpec (spec) where

import ConstFolding.Common (constFoldError)
import ControlFlow.ConstFolding.Errors (Error(..))
import Utils.Annotations (AnnotatedError(..))

import Test.Hspec

-- CPE-004: 256 does not fit in a u8.
overflow :: String
overflow = "function f() -> u8 {\n    return 256 : u16 as u8;\n}"

-- CPE-010: slicing [0 .. 5] out of a 4-element array.
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

-- CPE-017: an unsigned expression is never less than zero.
unsignedBelowZero :: String
unsignedBelowZero =
  "function f(x : u32) -> u32 {\n" ++
  "    var y : u32 = 0 : u32;\n" ++
  "    if (x < 0 : u32) {\n" ++
  "        y = 1 : u32;\n" ++
  "    }\n" ++
  "    return y;\n" ++
  "}"

-- CPE-017: with the constant on the left, 255 >= x holds for every u8.
constantAtUpperLimit :: String
constantAtUpperLimit =
  "function f(x : u8) -> u32 {\n" ++
  "    var y : u32 = 0 : u32;\n" ++
  "    if (255 : u8 >= x) {\n" ++
  "        y = 1 : u32;\n" ++
  "    }\n" ++
  "    return y;\n" ++
  "}"

spec :: Spec
spec = describe "ConstFolding: error detail (carried value)" $ do
  it "CPE-017 carries the constant and the fixed result (lower limit)" $
    constFoldError unsignedBelowZero `shouldSatisfy` \case
      Just (AnnotatedError (EInvariantComparison 0 _ False) _) -> True
      _ -> False
  it "CPE-017 carries the constant and the fixed result (upper limit, swapped operands)" $
    constFoldError constantAtUpperLimit `shouldSatisfy` \case
      Just (AnnotatedError (EInvariantComparison 255 _ True) _) -> True
      _ -> False
  it "CPE-004 carries the overflowing constant" $
    constFoldError overflow `shouldSatisfy` \case
      Just (AnnotatedError (EConstIntegerOverflow 256 _) _) -> True
      _ -> False
  it "CPE-010 carries the out-of-bounds slice bounds" $
    constFoldError sliceOutOfBounds `shouldSatisfy` \case
      Just (AnnotatedError (EArraySliceOutOfBounds size upper) _) -> size == 4 && upper == 5
      _ -> False
