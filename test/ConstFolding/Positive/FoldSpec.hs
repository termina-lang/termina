-- | Constant-folding positive tests: well-formed constant expressions fold and
-- the program compiles cleanly (no error from any pipeline stage).
module ConstFolding.Positive.FoldSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec

-- A constexpr-sized array whose fill initializer has the matching size.
matchingArraySize :: String
matchingArraySize =
  "constexpr n : usize = 4;\n" ++
  "function f() {\n" ++
  "    var a : [u8; n] = [0 : u8; n];\n" ++
  "    a[0] = 1 : u8;\n" ++
  "    return;\n" ++
  "}"

-- A constant arithmetic expression that folds without overflow.
foldsArithmetic :: String
foldsArithmetic =
  "function f() -> u32 {\n" ++
  "    return 1 : u32 + 2 : u32;\n" ++
  "}"

-- A slice whose length matches the expected reference size.
matchingSlice :: String
matchingSlice =
  "function take2(_data : &[u8; 2]) {\n    return;\n}\n" ++
  "const lo : usize = 0;\n" ++
  "const hi : usize = 2;\n" ++
  "function f() {\n" ++
  "    var a : [u8; 4] = [0 : u8; 4];\n" ++
  "    take2(&a[lo .. hi]);\n" ++
  "    return;\n" ++
  "}"

-- A for loop with a strictly positive constant iteration count.
positiveLoop :: String
positiveLoop =
  "function f() {\n" ++
  "    var acc : u32 = 0 : u32;\n" ++
  "    for i : usize in 0 : usize .. 4 : usize {\n" ++
  "        acc = acc + 1 : u32;\n" ++
  "    }\n" ++
  "    return;\n" ++
  "}"

spec :: Spec
spec = describe "ConstFolding: well-formed constants compile cleanly" $
  mapM_ (\(name, src) -> it name $ compileErrorCode src `shouldBe` Nothing)
    [ ("accepts a constexpr-sized array with a matching initializer", matchingArraySize)
    , ("accepts a constant arithmetic expression that folds in range", foldsArithmetic)
    , ("accepts a slice whose length matches the expected size", matchingSlice)
    , ("accepts a for loop with a positive iteration count", positiveLoop)
    ]
