-- | Constant-folding positive tests: well-formed constant expressions fold and
-- the program compiles cleanly (no error from any pipeline stage).
module ConstFolding.Positive.FoldSpec (spec) where

import Pipeline.Common (compileErrorCode, runFullBuild)
import Data.Text (isInfixOf, pack)
import Architecture.Negative.CodeSpec (timerTaskClass)

import Test.Hspec

-- A constexpr-sized array whose fill initializer has the matching size.
matchingArraySize :: String
matchingArraySize =
  "constexpr n : usize = 4;\n" ++
  "function f() -> u8 {\n" ++
  "    var a : [u8; n] = [0 : u8; n];\n" ++
  "    a[0] = 1 : u8;\n" ++
  "    return a[0];\n" ++
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

-- Comparisons against constants strictly inside the range of the type.
comparisonsInRange :: String
comparisonsInRange =
  "function f(x : u8) -> u32 {\n" ++
  "    var y : u32 = 0 : u32;\n" ++
  "    if ((x > 0 : u8) && (255 : u8 > x)) {\n" ++
  "        y = 1 : u32;\n" ++
  "    }\n" ++
  "    return y;\n" ++
  "}"

-- | A global constant computed from another, which C only admits as the
-- initializer of a static object once it is a literal.
derivedConstant :: String
derivedConstant =
  "const K : i8 = 1 : i8;\n" ++
  "const L : i8 = K * 100 : i8;\n"

-- | The same, for the elements of a constant array.
derivedArray :: String
derivedArray =
  "const K : i8 = 2 : i8;\n" ++
  "const A : [i8; 2] = {K * 10 : i8, 1 : i8};\n"

spec :: Spec
spec = do
  it "writes a global constant computed from another as a literal" $
    runFullBuild derivedConstant `shouldSatisfy` isInfixOf (pack "const int8_t L = 100L;")
  it "writes the elements of a constant array computed from a constant as literals" $
    runFullBuild derivedArray `shouldSatisfy` isInfixOf (pack "const int8_t A[2U] = { 20L, 1L };")
  describe "ConstFolding: well-formed constants compile cleanly" $
    mapM_ (\(name, src) -> it name $ compileErrorCode src `shouldBe` Nothing)
    [ ("accepts a constexpr-sized array with a matching initializer", matchingArraySize)
    , ("accepts a constant arithmetic expression that folds in range", foldsArithmetic)
    , ("accepts a slice whose length matches the expected size", matchingSlice)
    , ("accepts a for loop with a positive iteration count", positiveLoop)
    , ("accepts comparisons against constants inside the type range", comparisonsInRange)
    , ("accepts a timer period of a whole number of ticks", periodInTicks)
    ]

-- | A period of 1.02 s, which the tick of 10000 microseconds divides, written
-- with a constant for the microseconds.
periodInTicks :: String
periodInTicks =
  "constexpr two_ticks : u32 = 20000 : u32;\n" ++
  timerTaskClass ++
  "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = two_ticks} };\n" ++
  "#[priority(1)]\n" ++
  "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
