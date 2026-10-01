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

-- | Aggregate constants: a struct, an enumeration and an option computed from
-- a constant, an array of structs that names another constant, and a copy of
-- another constant.
aggregateConstants :: String
aggregateConstants =
  "struct Point {\n" ++
  "    x : u32;\n" ++
  "    y : u32;\n" ++
  "};\n" ++
  "enum Mode {\n" ++
  "    Off,\n" ++
  "    On(u8, Point)\n" ++
  "};\n" ++
  "const K : u32 = 3 : u32;\n" ++
  "const P : Point = {x = K * 2 : u32, y = 1 : u32};\n" ++
  "const M : Mode = Mode::On(K as u8 + 1 : u8, {x = 0 : u32, y = K});\n" ++
  "const O : Option<u32> = Some(K + 1 : u32);\n" ++
  "const PS : [Point; 2] = {P, {x = 4 : u32, y = 5 : u32}};\n" ++
  "const Q : Point = P;\n" ++
  "function f() -> u32 {\n" ++
  "    var r : u32 = P.x + Q.y;\n" ++
  "    match M {\n" ++
  "        case On(n, p) => {\n" ++
  "            r = r + p.y + n as u32;\n" ++
  "        }\n" ++
  "        case Off => {\n" ++
  "        }\n" ++
  "    }\n" ++
  "    return r + PS[1].x;\n" ++
  "}"

-- | A floating-point constant that copies another, which the folding leaves to
-- the C compiler but has to name by its value.
copiedFloat :: String
copiedFloat =
  "const F : f32 = 1.5 : f32;\n" ++
  "const G : f32 = F;\n"

-- | Loop bounds read from a field or an element of a constant, and from the
-- name of a scalar constant, which the generated code writes as literals.
constantPathBounds :: String
constantPathBounds =
  "struct Config {\n" ++
  "    len : usize;\n" ++
  "};\n" ++
  "const CFG : Config = {len = 4 : usize};\n" ++
  "const A : [usize; 2] = {5 : usize, CFG.len * 2 : usize};\n" ++
  "const N : usize = 6 : usize;\n" ++
  "function f() -> usize {\n" ++
  "    var r : usize = 0 : usize;\n" ++
  "    for j : usize in 0 : usize .. A[1] {\n" ++
  "        r = r + j;\n" ++
  "    }\n" ++
  "    for k : usize in CFG.len .. N {\n" ++
  "        r = r + k;\n" ++
  "    }\n" ++
  "    return r;\n" ++
  "}"

-- | A parameter that refers to an array sized with a constant.
constantSizedParameter :: String
constantSizedParameter =
  "const N : usize = 4;\n" ++
  "function first(a : &[u8; N]) -> u8 {\n" ++
  "    return (*a)[0];\n" ++
  "}"

spec :: Spec
spec = do
  it "writes the size of an array behind a reference parameter as a literal" $
    runFullBuild constantSizedParameter `shouldSatisfy` isInfixOf (pack "uint8_t first(const uint8_t a[4U])")
  it "writes an element of a constant in a loop bound as a literal" $
    runFullBuild constantPathBounds `shouldSatisfy` isInfixOf (pack "for (size_t j = 0U; j < 8U;")
  it "writes a field and the name of a constant in a loop bound as literals" $
    runFullBuild constantPathBounds `shouldSatisfy` isInfixOf (pack "for (size_t k = 4U; k < 6U;")
  it "writes a floating-point constant that copies another as a literal" $
    runFullBuild copiedFloat `shouldSatisfy` isInfixOf (pack "const float32_t G = 1.5f;")
  describe "writes aggregate constants as initializers of literals" $
    mapM_ (\(name, line) -> it name $
      runFullBuild aggregateConstants `shouldSatisfy` isInfixOf (pack line))
    [ ("a struct", "const Point P = { .x = 6U, .y = 1U };")
    , ("an enumeration", "const Mode M = { ._variant = Mode__On, .On = { ._0 = 4U, ._1 = { .x = 0U,")
    , ("an option", "const Option__u32 O = { ._variant = Option__Some, .Some = { ._0 = 4U } };")
    , ("an array of structs that names another constant", "const Point PS[2U] = { { .x = 6U, .y = 1U }, { .x = 4U, .y = 5U } };")
    , ("a copy of another constant", "const Point Q = { .x = 6U, .y = 1U };")
    ]
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
