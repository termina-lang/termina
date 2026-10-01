module ConstFolding.Negative.ConstEvalSpec (spec) where

import Pipeline.Common (compileErrorCode)
import Architecture.Negative.CodeSpec (timerTaskClass, periodicEmitter)

import Test.Hspec
import Data.Text (pack)

spec :: Spec
spec = do
  describe "ConstFolding: constant-evaluation errors" $ do

    it "CFE-004: constant integer overflow on cast" $ do
      let src = "function f() -> u8 {\n" ++
                "    return 256 : u16 as u8;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-004")

    it "CFE-005: constant integer underflow" $ do
      let src = "function f() -> u8 {\n" ++
                "    return 0 : u8 - 1 : u8;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-005")

    it "CFE-006: constant division by zero" $ do
      let src = "function f() -> u32 {\n" ++
                "    return 1 : u32 / 0 : u32;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-006")

    it "CFE-006: division of a variable by a constant zero" $ do
      let src = "function f(a : i32) -> i32 {\n" ++
                "    return a / 0 : i32;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-006")

    it "CFE-006: remainder of a variable by a constant zero" $ do
      let src = "function f(a : u32) -> u32 {\n" ++
                "    return a % 0 : u32;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-006")

    it "CFE-006: divisor that a truncated quotient makes zero" $ do
      let src = "function f() -> i32 {\n" ++
                "    return 10 : i32 / (-7 : i32 / 2 : i32 + 3 : i32);\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-006")

    it "CFE-004: constant quotient of the minimum of the type by -1" $ do
      let src = "function f() -> i8 {\n" ++
                "    return -128 : i8 / -1 : i8;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-004")

    it "CFE-004: global constant whose initializer overflows" $ do
      let src = "const K : i8 = 2 : i8;\n" ++
                "const L : i8 = K * 100 : i8;\n"
      compileErrorCode src `shouldBe` Just (pack "CFE-004")

    it "CFE-004: element of a constant array that overflows" $ do
      let src = "const K : i8 = 2 : i8;\n" ++
                "const A : [i8; 2] = {K * 100 : i8, 1 : i8};\n"
      compileErrorCode src `shouldBe` Just (pack "CFE-004")

    it "CFE-004: fill value of a constant array that overflows" $ do
      let src = "const K : i8 = 2 : i8;\n" ++
                "const A : [i8; 3] = [K * 100 : i8; 3];\n"
      compileErrorCode src `shouldBe` Just (pack "CFE-004")

    it "CFE-006: global constant whose initializer divides by zero" $ do
      let src = "const K : u32 = 0 : u32;\n" ++
                "const L : u32 = 10 : u32 / K;\n"
      compileErrorCode src `shouldBe` Just (pack "CFE-006")

    it "CFE-010: array index out of bounds with constant index" $ do
      let src = "const bad_idx : usize = 10;\n" ++
                "function f() -> u8 {\n" ++
                "    var a : [u8; 4] = [0; 4];\n" ++
                "    a[bad_idx] = 0 : u8;\n" ++
                "    return a[0];\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-010")

    it "CFE-013: shift amount greater than or equal to the type width" $ do
      let src = "function f() -> u8 {\n" ++
                "    var x : u8 = 0 : u8;\n" ++
                "    x = x << 8 : usize;\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "CFE-013")

    it "CFE-014: task priority reserved for the runtime" $ do
      let src = timerTaskClass
             ++ periodicEmitter "timer" 1
             ++ "#[priority(0)]\n"
             ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
      compileErrorCode src `shouldBe` Just (pack "CFE-014")

    it "CFE-014: task priority out of range after folding a constant" $ do
      let src = "constexpr base : u32 = 250 : u32;\n"
             ++ timerTaskClass
             ++ periodicEmitter "timer" 1
             ++ "#[priority(base + 5 : u32)]\n"
             ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
      compileErrorCode src `shouldBe` Just (pack "CFE-014")

    -- | The tick is 10000 microseconds unless the project says otherwise.
    it "CFE-015: timer period that is not a whole number of ticks" $ do
      let src = timerTaskClass
             ++ "emitter timer : PeriodicTimer = { period = {tv_sec = 0, tv_usec = 15000} };\n"
             ++ "#[priority(1)]\n"
             ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
      compileErrorCode src `shouldBe` Just (pack "CFE-015")

    it "CFE-015: timer period of zero" $ do
      let src = timerTaskClass
             ++ "emitter timer : PeriodicTimer = { period = {tv_sec = 0, tv_usec = 0} };\n"
             ++ "#[priority(1)]\n"
             ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
      compileErrorCode src `shouldBe` Just (pack "CFE-015")

    -- | 50000000 s are 5000000000 ticks of 10000 microseconds, above the
    -- 32-bit count the test platform admits.
    it "CFE-016: timer period longer than the platform admits" $ do
      let src = timerTaskClass
             ++ "emitter timer : PeriodicTimer = { period = {tv_sec = 50000000, tv_usec = 0} };\n"
             ++ "#[priority(1)]\n"
             ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
      compileErrorCode src `shouldBe` Just (pack "CFE-016")

    it "CFE-015: timer period off the tick after folding a constant" $ do
      let src = "constexpr half_tick : u32 = 5000 : u32;\n"
             ++ timerTaskClass
             ++ "emitter timer : PeriodicTimer = { period = {tv_sec = 1, tv_usec = half_tick} };\n"
             ++ "#[priority(1)]\n"
             ++ "task t : TimerTask = { ticks = 0, timer_port <- timer };\n"
      compileErrorCode src `shouldBe` Just (pack "CFE-015")
