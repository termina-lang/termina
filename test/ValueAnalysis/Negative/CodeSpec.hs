-- | Value analysis negative tests: a condition whose value is the same every
-- time it is evaluated (VAE-001), whichever of the four sources the value
-- comes from. The last case pins which finding a body with more than one of
-- them reports, since the user resolves them one at a time.
module ValueAnalysis.Negative.CodeSpec (spec) where

import Pipeline.Common (compileErrorCode, compileErrorMessage)

import Test.Hspec
import Data.Text (isInfixOf, pack)

spec :: Spec
spec = do
  describe "ValueAnalysis: invariant control expressions" $ do

    it "VAE-001: condition built from literals alone" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (1 : u32 == 1 : u32) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    it "VAE-001: condition that reads a constant of the module" $ do
      let src = "const enabled : bool = true;\n" ++
                "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (enabled) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    it "VAE-001: condition that reads a local whose value is known" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var flag : bool = true;\n" ++
                "    if (flag) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    it "VAE-001: comparison of a local against the value it holds" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (x == 0 : u32) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    it "VAE-001: condition the branch it is nested in has already decided" $ do
      let src = "function f(flag : bool) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (flag) {\n" ++
                "        if (flag) {\n" ++
                "            x = 1 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    it "VAE-001: comparison refined by the branch it is nested in" $ do
      let src = "function f(n : u32) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (n == 3 : u32) {\n" ++
                "        if (n == 3 : u32) {\n" ++
                "            x = 1 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    it "VAE-001: break condition of a loop that nothing in it changes" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var go : bool = true;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize while (go) {\n" ++
                "        x = x + 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    it "VAE-001: condition of an else-if that reads a constant" $ do
      let src = "const enabled : bool = false;\n" ++
                "function f(flag : bool) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (flag) {\n" ++
                "        x = 1 : u32;\n" ++
                "    } else if (enabled) {\n" ++
                "        x = 2 : u32;\n" ++
                "    } else {\n" ++
                "        x = x + 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | The comparison is not decided by a single value but by every value the
    -- paths leave, which is what the abstract evaluator adds to the evaluator
    -- of the folding.
    it "VAE-001: comparison every value a variable may hold decides alike" $ do
      let src = "function f(flag : bool) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (flag) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (x < 5 : u32) {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | The values a variable may hold and the one it is compared against have
    -- nothing in common, so the equality is false whichever of them it takes.
    it "VAE-001: equality against a value the variable can never take" $ do
      let src = "function f(flag : bool) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (flag) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (x == 7 : u32) {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | The loop runs its iterator over four values and the condition holds
    -- for all four, which no assignment in the body says.
    it "VAE-001: condition the range of a loop iterator decides" $ do
      let src = "function f() -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize {\n" ++
                "        if (i < 10 : usize) {\n" ++
                "            y = y + 1 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | A loop of more turns than the limit gives its iterator an interval
    -- instead of a set, which is the only way a source program reaches the
    -- verdict that reads the ends of the two operands.
    it "VAE-001: condition the range of a long loop decides" $ do
      let src = "function f() -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    for i : usize in 0 : usize .. 1000 : usize {\n" ++
                "        if (i < 2000 : usize) {\n" ++
                "            y = y + 1 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | Neither half of the guard pins the parameter to a value, but the two
    -- together bound it, and the bound decides the condition inside.
    it "VAE-001: condition the range its guard leaves already decides" $ do
      let src = "function f(x : u32) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (x > 0 : u32 && x < 20 : u32) {\n" ++
                "        if (x < 30 : u32) {\n" ++
                "            y = 1 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | An order comparison teaches on both sides of the branch, so the else
    -- knows the bound the condition rules out.
    it "VAE-001: condition the else of a comparison already decides" $ do
      let src = "function f(x : u32) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (x < 20 : u32) {\n" ++
                "        y = 1 : u32;\n" ++
                "    } else {\n" ++
                "        if (x >= 20 : u32) {\n" ++
                "            y = 2 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | Which side of the comparison the variable is on is not fixed by the
    -- syntax, so the wiring has to read the operands whichever way round the
    -- source writes them.
    it "VAE-001: comparison with the variable on the right" $ do
      let src = "function f(flag : bool) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (flag) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (5 : u32 > x) {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | Two invariant conditions in the same body: the one the source reaches
    -- first is the one reported, so the message names line 5 and the value it
    -- evaluates to there. The second one, on line 8, waits its turn.
    it "VAE-001: the first of two findings is the one reported" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var yes : bool = true;\n" ++
                "    var no : bool = false;\n" ++
                "    if (yes) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    if (no) {\n" ++
                "        x = 2 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      let message = compileErrorMessage src
      fmap (pack "test:5:9" `isInfixOf`) message `shouldBe` Just True
      fmap (pack "yes takes that value here" `isInfixOf`) message `shouldBe` Just True
