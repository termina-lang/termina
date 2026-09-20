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

    -- | The values a function gives back are what the walk of its body found,
    -- and a call site reads them: neither branch of the callee is a constant
    -- the folding could reach.
    it "VAE-001: condition the values a function gives back decide" $ do
      let src = "function pick(b : bool) -> u32 {\n" ++
                "    var r : u32 = 1 : u32;\n" ++
                "    if (b) {\n" ++
                "        r = 2 : u32;\n" ++
                "    }\n" ++
                "    return r;\n" ++
                "}\n" ++
                "function f(b : bool) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    var v : u32 = pick(b);\n" ++
                "    if (v < 5 : u32) {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | A member is filed under its class as well as its name, so a call
    -- through self reaches it.
    it "VAE-001: condition the values a member gives back decide" $ do
      let src = "interface Iface0 {\n" ++
                "    procedure proc0(&mut self);\n" ++
                "};\n" ++
                "resource class Class0 provides Iface0 {\n" ++
                "    base : u32;\n" ++
                "    method pick(&self) -> u32 {\n" ++
                "        var r : u32 = 1 : u32;\n" ++
                "        if (self->base == 0 : u32) {\n" ++
                "            r = 2 : u32;\n" ++
                "        }\n" ++
                "        return r;\n" ++
                "    }\n" ++
                "    procedure proc0(&mut self) {\n" ++
                "        var y : u32 = 0 : u32;\n" ++
                "        var v : u32 = self->pick();\n" ++
                "        if (v < 5 : u32) {\n" ++
                "            y = 1 : u32;\n" ++
                "        }\n" ++
                "        self->base = y;\n" ++
                "        return;\n" ++
                "    }\n" ++
                "};"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | Neither branch leaves the enumeration at the variant the condition
    -- asks about, and a variant is a value of the lattice like a number is.
    it "VAE-001: variant test no assignment of the enumeration reaches" $ do
      let src = "enum State { Init, Running, Exit };\n" ++
                "function f(n : u32) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    var s : State = State::Init;\n" ++
                "    if (n == 1 : u32) {\n" ++
                "        s = State::Running;\n" ++
                "    }\n" ++
                "    if (s is State::Exit) {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | The case of a match says which variant its discriminant holds inside
    -- the body, which is how a state machine is followed from one turn of its
    -- loop to the next.
    it "VAE-001: variant test the case it sits in has already decided" $ do
      let src = "enum State { Init, Running, Exit };\n" ++
                "function f(s : State) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    match s {\n" ++
                "        case Init => {\n" ++
                "            if (s is State::Exit) {\n" ++
                "                y = 1 : u32;\n" ++
                "            }\n" ++
                "        }\n" ++
                "        case Running => {\n" ++
                "            y = 2 : u32;\n" ++
                "        }\n" ++
                "        case Exit => {\n" ++
                "            y = 3 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | An option, a status and a result are a different shape in the AST
    -- from an enumeration and the same thing here: one name out of a closed
    -- set, and the same tag in the generated C.
    it "VAE-001: variant test of an option no path can satisfy" $ do
      let src = "function f(n : u32) -> u32 {\n" ++
                "    var y : u32;\n" ++
                "    var opt : Option<u32>;\n" ++
                "    if (n == 1 : u32) {\n" ++
                "        opt = Some(3 : u32);\n" ++
                "    } else {\n" ++
                "        opt = Some(4 : u32);\n" ++
                "    }\n" ++
                "    if (opt is None) {\n" ++
                "        y = 7 : u32;\n" ++
                "    } else {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-001")

    -- | The walk of the callee found three of the four variants, so the
    -- fourth case of the match is a body that never runs. The match has to
    -- list it, since a match is exhaustive over the type, which is what says
    -- the type is wider than the values it ever takes.
    it "VAE-002: case of a variant the discriminant never holds" $ do
      let src = "enum Range { Above, Below, Within, Unchecked };\n" ++
                "function check(n : u32) -> Range {\n" ++
                "    var s : Range;\n" ++
                "    if (n == 1 : u32) {\n" ++
                "        s = Range::Above;\n" ++
                "    } else if (n == 2 : u32) {\n" ++
                "        s = Range::Below;\n" ++
                "    } else {\n" ++
                "        s = Range::Within;\n" ++
                "    }\n" ++
                "    return s;\n" ++
                "}\n" ++
                "function f(n : u32) -> u32 {\n" ++
                "    var y : u32;\n" ++
                "    var s : Range = check(n);\n" ++
                "    match s {\n" ++
                "        case Above => {\n" ++
                "            y = 1 : u32;\n" ++
                "        }\n" ++
                "        case Below => {\n" ++
                "            y = 2 : u32;\n" ++
                "        }\n" ++
                "        case Within => {\n" ++
                "            y = 3 : u32;\n" ++
                "        }\n" ++
                "        case Unchecked => {\n" ++
                "            y = 4 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Just (pack "VAE-002")

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
