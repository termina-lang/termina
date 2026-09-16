-- | Value analysis positive tests: a condition the pass cannot show to be
-- invariant raises no error.
--
-- These are the cases that decide whether the pass is usable at all, since
-- VAE-001 stops a build: a value the walk of a loop has not settled yet, a
-- variable two branches disagree on, one that a call may have written behind
-- the pass's back, and one whose value comes from outside the body.
module ValueAnalysis.Positive.ValuesSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec

spec :: Spec
spec = do
  describe "ValueAnalysis: a condition that is not invariant raises no error" $ do

    it "accepts a condition that only the first turn of a loop decides" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize {\n" ++
                "        if (x == 0 : u32) {\n" ++
                "            y = y + 1 : u32;\n" ++
                "        }\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "accepts a break condition the body of the loop falsifies" $ do
      let src = "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var go : bool = true;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize while (go) {\n" ++
                "        x = x + 1 : u32;\n" ++
                "        go = false;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "accepts a variable the two paths before it disagree on" $ do
      let src = "function f(flag : bool) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    var x : u32 = 1 : u32;\n" ++
                "    if (flag) {\n" ++
                "        x = 2 : u32;\n" ++
                "    }\n" ++
                "    if (x == 2 : u32) {\n" ++
                "        y = y + 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "accepts a variable a call may have written through a mutable reference" $ do
      let src = "function g(dest : &mut u32) {\n" ++
                "    *dest = 1 : u32;\n" ++
                "    return;\n" ++
                "}\n" ++
                "function f() -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    g(&mut x);\n" ++
                "    if (x == 0 : u32) {\n" ++
                "        y = y + 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "accepts a condition on a value that comes from a parameter" $ do
      let src = "function f(n : u32) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (n == 0 : u32) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "accepts a variable a loop leaves at a value it did not start from" $ do
      let src = "function f() -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize {\n" ++
                "        x = x + 1 : u32;\n" ++
                "    }\n" ++
                "    if (x == 0 : u32) {\n" ++
                "        y = y + 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    it "accepts a comparison the branch it is nested in does not decide" $ do
      let src = "function f(n : u32) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (n == 3 : u32) {\n" ++
                "        x = 1 : u32;\n" ++
                "    } else {\n" ++
                "        if (n == 4 : u32) {\n" ++
                "            x = 2 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return x;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    -- | Every case of the match is a variant the callee gives back, so the
    -- match says no more than the type does.
    it "accepts a match every case of which the discriminant may hold" $ do
      let src = "enum Status { Above, Below, Within };\n" ++
                "function check(n : u32) -> Status {\n" ++
                "    var s : Status;\n" ++
                "    if (n == 1 : u32) {\n" ++
                "        s = Status::Above;\n" ++
                "    } else if (n == 2 : u32) {\n" ++
                "        s = Status::Below;\n" ++
                "    } else {\n" ++
                "        s = Status::Within;\n" ++
                "    }\n" ++
                "    return s;\n" ++
                "}\n" ++
                "function f(n : u32) -> u32 {\n" ++
                "    var y : u32;\n" ++
                "    var s : Status = check(n);\n" ++
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
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    -- | One of the two branches leaves the enumeration at the variant the
    -- condition asks about and the other does not.
    it "accepts a variant test only some paths decide" $ do
      let src = "enum State { Init, Running, Exit };\n" ++
                "function f(n : u32) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    var s : State = State::Init;\n" ++
                "    if (n == 1 : u32) {\n" ++
                "        s = State::Exit;\n" ++
                "    }\n" ++
                "    if (s is State::Exit) {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    -- | The body of the callee gives back a parameter, which the walk knows
    -- nothing about, so the function is left without values and the call says
    -- nothing. A summary that missed one of the values a function gives back
    -- would have the call site claim more than the callee does.
    it "accepts a condition on a call whose value the callee does not fix" $ do
      let src = "function passthrough(n : u32) -> u32 {\n" ++
                "    return n;\n" ++
                "}\n" ++
                "function f(n : u32) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (passthrough(n) < 5 : u32) {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    -- | The two values the callee gives back decide the comparison in opposite
    -- ways.
    it "accepts a condition only some of the values a call gives back decide" $ do
      let src = "function pick(b : bool) -> u32 {\n" ++
                "    var r : u32 = 1 : u32;\n" ++
                "    if (b) {\n" ++
                "        r = 2 : u32;\n" ++
                "    }\n" ++
                "    return r;\n" ++
                "}\n" ++
                "function f(b : bool) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (pick(b) < 2 : u32) {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    -- | The walk of the pass never sees the increment of a loop, which lives
    -- only in the generated C, so an iterator seeded with the value it starts
    -- at would keep that value for ever and this condition would be reported.
    -- It is seeded with the whole range instead, and this pins that.
    it "accepts a condition on the value a loop iterator starts at" $ do
      let src = "function f() -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize {\n" ++
                "        if (i == 0 : usize) {\n" ++
                "            y = y + 1 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    -- | The condition holds on the first two turns and fails on the other two.
    it "accepts a condition only some turns of a loop decide" $ do
      let src = "function f() -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    for i : usize in 0 : usize .. 4 : usize {\n" ++
                "        if (i < 2 : usize) {\n" ++
                "            y = y + 1 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    -- | From a disjunction that holds, all that follows is that one of its two
    -- halves does, and which one is not known. Learning either of them would
    -- be a finding on correct code, so this pins the silence.
    it "accepts a condition under a disjunction, which bounds nothing" $ do
      let src = "function f(x : u32) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (x > 5 : u32 || x < 3 : u32) {\n" ++
                "        if (x < 30 : u32) {\n" ++
                "            y = 1 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    -- | The mirror of the case above: from a conjunction that fails, all that
    -- follows is that one of its halves does, so the else learns nothing.
    it "accepts a condition in the else of a conjunction, which bounds nothing" $ do
      let src = "function f(x : u32) -> u32 {\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (x > 0 : u32 && x < 20 : u32) {\n" ++
                "        y = 1 : u32;\n" ++
                "    } else {\n" ++
                "        if (x < 30 : u32) {\n" ++
                "            y = 2 : u32;\n" ++
                "        }\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing

    -- | The two values the paths leave decide the comparison in opposite ways,
    -- which is the case the abstract evaluator has to leave alone.
    it "accepts a comparison only some of the values a variable may hold decide" $ do
      let src = "function f(flag : bool) -> u32 {\n" ++
                "    var x : u32 = 0 : u32;\n" ++
                "    if (flag) {\n" ++
                "        x = 1 : u32;\n" ++
                "    }\n" ++
                "    var y : u32 = 0 : u32;\n" ++
                "    if (x < 1 : u32) {\n" ++
                "        y = 1 : u32;\n" ++
                "    }\n" ++
                "    return y;\n" ++
                "}"
      compileErrorCode src `shouldBe` Nothing
