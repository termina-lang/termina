-- | The verdict of each case from the records, the report of the console and
-- the reading of the output of atos and addr2line.
module Runner.AnalysisSpec (spec) where

import Runner.Analysis
import Runner.Descriptor
import Runner.Record
import Runner.Symbols

import qualified Data.Map.Strict as M
import Test.Hspec

descriptor :: Descriptor
descriptor = Descriptor {
    descriptorVersion = 1, descriptorStamp = 0x1234, descriptorName = "hk", descriptorLevel = "unit",
    descriptorPlatform = "posix-gcc", descriptorTimeout = Just 10,
    descriptorCases = [ DescriptorCase n ("t." ++ c) "t" c "tests/hk/test.fin" (10 * n) [] | (n, c) <- zip [1 ..] ["a", "b", "c", "d"] ],
    descriptorChecks = [DescriptorCheck "tests/hk/test.fin" 25 "check_eq_u32" "self->chk.check_eq_u32(v, 95 : u32)"],
    descriptorTasks = M.fromList [(0, "uut"), (1, "harness")],
    descriptorEmitters = M.empty,
    descriptorSymbols = Symbols "termina__test__header" "termina__test__init" "termina__test__end" "termina__exec__reboot"
  }

failure :: Check
failure = Check 0 0x1000 (Just (Scalars 0 95))

verdicts :: [Record] -> Bool -> [Verdict]
verdicts records killed = map resultVerdict (analyse descriptor records killed)

spec :: Spec
spec = describe "Runner: verdicts and report" $ do

  it "gives passed, failed and not run" $
    verdicts [CaseBegin 1, CaseEnd 1, CaseBegin 2, Failure "check_eq_u32" failure, CaseEnd 2, End] False
      `shouldBe` [Passed, Failed, NotRun, NotRun]

  it "leaves the cases after an exception as not run" $ do
    let info = ExceptionInfo 1 "EArithmeticOverflow" [("address", 0x2000)]
        results = analyse descriptor [CaseBegin 1, CaseEnd 1, CaseBegin 2, Exception info] False
    map resultVerdict results `shouldBe` [Passed, Raised, NotRun, NotRun]
    resultException (results !! 1) `shouldBe` Just info

  it "gives a timeout to the case that was running when the process was killed" $
    verdicts [CaseBegin 1, CaseEnd 2, CaseBegin 2] True `shouldBe` [Passed, TimedOut, NotRun, NotRun]

  it "gives an exception to a case the process ended in without a record of it" $
    verdicts [CaseBegin 1] False `shouldBe` [Raised, NotRun, NotRun, NotRun]

  it "writes the report with the line and the text of a failed check" $ do
    let results = analyse descriptor [CaseBegin 1, CaseEnd 0, CaseBegin 2, Failure "check_eq_u32" failure, CaseEnd 1, End] False
        locations = M.fromList [(0x1000, Just (Location "test.fin" 25))]
    renderReport descriptor locations (TestResult "hk (posix-gcc)" Nothing results False 0) `shouldBe` [
        "hk (posix-gcc)",
        "  ok       t.a    0 checks",
        "           warning: the case ran no checks",
        "  FAIL     t.b    1 check",
        "           tests/hk/test.fin:25  self->chk.check_eq_u32(v, 95 : u32)  expected 95, got 0",
        "  --       t.c    not run",
        "  --       t.d    not run",
        "  1 passed, 1 failed, 2 not run"
      ]

  it "writes an exception with its task and its fields" $ do
    let info = ExceptionInfo 0 "ERuntimeFailure" [("address", 0), ("operation", 10), ("error", -2)]
        results = analyse descriptor [CaseBegin 1, Exception info] False
    take 3 (renderReport descriptor M.empty (TestResult "hk (posix-gcc)" Nothing results False 0)) `shouldBe` [
        "hk (posix-gcc)",
        "  EXCEPT   t.a",
        "           ERuntimeFailure in the task uut: operation 10, error -2"
      ]

  it "exits with 0, 1 or 2" $ do
    let passed = analyse descriptor [ r | n <- [1 .. 4], r <- [CaseBegin n, CaseEnd 1] ] False
    exitCodeOf (TestResult "" Nothing passed False 0) `shouldBe` 0
    exitCodeOf (TestResult "" Nothing (analyse descriptor [] False) False 0) `shouldBe` 1
    exitCodeOf (TestResult "" (Just "stamp") [] False 0) `shouldBe` 2

  it "reads the output of atos and of addr2line" $ do
    atosLocation "CHkFdirTests__high_severity_action (in hk_fdir) (test.fin:228)" `shouldBe` Just (Location "test.fin" 228)
    atosLocation "0x100003f50" `shouldBe` Nothing
    addr2lineLocation "./tests/hk_fdir/test.fin:230" `shouldBe` Just (Location "tests/hk_fdir/test.fin" 230)
    addr2lineLocation "/src/a.fin:12 (discriminator 3)" `shouldBe` Just (Location "/src/a.fin" 12)
    addr2lineLocation "??:0" `shouldBe` Nothing
