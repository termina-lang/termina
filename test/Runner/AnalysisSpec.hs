-- | The verdict of each case from the records, the report of the console, the
-- files of the result and the reading of the output of atos and addr2line.
module Runner.AnalysisSpec (spec) where

import Configuration.Platform (Platform(..), callSite)
import Runner.Analysis
import Runner.Descriptor
import Runner.Record
import Runner.Results
import Runner.Symbols

import Data.Aeson (Value(..), toJSON)
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (lookup)
import Data.Foldable (toList)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)
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

-- | A run of every case of the descriptor, whose last step is 7
runOf :: [Record] -> Bool -> Run
runOf records killed = Run (descriptorCases descriptor) records killed False 7 []

verdicts :: [Record] -> Bool -> [Verdict]
verdicts records killed = map resultVerdict (analyse (runOf records killed))

-- | The value at a path of keys of objects and positions of arrays
at :: Value -> [Either Int String] -> Maybe Value
at value path =
    case (path, value) of
        ([], _) -> Just value
        (Right key : rest, Object o) -> lookup (fromString key) o >>= (`at` rest)
        (Left i : rest, Array a) ->
            case drop i (toList a) of
                (v : _) -> at v rest
                [] -> Nothing
        _ -> Nothing

evidence :: Evidence
evidence = Evidence "hk" "posix-gcc" "linux" "debug" "all" "process" (Just 10) (Just ("773e607", False)) (Just "ab") Nothing

spec :: Spec
spec = describe "Runner: verdicts and report" $ do

  it "gives passed, failed and not run" $
    verdicts [CaseBegin 1, CaseEnd 1, CaseBegin 2, Failure "check_eq_u32" failure, CaseEnd 2, End] False
      `shouldBe` [Passed, Failed, NotRun, NotRun]

  it "leaves the cases after an exception as not run" $ do
    let info = ExceptionInfo 1 "EArithmeticOverflow" [("address", 0x2000)]
        results = analyse (runOf [CaseBegin 1, CaseEnd 1, CaseBegin 2, Exception info] False)
    map resultVerdict results `shouldBe` [Passed, Raised, NotRun, NotRun]
    resultException (results !! 1) `shouldBe` Just info

  it "gives a timeout and the last step to the case that was running when the process was killed" $ do
    let results = analyse (runOf [CaseBegin 1, CaseEnd 2, CaseBegin 2] True)
    map resultVerdict results `shouldBe` [Passed, TimedOut, NotRun, NotRun]
    map resultLastStep results `shouldBe` [Nothing, Just 7, Nothing, Nothing]

  it "gives an exception to a case the process ended in without a record of it" $
    verdicts [CaseBegin 1] False `shouldBe` [Raised, NotRun, NotRun, NotRun]

  it "leaves the cases beyond an overflow of the record without a record" $
    map resultVerdict (analyse (Run (descriptorCases descriptor) [CaseBegin 1, CaseEnd 1, CaseBegin 2] False True 0 []))
      `shouldBe` [Passed, Unrecorded, Unrecorded, Unrecorded]

  it "crosses the records only with the cases of the run" $
    map resultVerdict (analyse (Run [descriptorCases descriptor !! 2] [CaseBegin 3, CaseEnd 1, End] False False 0 []))
      `shouldBe` [Passed]

  it "subtracts the load offset from the addresses of a record" $ do
    relocate 0x100 (Failure "check" (Check 0 0x1100 Nothing)) `shouldBe` Failure "check" (Check 0 0x1000 Nothing)
    relocate 0x100 (Exception (ExceptionInfo 0 "ERuntimeFailure" [("address", 0), ("operation", 10), ("error", -2)]))
      `shouldBe` Exception (ExceptionInfo 0 "ERuntimeFailure" [("address", 0), ("operation", 10), ("error", -2)])

  it "writes the report with the line and the text of a failed check" $ do
    let results = analyse (runOf [CaseBegin 1, CaseEnd 0, CaseBegin 2, Failure "check_eq_u32" failure, CaseEnd 1, End] False)
        locations = M.fromList [(0x1000, Just (Location "test.fin" 25))]
    renderReport descriptor locations (TestResult "hk (posix-gcc)" Nothing results Nothing) `shouldBe` [
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
        results = analyse (runOf [CaseBegin 1, Exception info] False)
    take 3 (renderReport descriptor M.empty (TestResult "hk (posix-gcc)" Nothing results Nothing)) `shouldBe` [
        "hk (posix-gcc)",
        "  EXCEPT   t.a",
        "           ERuntimeFailure in the task uut: operation 10, error -2"
      ]

  it "exits with 0, 1 or 2" $ do
    let passed = analyse (runOf [ r | n <- [1 .. 4], r <- [CaseBegin n, CaseEnd 1] ] False)
    exitCodeOf (TestResult "" Nothing passed Nothing) `shouldBe` 0
    exitCodeOf (TestResult "" Nothing (analyse (runOf [] False)) Nothing) `shouldBe` 1
    exitCodeOf (TestResult "" (Just "stamp") [] Nothing) `shouldBe` 2

  it "writes the values of a failure and its location into results.json" $ do
    let results = analyse (runOf [CaseBegin 1, Failure "check_eq_u32" failure, CaseEnd 1, End] False)
        locations = M.fromList [(0x1000, Just (Location "test.fin" 25))]
        json = resultsJson evidence (Just descriptor) locations (TestResult "hk (posix-gcc)" Nothing results Nothing)
        failed = [Right "cases", Left 0, Right "failures", Left 0]
    at json [Right "verdict"] `shouldBe` Just (toJSON ("failed" :: String))
    at json [Right "run", Right "executor"] `shouldBe` Just (toJSON ("process" :: String))
    at json [Right "cases", Left 0, Right "verdict"] `shouldBe` Just (toJSON ("failed" :: String))
    at json (failed ++ [Right "line"]) `shouldBe` Just (toJSON (25 :: Int))
    at json (failed ++ [Right "expected"]) `shouldBe` Just (toJSON ("95" :: String))
    at json (failed ++ [Right "obtained"]) `shouldBe` Just (toJSON ("0" :: String))

  it "writes a failure, an error and a skipped case into report.xml" $ do
    let info = ExceptionInfo 0 "EDivisionByZero" [("address", 0)]
        results = analyse (runOf [CaseBegin 1, CaseEnd 1, CaseBegin 2, Failure "check_eq_u32" failure, CaseEnd 1, CaseBegin 3, Exception info] False)
        xml = renderJUnit evidence (Just descriptor) M.empty (TestResult "hk (posix-gcc)" Nothing results Nothing)
    xml `shouldSatisfy` isInfixOf "<testsuite name=\"hk (posix-gcc)\" tests=\"4\" failures=\"1\" errors=\"1\" skipped=\"1\">"
    xml `shouldSatisfy` isInfixOf "<testcase classname=\"hk.t\" name=\"a\" file=\"tests/hk/test.fin\" line=\"10\"/>"
    xml `shouldSatisfy` isInfixOf "<failure type=\"check_eq_u32\" message=\"0x1000  check_eq_u32  expected 95, got 0\">"
    xml `shouldSatisfy` isInfixOf "<error type=\"EDivisionByZero\""
    xml `shouldSatisfy` isInfixOf "<skipped/>"

  it "takes the program counter of a processor fault as it is and every other address as a return address" $ do
    let fault = ExceptionInfo 0 "EProcessorFault" [("exception", 4), ("status", 1), ("address", 0x104476), ("fault_address", 0xa5a5a5b5)]
        overflow = ExceptionInfo 0 "EArithmeticOverflow" [("address", 0x2001)]
    addressesOf (analyse (runOf [CaseBegin 1, Failure "check_eq_u32" failure, CaseEnd 1, CaseBegin 2, Exception fault] False))
      `shouldBe` [(0x1000, ReturnAddress), (0x104476, FaultPc)]
    addressesOf (analyse (runOf [CaseBegin 1, Exception overflow] False)) `shouldBe` [(0x2001, ReturnAddress)]

  it "finds the call of a return address on each platform" $ do
    -- the blx at 0x105ab4 of the PYNQ, whose lr is 0x105ab7
    callSite RTEMS6ZYNQ7000PYNQZ2 0x105ab7 `shouldBe` 0x105ab5
    callSite FreeRTOS10STM32L432NUCLEOL432KC 0x8001235 `shouldBe` 0x8001233
    -- %o7 on SPARC is the address of the call
    callSite RTEMS5LEON3SFNEXYSA7 0x40001000 `shouldBe` 0x40001000
    callSite POSIXGCC 0x100003f55 `shouldBe` 0x100003f54

  it "reads the output of atos and of addr2line" $ do
    atosLocation "CHkFdirTests__high_severity_action (in hk_fdir) (test.fin:228)" `shouldBe` Just (Location "test.fin" 228)
    atosLocation "0x100003f50" `shouldBe` Nothing
    addr2lineLocation "./tests/hk_fdir/test.fin:230" `shouldBe` Just (Location "tests/hk_fdir/test.fin" 230)
    addr2lineLocation "/src/a.fin:12 (discriminator 3)" `shouldBe` Just (Location "/src/a.fin" 12)
    addr2lineLocation "??:0" `shouldBe` Nothing
