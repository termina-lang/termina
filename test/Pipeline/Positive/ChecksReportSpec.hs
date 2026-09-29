module Pipeline.Positive.ChecksReportSpec (spec) where

import Pipeline.Common
import Golden

import Test.Hspec

-- | One obligation of each kind of outcome: an index discharged by the
-- constant prover and one by the guard prover, an index, a divisor and a signed
-- addition that keep their checks, and a shift by a constant.
testChecks :: String
testChecks =
    "function first(buf : &[u32; 4]) -> u32 {\n" ++
    "    return buf[0 : usize];\n" ++
    "}\n" ++
    "\n" ++
    "function guarded(buf : &[u32; 4], i : usize) -> bool {\n" ++
    "    return i < 4 : usize && buf[i] > 0 : u32;\n" ++
    "}\n" ++
    "\n" ++
    "function unguarded(buf : &[u32; 4], i : usize) -> bool {\n" ++
    "    return buf[i] > 0 : u32;\n" ++
    "}\n" ++
    "\n" ++
    "function divided(y : u32, x : u32) -> u32 {\n" ++
    "    return y / x;\n" ++
    "}\n" ++
    "\n" ++
    "function shifted(v : u32) -> u32 {\n" ++
    "    return v << 2 : u32;\n" ++
    "}\n" ++
    "\n" ++
    "function added(a : i32, b : i32) -> i32 {\n" ++
    "    return a + b;\n" ++
    "}\n"

spec :: Spec
spec = do
  describe "Report of the run-time checks" $ do
    it "Lists each obligation with its outcome" $
      goldenJSON "checks_report" (runChecksReport testChecks)
