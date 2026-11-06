module SMTBounds.SMTBoundsSpec (spec) where

import Analysis.SMTBounds (smtProjectProofs)
import Configuration.Platform (Platform(..))
import Pipeline.Common

import qualified Data.Map.Strict as M
import Test.Hspec

-- | An index and a division whose operands no other prover bounds.
unbounded :: String
unbounded =
    "function read(buf : &[u32; 8], i : usize) -> u32 {\n" ++
    "    return buf[i];\n" ++
    "}\n" ++
    "\n" ++
    "function divide(y : u32, x : u32) -> u32 {\n" ++
    "    return y / x;\n" ++
    "}\n"

spec :: Spec
spec = describe "SMT prover" $
  it "gives an empty map of proofs for each module" $
    case runFoldedProject [("test", unbounded)] of
      Left _ -> expectationFailure "the program does not build"
      Right folded -> do
        proofs <- smtProjectProofs TestPlatform folded
        M.keys proofs `shouldBe` M.keys folded
        proofs `shouldSatisfy` all M.null
