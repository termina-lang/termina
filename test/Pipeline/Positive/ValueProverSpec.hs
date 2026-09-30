module Pipeline.Positive.ValueProverSpec (spec) where

import Pipeline.Common
import Golden

import qualified Data.Set as S
import Data.Text (unpack)
import Test.Hspec

-- | Every shape the guard prover discharges: an index, a divisor and a shift
-- kept in range by the left operand of @&&@ or @||@, against a literal and
-- against the constant that sizes the array.
guardedShapes :: String
guardedShapes =
    "constexpr SIZE : usize = 4;\n" ++
    "\n" ++
    "function guarded(buf : &[u32; 4], i : usize) -> bool {\n" ++
    "    return i < 4 : usize && buf[i] > 0 : u32;\n" ++
    "}\n" ++
    "\n" ++
    "function guarded_size(buf : &[u32; SIZE], i : usize) -> bool {\n" ++
    "    return i < SIZE && buf[i] > 0 : u32;\n" ++
    "}\n" ++
    "\n" ++
    "function guarded_or(buf : &[u32; 4], i : usize) -> bool {\n" ++
    "    return i >= 4 : usize || buf[i] > 0 : u32;\n" ++
    "}\n" ++
    "\n" ++
    "function divided(y : u32, x : u32) -> bool {\n" ++
    "    return x != 0 : u32 && y / x > 1 : u32;\n" ++
    "}\n" ++
    "\n" ++
    "function divided_or(y : u32, x : u32) -> bool {\n" ++
    "    return x == 0 : u32 || y % x == 0 : u32;\n" ++
    "}\n" ++
    "\n" ++
    "function shifted(v : u32, s : u32) -> bool {\n" ++
    "    return s < 32 : u32 && v << s > 0 : u32;\n" ++
    "}\n"

-- | Checks that only the values of the path show: a shift by an amount computed
-- from a variable an @if@ bounds, the index of a loop over the array, and a
-- signed addition of two bounded operands.
valueShapes :: String
valueShapes =
    "function bit_of(led : u8, value : u32) -> u32 {\n" ++
    "    var mask : u32 = 0;\n" ++
    "    if led > 5 : u8 && led < 10 : u8 {\n" ++
    "        let bit : u8 = led + 17 : u8;\n" ++
    "        if value == 0 : u32 {\n" ++
    "            mask = 0xFFFFFFFF : u32 ^ (1 : u32 << bit);\n" ++
    "        } else {\n" ++
    "            mask = 1 : u32 << bit;\n" ++
    "        }\n" ++
    "    }\n" ++
    "    return mask;\n" ++
    "}\n" ++
    "\n" ++
    "function sum(buf : &[u32; 8]) -> u32 {\n" ++
    "    var total : u32 = 0;\n" ++
    "    for i : usize in 0 .. 8 {\n" ++
    "        total = total + buf[i];\n" ++
    "    }\n" ++
    "    return total;\n" ++
    "}\n" ++
    "\n" ++
    "function offset(a : i32) -> i32 {\n" ++
    "    var r : i32 = 0;\n" ++
    "    if a > -100 : i32 && a < 100 : i32 {\n" ++
    "        r = a + 1000 : i32;\n" ++
    "    }\n" ++
    "    return r;\n" ++
    "}\n"

spec :: Spec
spec = do
  describe "Value prover" $ do
    it "Discharges every check the guard prover discharges" $
      case runProverDischarges guardedShapes of
        Left err -> expectationFailure (unpack (failMessage err))
        Right (byGuard, byValue) -> do
          byGuard `shouldNotBe` S.empty
          S.difference byGuard byValue `shouldBe` S.empty
    it "Discharges the checks that the values of the path show" $
      goldenJSON "value_prover" (runChecksReport valueShapes)
