module Pipeline.Positive.FloatSpec (spec) where

import Pipeline.Common
import Pipeline.Golden

import Test.Hspec

-- | A call to a Prelude float function (@f32_from_bits@) routed through the
-- whole pipeline, ConstFolding included. Exercises the constant folding pass on
-- call expressions, which the IT golden tests never reach.
testFromBits :: String
testFromBits =
    "function pipeline_floats(raw : u32) -> f32 {\n" ++
    "    var value : f32 = f32_from_bits(raw);\n" ++
    "    value = value - 1.0 : f32;\n" ++
    "    return value;\n" ++
    "}"

spec :: Spec
spec = do
  describe "Full pipeline with floating-point Prelude calls" $ do
    it "Folds and lowers a call to f32_from_bits without crashing" $
      goldenC "float_from_bits" (runFullBuild testFromBits)
