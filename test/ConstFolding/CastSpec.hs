module ConstFolding.CastSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec

spec :: Spec
spec = do
  describe "ConstFolding: constant int<->float casts" $ do

    -- Regression: a *constant* integer cast to a float type must fold (or pass
    -- through) cleanly. It used to hit the integer-range check in
    -- constSimplExpression and throw a spurious EConstIntegerOverflow (CPE-004),
    -- because memberIntCons returns False for non-integer target types.
    it "Folds a constant i32 -> f32 cast without a spurious overflow" $
      compileErrorCode "function f() -> f32 {\n    return 5 : i32 as f32;\n}"
        `shouldBe` Nothing

    it "Folds a constant u32 -> f64 cast without a spurious overflow" $
      compileErrorCode "function f() -> f64 {\n    return 100 : u32 as f64;\n}"
        `shouldBe` Nothing

    it "Folds a constant f32 -> i32 cast cleanly" $
      compileErrorCode "function f() -> i32 {\n    return 5.0 : f32 as i32;\n}"
        `shouldBe` Nothing
