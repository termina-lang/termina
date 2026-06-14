module Semantic.ConstArraySpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (Text, pack)

-- A global const array keeps its bare element type ('u32' / 'f32', not a
-- TConstSubtype) but is bound with Immutable access. So indexing it yields a
-- readable value of the element type, while assigning to an element is rejected
-- as a write through an immutable access.

constFloatArray :: String
constFloatArray = "const farr : [f32; 3] = {1.0 : f32, 2.0 : f32, 3.0 : f32};\n"

constIntArray :: String
constIntArray = "const iarr : [u32; 3] = {10 : u32, 20 : u32, 30 : u32};\n"

-- | The error code raised when assigning to a const-array element: the const
-- is read-only (EConstantIsReadOnly).
writeToImmutable :: Maybe Text
writeToImmutable = Just (pack "SE-080")

spec :: Spec
spec = do
  describe "Global const arrays: read vs write" $ do

    it "Reads an element of a const f32 array" $
      compileErrorCode (constFloatArray ++
        "function read_float() -> f32 {\n" ++
        "    return farr[1];\n" ++
        "}")
        `shouldBe` Nothing

    it "Reads an element of a const u32 array" $
      compileErrorCode (constIntArray ++
        "function read_int() -> u32 {\n" ++
        "    return iarr[1];\n" ++
        "}")
        `shouldBe` Nothing

    it "Rejects writing to a const f32 array element" $
      compileErrorCode (constFloatArray ++
        "function write_float() {\n" ++
        "    farr[0] = 9.0 : f32;\n" ++
        "    return;\n" ++
        "}")
        `shouldBe` writeToImmutable

    it "Rejects writing to a const u32 array element" $
      compileErrorCode (constIntArray ++
        "function write_int() {\n" ++
        "    iarr[0] = 99 : u32;\n" ++
        "    return;\n" ++
        "}")
        `shouldBe` writeToImmutable
