-- | A call where a constant expression is expected. The initializer of a
-- constant is typed against its const type, and so is each field, element and
-- variant value of it, so a call anywhere inside is an SE-003, as in the bound
-- of a for loop. A call used to fail with an internal error in the initializer
-- of a scalar constant, to pass in that of an aggregate one and to give
-- another error inside an operation or a cast.
module Semantic.Negative.ConstantSpec (spec) where

import Test.Hspec
import Semantic.Common
import Configuration.Platform (Platform(TestPlatform))

import Data.Text (pack)

-- | The declarations every case uses.
prelude :: String
prelude =
  "struct Inner {\n" ++
  "    data : [u8; 4];\n" ++
  "    n : u32;\n" ++
  "};\n" ++
  "struct Outer {\n" ++
  "    items : [Inner; 2];\n" ++
  "    flag : bool;\n" ++
  "};\n" ++
  "function twice(k : u32) -> u32 {\n" ++
  "    return 2 * k;\n" ++
  "}\n" ++
  "function make_inner(k : u8) -> Inner {\n" ++
  "    var r : Inner = { data = [k; 4], n = 1 };\n" ++
  "    return r;\n" ++
  "}\n" ++
  "const K8 : u8 = 3;\n"

-- | The code of the first error of the prelude followed by a declaration.
errorOf :: String -> Maybe String
errorOf decl = show <$> typeCheckErrorOn TestPlatform (prelude ++ decl)

notConstant :: Maybe String
notConstant = Just (show (pack "SE-003"))

spec :: Spec
spec = describe "A call where a constant expression is expected" $ do
  it "SE-003: a call that initializes a scalar constant" $
    errorOf "const G : u32 = twice(3);\n" `shouldBe` notConstant
  it "SE-003: a call inside an operation" $
    errorOf "const G : u32 = twice(3) + 1;\n" `shouldBe` notConstant
  it "SE-003: a call inside a cast" $
    errorOf "const G : i32 = twice(3) as i32;\n" `shouldBe` notConstant
  it "SE-003: a call that initializes a whole struct" $
    errorOf "const G : Inner = make_inner(3);\n" `shouldBe` notConstant
  it "SE-003: a call in a field of a struct" $
    errorOf "const G : Inner = { data = [1; 4], n = twice(3) };\n" `shouldBe` notConstant
  it "SE-003: a call in the fill of an array" $
    errorOf "const G : [u32; 3] = [twice(3); 3];\n" `shouldBe` notConstant
  it "SE-003: a call in an array nested in a struct" $
    errorOf "const G : Outer = { items = [make_inner(3); 2], flag = true };\n" `shouldBe` notConstant
  it "SE-003: a call in the value of a Status" $
    errorOf "const G : Status<i32> = Failure(twice(3) as i32);\n" `shouldBe` notConstant
  it "SE-003: a call in the bound of a for loop" $
    errorOf ("function f(k : u32) -> u32 {\n" ++
             "    var s : u32 = 0;\n" ++
             "    for i : u32 in 0 .. twice(k) {\n" ++
             "        s = s + i;\n" ++
             "    }\n" ++
             "    return s;\n" ++
             "}\n") `shouldBe` notConstant
  it "accepts constants made of literals, other constants and casts of them" $
    errorOf ("const A : Inner = { data = {1, 2, 3, 4}, n = 9 };\n" ++
             "const B : Inner = A;\n" ++
             "const C : u32 = A.n + (K8 as u32);\n" ++
             "const D : Status<i32> = Failure(K8 as i32);\n" ++
             "const E : [u8; 4] = [K8; 4];\n") `shouldBe` Nothing
