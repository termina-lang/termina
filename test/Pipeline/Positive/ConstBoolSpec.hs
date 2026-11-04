-- | Full-pipeline golden for boolean constants made of comparisons and logical
-- operations: those of integers and booleans are worked out by the
-- transpiler, those of reals and characters are left to the C compiler, as
-- the real constants are.
module Pipeline.Positive.ConstBoolSpec (spec) where

import Pipeline.Common
import Golden

import Test.Hspec

constBoolSrc :: String
constBoolSrc =
    "const K : u32 = 4;\n" ++
    "const F : f32 = 1.5;\n" ++
    "const X : char = 'a';\n" ++
    "const A : bool = K > 1;\n" ++
    "const B : bool = A && K == 4;\n" ++
    "const C : bool = A == B;\n" ++
    "const N : bool = K < 2 || K > 8;\n" ++
    "const R : bool = F > 1.0;\n" ++
    "const D : bool = X == 'a';\n" ++
    "const T : u32 = F as u32;\n"

spec :: Spec
spec = describe "Full pipeline: boolean constants" $
  it "computes those of integers and booleans and leaves those of reals and characters to C" $
    goldenC "const_bool" (buildAndRenderModule "test" [("test", constBoolSrc)])
