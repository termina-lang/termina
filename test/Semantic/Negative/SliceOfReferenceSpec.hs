-- | Slices of a reference to an array, which is dereferenced as when it is
-- indexed.
module Semantic.Negative.SliceOfReferenceSpec (spec) where

import Test.Hspec
import Semantic.AST
import Semantic.Errors
import Semantic.Common
import Semantic.Types

-- | A function that takes a part of an array and one that slices the given
-- parameter in the given statement.
functions :: String -> String -> String
functions param stmt =
    "function fill(dst : &mut [u8; 4]) -> u8 {\n" ++
    "    dst[0] = 0;\n" ++
    "    return dst[1];\n" ++
    "}\n" ++
    "function read(src : &[u8; 4]) -> u8 {\n" ++
    "    return src[0];\n" ++
    "}\n" ++
    "function part(" ++ param ++ ") -> u8 {\n" ++
    "    " ++ stmt ++ "\n" ++
    "    return r;\n" ++
    "}\n"

isMutableToImmutable :: Maybe Error -> Bool
isMutableToImmutable = \case
  Just EMutableReferenceToImmutable -> True
  _ -> False

isInvalidIndexing :: Maybe Error -> Bool
isInvalidIndexing = \case
  Just (EInvalidArrayIndexing (TReference Mutable TUInt32)) -> True
  _ -> False

spec :: Spec
spec = describe "Semantic: slices of a reference to an array" $ do

  it "accepts a mutable slice of a mutable reference" $
    runNegativeTestTypeCheck (functions "x : &mut [u8; 8]" "let r : u8 = fill(&mut x[2 .. 6]);")
      `shouldSatisfy` null

  it "accepts an immutable slice of an immutable reference" $
    runNegativeTestTypeCheck (functions "x : &[u8; 8]" "let r : u8 = read(&x[2 .. 6]);")
      `shouldSatisfy` null

  it "rejects a mutable slice of an immutable reference" $
    runNegativeTestTypeCheck (functions "x : &[u8; 8]" "let r : u8 = fill(&mut x[2 .. 6]);")
      `shouldSatisfy` isMutableToImmutable

  it "rejects a slice of an object that is not an array" $
    runNegativeTestTypeCheck (functions "x : &mut u32" "let r : u8 = fill(&mut x[2 .. 6]);")
      `shouldSatisfy` isInvalidIndexing
