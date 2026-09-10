-- | End-to-end coverage of the semantic move check (EObjectPreviouslyMoved,
-- SE-190), the analysis that consumes `getMovedHash`. The only move trigger is
-- a `match`, which moves the scrutinee's root for the duration of the case
-- bodies (a local scope). These cases pin what the root-granular key actually
-- detects: use-after-move inside the match, the scoped restore afterwards, and
-- that an unrelated root (including `self`, which is never moved) is untouched.
module Semantic.Positive.MovedObjectSpec (spec) where

import Semantic.Common (typeCheckErrorOn)

import Configuration.Platform (Platform(TestPlatform))
import Data.Text (pack)
import Test.Hspec

-- A resource whose procedure receives a linear option-box, wrapping a varying
-- procedure body. Type-check-only, so box linearity (a later pass) does not
-- interfere; the move check runs in the type checker.
withBoxProc :: String -> String
withBoxProc body =
    "enum St { A, B };\n" ++
    "interface I { procedure p(&mut self, data : &mut Option<box u32>); };\n" ++
    "resource class R provides I {\n" ++
    "    local : u32;\n" ++
    "    st : St;\n" ++
    "    procedure p(&mut self, data : &mut Option<box u32>) {\n" ++
    body ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

-- Re-match the moved scrutinee inside its own case: the classic use-after-move.
reMatchInside :: String
reMatchInside = withBoxProc $
    "        match data {\n" ++
    "            case Some(v) => {\n" ++
    "                match data {\n" ++
    "                    case Some(v2) => { self->local = *v2; }\n" ++
    "                    case None => {}\n" ++
    "                }\n" ++
    "            }\n" ++
    "            case None => {}\n" ++
    "        }\n"

-- Two sequential matches on the same scrutinee: the move is restored after the
-- first match, so the second is fine.
sequentialMatches :: String
sequentialMatches = withBoxProc $
    "        match data { case Some(v) => { self->local = *v; } case None => {} }\n" ++
    "        match data { case Some(v) => { self->local = *v; } case None => {} }\n"

-- Access a different root inside the case: not the moved object.
differentRootInside :: String
differentRootInside = withBoxProc $
    "        match data {\n" ++
    "            case Some(v) => { self->local = *v; }\n" ++
    "            case None => {}\n" ++
    "        }\n"

-- Match a field of self, then access a sibling field inside a case. `self` is
-- never moved, so the match records nothing and the sibling access is fine.
matchSelfFieldSibling :: String
matchSelfFieldSibling = withBoxProc $
    "        match self->st {\n" ++
    "            case A => { self->local = 1 : u32; }\n" ++
    "            case B => {}\n" ++
    "        }\n"

spec :: Spec
spec = do
  describe "SE-190 (move check): detection fires" $
    it "re-accessing the moved scrutinee inside its own match is caught" $
      typeCheckErrorOn TestPlatform reMatchInside `shouldBe` Just (pack "SE-190")

  describe "SE-190 (move check): no false positive" $ do
    it "the scrutinee is usable again after the match (move is scoped)" $
      typeCheckErrorOn TestPlatform sequentialMatches `shouldBe` Nothing
    it "a different root is untouched inside the case" $
      typeCheckErrorOn TestPlatform differentRootInside `shouldBe` Nothing

  describe "SE-190 (move check): self is never moved" $
    it "matching a self field leaves sibling fields accessible" $
      typeCheckErrorOn TestPlatform matchSelfFieldSibling `shouldBe` Nothing
