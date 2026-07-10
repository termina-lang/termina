-- | Unit tests for the structured access path (`objectPath`) and the move-key
-- projection (`getMovedHash`) in "Semantic.Utils". These are the shared
-- primitive for the analyses that reason about object identity, so their
-- structure is pinned directly rather than only through a client analysis.
-- Objects are built with @()@ annotations, which `objectPath` ignores.
module Semantic.AccessPathSpec (spec) where

import Semantic.Utils (objectPath, getMovedHash, AccessPath(..), AccessStep(..))
import Semantic.AST (Object(..), Expression(..))

import Test.Hspec

var :: String -> Object ()
var x = Variable x ()

fld :: Object () -> String -> Object ()
fld o f = MemberAccess o f ()

-- | @o[e]@ indexed by the (irrelevant) variable @e@.
at :: Object () -> String -> Object ()
at o e = ArrayIndexExpression o (AccessObject (var e)) ()

spec :: Spec
spec = do
  describe "objectPath: structural access path" $ do
    it "a variable is a bare root" $
      objectPath (var "s") `shouldBe` AccessPath "s" []
    it "a field access records the field" $
      objectPath (fld (var "s") "a") `shouldBe` AccessPath "s" [FieldStep "a"]
    it "nested fields are recorded root to leaf" $
      objectPath (fld (fld (var "s") "a") "b")
        `shouldBe` AccessPath "s" [FieldStep "a", FieldStep "b"]
    it "an array index is an opaque step" $
      objectPath (at (var "arr") "i") `shouldBe` AccessPath "arr" [IndexStep]
    it "keeps the field chain before an index" $
      objectPath (at (fld (var "s") "arr") "i")
        `shouldBe` AccessPath "s" [FieldStep "arr", IndexStep]
    it "a deref member access is a deref then a field" $
      objectPath (DereferenceMemberAccess (var "p") "f" ())
        `shouldBe` AccessPath "p" [DerefStep, FieldStep "f"]
    it "a plain deref is a deref step" $
      objectPath (Dereference (var "p") ()) `shouldBe` AccessPath "p" [DerefStep]
    it "an unbox is an opaque step" $
      objectPath (Unbox (var "b") ()) `shouldBe` AccessPath "b" [UnboxStep]

  describe "objectPath: the invariants the aliasing rule relies on" $ do
    it "array indices are opaque: two accesses differing only in the index share a path" $
      objectPath (at (var "arr") "i") `shouldBe` objectPath (at (var "arr") "j")
    it "distinct struct fields diverge (disjoint storage)" $
      objectPath (fld (var "s") "a") `shouldNotBe` objectPath (fld (var "s") "b")
    it "a whole-array index and a field of the same struct diverge" $
      objectPath (at (fld (var "s") "arr") "i")
        `shouldNotBe` objectPath (fld (var "s") "other")
    it "p->f is sugar for (*p).f: both give the same path (deref then field)" $
      objectPath (DereferenceMemberAccess (var "p") "f" ())
        `shouldBe` objectPath (MemberAccess (Dereference (var "p") ()) "f" ())

  describe "getMovedHash: the move key is the root of any access" $ do
    it "collapses a nested field access to the root" $
      getMovedHash (fld (fld (var "s") "a") "b") `shouldBe` "s"
    it "collapses an array index to the root" $
      getMovedHash (at (fld (var "s") "arr") "i") `shouldBe` "s"
    it "collapses a deref member access to the root" $
      getMovedHash (DereferenceMemberAccess (var "self") "pool" ()) `shouldBe` "self"
    it "a bare variable is its own key" $
      getMovedHash (var "b") `shouldBe` "b"
