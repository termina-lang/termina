-- | Unit tests for the structured access path (`objectPath`) and the move-key
-- projection (`getMovedHash`) in "Semantic.Utils". These are the shared
-- primitive for the analyses that reason about object identity, so their
-- structure is pinned directly rather than only through a client analysis.
-- Objects are built with @()@ annotations, which `objectPath` ignores.
module Semantic.AccessPathSpec (spec) where

import Semantic.Utils (objectPath, getMovedHash, mayAlias, AccessPath(..), AccessStep(..))
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

  describe "mayAlias: overlap of two access paths" $ do
    it "a path aliases itself" $
      mayAlias (AccessPath "s" [FieldStep "a"]) (AccessPath "s" [FieldStep "a"])
        `shouldBe` True
    it "distinct roots never alias" $
      mayAlias (AccessPath "x" []) (AccessPath "y" []) `shouldBe` False
    it "distinct struct fields are disjoint" $
      mayAlias (AccessPath "s" [FieldStep "a"]) (AccessPath "s" [FieldStep "b"])
        `shouldBe` False
    it "a field and the whole object overlap (prefix)" $
      mayAlias (AccessPath "s" []) (AccessPath "s" [FieldStep "a"])
        `shouldBe` True
    it "two array elements may alias (index is opaque)" $
      mayAlias (AccessPath "arr" [IndexStep]) (AccessPath "arr" [IndexStep])
        `shouldBe` True
    it "distinct nested fields are disjoint" $
      mayAlias (AccessPath "s" [FieldStep "a", FieldStep "x"])
               (AccessPath "s" [FieldStep "a", FieldStep "y"])
        `shouldBe` False
    it "an array field and a sibling field are disjoint" $
      mayAlias (AccessPath "s" [FieldStep "arr", IndexStep])
               (AccessPath "s" [FieldStep "other"])
        `shouldBe` False
    it "an array element and its whole array field overlap (prefix)" $
      mayAlias (AccessPath "s" [FieldStep "arr", IndexStep])
               (AccessPath "s" [FieldStep "arr"])
        `shouldBe` True
    it "distinct fields through a dereference are disjoint (p->a vs p->b)" $
      mayAlias (AccessPath "p" [DerefStep, FieldStep "a"])
               (AccessPath "p" [DerefStep, FieldStep "b"])
        `shouldBe` False
    it "the same field through a dereference aliases (p->a vs p->a)" $
      mayAlias (AccessPath "p" [DerefStep, FieldStep "a"])
               (AccessPath "p" [DerefStep, FieldStep "a"])
        `shouldBe` True

  describe "getMovedHash: the move key is the root of any access" $ do
    it "collapses a nested field access to the root" $
      getMovedHash (fld (fld (var "s") "a") "b") `shouldBe` "s"
    it "collapses an array index to the root" $
      getMovedHash (at (fld (var "s") "arr") "i") `shouldBe` "s"
    it "collapses a deref member access to the root" $
      getMovedHash (DereferenceMemberAccess (var "self") "pool" ()) `shouldBe` "self"
    it "a bare variable is its own key" $
      getMovedHash (var "b") `shouldBe` "b"
