-- | The verdict of a comparison, driven from the function itself.
--
-- The two tables it holds, the one that pairs the values up and the one that
-- answers from the ends, decide whether a condition is reported, so an
-- inverted bound in either of them reports a condition of correct code. A
-- source program reaches the ends table through the iterator of a loop of more
-- turns than the limit, and through little else: a set outgrows the limit only
-- after ten branches assigning ten values, and the interval it lands on is the
-- whole range of the type, whose comparisons the folding rejects earlier as
-- CFE-017.
module ValueAnalysis.CompareSpec (spec) where

import ControlFlow.ValueAnalysis (Integers(..), compareValues)
import Core.AST (Op(..))

import qualified Data.Set as S
import Control.Monad (forM_)
import Test.Hspec

listed :: [Integer] -> Integers
listed = Listed . S.fromList

spanning :: Integer -> Integer -> Integers
spanning = Spanning

-- | A row of either table: what is compared, and the verdict it has to give.
type Case = (String, Op, Integers, Integers, Maybe Bool)

-- | Both operands written out, which is the exact answer: every pair is tried,
-- so a gap in the middle of a list counts.
listedCases :: [Case]
listedCases =
  [ ("<  holds for every pair", RelationalLT, listed [0, 1], listed [5], Just True)
  , ("<  holds for no pair", RelationalLT, listed [5, 6], listed [0], Just False)
  , ("<  holds for some pairs", RelationalLT, listed [0, 5], listed [3], Nothing)
  , ("<= holds up to the shared end", RelationalLTE, listed [0, 3], listed [3], Just True)
  , ("<= holds for no pair", RelationalLTE, listed [4], listed [0, 3], Just False)
  , ("<= holds for some pairs", RelationalLTE, listed [0, 5], listed [3], Nothing)
  , (">  holds for every pair", RelationalGT, listed [5, 6], listed [0, 3], Just True)
  , (">  holds for no pair", RelationalGT, listed [0, 1], listed [5], Just False)
  , (">  holds for some pairs", RelationalGT, listed [0, 5], listed [3], Nothing)
  , (">= holds down to the shared end", RelationalGTE, listed [3, 4], listed [0, 3], Just True)
  , (">= holds for no pair", RelationalGTE, listed [0, 1], listed [5], Just False)
  , (">= holds for some pairs", RelationalGTE, listed [0, 5], listed [3], Nothing)
  , ("== holds for the only pair", RelationalEqual, listed [3], listed [3], Just True)
  , ("== holds for no pair", RelationalEqual, listed [0, 1], listed [7], Just False)
  , ("== holds for one pair of several", RelationalEqual, listed [0, 7], listed [7], Nothing)
  , ("== over a gap holds for no pair", RelationalEqual, listed [0, 2], listed [1], Just False)
  , ("!= holds for every pair", RelationalNotEqual, listed [0, 1], listed [7], Just True)
  , ("!= holds for no pair", RelationalNotEqual, listed [3], listed [3], Just False)
  , ("!= holds for one pair of several", RelationalNotEqual, listed [0, 7], listed [7], Nothing)
  ]

-- | At least one interval, so only the ends are known. The gap of a list is
-- lost here, which costs findings and never invents one.
endsCases :: [Case]
endsCases =
  [ ("<  every value below every other", RelationalLT, spanning 0 3, spanning 5 9, Just True)
  , ("<  no value below any other", RelationalLT, spanning 5 9, spanning 0 3, Just False)
  , ("<  ranges that overlap", RelationalLT, spanning 0 5, spanning 3 9, Nothing)
  , ("<  ranges that touch at one end", RelationalLT, spanning 0 3, spanning 3 9, Nothing)
  , ("<  the same single value", RelationalLT, spanning 3 3, spanning 3 3, Just False)
  , ("<= up to the shared end", RelationalLTE, spanning 0 3, spanning 3 9, Just True)
  , ("<= no value at or below any other", RelationalLTE, spanning 4 9, spanning 0 3, Just False)
  , ("<= ranges that overlap", RelationalLTE, spanning 0 5, spanning 3 9, Nothing)
  , (">  every value above every other", RelationalGT, spanning 5 9, spanning 0 3, Just True)
  , (">  no value above any other", RelationalGT, spanning 0 3, spanning 5 9, Just False)
  , (">  ranges that overlap", RelationalGT, spanning 0 5, spanning 3 9, Nothing)
  , (">= down to the shared end", RelationalGTE, spanning 3 9, spanning 0 3, Just True)
  , (">= no value at or above any other", RelationalGTE, spanning 0 3, spanning 4 9, Just False)
  , (">= ranges that overlap", RelationalGTE, spanning 0 5, spanning 3 9, Nothing)
  , ("== both pinned to the same value", RelationalEqual, spanning 3 3, spanning 3 3, Just True)
  , ("== ranges that share nothing", RelationalEqual, spanning 0 3, spanning 5 9, Just False)
  , ("== the same range of several values", RelationalEqual, spanning 0 3, spanning 0 3, Nothing)
  , ("!= ranges that share nothing", RelationalNotEqual, spanning 0 3, spanning 5 9, Just True)
  , ("!= both pinned to the same value", RelationalNotEqual, spanning 3 3, spanning 3 3, Just False)
  , ("!= ranges that overlap", RelationalNotEqual, spanning 0 5, spanning 3 9, Nothing)
  , ("a list against a range reads the ends", RelationalLT, listed [0, 1], spanning 5 9, Just True)
  , ("a range against a list reads the ends", RelationalGT, spanning 5 9, listed [0, 1], Just True)
  ]

-- | The answers that have to be no answer whatever the values are.
refusedCases :: [Case]
refusedCases =
  [ ("an operator that is not a comparison", Addition, listed [0], listed [1], Nothing)
  , ("an operator that is not a comparison, over ranges", Addition, spanning 0 3, spanning 5 9, Nothing)
  , ("an operand with no values at all", RelationalLT, listed [], listed [5], Nothing)
  , ("both operands with no values at all", RelationalLT, listed [], listed [], Nothing)
  ]

spec :: Spec
spec = do
  describe "ValueAnalysis: the verdict of a comparison, pairing the values up" $
    forM_ listedCases check

  describe "ValueAnalysis: the verdict of a comparison, from the ends" $
    forM_ endsCases check

  describe "ValueAnalysis: the comparisons that get no verdict" $
    forM_ refusedCases check

  where

    check (what, op, left, right, expected) =
      it what $ compareValues op left right `shouldBe` expected
