{-# LANGUAGE OverloadedStrings #-}
-- | VarUsage (move/borrow) negative tests, *code* flavour: one assertion per
-- reachable VE-NNN code, over the source programs the detail spec defines.
module VarUsage.Negative.CodeSpec (spec) where

import VarUsage.Common (varUsageErrorCode)
import VarUsage.Negative.DetailSpec
  ( testVE001, testVE002, testVE003, testVE004, testVE005, testVE006, testVE007
  , testVE008, testVE009, testVE010, testVE011, testVE012, testVE013, testVE014 )

import Data.Text (Text, unpack)
import Control.Monad (forM_)
import Test.Hspec

-- | (code, title, source) for every reachable VarUsage error.
cases :: [(Text, String, String)]
cases =
  [ ("VE-001", "ignored parameter is used", testVE001)
  , ("VE-002", "variable not used", testVE002)
  , ("VE-003", "box variable not moved", testVE003)
  , ("VE-004", "box variable moved twice", testVE004)
  , ("VE-005", "option-box variable moved twice", testVE005)
  , ("VE-006", "option-box final state mismatch across branches", testVE006)
  , ("VE-007", "option-box used in a branch that may not run", testVE007)
  , ("VE-008", "option-box used in a previous branch but missing in another", testVE008)
  , ("VE-009", "box variable not moved in all branches", testVE009)
  , ("VE-010", "box variable moved in a branch that may not run", testVE010)
  , ("VE-011", "option-box allocated but not moved", testVE011)
  , ("VE-012", "option-box allocated twice", testVE012)
  , ("VE-013", "option-box moved without being allocated", testVE013)
  , ("VE-014", "option-box match missing the Some case", testVE014)
  ]

spec :: Spec
spec = describe "VarUsage: error-code coverage" $
  forM_ cases $ \(code, title, src) ->
    it (unpack code ++ ": " ++ title) $
      varUsageErrorCode src `shouldBe` Just code
