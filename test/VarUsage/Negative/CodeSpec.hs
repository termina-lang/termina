{-# LANGUAGE OverloadedStrings #-}
-- | Negative tests of the two usage checks, *code* flavour: one assertion per
-- reachable code of each family, over the source programs the detail spec
-- defines.
module VarUsage.Negative.CodeSpec (spec) where

import VarUsage.Common (boxUsageErrorCode, varUsageErrorCode)
import VarUsage.Negative.DetailSpec
  ( testVE001, testVE002, testBE001, testBE002, testBE003, testBE004, testBE005
  , testBE006, testBE007, testBE008, testBE009, testBE010, testBE011, testBE012
  , testVE003, testVE004, testVE005, testVE006, testVE007, testVE008 )

import Data.Text (Text, unpack)
import Control.Monad (forM_)
import Test.Hspec

-- | (code, title, source) for every reachable error of the backward pass,
-- which owns the linearity of boxes and option-boxes.
boxCases :: [(Text, String, String)]
boxCases =
  [ ("BE-001", "box variable not moved", testBE001)
  , ("BE-002", "box variable moved twice", testBE002)
  , ("BE-003", "option-box variable moved twice", testBE003)
  , ("BE-004", "option-box final state mismatch across branches", testBE004)
  , ("BE-005", "option-box used in a branch that may not run", testBE005)
  , ("BE-006", "option-box used in a previous branch but missing in another", testBE006)
  , ("BE-007", "box variable not moved in all branches", testBE007)
  , ("BE-008", "box variable moved in a branch that may not run", testBE008)
  , ("BE-009", "option-box allocated but not moved", testBE009)
  , ("BE-010", "option-box allocated twice", testBE010)
  , ("BE-011", "option-box moved without being allocated", testBE011)
  , ("BE-012", "option-box match missing the Some case", testBE012)
  ]

-- | (code, title, source) for the codes raised by the forward pass, which owns
-- definite assignment and the usage of variables, fields and member functions.
varCases :: [(Text, String, String)]
varCases =
  [ ("VE-001", "ignored parameter is used", testVE001)
  , ("VE-002", "variable not used", testVE002)
  , ("VE-003", "action does not use self", testVE003)
  , ("VE-004", "method or viewer does not use self", testVE004)
  , ("VE-005", "method or viewer never called", testVE005)
  , ("VE-006", "assigned value never read", testVE006)
  , ("VE-007", "object read before it is assigned", testVE007)
  , ("VE-008", "partial write before the object is assigned", testVE008)
  ]

spec :: Spec
spec = describe "Usage checks: error-code coverage" $ do
  forM_ boxCases $ \(code, title, src) ->
    it (unpack code ++ ": " ++ title) $
      boxUsageErrorCode src `shouldBe` Just code
  forM_ varCases $ \(code, title, src) ->
    it (unpack code ++ ": " ++ title) $
      varUsageErrorCode src `shouldBe` Just code
