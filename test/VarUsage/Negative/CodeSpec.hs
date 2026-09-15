{-# LANGUAGE OverloadedStrings #-}
-- | Negative tests of the two usage checks, *code* flavour: one assertion per
-- reachable code of each family, over the source programs the detail spec
-- defines.
module VarUsage.Negative.CodeSpec (spec) where

import VarUsage.Common (boxUsageErrorCode, varUsageErrorCode)
import VarUsage.Negative.DetailSpec
  ( testVUE001, testVUE002, testBE001, testBE002, testBE003, testBE004, testBE005
  , testBE006, testBE006_1, testBE007, testBE008, testBE009, testBE010, testBE011, testBE012
  , testVUE003, testVUE004, testVUE005, testVUE006, testVUE007, testVUE008
  , testVUE006_3 )

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
  , ("BE-006", "the same, with a field of the same name read through a reference", testBE006_1)
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
  [ ("VUE-001", "ignored parameter is used", testVUE001)
  , ("VUE-002", "variable not used", testVUE002)
  , ("VUE-003", "action does not use self", testVUE003)
  , ("VUE-004", "method or viewer does not use self", testVUE004)
  , ("VUE-005", "method or viewer never called", testVUE005)
  , ("VUE-006", "assigned value never read", testVUE006)
  , ("VUE-007", "object read before it is assigned", testVUE007)
  , ("VUE-008", "partial write before the object is assigned", testVUE008)
  , ("VUE-009", "initializer never read", testVUE006_3)
  ]

spec :: Spec
spec = describe "Usage checks: error-code coverage" $ do
  forM_ boxCases $ \(code, title, src) ->
    it (unpack code ++ ": " ++ title) $
      boxUsageErrorCode src `shouldBe` Just code
  forM_ varCases $ \(code, title, src) ->
    it (unpack code ++ ": " ++ title) $
      varUsageErrorCode src `shouldBe` Just code
