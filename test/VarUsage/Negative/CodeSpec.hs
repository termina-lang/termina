{-# LANGUAGE OverloadedStrings #-}
-- | Negative tests of the two usage checks, *code* flavour: one assertion per
-- reachable code of each family, over the source programs the detail spec
-- defines.
module VarUsage.Negative.CodeSpec (spec) where

import VarUsage.Common (boxUsageErrorCode, varUsageErrorCode)
import VarUsage.Negative.DetailSpec
  ( testVUE001, testVUE002, testBE001, testBE002, testBE003, testBE004, testBE005
  , testBE006, testBE006_1, testBE007, testBE008, testBE009, testBE010, testBE011, testBE012
  , testBE013
  , testVUE003, testVUE004, testVUE005, testVUE006, testVUE007, testVUE008
  , testVUE006_3 )

import Architecture.Negative.CodeSpec (executorClass)
import Pipeline.Common (compileTestErrorCode)

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
  , ("BE-013", "option-box parameter read or not filled", testBE013)
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

-- | A case that takes a box from a test channel, with the statements that
-- follow the take, and with or without a port to free it
takeFromTestChannel :: Bool -> String -> String
takeFromTestChannel frees afterTake =
  executorClass ++
  "test class CTCExecutorTests {\n" ++
  "    tc_out : out TCCommand;\n" ++
  "    tm_rx : access TestChannelAccess<box TMPacket>;\n" ++
  (if frees then "    tm_pool : access Allocator<TMPacket>;\n" else "") ++
  "    tm_pool_state : access TestPoolAccess;\n" ++
  "    case answers(&mut self) {\n" ++
  "        var tm_opt : Option<box TMPacket> = None;\n" ++
  "        let tc : TCCommand = { service = 17 } : TCCommand;\n" ++
  "        self->tm_pool_state.fail_allocs(0 : usize);\n" ++
  "        self->tc_out.send(tc);\n" ++
  "        self->tm_rx.take(&mut tm_opt);\n" ++
  afterTake ++
  "        return;\n" ++
  "    }\n" ++
  "};\n" ++
  "resource tm_pool : TestPool<TMPacket; 4>;\n" ++
  "channel tcs : MsgQueue<TCCommand; 1>;\n" ++
  "channel tms : TestChannel<box TMPacket; 4>;\n" ++
  "#[priority(5)]\n" ++
  "task uut : CTCExecutor = { tc_in <- tcs, tm_out -> tms, tm_pool <-> tm_pool };\n" ++
  "test exec_tests : CTCExecutorTests = { tc_out -> tcs, tm_rx <-> tms, " ++
  (if frees then "tm_pool <-> tm_pool, " else "") ++ "tm_pool_state <-> tm_pool };\n"

-- | (code, title, whether the case can free, statements after the take) for
-- the box that a case takes from a test channel, which is linear as anywhere
-- else
testChannelCases :: [(Text, String, Bool, String)]
testChannelCases =
  [ ("BE-001", "box taken from a test channel and not moved", False,
      "        match tm_opt {\n" ++
      "            case Some(_b) => { }\n" ++
      "            case None => { }\n" ++
      "        }\n")
  , ("BE-009", "option-box filled from a test channel and not moved", False, "")
  , ("BE-010", "option-box filled twice from a test channel", True,
      "        self->tm_rx.take(&mut tm_opt);\n" ++
      "        match tm_opt {\n" ++
      "            case Some(b) => { self->tm_pool.free(b); }\n" ++
      "            case None => { }\n" ++
      "        }\n")
  ]

spec :: Spec
spec = describe "Usage checks: error-code coverage" $ do
  forM_ testChannelCases $ \(code, title, frees, afterTake) ->
    it (unpack code ++ ": " ++ title) $
      compileTestErrorCode (takeFromTestChannel frees afterTake) `shouldBe` Just code
  forM_ boxCases $ \(code, title, src) ->
    it (unpack code ++ ": " ++ title) $
      boxUsageErrorCode src `shouldBe` Just code
  forM_ varCases $ \(code, title, src) ->
    it (unpack code ++ ": " ++ title) $
      varUsageErrorCode src `shouldBe` Just code
