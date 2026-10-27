{-# LANGUAGE OverloadedStrings #-}

-- | The descriptor of a unit test: its cases with their numbers, its checks
-- with their text, the tasks and the emitters by identifier, and its stamp.
module Pipeline.Positive.TestDescriptorSpec (spec) where

import Pipeline.Common

import Data.Aeson (FromJSON(..), eitherDecode, withObject, (.:))
import Data.Text (unpack)
import Test.Hspec
import Text.Printf (printf)

-- | The fields of the descriptor that the tests read
data Descriptor = Descriptor {
    descriptorStamp :: String,
    descriptorCases :: [DescriptorCase],
    descriptorChecks :: [DescriptorCheck],
    descriptorTasks :: [DescriptorEntry],
    descriptorEmitters :: [DescriptorEntry]
  }

data DescriptorCase = DescriptorCase Integer String
  deriving (Eq, Show)

data DescriptorCheck = DescriptorCheck Integer String String
  deriving (Eq, Show)

data DescriptorEntry = DescriptorEntry Integer String
  deriving (Eq, Show)

instance FromJSON Descriptor where
  parseJSON = withObject "descriptor" $ \o -> Descriptor
    <$> o .: "stamp" <*> o .: "cases" <*> o .: "checks" <*> o .: "tasks" <*> o .: "emitters"

instance FromJSON DescriptorCase where
  parseJSON = withObject "case" $ \o -> DescriptorCase <$> o .: "number" <*> o .: "id"

instance FromJSON DescriptorCheck where
  parseJSON = withObject "check" $ \o -> DescriptorCheck <$> o .: "line" <*> o .: "procedure" <*> o .: "text"

instance FromJSON DescriptorEntry where
  parseJSON = withObject "entry" $ \o -> DescriptorEntry <$> o .: "id" <*> o .: "name"

-- | Two instances and two cases declared against the alphabetical order, with
-- a check on two lines and a step, which is not a check
descriptorTest :: String
descriptorTest =
    "task class CTicker {\n" ++
    "    tick : sink TimeVal triggers run;\n" ++
    "    count : u32;\n" ++
    "    action run(&priv self, _t : TimeVal) {\n" ++
    "        self->count = self->count + 1;\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "test class CTickerTests {\n" ++
    "    tick : access TestEmitterAccess<TimeVal>;\n" ++
    "    t : access TestChecks;\n" ++
    "    case zeta(&mut self) {\n" ++
    "        self->t.step(1 : usize);\n" ++
    "        self->tick.fire({tv_sec = 1, tv_usec = 0} : TimeVal);\n" ++
    "        self->t.check_eq_u32(1 : u32,\n" ++
    "            1 : u32);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "    case alpha(&mut self) {\n" ++
    "        self->t.fail();\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter tick : TestEmitter<TimeVal>;\n" ++
    "#[priority(5)]\n" ++
    "task ticker : CTicker = { count = 0, tick <- tick };\n" ++
    "test b_tests : CTickerTests = { tick <-> tick, t <-> test_checks };\n" ++
    "test a_tests : CTickerTests = { tick <-> tick, t <-> test_checks };\n"

-- | The descriptor of a test and its stamp as the descriptor writes it
withDescriptor :: String -> (Descriptor -> String -> Expectation) -> Expectation
withDescriptor src check =
    case runTestDescriptor src of
        Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
        Right (text, stamp) -> case eitherDecode text of
            Left err -> expectationFailure $ "the descriptor does not have the expected form: " ++ err
            Right descriptor -> check descriptor (printf "0x%08x" stamp)

spec :: Spec
spec = describe "Full pipeline: the descriptor of a unit test" $ do
  it "numbers the cases as the harness does" $
    withDescriptor descriptorTest $ \d _ ->
      descriptorCases d `shouldBe` [DescriptorCase 1 "b_tests.zeta", DescriptorCase 2 "b_tests.alpha",
                                    DescriptorCase 3 "a_tests.zeta", DescriptorCase 4 "a_tests.alpha"]

  it "lists every check with its line and its text in one line, and leaves the steps out" $
    withDescriptor descriptorTest $ \d _ ->
      descriptorChecks d `shouldBe` [DescriptorCheck 15 "check_eq_u32" "self->t.check_eq_u32(1 : u32, 1 : u32)",
                                     DescriptorCheck 20 "fail" "self->t.fail()"]

  it "gives the tasks and the emitters in the order of their identifiers" $
    withDescriptor descriptorTest $ \d _ -> do
      descriptorTasks d `shouldBe` [DescriptorEntry 0 "ticker", DescriptorEntry 1 "harness"]
      descriptorEmitters d `shouldBe` [DescriptorEntry 0 "system_except", DescriptorEntry 1 "harness", DescriptorEntry 2 "tick"]

  it "carries the stamp that the main file gets" $
    withDescriptor descriptorTest $ \d stamp ->
      descriptorStamp d `shouldBe` stamp

  it "changes the stamp when a check changes" $
    case (runTestDescriptor descriptorTest, runTestDescriptor (concatMap (\c -> if c == '1' then "2" else [c]) descriptorTest)) of
      (Right (_, s1), Right (_, s2)) -> s1 `shouldNotBe` s2
      _ -> expectationFailure "pipeline failed"
