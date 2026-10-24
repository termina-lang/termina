-- | How termina test --list reads the cases of a test from its modules,
-- without type checking them.
module Configuration.TestListSpec (spec) where

import Command.Test (TestCase(..), testCases)
import Core.AST (frags)
import Parser.Parsing (terminaModuleParser)
import Text.Parsec (runParser)
import Test.Hspec

-- | A test class with a case per name, each of four lines
caseClass :: String -> [String] -> String
caseClass cls cases =
  "test class " ++ cls ++ " {\n" ++
  "    t : access TestChecks;\n" ++
  concat [ "    case " ++ c ++ "(&mut self) {\n        self->t.check(true);\n        return;\n    }\n" | c <- cases ] ++
  "};\n"

spec :: Spec
spec = describe "termina test --list" $ do

  it "gives the cases of each test instance in the order of declaration" $
    casesIn [("tests/t/test.fin",
              caseClass "CFirst" ["one", "two"] ++ caseClass "CSecond" ["three"]
              ++ "test b : CSecond = { t <-> test_checks };\n"
              ++ "test a : CFirst = { t <-> test_checks };\n")]
      `shouldBe` Right [("b", "three", "tests/t/test.fin", 14), ("a", "one", "tests/t/test.fin", 3), ("a", "two", "tests/t/test.fin", 7)]

  it "finds the test class in another module of the test" $
    casesIn [("tests/t/src/cases/first.fin", caseClass "CFirst" ["one"]),
             ("tests/t/test.fin", "test a : CFirst = { t <-> test_checks };\n")]
      `shouldBe` Right [("a", "one", "tests/t/src/cases/first.fin", 3)]

  it "gives no cases for a concurrent test" $
    casesIn [("tests/t/test.fin", "channel c : MsgQueue<u32; 1>;\n")] `shouldBe` Right []

  it "rejects a test instance whose class is not a test class" $
    casesIn [("tests/t/test.fin", "test a : CMissing = { t <-> test_checks };\n")]
      `shouldBe` Left "the test instance a is of type CMissing, which is not a test class"

  where

    casesIn files =
      map (\c -> (caseInstance c, caseName c, caseFile c, caseLine c))
        <$> testCases [ either (error . show) frags (runParser terminaModuleParser path path src) | (path, src) <- files ]
