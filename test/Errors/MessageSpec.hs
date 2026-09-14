-- | Golden of the messages the compiler prints. Every other negative test
-- asserts the @XX-NNN@ code of an error and nothing else, so the text a user
-- reads was not fixed anywhere: a message could lose a sentence, a pointer or a
-- whole block without a single test noticing. The cases below cover each shape
-- the printer can produce, so that moving a family of errors to its description
-- table is a change that can be checked byte for byte.
module Errors.MessageSpec (spec) where

import Golden (goldenMessage)
import Pipeline.Common (compileErrorMessage)

import Data.Text (Text)
import Test.Hspec

-- | A variable nobody reads, which is the plain shape: one block, one pointer,
-- an explanation underneath, and the tag that greys the code out in an editor.
unusedVariable :: String
unusedVariable =
  "function f(x : u32) -> u32 {\n" ++
  "    var never_read : u32 = 0;\n" ++
  "    return x;\n" ++
  "}\n"

-- | Two mutable references to the same object in one expression, which is the
-- shape with a related position in the same file: one block with two pointers,
-- the second one labelled.
twoMutableReferences :: String
twoMutableReferences =
  "function set2(a : &mut u32, b : &mut u32) {\n" ++
  "    *a = 0 : u32;\n" ++
  "    *b = 1 : u32;\n" ++
  "    return;\n" ++
  "}\n" ++
  "function trigger() {\n" ++
  "    var x : u32 = 0;\n" ++
  "    set2(&mut x, &mut x);\n" ++
  "    return;\n" ++
  "}\n"

-- | A type error, which is the shape of the family the language server reports.
typeMismatch :: String
typeMismatch =
  "function f() -> u32 {\n" ++
  "    var x : u32 = 0;\n" ++
  "    x = true;\n" ++
  "    return x;\n" ++
  "}\n"

-- | An action that never exits, which is a message with two sentences and no
-- value quoted in it.
missingReturn :: String
missingReturn =
  "function f(x : u32) -> u32 {\n" ++
  "    var y : u32 = x;\n" ++
  "    y = y + 1 : u32;\n" ++
  "}\n"

message :: String -> Text
message src =
  case compileErrorMessage src of
    Just text -> text
    Nothing -> error "the program was expected to fail"

spec :: Spec
spec = describe "Errors: the message a user reads" $ do

  it "a variable that nobody reads" $
    goldenMessage "unused_variable" (message unusedVariable)

  it "two mutable references to the same object" $
    goldenMessage "two_mutable_references" (message twoMutableReferences)

  it "a value assigned to a variable of another type" $
    goldenMessage "type_mismatch" (message typeMismatch)

  it "a function with no return statement" $
    goldenMessage "missing_return" (message missingReturn)
