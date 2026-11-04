-- | The modifiers that each declaration accepts, and whether they take a
-- value.
module Semantic.Negative.ModifierSpec (spec) where

import Test.Hspec
import Semantic.Errors
import Semantic.Common
import Semantic.Modifiers (Declaration(..))

-- | A task class, a channel for it and a task with the given modifiers.
task :: String -> String
task mods =
    "task class TaskClass0 {\n" ++
    "    in_port : in u32 triggers handle;\n" ++
    "    action handle(&priv self, _msg : u32) {\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "channel chan : MsgQueue<u32; 10>;\n" ++
    mods ++
    "task task0 : TaskClass0 = { in_port <- chan };\n"

-- | A resource with no regular fields, with the given modifiers.
resource :: String -> String
resource mods =
    "interface IFoo {\n" ++
    "    procedure get(&mut self, r : &mut u32);\n" ++
    "};\n" ++
    "resource class FooRes provides IFoo {\n" ++
    "    procedure get(&mut self, r : &mut u32) {\n" ++
    "        *r = 0;\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    mods ++
    "resource foo : FooRes;\n"

-- | A struct with the given modifiers.
struct :: String -> String
struct mods =
    mods ++
    "struct Packet {\n" ++
    "    header : u32;\n" ++
    "    flags : u8;\n" ++
    "};\n"

spec :: Spec
spec = describe "Semantic: modifiers" $ do

  it "accepts priority and stack_size on a task" $
    runNegativeTestTypeCheck (task "#[priority(10)]\n#[stack_size(8192)]\n") `shouldSatisfy` null

  it "accepts unprotected on a resource" $
    runNegativeTestTypeCheck (resource "#[unprotected]\n") `shouldSatisfy` null

  it "accepts packed on a struct" $
    runNegativeTestTypeCheck (struct "#[packed]\n") `shouldSatisfy` null

  it "rejects a modifier that no declaration accepts" $
    runNegativeTestTypeCheck (task "#[priority(10)]\n#[stack(2048)]\n") `shouldSatisfy` \case
      Just (EUnknownModifier "stack" TaskDeclaration) -> True
      _ -> False

  it "rejects a modifier of another declaration" $
    runNegativeTestTypeCheck (task "#[priority(10)]\n#[unprotected]\n") `shouldSatisfy` \case
      Just (EUnknownModifier "unprotected" TaskDeclaration) -> True
      _ -> False

  it "rejects a modifier on a declaration that accepts none" $
    runNegativeTestTypeCheck "#[packed]\nfunction f() {\n    return;\n}\n" `shouldSatisfy` \case
      Just (EUnknownModifier "packed" FunctionDeclaration) -> True
      _ -> False

  it "rejects a modifier without the value it takes" $
    runNegativeTestTypeCheck (task "#[priority]\n") `shouldSatisfy` \case
      Just (EModifierValue "priority" True) -> True
      _ -> False

  it "rejects a modifier with a value it does not take" $
    runNegativeTestTypeCheck (struct "#[packed(3)]\n") `shouldSatisfy` \case
      Just (EModifierValue "packed" False) -> True
      _ -> False
