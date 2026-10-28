-- | The reading of the lines of GDB/MI, from lines that gdb 15 wrote in a run
-- of a test under the executor of GDB.
module Runner.MiSpec (spec) where

import Runner.Mi

import Test.Hspec

spec :: Spec
spec = describe "Runner: lines of GDB/MI" $ do

  it "reads a stop at a breakpoint with the function of its frame" $ do
    let line = "*stopped,reason=\"breakpoint-hit\",disp=\"keep\",bkptno=\"1\",frame={addr=\"0x00005555555579b8\",func=\"termina__test__init\",args=[],file=\"record.c\",line=\"15\"},thread-id=\"1\""
    case parseMiLine line of
      MiAsync '*' "stopped" results -> do
        miText "reason" results `shouldBe` Just "breakpoint-hit"
        case miField "frame" results of
          Just (MiTuple frame) -> miText "func" frame `shouldBe` Just "termina__test__init"
          other -> expectationFailure ("no frame: " ++ show other)
      other -> expectationFailure ("not a stop: " ++ show other)

  it "reads the result of a command, an error and its message" $ do
    parseMiLine "1^done" `shouldBe` MiResult (Just 1) "done" []
    parseMiLine "^running" `shouldBe` MiResult Nothing "running" []
    parseMiLine "^error,msg=\"No symbol \\\"foo\\\" in current context.\"" `shouldBe`
      MiResult Nothing "error" [("msg", MiString "No symbol \"foo\" in current context.")]

  it "reads a list of frames, whose results lose their names" $
    parseMiLine "^done,stack=[frame={level=\"0\",func=\"termina__exec__reboot\"},frame={level=\"1\",func=\"main\"}]" `shouldBe`
      MiResult Nothing "done" [("stack", MiList [
          MiTuple [("level", MiString "0"), ("func", MiString "termina__exec__reboot")],
          MiTuple [("level", MiString "1"), ("func", MiString "main")]])]

  it "reads the text of the console, the prompt and the output of the program" $ do
    parseMiLine "~\"15\\t    termina__os__test__open();\\n\"" `shouldBe` MiStream '~' "15\t    termina__os__test__open();\n"
    parseMiLine "(gdb) " `shouldBe` MiPrompt
    parseMiLine "hello from the binary" `shouldBe` MiOther "hello from the binary"

  it "escapes a command of the console as a string of GDB/MI" $
    miEscape "dump binary value /tmp/a \"b\".bin termina__test__header" `shouldBe`
      "\"dump binary value /tmp/a \\\"b\\\".bin termina__test__header\""
