-- | Full-pipeline golden for struct and enum variant initializers passed to
-- send, and to the fire of a test emitter: each one initializes the
-- temporary that goes by address, as the initializer of a declaration.
module Pipeline.Positive.SendInitializerSpec (spec) where

import Pipeline.Common
import Golden

import qualified Data.Map.Strict as M
import Data.Text (pack, unpack)
import Test.Hspec

-- | A task that sends a struct initializer and two enum variants, and a case
-- that fires an enum variant and a struct initializer.
sendInitializerSrc :: String
sendInitializerSrc =
    "enum Level {\n" ++
    "    Low,\n" ++
    "    Value(u8)\n" ++
    "};\n" ++
    "struct Pin {\n" ++
    "    port : usize;\n" ++
    "    pin : usize;\n" ++
    "};\n" ++
    "task class CSender {\n" ++
    "    levels_in : sink Level triggers run;\n" ++
    "    pins_in : sink Pin triggers on_pin;\n" ++
    "    pins : out Pin;\n" ++
    "    levels : out Level;\n" ++
    "    action run(&priv self, _l : Level) {\n" ++
    "        self->pins.send({port = 1, pin = 2});\n" ++
    "        self->levels.send(Level::Value(3));\n" ++
    "        self->levels.send(Level::Low);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "    action on_pin(&priv self, p : Pin) {\n" ++
    "        self->pins.send(p);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "test class CSenderTests {\n" ++
    "    level : access TestEmitterAccess<Level>;\n" ++
    "    pin : access TestEmitterAccess<Pin>;\n" ++
    "    pins : access TestChannelAccess<Pin>;\n" ++
    "    levels : access TestChannelAccess<Level>;\n" ++
    "    case sends(&mut self) {\n" ++
    "        var n : usize = 0;\n" ++
    "        self->level.fire(Level::Value(4));\n" ++
    "        self->pin.fire({port = 3, pin = 4});\n" ++
    "        self->pins.pending(&mut n);\n" ++
    "        self->levels.pending(&mut n);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n" ++
    "emitter level : TestEmitter<Level>;\n" ++
    "emitter pin : TestEmitter<Pin>;\n" ++
    "channel pins : TestChannel<Pin; 2>;\n" ++
    "channel levels : TestChannel<Level; 2>;\n" ++
    "#[priority(5)]\n" ++
    "task sender : CSender = { levels_in <- level, pins_in <- pin, pins -> pins, levels -> levels };\n" ++
    "test sender_tests : CSenderTests = { level <-> level, pin <-> pin, pins <-> pins, levels <-> levels };\n"

-- | A task that sends an enum variant through a port of a struct.
wrongInitializerSrc :: String
wrongInitializerSrc =
    "enum Level {\n" ++
    "    Low,\n" ++
    "    Value(u8)\n" ++
    "};\n" ++
    "struct Pin {\n" ++
    "    port : usize;\n" ++
    "    pin : usize;\n" ++
    "};\n" ++
    "task class CSender {\n" ++
    "    levels_in : sink Level triggers run;\n" ++
    "    pins : out Pin;\n" ++
    "    action run(&priv self, _l : Level) {\n" ++
    "        self->pins.send(Level::Value(3));\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

spec :: Spec
spec = describe "Full pipeline: initializers sent and fired" $ do
  it "initializes the temporary of send and of fire with each initializer" $
    case runTestBuild sendInitializerSrc of
      Left err -> expectationFailure $ "pipeline failed: " ++ unpack (failMessage err)
      Right modules -> maybe (expectationFailure "no module test") (goldenC "send_initializer") (M.lookup "test" modules)
  it "rejects an initializer of another type than the port, as in a call" $
    compileErrorCode wrongInitializerSrc `shouldBe` Just (pack "SE-087")
