-- | Full-pipeline golden for enum variant and struct initializers passed as the
-- arguments of a call: each one becomes a compound literal (T){ ... }.
module Pipeline.Positive.InitializerArgumentSpec (spec) where

import Pipeline.Common
import Golden

import Test.Hspec

-- | A function called with a struct initializer and with enum variants with
-- and without data, from another function and from a procedure.
initializerArgumentSrc :: String
initializerArgumentSrc =
    "enum Level {\n" ++
    "    Low,\n" ++
    "    High,\n" ++
    "    Value(u8)\n" ++
    "};\n" ++
    "struct Pin {\n" ++
    "    port : usize;\n" ++
    "    pin : usize;\n" ++
    "};\n" ++
    "function code(p : Pin, l : Level) -> usize {\n" ++
    "    var r : usize = p.port + p.pin;\n" ++
    "    match l {\n" ++
    "        case Low => {\n" ++
    "        }\n" ++
    "        case High => {\n" ++
    "            r = r + 1;\n" ++
    "        }\n" ++
    "        case Value(v) => {\n" ++
    "            r = r + v as usize;\n" ++
    "        }\n" ++
    "    }\n" ++
    "    return r;\n" ++
    "}\n" ++
    "function high_code(p : Pin) -> usize {\n" ++
    "    return code(p, Level::High);\n" ++
    "}\n" ++
    "interface IC {\n" ++
    "    procedure set(&mut self, l : Level);\n" ++
    "};\n" ++
    "resource class C provides IC {\n" ++
    "    v : usize;\n" ++
    "    procedure set(&mut self, l : Level) {\n" ++
    "        self->v = self->v + code({port = 1, pin = 2}, Level::Value(3)) + code({port = 0, pin = 0}, l);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

spec :: Spec
spec = describe "Full pipeline: initializers as arguments" $
  it "passes each initializer as a compound literal" $
    goldenC "initializer_argument" (buildAndRenderModule "test" [("test", initializerArgumentSrc)])
