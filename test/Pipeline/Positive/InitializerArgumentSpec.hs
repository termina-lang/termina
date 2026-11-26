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

-- | Functions called with each option, status and result variant, from another
-- function and from a procedure.
monadicArgumentSrc :: String
monadicArgumentSrc =
    "function from_option(o : Option<u8>) -> u8 {\n" ++
    "    var r : u8;\n" ++
    "    match o {\n" ++
    "        case Some(v) => {\n" ++
    "            r = v;\n" ++
    "        }\n" ++
    "        case None => {\n" ++
    "            r = 0;\n" ++
    "        }\n" ++
    "    }\n" ++
    "    return r;\n" ++
    "}\n" ++
    "function from_status(s : Status<i32>) -> u8 {\n" ++
    "    var r : u8 = 0;\n" ++
    "    if s is Success {\n" ++
    "        r = 1;\n" ++
    "    }\n" ++
    "    return r;\n" ++
    "}\n" ++
    "function from_result(s : Result<u8, i32>) -> u8 {\n" ++
    "    var r : u8;\n" ++
    "    match s {\n" ++
    "        case Ok(v) => {\n" ++
    "            r = v;\n" ++
    "        }\n" ++
    "        case Error(_e) => {\n" ++
    "            r = 2;\n" ++
    "        }\n" ++
    "    }\n" ++
    "    return r;\n" ++
    "}\n" ++
    "function sum(x : u8) -> u8 {\n" ++
    "    return from_option(Some(x)) + from_option(None) + from_status(Success) + from_status(Failure(-3));\n" ++
    "}\n" ++
    "interface IC {\n" ++
    "    procedure set(&mut self, x : u8);\n" ++
    "};\n" ++
    "resource class C provides IC {\n" ++
    "    v : u8;\n" ++
    "    procedure set(&mut self, x : u8) {\n" ++
    "        self->v = self->v + from_result(Ok(x + 1)) + from_result(Error(7)) + sum(x);\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

spec :: Spec
spec = describe "Full pipeline: initializers as arguments" $ do
  it "passes each initializer as a compound literal" $
    goldenC "initializer_argument" (buildAndRenderModule "test" [("test", initializerArgumentSrc)])
  it "passes each option, status and result initializer as a compound literal" $
    goldenC "monadic_argument" (buildAndRenderModule "test" [("test", monadicArgumentSrc)])
