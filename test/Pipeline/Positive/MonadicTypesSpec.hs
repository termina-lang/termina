-- | The option types that the generator has to emit: an option held in an
-- array is one of them, whether the array is a field, a parameter or a local.
module Pipeline.Positive.MonadicTypesSpec (spec) where

import Pipeline.Common
import Generator.Monadic
import Semantic.AST

import qualified Data.Set as S
import Test.Hspec

-- | A class field that is an array of options of a struct.
optionArrayFieldSrc :: String
optionArrayFieldSrc =
    "struct Pin {\n" ++
    "    port : usize;\n" ++
    "    pin : usize;\n" ++
    "};\n" ++
    "interface IC {\n" ++
    "    procedure set(&mut self, i : usize);\n" ++
    "};\n" ++
    "resource class C provides IC {\n" ++
    "    pins : [Option<Pin>; 4];\n" ++
    "    v : usize;\n" ++
    "    procedure set(&mut self, i : usize) {\n" ++
    "        match self->pins[i] {\n" ++
    "            case Some(p) => {\n" ++
    "                self->v = self->v + p.pin;\n" ++
    "            }\n" ++
    "            case None => {\n" ++
    "            }\n" ++
    "        }\n" ++
    "        return;\n" ++
    "    }\n" ++
    "};\n"

spec :: Spec
spec = describe "Monadic types to generate" $
  it "includes the option held in an array field" $
    case runTypedModule "test" [("test", optionArrayFieldSrc)] of
      Left _ -> expectationFailure "the program does not type check"
      Right prog ->
        S.member (TStruct "Pin")
          (optionTypes (runMapMonadicTypesAnnotatedProgram emptyMonadicTypes prog))
          `shouldBe` True
