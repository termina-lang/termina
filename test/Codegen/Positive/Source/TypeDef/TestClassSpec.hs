module Codegen.Positive.Source.TypeDef.TestClassSpec (spec) where

import Codegen.Positive.Source.Common

import Test.Hspec
import Data.Text hiding (empty)

-- | A test class with an out port, a port to test_checks, a port to a TestPool,
-- a method and two cases, one of them with an immutable self reference.
test0 :: String
test0 = "test class CTests {\n" ++
        "    tc_out : out u32;\n" ++
        "    chk : access TestChecks;\n" ++
        "    pool_state : access TestPoolAccess;\n" ++
        "    method send_one(&mut self) {\n" ++
        "        self->tc_out.send(1 : u32);\n" ++
        "        return;\n" ++
        "    }\n" ++
        "    case sends(&mut self) {\n" ++
        "        self->send_one();\n" ++
        "        self->chk.check(true);\n" ++
        "        return;\n" ++
        "    }\n" ++
        "    case counts(&self) {\n" ++
        "        var n : usize = 0;\n" ++
        "        self->pool_state.in_use(&mut n);\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};\n"

spec :: Spec
spec = do
  describe "Pretty printing test classes" $ do
    it "Prints declaration of test class CTests" $ do
      renderTestHeader test0 `shouldBe`
        pack ("#ifndef TEST_H__\n" ++
              "#define TEST_H__\n" ++
              "\n" ++
              "#include <termina.h>\n" ++
              "\n" ++
              "typedef struct {\n" ++
              "    struct {\n" ++
              "        void * _that;\n" ++
              "        void (* in_use)(const termina__event_t * const termina__ev,\n" ++
              "                        const void * const termina__this, size_t * const n);\n" ++
              "    } pool_state;\n" ++
              "    struct {\n" ++
              "        void * _that;\n" ++
              "        void (* check)(const termina__event_t * const termina__ev,\n" ++
              "                       void * const termina__this, const _Bool cond);\n" ++
              "    } chk;\n" ++
              "    termina__out_port_t tc_out;\n" ++
              "} CTests;\n" ++
              "\n" ++
              "void CTests__counts(const termina__event_t * const termina__ev,\n" ++
              "                    const void * const termina__this);\n" ++
              "\n" ++
              "void CTests__sends(const termina__event_t * const termina__ev,\n" ++
              "                   void * const termina__this);\n" ++
              "\n" ++
              "#endif\n")
    it "Prints definition of test class CTests" $ do
      renderTestSource test0 `shouldBe`
        pack ("\n" ++
              "#include \"test.h\"\n" ++
              "\n" ++
              "static void CTests__send_one(const termina__event_t * const termina__ev,\n" ++
              "                             CTests * const self);\n" ++
              "\n" ++
              "void CTests__counts(const termina__event_t * const termina__ev,\n" ++
              "                    const void * const termina__this) {\n" ++
              "    \n" ++
              "    const CTests * self = (const CTests *)termina__this;\n" ++
              "\n" ++
              "    size_t n = 0U;\n" ++
              "\n" ++
              "    self->pool_state.in_use(termina__ev, self->pool_state._that, &n);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "static void CTests__send_one(const termina__event_t * const termina__ev,\n" ++
              "                             CTests * const self) {\n" ++
              "    {\n" ++
              "        const uint32_t msg = 1U;\n" ++
              "        termina__out_port__send(termina__ev, self->tc_out, (const void *)&msg);\n" ++
              "    }\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n" ++
              "\n" ++
              "void CTests__sends(const termina__event_t * const termina__ev,\n" ++
              "                   void * const termina__this) {\n" ++
              "    \n" ++
              "    CTests * self = (CTests *)termina__this;\n" ++
              "\n" ++
              "    CTests__send_one(termina__ev, self);\n" ++
              "\n" ++
              "    self->chk.check(termina__ev, self->chk._that, true);\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
