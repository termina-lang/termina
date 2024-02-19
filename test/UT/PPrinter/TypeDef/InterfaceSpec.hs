module UT.PPrinter.TypeDef.InterfaceSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text
import Semantic.Monad
import Semantic.Option (OptionMap)
import Data.Maybe
import qualified Data.Map as M

interfaceWithOneProcedure :: AnnASTElement SemanticAnns
interfaceWithOneProcedure = TypeDefinition (Interface "iface0" [
    InterfaceProcedure "procedure0" [
      Parameter "param0" UInt8,
      Parameter "param1" UInt16,
      Parameter "param2" UInt32,
      Parameter "param3" UInt64,
      Parameter "param4" Int8,
      Parameter "param5" Int16,
      Parameter "param6" Int32,
      Parameter "param7" Int64
    ] undefined
  ] []) undefined

renderTypedefDeclaration :: OptionMap -> AnnASTElement SemanticAnns -> Text
renderTypedefDeclaration opts = render . fromJust . (ppHeaderASTElement opts)

spec :: Spec
spec = do
  describe "Pretty printing classes" $ do
    it "Prints an interface with one procedure" $ do
      renderTypedefDeclaration M.empty interfaceWithOneProcedure `shouldBe`
        pack (
          "typedef struct {\n" ++
          "    void * __that;\n" ++
          "    void (*procedure0)(void * __self, uint8_t param0, uint16_t param1,\n" ++
          "                       uint32_t param2, uint64_t param3, int8_t param4,\n" ++
          "                       int16_t param5, int32_t param6, int64_t param7);\n" ++
          "} iface0;\n")