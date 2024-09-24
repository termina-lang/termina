module UT.PPrinter.TypeDefinition.EnumSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types
import qualified Data.Map as M

import Prettyprinter
import Control.Monad.Reader
import Generator.LanguageC.Printer
import Generator.CodeGen.TypeDefinition
import Generator.CodeGen.Common
import ControlFlow.Common
import Control.Monad.Except


enumWithOneRegularField :: AnnASTElement SemanticAnn
enumWithOneRegularField = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" []
  ] []) undefined

enumWithTwoRegularFields :: AnnASTElement SemanticAnn
enumWithTwoRegularFields = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" [],
    EnumVariant "variant1" []
  ] []) undefined

enumWithOneParameterizedField :: AnnASTElement SemanticAnn
enumWithOneParameterizedField = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" [UInt32]
  ] []) undefined

enumWithMultipleParameterizedFields :: AnnASTElement SemanticAnn
enumWithMultipleParameterizedFields = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" [UInt32],
    EnumVariant "variant1" [],
    EnumVariant "variant2" [UInt64, DefinedType "id1", Char],
    EnumVariant "variant3" [Int8, Array (Array Char (K (TInteger 20 DecRepr))) (K (TInteger 35 DecRepr))]
  ] []) undefined

renderTypeDefinitionDecl :: OptionTypes -> AnnASTElement SemanticAnn -> Text
renderTypeDefinitionDecl opts decl = 
  case runExcept . genBBAnnASTElement $ decl of
    Left err -> pack $ show err
    Right bbDecl ->
      case runReaderT (genTypeDefinitionDecl bbDecl) opts of
        Left err -> pack $ show err
        Right cDecls -> render $ vsep $ runReader (mapM pprint cDecls) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing enums" $ do
    it "Prints an enum with one regular variant" $ do
      renderTypeDefinitionDecl M.empty enumWithOneRegularField `shouldBe`
        pack (
            "\ntypedef enum {\n" ++
            "    id0__variant0\n" ++
            "} __enum_id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    __enum_id0_t __variant;\n" ++
            "} id0;")
    it "Prints an enum with two regular variants" $ do
      renderTypeDefinitionDecl M.empty enumWithTwoRegularFields `shouldBe`
        pack (
            "\ntypedef enum {\n" ++
            "    id0__variant0,\n" ++
            "    id0__variant1\n" ++
            "} __enum_id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    __enum_id0_t __variant;\n" ++
            "} id0;")
    it "Prints an enum with one parameterized variant" $ do
      renderTypeDefinitionDecl M.empty enumWithOneParameterizedField `shouldBe`
        pack (
            "\ntypedef enum {\n" ++
            "    id0__variant0\n" ++
            "} __enum_id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    uint32_t __0;\n" ++
            "} __enum_id0__variant0_params_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    __enum_id0_t __variant;\n" ++
            "    __enum_id0__variant0_params_t variant0;\n" ++
            "} id0;")
    it "Prints an enum with multiple parameterized variants" $ do
      renderTypeDefinitionDecl M.empty enumWithMultipleParameterizedFields `shouldBe`
        pack (
            "\ntypedef enum {\n" ++
            "    id0__variant0,\n" ++
            "    id0__variant1,\n" ++
            "    id0__variant2,\n" ++
            "    id0__variant3\n" ++
            "} __enum_id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    uint32_t __0;\n" ++
            "} __enum_id0__variant0_params_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    uint64_t __0;\n" ++
            "    id1 __1;\n" ++
            "    char __2;\n" ++
            "} __enum_id0__variant2_params_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    int8_t __0;\n" ++
            "    char __1[35][20];\n" ++
            "} __enum_id0__variant3_params_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    __enum_id0_t __variant;\n" ++
            "    union {\n" ++
            "        __enum_id0__variant0_params_t variant0;\n" ++
            "        __enum_id0__variant2_params_t variant2;\n" ++
            "        __enum_id0__variant3_params_t variant3;\n" ++
            "    };\n" ++
            "} id0;");