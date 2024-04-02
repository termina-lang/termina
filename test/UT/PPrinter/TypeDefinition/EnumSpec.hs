module UT.PPrinter.TypeDefinition.EnumSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text
import Semantic.Monad
import qualified Data.Map as M

import Prettyprinter
import Control.Monad.Reader
import Generator.LanguageC.Printer
import Generator.TypeDefinition
import Generator.Common


enumWithOneRegularField :: AnnASTElement SemanticAnns
enumWithOneRegularField = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" []
  ] []) undefined

enumWithTwoRegularFields :: AnnASTElement SemanticAnns
enumWithTwoRegularFields = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" [],
    EnumVariant "variant1" []
  ] []) undefined

enumWithOneParameterizedField :: AnnASTElement SemanticAnns
enumWithOneParameterizedField = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" [UInt32]
  ] []) undefined

enumWithMultipleParameterizedFields :: AnnASTElement SemanticAnns
enumWithMultipleParameterizedFields = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" [UInt32],
    EnumVariant "variant1" [],
    EnumVariant "variant2" [UInt64, DefinedType "id1", Char],
    EnumVariant "variant3" [Int8, Vector (Vector Char (K 20)) (K 35)]
  ] []) undefined

renderTypeDefinitionDecl :: OptionTypes -> AnnASTElement SemanticAnns -> Text
renderTypeDefinitionDecl opts decl = 
  case runReaderT (genTypeDefinitionDecl decl) opts of
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