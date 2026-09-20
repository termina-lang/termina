module Codegen.Positive.Printer.TypeDefinition.EnumSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types
import Utils.Annotations
import Generator.Monadic

import Codegen.Positive.Printer.Common
import qualified Data.Set as S
import qualified Data.Map.Strict as M

enumWithOneRegularField :: AnnASTElement SemanticAnn
enumWithOneRegularField = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" []
  ] []) (buildTypeAnn Internal)

enumWithTwoRegularFields :: AnnASTElement SemanticAnn
enumWithTwoRegularFields = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" [],
    EnumVariant "variant1" []
  ] []) (buildTypeAnn Internal)

enumWithOneParameterizedField :: AnnASTElement SemanticAnn
enumWithOneParameterizedField = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" [TUInt32]
  ] []) (buildTypeAnn Internal)

enumWithMultipleParameterizedFields :: AnnASTElement SemanticAnn
enumWithMultipleParameterizedFields = TypeDefinition
  (Enum "id0" [
    EnumVariant "variant0" [TUInt32],
    EnumVariant "variant1" [],
    EnumVariant "variant2" [TUInt64, TEnum "id1", TChar],
    EnumVariant "variant3" [TInt8, TArray (TArray TChar (buildConstExprTUSize 20)) (buildConstExprTUSize 35)]
  ] []) (buildTypeAnn Internal)

spec :: Spec
spec = do
  describe "Pretty printing enums" $ do
    it "Prints an enum with one regular variant" $ do
      renderTypeDefinitionDecl (MonadicTypes (S.fromList [TEnum "id0"]) (S.fromList [TEnum "id0"]) M.empty M.empty) enumWithOneRegularField `shouldBe`
        pack (
            "\ntypedef enum {\n" ++
            "    id0__variant0\n" ++
            "} termina__enum__id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    termina__enum__id0_t _variant;\n" ++
            "} id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    id0 _0;\n" ++
            "} termina__enum__Option__id0__Some_params_t;\n" ++
            "\n" ++     
            "typedef struct {\n" ++
            "    termina__enum__Option__id0__Some_params_t Some;\n" ++
            "    termina__enum__Option_t _variant;\n" ++
            "} Option__id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    id0 _0;\n" ++
            "} termina__enum__Status__id0__Failure_params_t;\n" ++
            "\n" ++     
            "typedef struct {\n" ++
            "    termina__enum__Status__id0__Failure_params_t Failure;\n" ++
            "    termina__enum__Status_t _variant;\n" ++
            "} Status__id0;")
    it "Prints an enum with two regular variants" $ do
      renderTypeDefinitionDecl (MonadicTypes S.empty (S.fromList [TEnum "id0"]) M.empty M.empty) enumWithTwoRegularFields `shouldBe`
        pack (
            "\ntypedef enum {\n" ++
            "    id0__variant0,\n" ++
            "    id0__variant1\n" ++
            "} termina__enum__id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    termina__enum__id0_t _variant;\n" ++
            "} id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    id0 _0;\n" ++
            "} termina__enum__Status__id0__Failure_params_t;\n" ++
            "\n" ++     
            "typedef struct {\n" ++
            "    termina__enum__Status__id0__Failure_params_t Failure;\n" ++
            "    termina__enum__Status_t _variant;\n" ++
            "} Status__id0;")
    it "Prints an enum with one parameterized variant" $ do
      renderTypeDefinitionDecl emptyMonadicTypes enumWithOneParameterizedField `shouldBe`
        pack (
            "\ntypedef enum {\n" ++
            "    id0__variant0\n" ++
            "} termina__enum__id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    uint32_t _0;\n" ++
            "} termina__enum__id0__variant0_params_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    termina__enum__id0_t _variant;\n" ++
            "    termina__enum__id0__variant0_params_t variant0;\n" ++
            "} id0;")
    it "Prints an enum with multiple parameterized variants" $ do
      renderTypeDefinitionDecl emptyMonadicTypes enumWithMultipleParameterizedFields `shouldBe`
        pack (
            "\ntypedef enum {\n" ++
            "    id0__variant0,\n" ++
            "    id0__variant1,\n" ++
            "    id0__variant2,\n" ++
            "    id0__variant3\n" ++
            "} termina__enum__id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    uint32_t _0;\n" ++
            "} termina__enum__id0__variant0_params_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    uint64_t _0;\n" ++
            "    id1 _1;\n" ++
            "    char _2;\n" ++
            "} termina__enum__id0__variant2_params_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    int8_t _0;\n" ++
            "    char _1[35U][20U];\n" ++
            "} termina__enum__id0__variant3_params_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    termina__enum__id0_t _variant;\n" ++
            "    union {\n" ++
            "        termina__enum__id0__variant0_params_t variant0;\n" ++
            "        termina__enum__id0__variant2_params_t variant2;\n" ++
            "        termina__enum__id0__variant3_params_t variant3;\n" ++
            "    };\n" ++
            "} id0;");