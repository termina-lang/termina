module UT.PPrinter.TypeDefinition.EnumSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types
import Utils.Annotations
import Generator.Monadic

import UT.PPrinter.Common
import qualified Data.Set as S
import qualified Data.Map as M

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
            "} __enum_id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    __enum_id0_t __variant;\n" ++
            "} id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    id0 __0;\n" ++
            "} __option_id0__Some_params_t;\n" ++
            "\n" ++     
            "typedef struct {\n" ++
            "    __option_id0__Some_params_t Some;\n" ++
            "    __enum_option_t __variant;\n" ++
            "} __option_id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    id0 __0;\n" ++
            "} __status_id0__Failure_params_t;\n" ++
            "\n" ++     
            "typedef struct {\n" ++
            "    __status_id0__Failure_params_t Failure;\n" ++
            "    __enum_status_t __variant;\n" ++
            "} __status_id0_t;")
    it "Prints an enum with two regular variants" $ do
      renderTypeDefinitionDecl (MonadicTypes S.empty (S.fromList [TEnum "id0"]) M.empty M.empty) enumWithTwoRegularFields `shouldBe`
        pack (
            "\ntypedef enum {\n" ++
            "    id0__variant0,\n" ++
            "    id0__variant1\n" ++
            "} __enum_id0_t;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    __enum_id0_t __variant;\n" ++
            "} id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    id0 __0;\n" ++
            "} __status_id0__Failure_params_t;\n" ++
            "\n" ++     
            "typedef struct {\n" ++
            "    __status_id0__Failure_params_t Failure;\n" ++
            "    __enum_status_t __variant;\n" ++
            "} __status_id0_t;")
    it "Prints an enum with one parameterized variant" $ do
      renderTypeDefinitionDecl emptyMonadicTypes enumWithOneParameterizedField `shouldBe`
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
      renderTypeDefinitionDecl emptyMonadicTypes enumWithMultipleParameterizedFields `shouldBe`
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
            "    char __1[35U][20U];\n" ++
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