module UT.PPrinter.TypeDef.EnumSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text
import Semantic.Monad

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
    EnumVariant "variant3" [Int8, Vector (Vector Char (KC (I UInt32 20))) (KC (I UInt32 35))]
  ] []) undefined

renderTypedefDeclaration :: AnnASTElement SemanticAnns -> Text
renderTypedefDeclaration = render . ppHeaderASTElement

spec :: Spec
spec = do
  describe "Pretty printing enums" $ do
    it "Prints an enum with one regular variant" $ do
      renderTypedefDeclaration enumWithOneRegularField `shouldBe`
        pack (
            "typedef enum {\n" ++
            "    variant0\n" ++
            "} __enum_id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "\n" ++
            "    __enum_id0 __variant;\n" ++
            "\n" ++
            "} id0;\n")
    it "Prints an enum with two regular variants" $ do
      renderTypedefDeclaration enumWithTwoRegularFields `shouldBe`
        pack (
            "typedef enum {\n" ++
            "    variant0,\n" ++
            "    variant1\n" ++
            "} __enum_id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "\n" ++
            "    __enum_id0 __variant;\n" ++
            "\n" ++
            "} id0;\n")
    it "Prints an enum with one parameterized variant" $ do
      renderTypedefDeclaration enumWithOneParameterizedField `shouldBe`
        pack (
            "typedef enum {\n" ++
            "    variant0\n" ++
            "} __enum_id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "\n" ++
            "    __enum_id0 __variant;\n" ++
            "    \n" ++
            "    union {\n" ++
            "        struct {\n" ++
            "            uint32_t __0;\n" ++
            "        } __variant0;\n" ++
            "    };\n" ++
            "\n" ++
            "} id0;\n")
    it "Prints an enum with multiple parameterized variants" $ do
      renderTypedefDeclaration enumWithMultipleParameterizedFields `shouldBe`
        pack (
            "typedef enum {\n" ++
            "    variant0,\n" ++
            "    variant1,\n" ++
            "    variant2,\n" ++
            "    variant3\n" ++
            "} __enum_id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "\n" ++
            "    __enum_id0 __variant;\n" ++
            "    \n" ++
            "    union {\n" ++
            "        struct {\n" ++
            "            uint32_t __0;\n" ++
            "        } __variant0;\n" ++
            "        struct {\n" ++
            "            uint64_t __0;\n" ++
            "            id1 __1;\n" ++
            "            char __2;\n" ++
            "        } __variant2;\n" ++
            "        struct {\n" ++
            "            int8_t __0;\n" ++
            "            char __1[35][20];\n" ++
            "        } __variant3;\n" ++
            "    };\n" ++
            "\n" ++
            "} id0;\n");