module PPrinter.TypeDef.EnumSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Parsing
import Data.Text

enumWithOneRegularField :: AnnASTElement Annotation
enumWithOneRegularField = TypeDefinition
  (Enum "id0" [
    EnumVariant "field0" []
  ] [] undefined)

enumWithTwoRegularFields :: AnnASTElement Annotation
enumWithTwoRegularFields = TypeDefinition
  (Enum "id0" [
    EnumVariant "field0" [],
    EnumVariant "field1" []
  ] [] undefined)

enumWithOneParameterizedField :: AnnASTElement Annotation
enumWithOneParameterizedField = TypeDefinition
  (Enum "id0" [
    EnumVariant "field0" [UInt32]
  ] [] undefined)

enumWithMultipleParameterizedFields :: AnnASTElement Annotation
enumWithMultipleParameterizedFields = TypeDefinition
  (Enum "id0" [
    EnumVariant "field0" [UInt32],
    EnumVariant "field1" [],
    EnumVariant "field2" [UInt64, DefinedType "id1", Char],
    EnumVariant "field3" [Int8, Vector (Vector Char (K 20)) (K 35)]
  ] [] undefined)

renderSingleASTElement :: AnnASTElement a -> Text
renderSingleASTElement = render . ppHeaderASTElement ppEmptyDoc ppEmptyDoc

spec :: Spec
spec = do
  describe "Pretty printing enums" $ do
    it "Prints an enum with one regular field" $ do
      renderSingleASTElement enumWithOneRegularField `shouldBe`
        pack (
            "\n" ++
            "typedef enum {\n" ++
            "    field0\n" ++
            "} __enum_id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "\n" ++
            "    __enum_id0 __variant;\n" ++
            "\n" ++
            "} id0;" ++
            "\n")
    it "Prints an enum with two regular fields" $ do
      renderSingleASTElement enumWithTwoRegularFields `shouldBe`
        pack (
            "\n" ++
            "typedef enum {\n" ++
            "    field0,\n" ++
            "    field1\n" ++
            "} __enum_id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "\n" ++
            "    __enum_id0 __variant;\n" ++
            "\n" ++
            "} id0;" ++
            "\n")
    it "Prints an enum with one parameterized field" $ do
      renderSingleASTElement enumWithOneParameterizedField `shouldBe`
        pack (
            "\n" ++
            "typedef enum {\n" ++
            "    field0\n" ++
            "} __enum_id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "\n" ++
            "    __enum_id0 __variant;\n" ++
            "    \n" ++
            "    union {\n" ++
            "        struct {\n" ++
            "            uint32_t __0;\n" ++
            "        } __field0;\n" ++
            "    };\n" ++
            "\n" ++
            "} id0;" ++
            "\n")
    it "Prints an enum with multiple parameterized fields" $ do
      renderSingleASTElement enumWithMultipleParameterizedFields `shouldBe`
        pack (
            "\n" ++
            "typedef enum {\n" ++
            "    field0,\n" ++
            "    field1,\n" ++
            "    field2,\n" ++
            "    field3\n" ++
            "} __enum_id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "\n" ++
            "    __enum_id0 __variant;\n" ++
            "    \n" ++
            "    union {\n" ++
            "        struct {\n" ++
            "            uint32_t __0;\n" ++
            "        } __field0;\n" ++
            "        struct {\n" ++
            "            uint64_t __0;\n" ++
            "            id1 __1;\n" ++
            "            char __2;\n" ++
            "        } __field2;\n" ++
            "        struct {\n" ++
            "            int8_t __0;\n" ++
            "            char __1[20][35];\n" ++
            "        } __field3;\n" ++
            "    };\n" ++
            "\n" ++
            "} id0;" ++
            "\n")