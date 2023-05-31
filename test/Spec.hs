import Test.Hspec
import PPrinter
import AST
import Parsing
import Data.Text hiding (concat)

{- | Struct type with a single field.
In Termina's context sytax:
struct id0 {
    field0 : u8;
};
-}
structWithOneField :: AnnASTElement Annotation
structWithOneField = TypeDefinition (Struct "id0" [FieldDefinition "field0" UInt8] [] undefined)

{- | Struct type with two fields.
In Termina's context sytax:
struct id0 {
    field0 : u8;
    field1 : u16;
};
-}
structWithTwoFields :: AnnASTElement Annotation
structWithTwoFields = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" UInt8,
    FieldDefinition "field1" UInt16
  ] [] undefined)

{- | Packed Struct type.
In Termina's context sytax:
#[packed]
struct id0 {
    field0 : u8;
    field1 : u16;
    field2 : [u16; 10 : u32];
};
-}
packedStruct :: AnnASTElement Annotation
packedStruct = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" UInt8,
    FieldDefinition "field1" UInt16,
    FieldDefinition "field2" (Vector UInt32 (K 10))
  ] [Modifier "packed" Nothing] undefined)

{- | Aligned Struct type.
In Termina's context sytax:
#[aligned(16)]
struct id0 {
    field0 : u8;
    field1 : u16;
    field2 : [u16; 10 : u32];
};
-}
alignedStruct :: AnnASTElement Annotation
alignedStruct = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" UInt8,
    FieldDefinition "field1" UInt16,
    FieldDefinition "field2" (Vector UInt32 (K 10))
  ] [Modifier "align" (Just (KC (I undefined 16)))] undefined)

{- | Aligned Struct type.
In Termina's context sytax:
#[packed]
#[aligned(16)]
struct id0 {
    field0 : u8;
    field1 : u16;
    field2 : [u16; 10 : u32];
};
-}
packedAndAlignedStruct :: AnnASTElement Annotation
packedAndAlignedStruct = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" UInt8,
    FieldDefinition "field1" UInt16,
    FieldDefinition "field2" (Vector UInt32 (K 10))
  ] [
      Modifier "packed" Nothing,
      Modifier "align" (Just (KC (I undefined 16)))
    ] undefined)


{- | Union type with three fields.
In Termina's context sytax:
union id0 {
    field0 : u8;
    field1 : u16;
    field2 : [u16; 10 : u32];
};
-}
unionWithThreeFields :: AnnASTElement Annotation
unionWithThreeFields = TypeDefinition
  (Union "id0" [
    FieldDefinition "field0" UInt8,
    FieldDefinition "field1" UInt16,
    FieldDefinition "field2" (Vector UInt32 (K 10))
  ] [] undefined)

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

main :: IO ()
main = hspec $ do
  describe "Pretty printing Structs" $ do
    it "Prints a struct with just one field" $ do
       ppProgram [structWithOneField] `shouldBe`
        pack (concat [
            "typedef struct {\n",
            "    uint8_t field0;\n",
            "} id0;"])
    it "Prints a struct with two fields" $ do
      ppProgram [structWithTwoFields] `shouldBe`
        pack (concat [
            "typedef struct {\n",
            "    uint8_t field0;\n",
            "    uint16_t field1;\n",
            "} id0;"])
    it "Prints a packed struct" $ do
      ppProgram [packedStruct] `shouldBe`
        pack (concat [
            "typedef struct {\n",
            "    uint8_t field0;\n",
            "    uint16_t field1;\n",
            "    uint32_t field2[10];\n",
            "} __attribute__((packed)) id0;"])
    it "Prints an aligned struct" $ do
      ppProgram [alignedStruct] `shouldBe`
        pack (concat [
            "typedef struct {\n",
            "    uint8_t field0;\n",
            "    uint16_t field1;\n",
            "    uint32_t field2[10];\n",
            "} __attribute__((align(16))) id0;"])
    it "Prints a packet & aligned struct" $ do
      ppProgram [packedAndAlignedStruct] `shouldBe`
        pack (concat [
            "typedef struct {\n",
            "    uint8_t field0;\n",
            "    uint16_t field1;\n",
            "    uint32_t field2[10];\n",
            "} __attribute__((packed, align(16))) id0;"])
  describe "Pretty printing unions" $ do
    it "Prints a union with just one field" $ do
      ppProgram [unionWithThreeFields] `shouldBe`
        pack (concat [
            "typedef union {\n",
            "    uint8_t field0;\n",
            "    uint16_t field1;\n",
            "    uint32_t field2[10];\n",
            "} id0;"])
  describe "Pretty printing enums" $ do
    it "Prints an enum with one regular field" $ do
      ppProgram [enumWithOneRegularField] `shouldBe`
        pack (concat [
            "typedef enum {\n",
            "    field0\n",
            "} __enum_id0;\n",
            "\n",
            "typedef struct {\n",
            "\n",
            "    __enum_id0 __variants;\n",
            "\n",
            "} id0;"])
    it "Prints an enum with two regular fields" $ do
      ppProgram [enumWithTwoRegularFields] `shouldBe`
        pack (concat [
            "typedef enum {\n",
            "    field0,\n",
            "    field1\n",
            "} __enum_id0;\n",
            "\n",
            "typedef struct {\n",
            "\n",
            "    __enum_id0 __variants;\n",
            "\n",
            "} id0;"])
    it "Prints an enum with one parameterized field" $ do
      ppProgram [enumWithOneParameterizedField] `shouldBe`
        pack (concat [
            "typedef enum {\n",
            "    field0\n",
            "} __enum_id0;\n",
            "\n",
            "typedef struct {\n",
            "\n",
            "    __enum_id0 __variants;\n",
            "    \n",
            "    union {\n",
            "        struct {\n",
            "            uint32_t __0;\n",
            "        } __field0;\n",
            "    };\n",
            "\n",
            "} id0;"])
    it "Prints an enum with multiple parameterized fields" $ do
      ppProgram [enumWithMultipleParameterizedFields] `shouldBe`
        pack (concat [
            "typedef enum {\n",
            "    field0,\n",
            "    field1,\n",
            "    field2,\n",
            "    field3\n",
            "} __enum_id0;\n",
            "\n",
            "typedef struct {\n",
            "\n",
            "    __enum_id0 __variants;\n",
            "    \n",
            "    union {\n",
            "        struct {\n",
            "            uint32_t __0;\n",
            "        } __field0;\n",
            "        struct {\n",
            "            uint64_t __0;\n",
            "            id1 __1;\n",
            "            char __2;\n",
            "        } __field2;\n",
            "        struct {\n",
            "            int8_t __0;\n",
            "            char __1[20][35];\n",
            "        } __field3;\n",
            "    };\n",
            "\n",
            "} id0;"])