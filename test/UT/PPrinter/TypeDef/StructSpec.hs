module UT.PPrinter.TypeDef.StructSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Parsing
import Data.Text

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
    FieldDefinition "field2" (Vector UInt32 (KC (I UInt32 10)))
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
    FieldDefinition "field2" (Vector UInt32 (KC (I UInt32 10)))
  ] [Modifier "align" (Just (KC (I UInt32 16)))] undefined)

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
    FieldDefinition "field2" (Vector UInt32 (KC (I UInt32 10)))
  ] [
      Modifier "packed" Nothing,
      Modifier "align" (Just (KC (I UInt32 16)))
    ] undefined)

renderSingleASTElement :: AnnASTElement a -> Text
renderSingleASTElement = render . ppHeaderASTElement ppEmptyDoc ppEmptyDoc

spec :: Spec
spec = do
  describe "Pretty printing Structs" $ do
    it "Prints a struct with just one field" $ do
       renderSingleASTElement structWithOneField `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "} id0;\n" ++
            "\n" ++
            "uint8_t __id0__eq(id0 * __lhs, id0 * __rhs);\n")
    it "Prints a struct with two fields" $ do
      renderSingleASTElement structWithTwoFields `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "} id0;\n" ++
            "\n" ++
            "uint8_t __id0__eq(id0 * __lhs, id0 * __rhs);\n")
    it "Prints a packed struct" $ do
      renderSingleASTElement packedStruct `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((packed)) id0;\n" ++
            "\n" ++
            "uint8_t __id0__eq(id0 * __lhs, id0 * __rhs);\n")
    it "Prints an aligned struct" $ do
      renderSingleASTElement alignedStruct `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((align(16))) id0;\n" ++
            "\n" ++
            "uint8_t __id0__eq(id0 * __lhs, id0 * __rhs);\n")
    it "Prints a packet & aligned struct" $ do
      renderSingleASTElement packedAndAlignedStruct `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((packed, align(16))) id0;\n" ++
            "\n" ++
            "uint8_t __id0__eq(id0 * __lhs, id0 * __rhs);\n")