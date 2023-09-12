module UT.PPrinter.TypeDef.StructSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text
import Semantic.Monad
import Data.Maybe

{- | Struct type with a single field.
In Termina's context sytax:
struct id0 {
    field0 : u8;
};
-}
structWithOneField :: AnnASTElement SemanticAnns
structWithOneField = TypeDefinition (Struct "id0" [FieldDefinition "field0" UInt8] []) undefined

{- | Struct type with two fields.
In Termina's context sytax:
struct id0 {
    field0 : u8;
    field1 : u16;
};
-}
structWithTwoFields :: AnnASTElement SemanticAnns
structWithTwoFields = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" UInt8,
    FieldDefinition "field1" UInt16
  ] []) undefined

{- | Packed Struct type.
In Termina's context sytax:
#[packed]
struct id0 {
    field0 : u8;
    field1 : u16;
    field2 : [u16; 10 : u32];
};
-}
packedStruct :: AnnASTElement SemanticAnns
packedStruct = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" UInt8,
    FieldDefinition "field1" UInt16,
    FieldDefinition "field2" (Vector UInt32 (KC (I UInt32 10)))
  ] [Modifier "packed" Nothing]) undefined

{- | Aligned Struct type.
In Termina's context sytax:
#[aligned(16)]
struct id0 {
    field0 : u8;
    field1 : u16;
    field2 : [u16; 10 : u32];
};
-}
alignedStruct :: AnnASTElement SemanticAnns
alignedStruct = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" UInt8,
    FieldDefinition "field1" UInt16,
    FieldDefinition "field2" (Vector UInt32 (KC (I UInt32 10)))
  ] [Modifier "align" (Just (KC (I UInt32 16)))]) undefined

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
packedAndAlignedStruct :: AnnASTElement SemanticAnns
packedAndAlignedStruct = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" UInt8,
    FieldDefinition "field1" UInt16,
    FieldDefinition "field2" (Vector UInt32 (KC (I UInt32 10)))
  ] [
      Modifier "packed" Nothing,
      Modifier "align" (Just (KC (I UInt32 16)))
    ]) undefined

renderTypedefDeclaration :: AnnASTElement SemanticAnns -> Text
renderTypedefDeclaration = render . fromJust . ppHeaderASTElement

spec :: Spec
spec = do
  describe "Pretty printing Structs" $ do
    it "Prints a struct with just one field" $ do
       renderTypedefDeclaration structWithOneField `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "} id0;\n")
    it "Prints a struct with two fields" $ do
      renderTypedefDeclaration structWithTwoFields `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "} id0;\n")
    it "Prints a packed struct" $ do
      renderTypedefDeclaration packedStruct `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((packed)) id0;\n")
    it "Prints an aligned struct" $ do
      renderTypedefDeclaration alignedStruct `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((align(16))) id0;\n")
    it "Prints a packet & aligned struct" $ do
      renderTypedefDeclaration packedAndAlignedStruct `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((packed, align(16))) id0;\n")