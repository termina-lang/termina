module UT.PPrinter.TypeDef.StructSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text
import Semantic.Monad
import Semantic.Option (OptionMap)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

{- | Struct type with a single field.
In Termina's concrete sytax:
struct id0 {
    field0 : u8;
};
-}
structWithOneField :: AnnASTElement SemanticAnns
structWithOneField = TypeDefinition (Struct "id0" [FieldDefinition "field0" UInt8] []) undefined

{- | Struct type with two fields.
In Termina's concrete sytax:
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
In Termina's concrete sytax:
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
    FieldDefinition "field2" (Vector UInt32 (K 10))
  ] [Modifier "packed" Nothing]) undefined

{- | Aligned Struct type.
In Termina's concrete sytax:
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
    FieldDefinition "field2" (Vector UInt32 (K 10))
  ] [Modifier "align" (Just (KC (I UInt32 16)))]) undefined

{- | Aligned Struct type.
In Termina's concrete sytax:
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
    FieldDefinition "field2" (Vector UInt32 (K 10))
  ] [
      Modifier "packed" Nothing,
      Modifier "align" (Just (KC (I UInt32 16)))
    ]) undefined

renderTypedefDeclaration :: OptionMap -> AnnASTElement SemanticAnns -> Text
renderTypedefDeclaration opts = render . fromJust . (ppHeaderASTElement opts)

spec :: Spec
spec = do
  describe "Pretty printing Structs" $ do
    it "Prints a struct with just one field" $ do
       renderTypedefDeclaration (M.fromList [(DefinedType "id0", S.fromList [Option (DefinedType "id0")])]) structWithOneField `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "} id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    id0 __0;\n" ++
            "} __option__id0_params_t;\n" ++
            "\n" ++     
            "typedef struct {\n" ++
            "\n" ++         
            "    __option__id0_params_t Some;\n" ++
            "\n" ++     
            "    __enum_option_t __variant;\n" ++
            "\n" ++     
            "} __option__id0_t;\n")
    it "Prints a struct with two fields" $ do
      renderTypedefDeclaration M.empty structWithTwoFields `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "} id0;\n")
    it "Prints a packed struct" $ do
      renderTypedefDeclaration M.empty packedStruct `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((packed)) id0;\n")
    it "Prints an aligned struct" $ do
      renderTypedefDeclaration M.empty alignedStruct `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((align(16))) id0;\n")
    it "Prints a packet & aligned struct" $ do
      renderTypedefDeclaration M.empty packedAndAlignedStruct `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((packed, align(16))) id0;\n")