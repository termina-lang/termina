module UT.PPrinter.TypeDefinition.StructSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types
import Utils.Annotations
import Generator.Monadic

import UT.PPrinter.Common
import qualified Data.Set as S
import qualified Data.Map as M

{- | Struct type with a single field.
In Termina's concrete sytax:
struct id0 {
    field0 : u8;
};
-}
structWithOneField :: AnnASTElement SemanticAnn
structWithOneField = TypeDefinition 
  (Struct "id0" [
    FieldDefinition "field0" TUInt8 (buildExpAnn Internal TUInt8)
  ] []) (buildTypeAnn Internal)

{- | Struct type with two fields.
In Termina's concrete sytax:
struct id0 {
    field0 : u8;
    field1 : u16;
};
-}
structWithTwoFields :: AnnASTElement SemanticAnn
structWithTwoFields = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" TUInt8 (buildExpAnn Internal TUInt8),
    FieldDefinition "field1" TUInt16 (buildExpAnn Internal TUInt16)
  ] []) (buildTypeAnn Internal)

{- | Packed Struct type.
In Termina's concrete sytax:
#[packed]
struct id0 {
    field0 : u8;
    field1 : u16;
    field2 : [u16; 10 : u32];
};
-}
packedStruct :: AnnASTElement SemanticAnn
packedStruct = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" TUInt8 (buildExpAnn Internal TUInt8),
    FieldDefinition "field1" TUInt16 (buildExpAnn Internal TUInt16),
    FieldDefinition "field2" (TArray TUInt32 (buildConstExprTUSize 10)) 
      (buildExpAnn Internal (TArray TUInt32 (buildConstExprTUSize 10)))
  ] [Modifier "packed" Nothing]) (buildTypeAnn Internal)

{- | Aligned Struct type.
In Termina's concrete sytax:
#[aligned(16)]
struct id0 {
    field0 : u8;
    field1 : u16;
    field2 : [u16; 10 : u32];
};
-}
alignedStruct :: AnnASTElement SemanticAnn
alignedStruct = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" TUInt8 (buildExpAnn Internal TUInt8),
    FieldDefinition "field1" TUInt16 (buildExpAnn Internal TUInt16),
    FieldDefinition "field2" (TArray TUInt32 (buildConstExprTUSize 10))
      (buildExpAnn Internal (TArray TUInt32 (buildConstExprTUSize 10)))
  ] [Modifier "aligned" (Just (I (TInteger 16 DecRepr) (Just TUInt32)))]) (buildTypeAnn Internal)

packedAndAlignedStruct :: AnnASTElement SemanticAnn
packedAndAlignedStruct = TypeDefinition
  (Struct "id0" [
    FieldDefinition "field0" TUInt8 (buildExpAnn Internal TUInt8),
    FieldDefinition "field1" TUInt16 (buildExpAnn Internal TUInt16),
    FieldDefinition "field2" (TArray TUInt32 (buildConstExprTUSize 10))
      (buildExpAnn Internal (TArray TUInt32 (buildConstExprTUSize 10)))
  ] [
      Modifier "packed" Nothing,
      Modifier "aligned" (Just (I (TInteger 16 DecRepr) (Just TUInt32)))
    ]) (buildTypeAnn Internal)

spec :: Spec
spec = do
  describe "Pretty printing Structs" $ do
    it "Prints a struct with just one field" $ do
       renderTypeDefinitionDecl (MonadicTypes (S.fromList [TStruct "id0"]) (S.fromList [TStruct "id0"]) M.empty M.empty) structWithOneField `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    uint8_t field0;\n" ++
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
    it "Prints a struct with two fields" $ do
      renderTypeDefinitionDecl (MonadicTypes S.empty (S.fromList [TStruct "id0"]) M.empty M.empty) structWithTwoFields `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
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
    it "Prints a packed struct" $ do
      renderTypeDefinitionDecl emptyMonadicTypes packedStruct `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10U];\n" ++
            "} __attribute__((packed)) id0;")
    it "Prints an aligned struct" $ do
      renderTypeDefinitionDecl emptyMonadicTypes alignedStruct `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10U];\n" ++
            "} __attribute__((aligned(16U))) id0;")
    it "Prints a packet & aligned struct" $ do
      renderTypeDefinitionDecl emptyMonadicTypes packedAndAlignedStruct `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10U];\n" ++
            "} __attribute__((packed, aligned(16U))) id0;")