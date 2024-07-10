module UT.PPrinter.TypeDefinition.StructSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text
import Semantic.Monad
import qualified Data.Map as M
import qualified Data.Set as S

import Prettyprinter
import Control.Monad.Reader
import Generator.LanguageC.Printer
import Generator.CodeGen.TypeDefinition
import Generator.CodeGen.Common

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
    FieldDefinition "field2" (Array UInt32 (K (TInteger 10 DecRepr)))
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
    FieldDefinition "field2" (Array UInt32 (K (TInteger 10 DecRepr)))
  ] [Modifier "aligned" (Just (I (TInteger 16 DecRepr) (Just UInt32)))]) undefined

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
    FieldDefinition "field2" (Array UInt32 (K (TInteger 10 DecRepr)))
  ] [
      Modifier "packed" Nothing,
      Modifier "aligned" (Just (I (TInteger 16 DecRepr) (Just UInt32)))
    ]) undefined

renderTypeDefinitionDecl :: OptionTypes -> AnnASTElement SemanticAnns -> Text
renderTypeDefinitionDecl opts decl = 
  case runReaderT (genTypeDefinitionDecl decl) opts of
    Left err -> pack $ show err
    Right cDecls -> render $ vsep $ runReader (mapM pprint cDecls) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing Structs" $ do
    it "Prints a struct with just one field" $ do
       renderTypeDefinitionDecl (M.fromList [(DefinedType "id0", S.fromList [Option (DefinedType "id0")])]) structWithOneField `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "} id0;\n" ++
            "\n" ++
            "typedef struct {\n" ++
            "    id0 __0;\n" ++
            "} __option_id0_params_t;\n" ++
            "\n" ++     
            "typedef struct {\n" ++
            "    __option_id0_params_t Some;\n" ++
            "    __enum_option_t __variant;\n" ++
            "} __option_id0_t;")
    it "Prints a struct with two fields" $ do
      renderTypeDefinitionDecl M.empty structWithTwoFields `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "} id0;")
    it "Prints a packed struct" $ do
      renderTypeDefinitionDecl M.empty packedStruct `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((packed)) id0;")
    it "Prints an aligned struct" $ do
      renderTypeDefinitionDecl M.empty alignedStruct `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((aligned(16))) id0;")
    it "Prints a packet & aligned struct" $ do
      renderTypeDefinitionDecl M.empty packedAndAlignedStruct `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} __attribute__((packed, aligned(16))) id0;")