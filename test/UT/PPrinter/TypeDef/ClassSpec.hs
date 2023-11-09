module UT.PPrinter.TypeDef.ClassSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text
import Semantic.Monad
import Data.Maybe


classWithOneProcedureAndZeroFields :: AnnASTElement SemanticAnns
classWithOneProcedureAndZeroFields = TypeDefinition (Class ResourceClass "id0" [
    ClassProcedure "procedure0" [
      Parameter "param0" UInt8,
      Parameter "param1" UInt16,
      Parameter "param2" UInt32,
      Parameter "param3" UInt64,
      Parameter "param4" Int8,
      Parameter "param5" Int16,
      Parameter "param6" Int32,
      Parameter "param7" Int64
    ] [] undefined
  ] []) undefined

classWithTwoProceduresAndZeroFields :: AnnASTElement SemanticAnns
classWithTwoProceduresAndZeroFields = TypeDefinition (Class ResourceClass "id0" [
    ClassProcedure "procedure0" [
      Parameter "param0" UInt8,
      Parameter "param1" (Option (DynamicSubtype (DefinedType "TMPacket")))
    ] [] undefined,
    ClassProcedure "procedure1" [
      Parameter "param0" UInt8,
      Parameter "param1" (Reference Mutable (Vector UInt8 (K 32)))
    ] [] undefined
  ] []) undefined

noHandlerClassWithoutOneProcedureAndZeroFields :: AnnASTElement SemanticAnns
noHandlerClassWithoutOneProcedureAndZeroFields = TypeDefinition (Class ResourceClass "id0" [
    ClassProcedure "procedure0" [] [] undefined
  ] []) undefined

classWithOneProcedureAndTwoFields :: AnnASTElement SemanticAnns
classWithOneProcedureAndTwoFields = TypeDefinition
  (Class ResourceClass "id0" [
    ClassField (FieldDefinition "field0" UInt8) undefined,
    ClassField (FieldDefinition "field1" (Vector UInt64 (K 24))) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] []) undefined

noHandlerClassWithOneEmptyProcedure :: AnnASTElement SemanticAnns
noHandlerClassWithOneEmptyProcedure = TypeDefinition
  (Class ResourceClass "id0" [
    ClassField (FieldDefinition "field0" UInt8) undefined,
    ClassField (FieldDefinition "field1" (Vector UInt64 (K 24))) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] [Modifier "no_handler" Nothing]) undefined

packedClass :: AnnASTElement SemanticAnns
packedClass = TypeDefinition
  (Class ResourceClass "id0" [
    ClassField (FieldDefinition "field0" UInt64) undefined,
    ClassField (FieldDefinition "field1" UInt16) undefined,
    ClassField (FieldDefinition "field2" (Vector (DefinedType "TMDescriptor") (K 32))) undefined,
    ClassProcedure "procedure0" [
      Parameter "param0" Char,
      Parameter "param1" (Reference Mutable (Vector UInt8 (K 16)))
    ] [] undefined
  ] [Modifier "packed" Nothing]) undefined

alignedClass :: AnnASTElement SemanticAnns
alignedClass = TypeDefinition
  (Class ResourceClass "id0" [
    ClassField (FieldDefinition "field0" UInt64) undefined,
    ClassField (FieldDefinition "field1" UInt16) undefined,
    ClassField (FieldDefinition "field2" (Vector (DefinedType "TMDescriptor") (K 32))) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] [Modifier "align" (Just (KC (I UInt32 16)))]) undefined

packedAndAlignedClass :: AnnASTElement SemanticAnns
packedAndAlignedClass = TypeDefinition
  (Class ResourceClass "id0" [
    ClassField (FieldDefinition "field0" UInt64) undefined,
    ClassField (FieldDefinition "field1" (DefinedType "TCDescriptor")) undefined,
    ClassField (FieldDefinition "field2" (Vector (DefinedType "TMDescriptor") (K 32))) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] [
      Modifier "packed" Nothing,
      Modifier "align" (Just (KC (I UInt32 16)))
    ]) undefined

classWithFixedLocationField :: AnnASTElement SemanticAnns
classWithFixedLocationField = TypeDefinition
  (Class ResourceClass "id0" [
    ClassField (FieldDefinition "field0" UInt32) undefined,
    ClassField (FieldDefinition "field1" (Location UInt32)) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] []) undefined

renderTypedefDeclaration :: AnnASTElement SemanticAnns -> Text
renderTypedefDeclaration = render . fromJust . ppHeaderASTElement

spec :: Spec
spec = do
  describe "Pretty printing classes" $ do
    it "Prints a class with one procedure and zero fields" $ do
      renderTypedefDeclaration classWithOneProcedureAndZeroFields `shouldBe`
        pack (
          "typedef struct {\n" ++
          "    __termina_resource_t __resource_id;\n" ++
          "} id0;\n" ++
          "\n" ++
          "void id0__procedure0(id0 * const self, uint8_t param0, uint16_t param1,\n" ++
          "                     uint32_t param2, uint64_t param3, int8_t param4,\n" ++
          "                     int16_t param5, int32_t param6, int64_t param7);\n")
    it "Prints a class with two procedures and zero fields" $ do
      renderTypedefDeclaration classWithTwoProceduresAndZeroFields `shouldBe`
        pack (
          "typedef struct {\n" ++
          "    __termina_resource_t __resource_id;\n" ++
          "} id0;\n" ++
          "\n" ++
          "void id0__procedure0(id0 * const self, uint8_t param0,\n" ++
          "                     __termina_option_dyn_t param1);\n" ++
          "\n" ++
          "void id0__procedure1(id0 * const self, uint8_t param0, uint8_t param1[32]);\n")
    it "Prints a class marked as no_handler with one procedure and zero fields" $ do
      renderTypedefDeclaration noHandlerClassWithoutOneProcedureAndZeroFields `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    __termina_resource_t __resource_id;\n" ++
            "} id0;\n" ++
            "\n" ++
            "void id0__procedure0(id0 * const self);\n")
    it "Prints a class marked as no_handler with two fields" $ do
      renderTypedefDeclaration noHandlerClassWithOneEmptyProcedure `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24];\n" ++
            "    __termina_resource_t __resource_id;\n" ++
            "} id0;\n" ++
            "\n" ++
            "void id0__procedure0(id0 * const self);\n")
    it "Prints a class with one procedure and two fields" $ do
      renderTypedefDeclaration classWithOneProcedureAndTwoFields `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24];\n" ++
            "    __termina_resource_t __resource_id;\n" ++
            "} id0;\n" ++
            "\n" ++
            "void id0__procedure0(id0 * const self);\n")
    it "Prints a packed class" $ do
      renderTypedefDeclaration packedClass `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "    __termina_resource_t __resource_id;\n" ++
            "} __attribute__((packed)) id0;\n" ++
            "\n" ++
            "void id0__procedure0(id0 * const self, char param0, uint8_t param1[16]);\n")
    it "Prints an aligned class" $ do
      renderTypedefDeclaration alignedClass `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "    __termina_resource_t __resource_id;\n" ++
            "} __attribute__((align(16))) id0;\n" ++
            "\n" ++
            "void id0__procedure0(id0 * const self);\n")
    it "Prints a packed & aligned class" $ do
      renderTypedefDeclaration packedAndAlignedClass `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    TCDescriptor field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "    __termina_resource_t __resource_id;\n" ++
            "} __attribute__((packed, align(16))) id0;\n" ++
            "\n" ++
            "void id0__procedure0(id0 * const self);\n")
    it "Prints a class with a fixed location field" $ do
      renderTypedefDeclaration classWithFixedLocationField `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint32_t field0;\n" ++
            "    volatile uint32_t * field1;\n" ++
            "    __termina_resource_t __resource_id;\n" ++
            "} id0;\n" ++
            "\n" ++
            "void id0__procedure0(id0 * const self);\n")