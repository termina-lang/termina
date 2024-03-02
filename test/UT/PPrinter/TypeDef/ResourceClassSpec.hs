module UT.PPrinter.TypeDef.ResourceClassSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text
import Semantic.Monad
import Semantic.Option (OptionMap)
import Data.Maybe
import qualified Data.Map as M


classWithOneProcedureAndZeroFields :: AnnASTElement SemanticAnns
classWithOneProcedureAndZeroFields = TypeDefinition (Class ResourceClass "Class0" [
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
  ] ["Interface0"] []) undefined

classWithTwoProceduresAndZeroFields :: AnnASTElement SemanticAnns
classWithTwoProceduresAndZeroFields = TypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure "procedure0" [
      Parameter "param0" UInt8,
      Parameter "param1" (Option (DynamicSubtype (DefinedType "TMPacket")))
    ] [] undefined,
    ClassProcedure "procedure1" [
      Parameter "param0" UInt8,
      Parameter "param1" (Reference Mutable (Vector UInt8 (K 32)))
    ] [] undefined
  ] ["Interface0"] []) undefined

noHandlerClassWithoutOneProcedureAndZeroFields :: AnnASTElement SemanticAnns
noHandlerClassWithoutOneProcedureAndZeroFields = TypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure "procedure0" [] [] undefined
  ] ["Interface0"] []) undefined

classWithOneProcedureAndTwoFields :: AnnASTElement SemanticAnns
classWithOneProcedureAndTwoFields = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" UInt8) undefined,
    ClassField (FieldDefinition "field1" (Vector UInt64 (K 24))) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] ["Interface0"] []) undefined

noHandlerClassWithOneEmptyProcedure :: AnnASTElement SemanticAnns
noHandlerClassWithOneEmptyProcedure = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" UInt8) undefined,
    ClassField (FieldDefinition "field1" (Vector UInt64 (K 24))) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] ["Interface0"] [Modifier "no_handler" Nothing]) undefined

packedClass :: AnnASTElement SemanticAnns
packedClass = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" UInt64) undefined,
    ClassField (FieldDefinition "field1" UInt16) undefined,
    ClassField (FieldDefinition "field2" (Vector (DefinedType "TMDescriptor") (K 32))) undefined,
    ClassProcedure "procedure0" [
      Parameter "param0" Char,
      Parameter "param1" (Reference Mutable (Vector UInt8 (K 16)))
    ] [] undefined
  ] ["Interface0"] [Modifier "packed" Nothing]) undefined

alignedClass :: AnnASTElement SemanticAnns
alignedClass = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" UInt64) undefined,
    ClassField (FieldDefinition "field1" UInt16) undefined,
    ClassField (FieldDefinition "field2" (Vector (DefinedType "TMDescriptor") (K 32))) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] ["Interface0"] [Modifier "align" (Just (KC (I UInt32 16)))]) undefined

packedAndAlignedClass :: AnnASTElement SemanticAnns
packedAndAlignedClass = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" UInt64) undefined,
    ClassField (FieldDefinition "field1" (DefinedType "TCDescriptor")) undefined,
    ClassField (FieldDefinition "field2" (Vector (DefinedType "TMDescriptor") (K 32))) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] ["Interface0"] [
      Modifier "packed" Nothing,
      Modifier "align" (Just (KC (I UInt32 16)))
    ]) undefined

classWithFixedLocationField :: AnnASTElement SemanticAnns
classWithFixedLocationField = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" UInt32) undefined,
    ClassField (FieldDefinition "field1" (Location UInt32)) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] ["Interface0"] []) undefined

classWithAccessPortField :: AnnASTElement SemanticAnns
classWithAccessPortField = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" UInt32) undefined,
    ClassField (FieldDefinition "field1" (AccessPort (DefinedType "Interface1"))) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] ["Interface0"] []) undefined

renderTypedefDeclaration :: OptionMap -> AnnASTElement SemanticAnns -> Text
renderTypedefDeclaration opts = render . fromJust . (ppHeaderASTElement opts)

spec :: Spec
spec = do
  describe "Pretty printing classes" $ do
    it "Prints a class with one procedure and zero fields" $ do
      renderTypedefDeclaration M.empty classWithOneProcedureAndZeroFields `shouldBe`
        pack (
          "typedef struct {\n" ++
          "    __termina__resource_t __resource;\n" ++
          "} Class0;\n" ++
          "\n" ++
          "void Class0__procedure0(void * const __this, uint8_t param0, uint16_t param1,\n" ++
          "                        uint32_t param2, uint64_t param3, int8_t param4,\n" ++
          "                        int16_t param5, int32_t param6, int64_t param7);\n")
    it "Prints a class with two procedures and zero fields" $ do
      renderTypedefDeclaration M.empty classWithTwoProceduresAndZeroFields `shouldBe`
        pack (
          "typedef struct {\n" ++
          "    __termina__resource_t __resource;\n" ++
          "} Class0;\n" ++
          "\n" ++
          "void Class0__procedure0(void * const __this, uint8_t param0,\n" ++
          "                        __option__dyn_t param1);\n" ++
          "\n" ++
          "void Class0__procedure1(void * const __this, uint8_t param0,\n" ++
          "                        uint8_t param1[32]);\n")
    it "Prints a class marked as no_handler with one procedure and zero fields" $ do
      renderTypedefDeclaration M.empty noHandlerClassWithoutOneProcedureAndZeroFields `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    __termina__resource_t __resource;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);\n")
    it "Prints a class marked as no_handler with two fields" $ do
      renderTypedefDeclaration M.empty noHandlerClassWithOneEmptyProcedure `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24];\n" ++
            "    __termina__resource_t __resource;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);\n")
    it "Prints a class with one procedure and two fields" $ do
      renderTypedefDeclaration M.empty classWithOneProcedureAndTwoFields `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24];\n" ++
            "    __termina__resource_t __resource;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);\n")
    it "Prints a packed class" $ do
      renderTypedefDeclaration M.empty packedClass `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "    __termina__resource_t __resource;\n" ++
            "} __attribute__((packed)) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this, char param0, uint8_t param1[16]);\n")
    it "Prints an aligned class" $ do
      renderTypedefDeclaration M.empty alignedClass `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "    __termina__resource_t __resource;\n" ++
            "} __attribute__((align(16))) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);\n")
    it "Prints a packed & aligned class" $ do
      renderTypedefDeclaration M.empty packedAndAlignedClass `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    TCDescriptor field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "    __termina__resource_t __resource;\n" ++
            "} __attribute__((packed, align(16))) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);\n")
    it "Prints a class with a fixed location field" $ do
      renderTypedefDeclaration M.empty classWithFixedLocationField `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint32_t field0;\n" ++
            "    volatile uint32_t * field1;\n" ++
            "    __termina__resource_t __resource;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);\n")
    it "Prints a class with an access port field" $ do
      renderTypedefDeclaration M.empty classWithAccessPortField `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint32_t field0;\n" ++
            "    Interface1 field1;\n" ++
            "    __termina__resource_t __resource;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);\n")