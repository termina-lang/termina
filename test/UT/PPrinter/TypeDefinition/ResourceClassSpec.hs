module UT.PPrinter.TypeDefinition.ResourceClassSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types
import qualified Data.Map as M

import UT.PPrinter.Common

classWithOneProcedureAndZeroFields :: AnnASTElement SemanticAnn
classWithOneProcedureAndZeroFields = TypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure "procedure0" [
      Parameter "param0" TUInt8,
      Parameter "param1" TUInt16,
      Parameter "param2" TUInt32,
      Parameter "param3" TUInt64,
      Parameter "param4" TInt8,
      Parameter "param5" TInt16,
      Parameter "param6" TInt32,
      Parameter "param7" TInt64
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) undefined

classWithTwoProceduresAndZeroFields :: AnnASTElement SemanticAnn
classWithTwoProceduresAndZeroFields = TypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure "procedure0" [
      Parameter "param0" TUInt8,
      Parameter "param1" (TOption (TBoxSubtype (TStruct "TMPacket")))
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined,
    ClassProcedure "procedure1" [
      Parameter "param0" TUInt8,
      Parameter "param1" (TReference Mutable (TArray TUInt8 (K (TInteger 32 DecRepr))))
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) undefined

noHandlerClassWithoutOneProcedureAndZeroFields :: AnnASTElement SemanticAnn
noHandlerClassWithoutOneProcedureAndZeroFields = TypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) undefined

classWithOneProcedureAndTwoFields :: AnnASTElement SemanticAnn
classWithOneProcedureAndTwoFields = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt8) undefined,
    ClassField (FieldDefinition "field1" (TArray TUInt64 (K (TInteger 24 DecRepr)))) undefined,
    ClassProcedure "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) undefined

noHandlerClassWithOneEmptyProcedure :: AnnASTElement SemanticAnn
noHandlerClassWithOneEmptyProcedure = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt8) undefined,
    ClassField (FieldDefinition "field1" (TArray TUInt64 (K (TInteger 24 DecRepr)))) undefined,
    ClassProcedure "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] [Modifier "no_handler" Nothing]) undefined

packedClass :: AnnASTElement SemanticAnn
packedClass = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt64) undefined,
    ClassField (FieldDefinition "field1" TUInt16) undefined,
    ClassField (FieldDefinition "field2" (TArray (TStruct "TMDescriptor") (K (TInteger 32 DecRepr)))) undefined,
    ClassProcedure "procedure0" [
      Parameter "param0" TChar,
      Parameter "param1" (TReference Mutable (TArray TUInt8 (K (TInteger 16 DecRepr))))
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] [Modifier "packed" Nothing]) undefined

alignedClass :: AnnASTElement SemanticAnn
alignedClass = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt64) undefined,
    ClassField (FieldDefinition "field1" TUInt16) undefined,
    ClassField (FieldDefinition "field2" (TArray (TStruct "TMDescriptor") (K (TInteger 32 DecRepr)))) undefined,
    ClassProcedure "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] [Modifier "aligned" (Just (I (TInteger 16 DecRepr) (Just TUInt32)))]) undefined

packedAndAlignedClass :: AnnASTElement SemanticAnn
packedAndAlignedClass = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt64) undefined,
    ClassField (FieldDefinition "field1" (TStruct "TCDescriptor")) undefined,
    ClassField (FieldDefinition "field2" (TArray (TStruct "TMDescriptor") (K (TInteger 32 DecRepr)))) undefined,
    ClassProcedure "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] [
      Modifier "packed" Nothing,
      Modifier "aligned" (Just (I (TInteger 16 DecRepr) (Just TUInt32)))
    ]) undefined

classWithFixedLocationField :: AnnASTElement SemanticAnn
classWithFixedLocationField = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt32) undefined,
    ClassField (FieldDefinition "field1" (TFixedLocation TUInt32)) undefined,
    ClassProcedure "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) undefined

classWithAccessPortField :: AnnASTElement SemanticAnn
classWithAccessPortField = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt32) undefined,
    ClassField (FieldDefinition "field1" (TAccessPort (TInterface RegularInterface "Interface1"))) undefined,
    ClassProcedure "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) undefined

spec :: Spec
spec = do
  describe "Pretty printing classes" $ do
    it "Prints a class with one procedure and zero fields" $ do
      renderTypeDefinitionDecl M.empty classWithOneProcedureAndZeroFields `shouldBe`
        pack (
          "\ntypedef struct {\n" ++
          "    __termina_resource_t __resource;\n" ++
          "} Class0;\n" ++
          "\n" ++
          "void Class0__procedure0(void * const __this, uint8_t param0, uint16_t param1,\n" ++
          "                        uint32_t param2, uint64_t param3, int8_t param4,\n" ++
          "                        int16_t param5, int32_t param6, int64_t param7);")
    it "Prints a class with two procedures and zero fields" $ do
      renderTypeDefinitionDecl M.empty classWithTwoProceduresAndZeroFields `shouldBe`
        pack (
          "\ntypedef struct {\n" ++
          "    __termina_resource_t __resource;\n" ++
          "} Class0;\n" ++
          "\n" ++
          "void Class0__procedure0(void * const __this, uint8_t param0,\n" ++
          "                        __option_box_t param1);\n" ++
          "\n" ++
          "void Class0__procedure1(void * const __this, uint8_t param0,\n" ++
          "                        uint8_t param1[32U]);")
    it "Prints a class marked as no_handler with one procedure and zero fields" $ do
      renderTypeDefinitionDecl M.empty noHandlerClassWithoutOneProcedureAndZeroFields `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);")
    it "Prints a class marked as no_handler with two fields" $ do
      renderTypeDefinitionDecl M.empty noHandlerClassWithOneEmptyProcedure `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24U];\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);")
    it "Prints a class with one procedure and two fields" $ do
      renderTypeDefinitionDecl M.empty classWithOneProcedureAndTwoFields `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24U];\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);")
    it "Prints a packed class" $ do
      renderTypeDefinitionDecl M.empty packedClass `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32U];\n" ++
            "} __attribute__((packed)) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this, char param0, uint8_t param1[16U]);")
    it "Prints an aligned class" $ do
      renderTypeDefinitionDecl M.empty alignedClass `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32U];\n" ++
            "} __attribute__((aligned(16U))) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);")
    it "Prints a packed & aligned class" $ do
      renderTypeDefinitionDecl M.empty packedAndAlignedClass `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "    uint64_t field0;\n" ++
            "    TCDescriptor field1;\n" ++
            "    TMDescriptor field2[32U];\n" ++
            "} __attribute__((packed, aligned(16U))) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);")
    it "Prints a class with a fixed location field" $ do
      renderTypeDefinitionDecl M.empty classWithFixedLocationField `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "    uint32_t field0;\n" ++
            "    volatile uint32_t * field1;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);")
    it "Prints a class with an access port field" $ do
      renderTypeDefinitionDecl M.empty classWithAccessPortField `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "    uint32_t field0;\n" ++
            "    Interface1 field1;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);")