module UT.PPrinter.TypeDefinition.ResourceClassSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types
import qualified Data.Map as M
import Utils.Annotations
import Generator.Monadic

import UT.PPrinter.Common

classWithOneProcedureAndZeroFields :: AnnASTElement SemanticAnn
classWithOneProcedureAndZeroFields = TypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure Mutable "procedure0" [
      Parameter "param0" TUInt8,
      Parameter "param1" TUInt16,
      Parameter "param2" TUInt32,
      Parameter "param3" TUInt64,
      Parameter "param4" TInt8,
      Parameter "param5" TInt16,
      Parameter "param6" TInt32,
      Parameter "param7" TInt64
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) (buildTypeAnn Internal)

classWithTwoProceduresAndZeroFields :: AnnASTElement SemanticAnn
classWithTwoProceduresAndZeroFields = TypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure Mutable "procedure0" [
      Parameter "param0" TUInt8,
      Parameter "param1" (TOption (TBoxSubtype (TStruct "TMPacket")))
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined,
    ClassProcedure Mutable "procedure1" [
      Parameter "param0" TUInt8,
      Parameter "param1" (TReference Mutable (TArray TUInt8 (buildConstExprTUSize 32)))
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) (buildTypeAnn Internal)

noHandlerClassWithoutOneProcedureAndZeroFields :: AnnASTElement SemanticAnn
noHandlerClassWithoutOneProcedureAndZeroFields = TypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) (buildTypeAnn Internal)

classWithOneProcedureAndTwoFields :: AnnASTElement SemanticAnn
classWithOneProcedureAndTwoFields = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt8 (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field1" (TArray TUInt64 (buildConstExprTUSize 24))
      (buildFieldAnn Internal)),
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) (buildTypeAnn Internal)

noHandlerClassWithOneEmptyProcedure :: AnnASTElement SemanticAnn
noHandlerClassWithOneEmptyProcedure = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt8 (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field1" (TArray TUInt64 (buildConstExprTUSize 24))
      (buildFieldAnn Internal)),
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] [Modifier "no_handler" Nothing]) (buildTypeAnn Internal)

packedClass :: AnnASTElement SemanticAnn
packedClass = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt64 (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field1" TUInt16 (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field2" (TArray (TStruct "TMDescriptor") (buildConstExprTUSize 32))
      (buildFieldAnn Internal)),
    ClassProcedure Mutable "procedure0" [
      Parameter "param0" TChar,
      Parameter "param1" (TReference Mutable (TArray TUInt8 (buildConstExprTUSize 16)))
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] [Modifier "packed" Nothing]) (buildTypeAnn Internal)

alignedClass :: AnnASTElement SemanticAnn
alignedClass = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt64 (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field1" TUInt16 (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field2" (TArray (TStruct "TMDescriptor") (buildConstExprTUSize 32))
      (buildFieldAnn Internal)),
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] [Modifier "aligned" (Just (I (TInteger 16 DecRepr) (Just TUInt32)))]) (buildTypeAnn Internal)

packedAndAlignedClass :: AnnASTElement SemanticAnn
packedAndAlignedClass = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt64 (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field1" (TStruct "TCDescriptor") (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field2" (TArray (TStruct "TMDescriptor") (buildConstExprTUSize 32))
      (buildFieldAnn Internal)),
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] [
      Modifier "packed" Nothing,
      Modifier "aligned" (Just (I (TInteger 16 DecRepr) (Just TUInt32)))
    ]) (buildTypeAnn Internal)

classWithFixedLocationField :: AnnASTElement SemanticAnn
classWithFixedLocationField = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt32 (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field1" (TFixedLocation TUInt32) (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field2" (TFixedLocation (TArray TUInt32 (buildConstExprTUSize 32))) (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field3" (TFixedLocation (TArray (TArray TUInt32 (buildConstExprTUSize 32)) (buildConstExprTUSize 8))) (buildFieldAnn Internal)),
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) (buildTypeAnn Internal)

classWithAccessPortField :: AnnASTElement SemanticAnn
classWithAccessPortField = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" TUInt32 (buildFieldAnn Internal)),
    ClassField (FieldDefinition "field1" (TAccessPort (TInterface RegularInterface "Interface1"))
      (buildAccessPortFieldAnn Internal (M.fromList [("test0", InterfaceProcedure Mutable "test0" [] [] (buildExpAnn Internal TUnit))]))),
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) undefined
  ] ["Interface0"] []) (buildTypeAnn Internal)

spec :: Spec
spec = do
  describe "Pretty printing classes" $ do
    it "Prints a class with one procedure and zero fields" $ do
      renderTypeDefinitionDecl emptyMonadicTypes classWithOneProcedureAndZeroFields `shouldBe`
        pack (
          "\ntypedef struct {\n" ++
          "    __termina_resource_lock_type_t __lock_type;\n" ++
          "} Class0;\n" ++
          "\n" ++
          "void Class0__procedure0(const __termina_event_t * const __ev,\n" ++
          "                        void * const __this, uint8_t param0, uint16_t param1,\n" ++
          "                        uint32_t param2, uint64_t param3, int8_t param4,\n" ++
          "                        int16_t param5, int32_t param6, int64_t param7);")
    it "Prints a class with two procedures and zero fields" $ do
      renderTypeDefinitionDecl emptyMonadicTypes classWithTwoProceduresAndZeroFields `shouldBe`
        pack (
          "\ntypedef struct {\n" ++
          "    __termina_resource_lock_type_t __lock_type;\n" ++
          "} Class0;\n" ++
          "\n" ++
          "void Class0__procedure0(const __termina_event_t * const __ev,\n" ++
          "                        void * const __this, uint8_t param0,\n" ++
          "                        __option_box_t param1);\n" ++
          "\n" ++
          "void Class0__procedure1(const __termina_event_t * const __ev,\n" ++
          "                        void * const __this, uint8_t param0,\n" ++
          "                        uint8_t param1[32U]);")
    it "Prints a class marked as no_handler with one procedure and zero fields" $ do
      renderTypeDefinitionDecl emptyMonadicTypes noHandlerClassWithoutOneProcedureAndZeroFields `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_lock_type_t __lock_type;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const __termina_event_t * const __ev,\n" ++
            "                        void * const __this);")
    it "Prints a class marked as no_handler with two fields" $ do
      renderTypeDefinitionDecl emptyMonadicTypes noHandlerClassWithOneEmptyProcedure `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_lock_type_t __lock_type;\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24U];\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const __termina_event_t * const __ev,\n" ++
            "                        void * const __this);")
    it "Prints a class with one procedure and two fields" $ do
      renderTypeDefinitionDecl emptyMonadicTypes classWithOneProcedureAndTwoFields `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_lock_type_t __lock_type;\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24U];\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const __termina_event_t * const __ev,\n" ++
            "                        void * const __this);")
    it "Prints a packed class" $ do
      renderTypeDefinitionDecl emptyMonadicTypes packedClass `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_lock_type_t __lock_type;\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32U];\n" ++
            "} __attribute__((packed)) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const __termina_event_t * const __ev,\n" ++
            "                        void * const __this, char param0, uint8_t param1[16U]);")
    it "Prints an aligned class" $ do
      renderTypeDefinitionDecl emptyMonadicTypes alignedClass `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_lock_type_t __lock_type;\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32U];\n" ++
            "} __attribute__((aligned(16U))) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const __termina_event_t * const __ev,\n" ++
            "                        void * const __this);")
    it "Prints a packed & aligned class" $ do
      renderTypeDefinitionDecl emptyMonadicTypes packedAndAlignedClass `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_lock_type_t __lock_type;\n" ++
            "    uint64_t field0;\n" ++
            "    TCDescriptor field1;\n" ++
            "    TMDescriptor field2[32U];\n" ++
            "} __attribute__((packed, aligned(16U))) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const __termina_event_t * const __ev,\n" ++
            "                        void * const __this);")
    it "Prints a class with a fixed location field" $ do
      renderTypeDefinitionDecl emptyMonadicTypes classWithFixedLocationField `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_lock_type_t __lock_type;\n" ++
            "    uint32_t field0;\n" ++
            "    volatile uint32_t * field1;\n" ++
            "    volatile uint32_t (* field2);\n" ++
            "    volatile uint32_t (* field3)[32U];\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const __termina_event_t * const __ev,\n" ++
            "                        void * const __this);")
    it "Prints a class with an access port field" $ do
      renderTypeDefinitionDecl emptyMonadicTypes classWithAccessPortField `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_lock_type_t __lock_type;\n" ++
            "    uint32_t field0;\n" ++
            "    struct {\n" ++
            "        void * __that;\n" ++
            "        void (* test0)(const __termina_event_t * const, void * const);\n" ++
            "    } field1;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const __termina_event_t * const __ev,\n" ++
            "                        void * const __this);")