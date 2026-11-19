module Codegen.Positive.Printer.TypeDefinition.ResourceClassSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types
import Parser.Types (undocumented)
import qualified Data.Map.Strict as M
import Utils.Annotations
import Generator.Monadic

import Codegen.Positive.Printer.Common
import Semantic.Environment (builtinParameter)

classWithOneProcedureAndZeroFields :: AnnASTElement SemanticAnn
classWithOneProcedureAndZeroFields = internalTypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure Mutable "procedure0" [
      builtinParameter "param0" TUInt8,
      builtinParameter "param1" TUInt16,
      builtinParameter "param2" TUInt32,
      builtinParameter "param3" TUInt64,
      builtinParameter "param4" TInt8,
      builtinParameter "param5" TInt16,
      builtinParameter "param6" TInt32,
      builtinParameter "param7" TInt64
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) internalProcedureAnn
  ] ["Interface0"] [])

classWithTwoProceduresAndZeroFields :: AnnASTElement SemanticAnn
classWithTwoProceduresAndZeroFields = internalTypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure Mutable "procedure0" [
      builtinParameter "param0" TUInt8,
      builtinParameter "param1" (TOption (TBoxSubtype (TStruct "TMPacket")))
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) internalProcedureAnn,
    ClassProcedure Mutable "procedure1" [
      builtinParameter "param0" TUInt8,
      builtinParameter "param1" (TReference Mutable (TArray TUInt8 (buildConstExprTUSize 32)))
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) internalProcedureAnn
  ] ["Interface0"] [])

noHandlerClassWithoutOneProcedureAndZeroFields :: AnnASTElement SemanticAnn
noHandlerClassWithoutOneProcedureAndZeroFields = internalTypeDefinition (Class ResourceClass "Class0" [
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) internalProcedureAnn
  ] ["Interface0"] [])

classWithOneProcedureAndTwoFields :: AnnASTElement SemanticAnn
classWithOneProcedureAndTwoFields = internalTypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (internalField "field0" TUInt8),
    ClassField (internalField "field1" (TArray TUInt64 (buildConstExprTUSize 24))),
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) internalProcedureAnn
  ] ["Interface0"] [])

noHandlerClassWithOneEmptyProcedure :: AnnASTElement SemanticAnn
noHandlerClassWithOneEmptyProcedure = internalTypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (internalField "field0" TUInt8),
    ClassField (internalField "field1" (TArray TUInt64 (buildConstExprTUSize 24))),
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) internalProcedureAnn
  ] ["Interface0"] [Modifier "no_handler" Nothing])

packedClass :: AnnASTElement SemanticAnn
packedClass = internalTypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (internalField "field0" TUInt64),
    ClassField (internalField "field1" TUInt16),
    ClassField (internalField "field2" (TArray (TStruct "TMDescriptor") (buildConstExprTUSize 32))),
    ClassProcedure Mutable "procedure0" [
      builtinParameter "param0" TChar,
      builtinParameter "param1" (TReference Mutable (TArray TUInt8 (buildConstExprTUSize 16)))
    ] (Block [ReturnStmt Nothing undefined] stmtSemAnn) internalProcedureAnn
  ] ["Interface0"] [Modifier "packed" Nothing])

classWithFixedLocationField :: AnnASTElement SemanticAnn
classWithFixedLocationField = internalTypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (internalField "field0" TUInt32),
    ClassField (internalField "field1" (TFixedLocation TUInt32)),
    ClassField (internalField "field2" (TFixedLocation (TArray TUInt32 (buildConstExprTUSize 32)))),
    ClassField (internalField "field3" (TFixedLocation (TArray (TArray TUInt32 (buildConstExprTUSize 32)) (buildConstExprTUSize 8)))),
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) internalProcedureAnn
  ] ["Interface0"] [])

classWithAccessPortField :: AnnASTElement SemanticAnn
classWithAccessPortField = internalTypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (internalField "field0" TUInt32),
    ClassField (internalAccessPortField "field1" (TAccessPort (TInterface RegularInterface "Interface1"))
      (M.fromList [("test0", InterfaceProcedure Mutable "test0" [] [] (buildExpAnn (undocumented Internal) TUnit))])),
    ClassProcedure Mutable "procedure0" [] (Block [ReturnStmt Nothing undefined] stmtSemAnn) internalProcedureAnn
  ] ["Interface0"] [])

spec :: Spec
spec = do
  describe "Pretty printing classes" $ do
    it "Prints a class with one procedure and zero fields" $ do
      renderTypeDefinitionDecl emptyMonadicTypes classWithOneProcedureAndZeroFields `shouldBe`
        pack ("\n" ++
              "typedef struct {\n" ++
              "    termina__resource_lock_type_t _lock_type;\n" ++
              "} Class0;\n" ++
              "\n" ++
              "void Class0__procedure0(const termina__event_t * const termina__ev,\n" ++
              "                        void * const termina__this, const uint8_t param0,\n" ++
              "                        const uint16_t param1, const uint32_t param2,\n" ++
              "                        const uint64_t param3, const int8_t param4,\n" ++
              "                        const int16_t param5, const int32_t param6,\n" ++
              "                        const int64_t param7);")
    it "Prints a class with two procedures and zero fields" $ do
      renderTypeDefinitionDecl emptyMonadicTypes classWithTwoProceduresAndZeroFields `shouldBe`
        pack (
          "\ntypedef struct {\n" ++
          "    termina__resource_lock_type_t _lock_type;\n" ++
          "} Class0;\n" ++
          "\n" ++
          "void Class0__procedure0(const termina__event_t * const termina__ev,\n" ++
          "                        void * const termina__this, const uint8_t param0,\n" ++
          "                        const Option__box param1);\n" ++
          "\n" ++
          "void Class0__procedure1(const termina__event_t * const termina__ev,\n" ++
          "                        void * const termina__this, const uint8_t param0,\n" ++
          "                        uint8_t param1[32U]);")
    it "Prints a class marked as no_handler with one procedure and zero fields" $ do
      renderTypeDefinitionDecl emptyMonadicTypes noHandlerClassWithoutOneProcedureAndZeroFields `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    termina__resource_lock_type_t _lock_type;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const termina__event_t * const termina__ev,\n" ++
            "                        void * const termina__this);")
    it "Prints a class marked as no_handler with two fields" $ do
      renderTypeDefinitionDecl emptyMonadicTypes noHandlerClassWithOneEmptyProcedure `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    termina__resource_lock_type_t _lock_type;\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24U];\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const termina__event_t * const termina__ev,\n" ++
            "                        void * const termina__this);")
    it "Prints a class with one procedure and two fields" $ do
      renderTypeDefinitionDecl emptyMonadicTypes classWithOneProcedureAndTwoFields `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    termina__resource_lock_type_t _lock_type;\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24U];\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const termina__event_t * const termina__ev,\n" ++
            "                        void * const termina__this);")
    it "Prints a packed class" $ do
      renderTypeDefinitionDecl emptyMonadicTypes packedClass `shouldBe`
        pack ("\n" ++
              "typedef struct {\n" ++
              "    termina__resource_lock_type_t _lock_type;\n" ++
              "    uint64_t field0;\n" ++
              "    uint16_t field1;\n" ++
              "    TMDescriptor field2[32U];\n" ++
              "} __attribute__((packed)) Class0;\n" ++
              "\n" ++
              "void Class0__procedure0(const termina__event_t * const termina__ev,\n" ++
              "                        void * const termina__this, const char param0,\n" ++
              "                        uint8_t param1[16U]);")
    it "Prints a class with a fixed location field" $ do
      renderTypeDefinitionDecl emptyMonadicTypes classWithFixedLocationField `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    termina__resource_lock_type_t _lock_type;\n" ++
            "    uint32_t field0;\n" ++
            "    volatile uint32_t * field1;\n" ++
            "    volatile uint32_t (* field2);\n" ++
            "    volatile uint32_t (* field3)[32U];\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const termina__event_t * const termina__ev,\n" ++
            "                        void * const termina__this);")
    it "Prints a class with an access port field" $ do
      renderTypeDefinitionDecl emptyMonadicTypes classWithAccessPortField `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    termina__resource_lock_type_t _lock_type;\n" ++
            "    uint32_t field0;\n" ++
            "    struct {\n" ++
            "        void * _that;\n" ++
            "        void (* test0)(const termina__event_t * const termina__ev,\n" ++
            "                       void * const termina__this);\n" ++
            "    } field1;\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(const termina__event_t * const termina__ev,\n" ++
            "                        void * const termina__this);")