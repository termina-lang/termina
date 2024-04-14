module UT.PPrinter.TypeDefinition.ResourceClassSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text
import Semantic.Monad
import qualified Data.Map as M

import Prettyprinter
import Control.Monad.Reader
import Generator.LanguageC.Printer
import Generator.TypeDefinition
import Generator.Common


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
  ] ["Interface0"] [Modifier "aligned" (Just (KC (I UInt32 16)))]) undefined

packedAndAlignedClass :: AnnASTElement SemanticAnns
packedAndAlignedClass = TypeDefinition
  (Class ResourceClass "Class0" [
    ClassField (FieldDefinition "field0" UInt64) undefined,
    ClassField (FieldDefinition "field1" (DefinedType "TCDescriptor")) undefined,
    ClassField (FieldDefinition "field2" (Vector (DefinedType "TMDescriptor") (K 32))) undefined,
    ClassProcedure "procedure0" [] [] undefined
  ] ["Interface0"] [
      Modifier "packed" Nothing,
      Modifier "aligned" (Just (KC (I UInt32 16)))
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

renderTypeDefinitionDecl :: OptionTypes -> AnnASTElement SemanticAnns -> Text
renderTypeDefinitionDecl opts decl = 
  case runReaderT (genTypeDefinitionDecl decl) opts of
    Left err -> pack $ show err
    Right cDecls -> render $ vsep $ runReader (mapM pprint cDecls) (CPrinterConfig False False)

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
          "                        __option_dyn_t param1);\n" ++
          "\n" ++
          "void Class0__procedure1(void * const __this, uint8_t param0,\n" ++
          "                        uint8_t param1[32]);")
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
            "    uint64_t field1[24];\n" ++
            "} Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);")
    it "Prints a class with one procedure and two fields" $ do
      renderTypeDefinitionDecl M.empty classWithOneProcedureAndTwoFields `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24];\n" ++
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
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((packed)) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this, char param0, uint8_t param1[16]);")
    it "Prints an aligned class" $ do
      renderTypeDefinitionDecl M.empty alignedClass `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((aligned(16))) Class0;\n" ++
            "\n" ++
            "void Class0__procedure0(void * const __this);")
    it "Prints a packed & aligned class" $ do
      renderTypeDefinitionDecl M.empty packedAndAlignedClass `shouldBe`
        pack (
            "\ntypedef struct {\n" ++
            "    __termina_resource_t __resource;\n" ++
            "    uint64_t field0;\n" ++
            "    TCDescriptor field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((packed, aligned(16))) Class0;\n" ++
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