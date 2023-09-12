module UT.PPrinter.TypeDef.ClassSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text
import Semantic.Monad
import Data.Maybe


classWithOneMethodAndZeroFields :: AnnASTElement SemanticAnns
classWithOneMethodAndZeroFields = TypeDefinition (Class "id0" [
    ClassMethod "method0" [
      Parameter "param0" UInt8,
      Parameter "param1" UInt16,
      Parameter "param2" UInt32,
      Parameter "param3" UInt64,
      Parameter "param4" Int8,
      Parameter "param5" Int16,
      Parameter "param6" Int32,
      Parameter "param7" Int64
    ] Self [] undefined
  ] []) undefined

classWithTwoMethodsAndZeroFields :: AnnASTElement SemanticAnns
classWithTwoMethodsAndZeroFields = TypeDefinition (Class "id0" [
    ClassMethod "method0" [
      Parameter "param0" UInt8,
      Parameter "param1" (Option (DynamicSubtype (DefinedType "TMPacket")))
    ] Self [] undefined,
    ClassMethod "method1" [
      Parameter "param0" UInt8,
      Parameter "param1" (Reference  (Vector UInt8 (KC (I UInt32 32))))
    ] NoSelf [] undefined
  ] []) undefined

noHandlerClassWithoutOneMethodAndZeroFields :: AnnASTElement SemanticAnns
noHandlerClassWithoutOneMethodAndZeroFields = TypeDefinition (Class "id0" [
    ClassMethod "method0" [] NoSelf [] undefined
  ] [Modifier "no_handler" Nothing]) undefined

classWithOneMethodAndTwoFields :: AnnASTElement SemanticAnns
classWithOneMethodAndTwoFields = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt8 undefined,
    ClassField "field1" (Vector UInt64 (KC (I UInt32 24))) undefined,
    ClassMethod "method0" [] Self [] undefined
  ] []) undefined

noHandlerClassWithOneEmptyMethod :: AnnASTElement SemanticAnns
noHandlerClassWithOneEmptyMethod = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt8 undefined,
    ClassField "field1" (Vector UInt64 (KC (I UInt32 24))) undefined,
    ClassMethod "method0" [] Self [] undefined
  ] [Modifier "no_handler" Nothing]) undefined

packedClass :: AnnASTElement SemanticAnns
packedClass = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt64 undefined,
    ClassField "field1" UInt16 undefined,
    ClassField "field2" (Vector (DefinedType "TMDescriptor") (KC (I UInt32 32))) undefined,
    ClassMethod "method0" [
      Parameter "param0" Char,
      Parameter "param1" (Reference (Vector UInt8 (KC (I UInt32 16))))
    ] Self [] undefined
  ] [Modifier "packed" Nothing]) undefined

alignedClass :: AnnASTElement SemanticAnns
alignedClass = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt64 undefined,
    ClassField "field1" UInt16 undefined,
    ClassField "field2" (Vector (DefinedType "TMDescriptor") (KC (I UInt32 32))) undefined,
    ClassMethod "method0" [] Self [] undefined
  ] [Modifier "align" (Just (KC (I UInt32 16)))]) undefined

packedAndAlignedClass :: AnnASTElement SemanticAnns
packedAndAlignedClass = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt64 undefined,
    ClassField "field1" (DefinedType "TCDescriptor") undefined,
    ClassField "field2" (Vector (DefinedType "TMDescriptor") (KC (I UInt32 32))) undefined,
    ClassMethod "method0" [] Self [] undefined
  ] [
      Modifier "packed" Nothing,
      Modifier "align" (Just (KC (I UInt32 16)))
    ]) undefined

renderTypedefDeclaration :: AnnASTElement SemanticAnns -> Text
renderTypedefDeclaration = render . fromJust . ppHeaderASTElement

spec :: Spec
spec = do
  describe "Pretty printing classes" $ do
    it "Prints a class with one method and zero fields" $ do
      renderTypedefDeclaration classWithOneMethodAndZeroFields `shouldBe`
        pack (
          "typedef struct {\n" ++
          "    uint32_t __dummy;\n" ++
          "} id0;\n" ++
          "\n" ++
          "void __id0_method0(uint8_t param0, uint16_t param1, uint32_t param2,\n" ++
          "                   uint64_t param3, int8_t param4, int16_t param5,\n" ++
          "                   int32_t param6, int64_t param7);\n")
    it "Prints a class with two methods and zero fields" $ do
      renderTypedefDeclaration classWithTwoMethodsAndZeroFields `shouldBe`
        pack (
          "typedef struct {\n" ++
          "    uint32_t __dummy;\n" ++
          "} id0;\n" ++
          "\n" ++
          "void __id0_method0(uint8_t param0, __Option_dyn_t param1);\n" ++
          "\n" ++
          "void __id0_method1(uint8_t param0, uint8_t param1[32]);\n")
    it "Prints a class marked as no_handler with one method and zero fields" $ do
      renderTypedefDeclaration noHandlerClassWithoutOneMethodAndZeroFields `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    __termina_mutex_id_t __mutex_id;\n" ++
            "} id0;\n" ++
            "\n" ++
            "void __id0_method0();\n")
    it "Prints a class marked as no_handler with two fields" $ do
      renderTypedefDeclaration noHandlerClassWithOneEmptyMethod `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24];\n" ++
            "    __termina_mutex_id_t __mutex_id;\n" ++
            "} id0;\n" ++
            "\n" ++
            "void __id0_method0();\n")
    it "Prints a class with one method and two fields" $ do
      renderTypedefDeclaration classWithOneMethodAndTwoFields `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24];\n" ++
            "} id0;\n" ++
            "\n" ++
            "void __id0_method0();\n")
    it "Prints a packed class" $ do
      renderTypedefDeclaration packedClass `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((packed)) id0;\n" ++
            "\n" ++
            "void __id0_method0(char param0, uint8_t param1[16]);\n")
    it "Prints an aligned class" $ do
      renderTypedefDeclaration alignedClass `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((align(16))) id0;\n" ++
            "\n" ++
            "void __id0_method0();\n")
    it "Prints a packed & aligned class" $ do
      renderTypedefDeclaration packedAndAlignedClass `shouldBe`
        pack (
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    TCDescriptor field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((packed, align(16))) id0;\n" ++
            "\n" ++
            "void __id0_method0();\n")