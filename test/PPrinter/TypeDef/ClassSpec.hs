module PPrinter.TypeDef.ClassSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Parsing
import Data.Text

classWithOneMethodAndZeroFields :: AnnASTElement Annotation
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
    ] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [] undefined)

classWithTwoMethodsAndZeroFields :: AnnASTElement Annotation
classWithTwoMethodsAndZeroFields = TypeDefinition (Class "id0" [
    ClassMethod "method0" [
      Parameter "param0" UInt8,
      Parameter "param1" (Option (DefinedType "TMPacket"))
    ] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined,
    ClassMethod "method1" [
      Parameter "param0" UInt8,
      Parameter "param1" (Vector UInt8 (K 32))
    ] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [] undefined)

noHandlerClassWithoutOneMethodAndZeroFields :: AnnASTElement Annotation
noHandlerClassWithoutOneMethodAndZeroFields = TypeDefinition (Class "id0" [
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [Modifier "no_handler" Nothing] undefined)

classWithOneMethodAndTwoFields :: AnnASTElement Annotation
classWithOneMethodAndTwoFields = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt8,
    ClassField "field1" (Vector UInt64 (K 24)),
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [] undefined)

noHandlerClassWithOneEmptyMethod :: AnnASTElement Annotation
noHandlerClassWithOneEmptyMethod = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt8,
    ClassField "field1" (Vector UInt64 (K 24)),
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [Modifier "no_handler" Nothing] undefined)

packedClass :: AnnASTElement Annotation
packedClass = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt64,
    ClassField "field1" UInt16,
    ClassField "field2" (Vector (DefinedType "TMDescriptor") (K 32)),
    ClassMethod "method0" [
      Parameter "param0" Char,
      Parameter "param1" (Vector UInt8 (K 16))
    ] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [Modifier "packed" Nothing] undefined)

alignedClass :: AnnASTElement Annotation
alignedClass = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt64,
    ClassField "field1" UInt16,
    ClassField "field2" (Vector (DefinedType "TMDescriptor") (K 32)),
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [Modifier "align" (Just (KC (I UInt32 16) undefined))] undefined)

packedAndAlignedClass :: AnnASTElement Annotation
packedAndAlignedClass = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt64,
    ClassField "field1" UInt16,
    ClassField "field2" (Vector (DefinedType "TMDescriptor") (K 32)),
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [
      Modifier "packed" Nothing,
      Modifier "align" (Just (KC (I UInt32 16) undefined))
    ] undefined)

renderSingleASTElement :: AnnASTElement a -> Text
renderSingleASTElement = render . ppHeaderASTElement ppEmptyDoc ppEmptyDoc

spec :: Spec
spec = do
  describe "Pretty printing classes" $ do
    it "Prints a class with one method and zero fields" $ do
      renderSingleASTElement classWithOneMethodAndZeroFields `shouldBe`
        pack (
          "\n" ++ 
          "void __id0_method0(uint8_t param0, uint16_t param1, uint32_t param2,\n" ++
          "                   uint64_t param3, int8_t param4, int16_t param5,\n" ++
          "                   int32_t param6, int64_t param7);\n")
    it "Prints a class with two methods and zero fields" $ do
      renderSingleASTElement classWithTwoMethodsAndZeroFields `shouldBe`
        pack (
          "\n" ++ 
          "void __id0_method0(uint8_t param0, TMPacket * param1);\n" ++
          "\n" ++
          "void __id0_method1(uint8_t param0, uint8_t param1[32]);\n")
    it "Prints a class marked as no_handler with one method and zero fields" $ do
      renderSingleASTElement noHandlerClassWithoutOneMethodAndZeroFields `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    __termina_mutex_id_t __mutex_id;\n" ++
            "} id0;\n" ++
            "\n" ++ 
            "void __id0_method0();\n")
    it "Prints a class marked as no_handler with two fields" $ do
      renderSingleASTElement noHandlerClassWithOneEmptyMethod `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24];\n" ++
            "    __termina_mutex_id_t __mutex_id;\n" ++
            "} id0;\n" ++
            "\n" ++
            "void __id0_method0();\n")
    it "Prints a class with one method and two fields" $ do
      renderSingleASTElement classWithOneMethodAndTwoFields `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24];\n" ++
            "} id0;" ++
            "\n" ++
            "\n" ++ 
            "void __id0_method0();\n")
    it "Prints a packed class" $ do
      renderSingleASTElement packedClass `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((packed)) id0;\n" ++
            "\n" ++
            "void __id0_method0(char param0, uint8_t param1[16]);\n")
    it "Prints an aligned class" $ do
      renderSingleASTElement alignedClass `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((align(16))) id0;\n" ++
            "\n" ++
            "void __id0_method0();\n")
    it "Prints a packed & aligned class" $ do
      renderSingleASTElement packedAndAlignedClass `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((packed, align(16))) id0;\n" ++
            "\n" ++
            "void __id0_method0();\n")