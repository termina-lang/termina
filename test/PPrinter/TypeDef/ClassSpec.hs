module PPrinter.TypeDef.ClassSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Parsing
import Data.Text

classWithoutFields :: AnnASTElement Annotation
classWithoutFields = TypeDefinition (Class "id0" [
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [] undefined)

noHandlerClassWithoutFields :: AnnASTElement Annotation
noHandlerClassWithoutFields = TypeDefinition (Class "id0" [
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [Modifier "no_handler" Nothing] undefined)

classWithOneEmptyMethod :: AnnASTElement Annotation
classWithOneEmptyMethod = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt8,
    ClassField "field1" (Vector UInt64 (K 24)),
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [] undefined)

packedClassWithOneEmptyMethod :: AnnASTElement Annotation
packedClassWithOneEmptyMethod = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt64,
    ClassField "field1" UInt16,
    ClassField "field2" (Vector (DefinedType "TMDescriptor") (K 32)),
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [Modifier "packed" Nothing] undefined)

alignedClassWithOneEmptyMethod :: AnnASTElement Annotation
alignedClassWithOneEmptyMethod = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt64,
    ClassField "field1" UInt16,
    ClassField "field2" (Vector (DefinedType "TMDescriptor") (K 32)),
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [Modifier "align" (Just (KC (I undefined 16)))] undefined)

packedAndAlignedClassWithOneEmptyMethod :: AnnASTElement Annotation
packedAndAlignedClassWithOneEmptyMethod = TypeDefinition
  (Class "id0" [
    ClassField "field0" UInt64,
    ClassField "field1" UInt16,
    ClassField "field2" (Vector (DefinedType "TMDescriptor") (K 32)),
    ClassMethod "method0" [] Nothing 
      (BlockRet [] (ReturnStmt Nothing undefined)) undefined
  ] [
      Modifier "packed" Nothing,
      Modifier "align" (Just (KC (I undefined 16)))
    ] undefined)

renderSingleASTElement :: AnnASTElement a -> Text
renderSingleASTElement = render . ppHeaderASTElement ppEmptyDoc ppEmptyDoc

spec :: Spec
spec = do
  describe "Pretty printing classes" $ do
    it "Prints a class without fields" $ do
      renderSingleASTElement classWithoutFields `shouldBe`
        pack ""
    it "Prints a class marked as no_handler without fields" $ do
      renderSingleASTElement noHandlerClassWithoutFields `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    __termina_mutex_id_t __mutex_id;\n" ++
            "} id0;" ++
            "\n")
    it "Prints a class with two fields and one empty method" $ do
      renderSingleASTElement classWithOneEmptyMethod `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint8_t field0;\n" ++
            "    uint64_t field1[24];\n" ++
            "} id0;" ++
            "\n")
    it "Prints a packed class with two fields and one empty method" $ do
      renderSingleASTElement packedClassWithOneEmptyMethod `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((packed)) id0;" ++
            "\n")
    it "Prints an aligned with two fields and one empty method" $ do
      renderSingleASTElement alignedClassWithOneEmptyMethod `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((align(16))) id0;" ++
            "\n")
    it "Prints a packed and aligned with two fields and one empty method" $ do
      renderSingleASTElement packedAndAlignedClassWithOneEmptyMethod `shouldBe`
        pack (
            "\n" ++
            "typedef struct {\n" ++
            "    uint64_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    TMDescriptor field2[32];\n" ++
            "} __attribute__((packed, align(16))) id0;" ++
            "\n")