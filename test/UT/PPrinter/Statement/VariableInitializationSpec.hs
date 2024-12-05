module UT.PPrinter.Statement.VariableInitializationSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

tmDescriptorTS, messageTS :: TerminaType
tmDescriptorTS = TStruct "TMDescriptor"
messageTS = TStruct "Message"

optionBoxUInt32TS :: TerminaType
optionBoxUInt32TS = TOption (TBoxSubtype TUInt32)

arrayTS, arrayTMDescriptorTS, twoDimArrayTS :: TerminaType
arrayTS = TArray TUInt32 (K (TInteger 10 DecRepr))
arrayTMDescriptorTS = TArray tmDescriptorTS (K (TInteger 20 DecRepr))
twoDimArrayTS = TArray (TArray TInt64 (K (TInteger 5 DecRepr))) (K (TInteger 10 DecRepr))

optionBoxUInt32ExprSemAnn :: SemanticAnn
optionBoxUInt32ExprSemAnn = optionBoxExprSemAnn TUInt32

arrayObjAnn, twoDymArrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable TUInt32 (K (TInteger 10 DecRepr))
twoDymArrayObjAnn = twoDymArrayObjSemAnn Mutable TInt64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

arrayExprAnn, arrayTMDescriptorExprAnn, twoDymArrayExprAnn, twoDymArrayRowExprAnn :: SemanticAnn
arrayExprAnn = arrayExprSemAnn TUInt32 (K (TInteger 10 DecRepr))
arrayTMDescriptorExprAnn = arrayExprSemAnn tmDescriptorTS (K (TInteger 20 DecRepr))
twoDymArrayRowExprAnn = arrayExprSemAnn TInt64 (K (TInteger 5 DecRepr))
twoDymArrayExprAnn = twoDymArrayExprSemAnn TInt64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

array0 :: Expression SemanticAnn
array0 = AccessObject (Variable "array0" arrayObjAnn)

array1, array2, array3, array4, array5 :: Statement SemanticAnn
array1 = Declaration "array1" Mutable arrayTS array0 stmtSemAnn
array2 = Declaration "array2" Mutable twoDimArrayTS (AccessObject (Variable "array1" twoDymArrayObjAnn)) stmtSemAnn
array3 = Declaration "array3" Mutable arrayTS (ArrayInitializer uint32Const0 (K (TInteger 10 DecRepr)) arrayExprAnn) stmtSemAnn
array4 = Declaration "array4" Mutable twoDimArrayTS (ArrayInitializer (ArrayInitializer uint32Const0 (K (TInteger 5 DecRepr)) twoDymArrayRowExprAnn) (K (TInteger 10 DecRepr)) twoDymArrayExprAnn) stmtSemAnn
array5 = Declaration "array5" Mutable arrayTMDescriptorTS (ArrayInitializer tmDescriptorFieldsInit0 (K (TInteger 10 DecRepr)) arrayTMDescriptorExprAnn) stmtSemAnn

foo0 :: Expression SemanticAnn
foo0 = AccessObject (Variable "foo0" (objSemAnn Mutable TUInt32))

foo1, foo2 :: Statement SemanticAnn
foo1 = Declaration "foo1" Mutable TUInt32 foo0 stmtSemAnn
foo2 = Declaration "foo2" Mutable TUInt32 uint32Const0 stmtSemAnn

tmDescriptorObjSemAnn :: SemanticAnn
tmDescriptorObjSemAnn = structObjSemAnn Mutable "TMDescriptor"

structAExprSemAnn, tmDescriptorExprSemAnn, messageExprSemAnn :: SemanticAnn
structAExprSemAnn = structExprSemAnn "StructA"
tmDescriptorExprSemAnn = structExprSemAnn "TMDescriptor"
messageExprSemAnn = structExprSemAnn "Message"

uint32Const0, uint32Const0xFFFF0000 :: Expression SemanticAnn
uint32Const0 = Constant (I (TInteger 0 DecRepr) (Just TUInt32)) uint32ExprSemAnn
uint32Const0xFFFF0000 = Constant (I (TInteger 4294901760 DecRepr) (Just TUInt32)) uint32ExprSemAnn

-- | Initialization expression:
-- { field_a = 0 : u32, field_b = 0xFFFF0000 : u32 } : StructA
structAFieldsInit0 :: Expression SemanticAnn
structAFieldsInit0 =
    StructInitializer 
        [FieldValueAssignment "field_a" uint32Const0 undefined,
         FieldValueAssignment "field_b" (ArrayInitializer uint32Const0 (K (TInteger 10 DecRepr)) arrayExprAnn) stmtSemAnn,
         FieldValueAssignment "field_c" uint32Const0xFFFF0000 stmtSemAnn] structAExprSemAnn

tmDescriptorFieldsInit0 :: Expression SemanticAnn
tmDescriptorFieldsInit0 =
    StructInitializer
        [FieldValueAssignment "field0" uint32Const0 undefined,
         FieldValueAssignment "field1" structAFieldsInit0 undefined] tmDescriptorExprSemAnn

struct0, struct1 :: Statement SemanticAnn
struct0 = Declaration "struct0" Mutable tmDescriptorTS tmDescriptorFieldsInit0 stmtSemAnn
struct1 = Declaration "struct1" Mutable tmDescriptorTS (AccessObject (Variable "struct0" tmDescriptorObjSemAnn)) stmtSemAnn

enum0, enum1 :: Statement SemanticAnn
enum0 = Declaration "enum0" Mutable messageTS (EnumVariantInitializer "Message" "Reset" [] messageExprSemAnn) stmtSemAnn
enum1 = Declaration "enum1" Mutable messageTS (EnumVariantInitializer "Message" "In" [uint32Const0, uint32Const0] messageExprSemAnn) stmtSemAnn

boxVar0 :: Expression SemanticAnn
boxVar0 = AccessObject (Variable "box_var0" boxUInt32SemAnn)

option0, option1 :: Statement SemanticAnn
option0 = Declaration "option0" Mutable optionBoxUInt32TS (OptionVariantInitializer (Some boxVar0) optionBoxUInt32ExprSemAnn) stmtSemAnn
option1 = Declaration "option1" Mutable optionBoxUInt32TS (OptionVariantInitializer None optionBoxUInt32ExprSemAnn) stmtSemAnn

spec :: Spec
spec = do
  describe "Pretty printing integer variable declarations" $ do
    it "Prints the statement var foo1 : u32 = foo0;" $ do
      renderStatement foo1 `shouldBe`
        pack "\nuint32_t foo1 = foo0;"
    it "Prints the statement var foo2 : u32 = 0 : u32;" $ do
      renderStatement foo2 `shouldBe`
        pack "\nuint32_t foo2 = 0U;"
  describe "Pretty printing option variable declarations" $ do
    it "Prints the statement var option0 : TOption <'box u32> = Some(box_var0);" $ do
      renderStatement option0 `shouldBe`
        pack (
          "\n__option_box_t option0;\n" ++
          "option0.__variant = Some;\n" ++
          "option0.Some.__0 = box_var0;")
    it "Prints the statement var option1 : TOption <'box u32> = None;" $ do
      renderStatement option1 `shouldBe`
        pack (
          "\n__option_box_t option1;\n" ++
          "option1.__variant = None;")
  describe "Pretty printing enum variable declarations" $ do
    it "Prints the statement var enum0 : Message = Message::Reset;" $ do
      renderStatement enum0 `shouldBe`
        pack (
          "\nMessage enum0;\n" ++
          "enum0.__variant = Message__Reset;")
    it "Prints the statement var enum1 : Message = Message::In(0 : u32, 0 : u32);" $ do
      renderStatement enum1 `shouldBe`
        pack (
          "\nMessage enum1;\n" ++
          "enum1.__variant = Message__In;\n" ++
          "enum1.In.__0 = 0U;\n" ++
          "enum1.In.__1 = 0U;")
  describe "Pretty printing struct variable declarations" $ do
    it "Prints the statement var struct0 : TMDescriptor = {field0 = 0 : u32; field1 = {field_a = 0U; field_b = 0xFFFF0000U} : StructA} : TMDescriptor;" $ do
      renderStatement struct0 `shouldBe`
        pack (
        "\nTMDescriptor struct0;\n" ++
        "struct0.field0 = 0U;\n" ++
        "struct0.field1.field_a = 0U;\n" ++
        "for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
        "    struct0.field1.field_b[__i0] = 0U;\n" ++
        "}\n" ++
        "struct0.field1.field_c = 4294901760U;")
    it "Prints the statement var struct1 : TMDescriptor = struct0;" $ do
      renderStatement struct1 `shouldBe`
        pack "\nTMDescriptor struct1 = struct0;"
  describe "Pretty printing array variable declarations" $ do
    it "Prints the statement var array1 : [u32; 10 : u32] = array0;" $ do
      renderStatement array1 `shouldBe`
        pack (
          "\nuint32_t array1[10U];\n" ++
          "for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    array1[__i0] = array0[__i0];\n" ++
          "}")
    it "Prints the statement var array2 : [[u32; 5 : u32]; 10 : u32] = array0;" $ do
      renderStatement array2 `shouldBe`
        pack (
          "\nint64_t array2[10U][5U];\n" ++
          "for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    for (size_t __i1 = 0U; __i1 < 5U; __i1 = __i1 + 1U) {\n" ++
          "        array2[__i0][__i1] = array1[__i0][__i1];\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement var array3 : [u32; 10 : u32] = [0 : u32; 10 : u32];" $ do
      renderStatement array3 `shouldBe`
        pack (
          "\nuint32_t array3[10U];\n" ++
          "for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    array3[__i0] = 0U;\n" ++
          "}")
    it "Prints the statement var array4 : [[u32; 5 : u32]; 10 : u32] = [[0 : u32; 5 : u32]; 10 : u32];" $ do
      renderStatement array4 `shouldBe`
        pack (
          "\nint64_t array4[10U][5U];\n" ++
          "for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    for (size_t __i1 = 0U; __i1 < 5U; __i1 = __i1 + 1U) {\n" ++
          "        array4[__i0][__i1] = 0U;\n" ++
          "    }\n" ++
          "}")
    it ("Prints the statement var array6 : [TMDescriptor; 20 : u32] = " ++
        "[{field0 = 0 : u32; field1 = {field_a = 0U; field_b = 0xFFFF0000} : StructA} : TMDescriptor; 20 : u32];") $ do
      renderStatement array5 `shouldBe`
        pack (
          "\nTMDescriptor array5[20U];\n" ++
          "for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    array5[__i0].field0 = 0U;\n" ++
          "    array5[__i0].field1.field_a = 0U;\n" ++
          "    for (size_t __i1 = 0U; __i1 < 10U; __i1 = __i1 + 1U) {\n" ++
          "        array5[__i0].field1.field_b[__i1] = 0U;\n" ++
          "    }\n" ++
          "    array5[__i0].field1.field_c = 4294901760U;\n" ++
          "}")
