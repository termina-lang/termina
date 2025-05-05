module UT.PPrinter.Statement.AssignmentSpec (spec) where

import UT.PPrinter.Common

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types

tmDescriptorTS :: TerminaType SemanticAnn
tmDescriptorTS = TStruct "TMDescriptor"

optionBoxUInt32ObjSemAnn :: SemanticAnn
optionBoxUInt32ObjSemAnn = optionBoxObjSemAnn Mutable TUInt32

optionBoxUInt32ExprSemAnn :: SemanticAnn
optionBoxUInt32ExprSemAnn = optionBoxExprSemAnn TUInt32

arrayObjAnn, arrayTMDescriptorObjAnn, twoDymArrayRowObjAnn, twoDymArrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable TUInt32 (buildConstExprTUSize 10)
arrayTMDescriptorObjAnn = arrayObjSemAnn Mutable tmDescriptorTS (buildConstExprTUSize 20)
twoDymArrayRowObjAnn = arrayObjSemAnn Mutable TInt64 (buildConstExprTUSize 5)
twoDymArrayObjAnn = twoDymArrayObjSemAnn Mutable TInt64 (buildConstExprTUSize 5) (buildConstExprTUSize 10)

arrayExprAnn, arrayTMDescriptorExprAnn, twoDymArrayRowExprAnn, twoDymArrayExprAnn  :: SemanticAnn
arrayExprAnn = arrayExprSemAnn TUInt32 (buildConstExprTUSize 10)
arrayTMDescriptorExprAnn = arrayExprSemAnn tmDescriptorTS (buildConstExprTUSize 20)
twoDymArrayRowExprAnn = arrayExprSemAnn TInt64 (buildConstExprTUSize 5)
twoDymArrayExprAnn = twoDymArrayExprSemAnn TInt64 (buildConstExprTUSize 5) (buildConstExprTUSize 10)

array1, array2, array3, array4, array5, array6 :: Object SemanticAnn
array1 = Variable "array1" arrayObjAnn
array2 = Variable "array2" twoDymArrayObjAnn
array3 = Variable "array3" arrayObjAnn
array4 = Variable "array4" twoDymArrayObjAnn
array5 = Variable "array5" twoDymArrayObjAnn
array6 = Variable "array6" arrayTMDescriptorObjAnn

array1Assign, array2Assign, array3Assign, array4Assign, array5Assign, array6Assign :: Statement SemanticAnn
array1Assign = AssignmentStmt array1 (AccessObject (Variable "array0" arrayObjAnn)) stmtSemAnn
array2Assign = AssignmentStmt array2 (AccessObject (Variable "array1" twoDymArrayObjAnn)) stmtSemAnn
array3Assign = AssignmentStmt array3 (ArrayInitializer uint32Const0 (buildConstExprTUSize 10) arrayExprAnn) stmtSemAnn
array4Assign = AssignmentStmt array4 (ArrayInitializer (ArrayInitializer uint32Const0 (buildConstExprTUSize 5) twoDymArrayRowExprAnn) (buildConstExprTUSize 10) twoDymArrayExprAnn) stmtSemAnn
array5Assign = AssignmentStmt array5 (ArrayInitializer (AccessObject (Variable "array_row" twoDymArrayRowObjAnn)) (buildConstExprTUSize 10) twoDymArrayExprAnn) stmtSemAnn
array6Assign = AssignmentStmt array6 (ArrayInitializer tmDescriptorFieldsInit0 (buildConstExprTUSize 10) arrayTMDescriptorExprAnn) stmtSemAnn

foo1, foo2 :: Object SemanticAnn
foo1 = Variable "foo1" (objSemAnn Mutable TUInt32)
foo2 = Variable "foo2" (objSemAnn Mutable TUInt32)

foo1Assign, foo2Assign :: Statement SemanticAnn
foo1Assign = AssignmentStmt foo1 (AccessObject (Variable "foo0" (objSemAnn Mutable TUInt32))) undefined
foo2Assign = AssignmentStmt foo2 uint32Const0 undefined

tmDescriptorObjSemAnn, messageObjSemAnn :: SemanticAnn
tmDescriptorObjSemAnn = structObjSemAnn Mutable "TMDescriptor"
messageObjSemAnn = structObjSemAnn Mutable "Message"

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
        [FieldValueAssignment "field_a" uint32Const0 stmtSemAnn,
         FieldValueAssignment "field_b" (ArrayInitializer uint32Const0 (buildConstExprTUSize 10) arrayExprAnn) stmtSemAnn,
         FieldValueAssignment "field_c" uint32Const0xFFFF0000 stmtSemAnn] structAExprSemAnn

tmDescriptorFieldsInit0 :: Expression SemanticAnn
tmDescriptorFieldsInit0 = 
    StructInitializer
        [FieldValueAssignment "field0" uint32Const0 stmtSemAnn,
         FieldValueAssignment "field1" structAFieldsInit0 stmtSemAnn] tmDescriptorExprSemAnn


struct0, struct1 :: Object SemanticAnn
struct0 = Variable "struct0" tmDescriptorObjSemAnn
struct1 = Variable "struct1" tmDescriptorObjSemAnn

struct0Assign, struct1Assign :: Statement SemanticAnn
struct0Assign = AssignmentStmt struct0 tmDescriptorFieldsInit0 stmtSemAnn
struct1Assign = AssignmentStmt struct1 (AccessObject struct0) stmtSemAnn

enum0, enum1 :: Object SemanticAnn
enum0 = Variable "enum0" messageObjSemAnn
enum1 = Variable "enum1" messageObjSemAnn

enum0Assign, enum1Assign :: Statement SemanticAnn
enum0Assign = AssignmentStmt enum0 (EnumVariantInitializer "Message" "Reset" [] messageExprSemAnn) stmtSemAnn
enum1Assign = AssignmentStmt enum1 (EnumVariantInitializer "Message" "In" [uint32Const0, uint32Const0] messageExprSemAnn) stmtSemAnn

boxVar0 :: Object SemanticAnn
boxVar0 = Variable "box_var0" boxUInt32SemAnn

option0, option1 :: Object SemanticAnn
option0 =  Variable "option0" optionBoxUInt32ObjSemAnn
option1 =  Variable "option1" optionBoxUInt32ObjSemAnn

unboxVar0 :: Object SemanticAnn
unboxVar0 = Unbox boxVar0 (objSemAnn Mutable TUInt32)

unboxVar0AssignFoo1, unboxVar0AssignConst :: Statement SemanticAnn
unboxVar0AssignFoo1 = AssignmentStmt unboxVar0 (AccessObject foo1) stmtSemAnn
unboxVar0AssignConst = AssignmentStmt unboxVar0 (Constant (I (TInteger 1024 DecRepr) (Just TUInt32)) uint32ExprSemAnn) stmtSemAnn

option0Assign, option1Assign :: Statement SemanticAnn
option0Assign = AssignmentStmt option0 (MonadicVariantInitializer (Some (AccessObject boxVar0)) optionBoxUInt32ExprSemAnn) stmtSemAnn
option1Assign = AssignmentStmt option1 (MonadicVariantInitializer None optionBoxUInt32ExprSemAnn) stmtSemAnn

str0 :: Object SemanticAnn
str0 = Variable "str0" (objSemAnn Mutable (TArray TChar (buildConstExprTUSize 12)))

str0Assign :: Statement SemanticAnn
str0Assign = AssignmentStmt str0 (StringInitializer "Hello world!" (arrayExprSemAnn TUInt32 (buildConstExprTUSize 12))) stmtSemAnn

spec :: Spec
spec = do
  describe "Pretty printing integer variable declarations" $ do
    it "Prints the statement foo1 = foo0; where foo1 : u32" $ do
      renderStatement foo1Assign `shouldBe`
        pack "\nfoo1 = foo0;"
    it "Prints the statement foo2 = 0 : u32; where foo2 : u32" $ do
      renderStatement foo2Assign `shouldBe`
        pack "\nfoo2 = 0U;"
  describe "Pretty printing option variable declarations" $ do
    it "Prints the statement option0 = Some(box_var0); where option0 : TOption <'box u32>" $ do
      renderStatement option0Assign `shouldBe`
        pack (
          "\noption0.__variant = Some;\n" ++
          "option0.Some.__0 = box_var0;")
    it "Prints the statement option1 = None; where option1 : TOption <'box u32>" $ do
      renderStatement option1Assign `shouldBe`
        pack "\noption1.__variant = None;"
  describe "Pretty printing enum variable declarations" $ do
    it "Prints the statement enum0 = Message::Reset; where enum0 : Message" $ do
      renderStatement enum0Assign `shouldBe`
        pack "\nenum0.__variant = Message__Reset;"
    it "Prints the statement enum1 = Message::In(0 : u32, 0 : u32); where enum1 : Message" $ do
      renderStatement enum1Assign `shouldBe`
        pack (
          "\nenum1.__variant = Message__In;\n" ++
          "enum1.In.__0 = 0U;\n" ++
          "enum1.In.__1 = 0U;")
  describe "Pretty printing struct variable declarations" $ do
    it "Prints the statement struct0 = {field0 = 0 : u32; field1 = {field_a = 0U; field_b = 0xFFFF0000} : StructA} : TMDescriptor; where struct0 : TMDescriptor" $ do
      renderStatement struct0Assign `shouldBe`
        pack (
        "\nstruct0.field0 = 0U;\n" ++
        "struct0.field1.field_a = 0U;\n" ++
        "for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
        "    struct0.field1.field_b[__i0] = 0U;\n" ++
        "}\n" ++
        "struct0.field1.field_c = 4294901760U;")
    it "Prints the statement struct1 = struct0; where struct1 : TMDescriptor" $ do
      renderStatement struct1Assign `shouldBe`
        pack "\nstruct1 = struct0;"
  describe "Pretty printing array variable declarations" $ do
    it "Prints the statement array1 = array0; where array1 : [u32; 10 : u32]" $ do
      renderStatement array1Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    array1[__i0] = array0[__i0];\n" ++
          "}")
    it "Prints the statement array2 = array0; where array2 : [[u32; 5 : u32]; 10 : u32]" $ do
      renderStatement array2Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    for (size_t __i1 = 0U; __i1 < 5U; __i1 = __i1 + 1U) {\n" ++
          "        array2[__i0][__i1] = array1[__i0][__i1];\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement array3 = [0 : u32; 10 : u32]; where array3 : [u32; 10 : u32]" $ do
      renderStatement array3Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    array3[__i0] = 0U;\n" ++
          "}")
    it "Prints the statement array4 = [[0 : u32; 5 : u32]; 10 : u32]; where array4 : [[u32; 5 : u32]; 10 : u32]s" $ do
      renderStatement array4Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    for (size_t __i1 = 0U; __i1 < 5U; __i1 = __i1 + 1U) {\n" ++
          "        array4[__i0][__i1] = 0U;\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement array4 = [array_row; 10 : u32]; where array4 : [[u32; 5 : u32]; 10 : u32]" $ do
      renderStatement array5Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    for (size_t __i1 = 0U; __i1 < 5U; __i1 = __i1 + 1U) {\n" ++
          "        array5[__i0][__i1] = array_row[__i1];\n" ++
          "    }\n" ++
          "}")
    it ("Prints the statement array6 = " ++
        "[{field0 = 0 : u32; field1 = {field_a = 0U; field_b = 0xFFFF0000} : StructA} : TMDescriptor; 20 : u32];") $ do
      renderStatement array6Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
          "    array6[__i0].field0 = 0U;\n" ++
          "    array6[__i0].field1.field_a = 0U;\n" ++
          "    for (size_t __i1 = 0U; __i1 < 10U; __i1 = __i1 + 1U) {\n" ++
          "        array6[__i0].field1.field_b[__i1] = 0U;\n" ++
          "    }\n" ++
          "    array6[__i0].field1.field_c = 4294901760U;\n" ++
          "}")
    it "Prints the statement str = \"Hello world!\"" $ do
      renderStatement str0Assign `shouldBe`
        pack (
          "\nstr0[0U] = 'H';\n" ++
          "str0[1U] = 'e';\n" ++
          "str0[2U] = 'l';\n" ++
          "str0[3U] = 'l';\n" ++
          "str0[4U] = 'o';\n" ++
          "str0[5U] = ' ';\n" ++
          "str0[6U] = 'w';\n" ++
          "str0[7U] = 'o';\n" ++
          "str0[8U] = 'r';\n" ++
          "str0[9U] = 'l';\n" ++
          "str0[10U] = 'd';\n" ++
          "str0[11U] = '!';")
  describe "Pretty printing unbox assignments" $ do
    it "Prints the statement box_var0 = foo1" $ do
      renderStatement unboxVar0AssignFoo1 `shouldBe`
        pack "\n*(uint32_t *)box_var0.data = foo1;"
    it "Prints the statement box_var0 = 1024 : u32" $ do
      renderStatement unboxVar0AssignConst `shouldBe`
        pack "\n*(uint32_t *)box_var0.data = 1024U;"