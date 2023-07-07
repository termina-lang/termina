module UT.PPrinter.Statement.AssignmentSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Statement
import UT.PPrinter.Expression.Common

tmDescriptorTS :: TypeSpecifier
tmDescriptorTS = DefinedType "TMDescriptor"

optionDynUInt32SemAnn :: SemanticAnns
optionDynUInt32SemAnn = optionDynSemAnn UInt32

vectorAnn, vectorTMDescriptorAnn, twoDymVectorAnn, twoDymVectorRowAnn :: SemanticAnns
vectorAnn = vectorSemAnn UInt32 (I UInt32 10)
vectorTMDescriptorAnn = vectorSemAnn tmDescriptorTS (I UInt32 20)
twoDymVectorRowAnn = vectorSemAnn Int64 (I UInt32 5)
twoDymVectorAnn = twoDymVectorSemAnn Int64 (I UInt32 5) (I UInt32 10)

vector0 :: Expression SemanticAnns
vector0 = Variable "vector0" vectorAnn

vector1, vector2, vector3, vector4, vector5, vector6 :: Statement SemanticAnns
vector1 = AssignmentStmt "vector1" vector0 undefined
vector2 = AssignmentStmt "vector2" (Variable "vector1" twoDymVectorAnn) undefined
vector3 = AssignmentStmt "vector3" (VectorInitExpression uint32Const0 (KC (I UInt32 10)) vectorAnn) undefined
vector4 = AssignmentStmt "vector4" (VectorInitExpression (VectorInitExpression uint32Const0 (KC (I UInt32 5)) twoDymVectorRowAnn) (KC (I UInt32 10)) twoDymVectorAnn) undefined
vector5 = AssignmentStmt "vector5" (VectorInitExpression (Variable "vector_row" twoDymVectorRowAnn) (KC (I UInt32 10)) twoDymVectorAnn) undefined
vector6 = AssignmentStmt "vector6" (VectorInitExpression tmDescriptorFieldsInit0 (KC (I UInt32 10)) vectorTMDescriptorAnn) undefined

foo0 :: Expression SemanticAnns
foo0 = Variable "foo0" uint32SemAnn

foo1, foo2 :: Statement SemanticAnns
foo1 = AssignmentStmt "foo1" foo0 undefined
foo2 = AssignmentStmt "foo2" uint32Const0 undefined

structASemAnn, tmDescriptorSemAnn, messageSemAnn :: SemanticAnns
structASemAnn = definedTypeSemAnn "StructA"
tmDescriptorSemAnn = definedTypeSemAnn "TMDescriptor"
messageSemAnn = definedTypeSemAnn "Message"

uint32Const0, uint32Const0xFFFF0000 :: Expression SemanticAnns
uint32Const0 = Constant (I UInt32 0) uint32SemAnn
uint32Const0xFFFF0000 = Constant (I UInt32 4294901760) uint32SemAnn

-- | Initialization expression:
-- { field_a = 0 : u32, field_b = 0xFFFF0000 : u32 } : StructA
structAFieldsInit0 :: Expression SemanticAnns
structAFieldsInit0 = 
    FieldValuesAssignmentsExpression "StructA"
        [FieldValueAssignment "field_a" uint32Const0,
         FieldValueAssignment "field_b" (VectorInitExpression uint32Const0 (KC (I UInt32 10)) vectorAnn),
         FieldValueAssignment "field_c" uint32Const0xFFFF0000] structASemAnn

tmDescriptorFieldsInit0 :: Expression SemanticAnns
tmDescriptorFieldsInit0 = 
    FieldValuesAssignmentsExpression "TMDescriptor"
        [FieldValueAssignment "field0" uint32Const0,
         FieldValueAssignment "field1" structAFieldsInit0] tmDescriptorSemAnn

struct0, struct1 :: Statement SemanticAnns
struct0 = AssignmentStmt "struct0" tmDescriptorFieldsInit0 undefined
struct1 = AssignmentStmt "struct1" (Variable "struct0" tmDescriptorSemAnn) undefined

enum0, enum1 :: Statement SemanticAnns
enum0 = AssignmentStmt "enum0" (EnumVariantExpression "Message" "Reset" [] messageSemAnn) undefined
enum1 = AssignmentStmt "enum1" (EnumVariantExpression "Message" "In" [uint32Const0, uint32Const0] messageSemAnn) undefined

dynVar0 :: Expression SemanticAnns
dynVar0 = Variable "dyn_var0" dynUInt32SemAnn

option0, option1 :: Statement SemanticAnns
option0 = AssignmentStmt "option0" (OptionVariantExpression (Some dynVar0) optionDynUInt32SemAnn) undefined
option1 = AssignmentStmt "option1" (OptionVariantExpression None optionDynUInt32SemAnn) undefined

renderStatement :: Statement SemanticAnns -> Text
renderStatement = render . ppStatement empty

spec :: Spec
spec = do
  describe "Pretty printing integer variable declarations" $ do
    it "Prints the statement foo1 = foo0; where foo1 : u32" $ do
      renderStatement foo1 `shouldBe`
        pack "foo1 = foo0;"
    it "Prints the statement foo2 = 0 : u32; where foo2 : u32" $ do
      renderStatement foo2 `shouldBe`
        pack "foo2 = (uint32_t)0;"
  describe "Pretty printing option variable declarations" $ do
    it "Prints the statement option0 = Some(dyn_var0); where option0 : Option <'dyn u32>" $ do
      renderStatement option0 `shouldBe`
        pack (
          "{\n" ++
          "    option0.__variant = Some;\n" ++
          "    option0.__Some.__0 = dyn_var0;\n" ++
          "}")
    it "Prints the statement option1 = None; where option1 : Option <'dyn u32>" $ do
      renderStatement option1 `shouldBe`
        pack (
          "{\n" ++
          "    option1.__variant = None;\n" ++
          "}")
  describe "Pretty printing enum variable declarations" $ do
    it "Prints the statement enum0 = Message::Reset; where enum0 : Message" $ do
      renderStatement enum0 `shouldBe`
        pack (
          "{\n" ++
          "    enum0.__variant = Reset;\n" ++
          "}")
    it "Prints the statement enum1 = Message::In(0 : u32, 0 : u32); where enum1 : Message" $ do
      renderStatement enum1 `shouldBe`
        pack (
          "{\n" ++
          "    enum1.__variant = In;\n" ++
          "    enum1.__In.__0 = (uint32_t)0;\n" ++
          "    enum1.__In.__1 = (uint32_t)0;\n" ++
          "}")
  describe "Pretty printing struct variable declarations" $ do
    it "Prints the statement struct0 = {field0 = 0 : u32; field1 = {field_a = 0; field_b = 0xFFFF0000} : StructA} : TMDescriptor; where struct0 : TMDescriptor" $ do
      renderStatement struct0 `shouldBe`
        pack (
        "{\n" ++
        "    struct0.field0 = (uint32_t)0;\n" ++
        "    struct0.field1.field_a = (uint32_t)0;\n" ++
        "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
        "        struct0.field1.field_b[__i0] = (uint32_t)0;\n" ++
        "    }\n" ++
        "    struct0.field1.field_c = (uint32_t)4294901760;\n" ++
        "}")
    it "Prints the statement struct1 = struct0; where struct1 : TMDescriptor" $ do
      renderStatement struct1 `shouldBe`
        pack "struct1 = struct0;"
  describe "Pretty printing vector variable declarations" $ do
    it "Prints the statement vector1 = vector0; where vector1 : [u32; 10 : u32]" $ do
      renderStatement vector1 `shouldBe`
        pack (
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        vector1[__i0] = vector0[__i0];\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement vector2 = vector0; where vector2 : [[u32; 5 : u32]; 10 : u32]" $ do
      renderStatement vector2 `shouldBe`
        pack (
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        for (uint32_t __i1 = 0; __i1 < (uint32_t)5; __i1 = __i1 + (uint32_t)1) {\n" ++
          "            vector2[__i0][__i1] = vector1[__i0][__i1];\n" ++
          "        }\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement vector3 = [0 : u32; 10 : u32]; where vector3 : [u32; 10 : u32]" $ do
      renderStatement vector3 `shouldBe`
        pack (
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        vector3[__i0] = (uint32_t)0;\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement vector4 = [[0 : u32; 5 : u32]; 10 : u32]; where vector4 : [[u32; 5 : u32]; 10 : u32]s" $ do
      renderStatement vector4 `shouldBe`
        pack (
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        for (uint32_t __i1 = 0; __i1 < (uint32_t)5; __i1 = __i1 + (uint32_t)1) {\n" ++
          "            vector4[__i0][__i1] = (uint32_t)0;\n" ++
          "        }\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement vector4 = [vector_row; 10 : u32]; where vector4 : [[u32; 5 : u32]; 10 : u32]" $ do
      renderStatement vector5 `shouldBe`
        pack (
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        for (uint32_t __i1 = 0; __i1 < (uint32_t)5; __i1 = __i1 + (uint32_t)1) {\n" ++
          "            vector5[__i0][__i1] = vector_row[__i1];\n" ++
          "        }\n" ++
          "    }\n" ++
          "}")
    it ("Prints the statement vector6 = " ++
        "[{field0 = 0 : u32; field1 = {field_a = 0; field_b = 0xFFFF0000} : StructA} : TMDescriptor; 20 : u32];") $ do
      renderStatement vector6 `shouldBe`
        pack (
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        vector6[__i0].field0 = (uint32_t)0;\n" ++
          "        vector6[__i0].field1.field_a = (uint32_t)0;\n" ++
          "        for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "            vector6[__i0].field1.field_b[__i0] = (uint32_t)0;\n" ++
          "        }\n" ++
          "        vector6[__i0].field1.field_c = (uint32_t)4294901760;\n" ++
          "    }\n" ++
          "}")
