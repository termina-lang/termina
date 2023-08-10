module UT.PPrinter.Statement.VariableInitializationSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Statement
import UT.PPrinter.Expression.Common

tmDescriptorTS, messageTS :: TypeSpecifier
tmDescriptorTS = DefinedType "TMDescriptor"
messageTS = DefinedType "Message"

optionDynUInt32TS :: TypeSpecifier
optionDynUInt32TS = Option (DynamicSubtype UInt32)

vectorTS, vectorTMDescriptorTS, twoDimVectorTS :: TypeSpecifier
vectorTS = Vector UInt32 (KC (I UInt32 10))
vectorTMDescriptorTS = Vector tmDescriptorTS (KC (I UInt32 20))
twoDimVectorTS = Vector (Vector Int64 (KC (I UInt32 5))) (KC (I UInt32 10))

optionDynUInt32SemAnn :: SemanticAnns
optionDynUInt32SemAnn = optionDynSemAnn UInt32

vectorAnn, vectorTMDescriptorAnn, twoDymVectorAnn, twoDymVectorRowAnn :: SemanticAnns
vectorAnn = vectorSemAnn UInt32 (I UInt32 10)
vectorTMDescriptorAnn = vectorSemAnn tmDescriptorTS (I UInt32 20)
twoDymVectorRowAnn = vectorSemAnn Int64 (I UInt32 5)
twoDymVectorAnn = twoDymVectorSemAnn Int64 (I UInt32 5) (I UInt32 10)

vector0 :: Expression SemanticAnns
vector0 = AccessObject (RHS (Variable "vector0" vectorAnn))

vector1, vector2, vector3, vector4, vector5, vector6 :: Statement SemanticAnns
vector1 = Declaration "vector1" vectorTS vector0 undefined
vector2 = Declaration "vector2" twoDimVectorTS (AccessObject (RHS (Variable "vector1" twoDymVectorAnn))) undefined
vector3 = Declaration "vector3" vectorTS (VectorInitExpression uint32Const0 (KC (I UInt32 10)) vectorAnn) undefined
vector4 = Declaration "vector4" twoDimVectorTS (VectorInitExpression (VectorInitExpression uint32Const0 (KC (I UInt32 5)) twoDymVectorRowAnn) (KC (I UInt32 10)) twoDymVectorAnn) undefined
vector5 = Declaration "vector5" twoDimVectorTS (VectorInitExpression (AccessObject (RHS (Variable "vector_row" twoDymVectorRowAnn))) (KC (I UInt32 10)) twoDymVectorAnn) undefined
vector6 = Declaration "vector6" vectorTMDescriptorTS (VectorInitExpression tmDescriptorFieldsInit0 (KC (I UInt32 10)) vectorTMDescriptorAnn) undefined

foo0 :: Expression SemanticAnns
foo0 = AccessObject (RHS (Variable "foo0" uint32SemAnn))

foo1, foo2 :: Statement SemanticAnns
foo1 = Declaration "foo1" UInt32 foo0 undefined
foo2 = Declaration "foo2" UInt32 uint32Const0 undefined

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
struct0 = Declaration "struct0" tmDescriptorTS tmDescriptorFieldsInit0 undefined
struct1 = Declaration "struct1" tmDescriptorTS (AccessObject (RHS (Variable "struct0" tmDescriptorSemAnn))) undefined

enum0, enum1 :: Statement SemanticAnns
enum0 = Declaration "enum0" messageTS (EnumVariantExpression "Message" "Reset" [] messageSemAnn) undefined
enum1 = Declaration "enum1" messageTS (EnumVariantExpression "Message" "In" [uint32Const0, uint32Const0] messageSemAnn) undefined

dynVar0 :: Expression SemanticAnns
dynVar0 = AccessObject (RHS (Variable "dyn_var0" dynUInt32SemAnn))

option0, option1 :: Statement SemanticAnns
option0 = Declaration "option0" optionDynUInt32TS (OptionVariantExpression (Some dynVar0) optionDynUInt32SemAnn) undefined
option1 = Declaration "option1" optionDynUInt32TS (OptionVariantExpression None optionDynUInt32SemAnn) undefined

renderStatement :: Statement SemanticAnns -> Text
renderStatement = render . ppStatement empty

spec :: Spec
spec = do
  describe "Pretty printing integer variable declarations" $ do
    it "Prints the statement var foo1 : u32 = foo0;" $ do
      renderStatement foo1 `shouldBe`
        pack "uint32_t foo1 = foo0;"
    it "Prints the statement var foo2 : u32 = 0 : u32;" $ do
      renderStatement foo2 `shouldBe`
        pack "uint32_t foo2 = (uint32_t)0;"
  describe "Pretty printing option variable declarations" $ do
    it "Prints the statement var option0 : Option <'dyn u32> = Some(dyn_var0);" $ do
      renderStatement option0 `shouldBe`
        pack (
          "__Option_dyn_t option0;\n" ++
          "\n" ++
          "{\n" ++
          "    option0.__variant = Some;\n" ++
          "    option0.__Some.__0 = dyn_var0;\n" ++
          "}")
    it "Prints the statement var option1 : Option <'dyn u32> = None;" $ do
      renderStatement option1 `shouldBe`
        pack (
          "__Option_dyn_t option1;\n" ++
          "\n" ++
          "{\n" ++
          "    option1.__variant = None;\n" ++
          "}")
  describe "Pretty printing enum variable declarations" $ do
    it "Prints the statement var enum0 : Message = Message::Reset;" $ do
      renderStatement enum0 `shouldBe`
        pack (
          "Message enum0;\n" ++
          "\n" ++
          "{\n" ++
          "    enum0.__variant = Reset;\n" ++
          "}")
    it "Prints the statement var enum1 : Message = Message::In(0 : u32, 0 : u32);" $ do
      renderStatement enum1 `shouldBe`
        pack (
          "Message enum1;\n" ++
          "\n" ++
          "{\n" ++
          "    enum1.__variant = In;\n" ++
          "    enum1.__In.__0 = (uint32_t)0;\n" ++
          "    enum1.__In.__1 = (uint32_t)0;\n" ++
          "}")
  describe "Pretty printing struct variable declarations" $ do
    it "Prints the statement var struct0 : TMDescriptor = {field0 = 0 : u32; field1 = {field_a = 0; field_b = 0xFFFF0000} : StructA} : TMDescriptor;" $ do
      renderStatement struct0 `shouldBe`
        pack (
        "TMDescriptor struct0;\n" ++
        "\n" ++
        "{\n" ++
        "    struct0.field0 = (uint32_t)0;\n" ++
        "    struct0.field1.field_a = (uint32_t)0;\n" ++
        "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
        "        struct0.field1.field_b[__i0] = (uint32_t)0;\n" ++
        "    }\n" ++
        "    struct0.field1.field_c = (uint32_t)4294901760;\n" ++
        "}")
    it "Prints the statement var struct1 : TMDescriptor = struct0;" $ do
      renderStatement struct1 `shouldBe`
        pack "TMDescriptor struct1 = struct0;"
  describe "Pretty printing vector variable declarations" $ do
    it "Prints the statement var vector1 : [u32; 10 : u32] = vector0;" $ do
      renderStatement vector1 `shouldBe`
        pack (
          "uint32_t vector1[10];\n" ++
          "\n" ++
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        vector1[__i0] = vector0[__i0];\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement var vector2 : [[u32; 5 : u32]; 10 : u32] = vector0;" $ do
      renderStatement vector2 `shouldBe`
        pack (
          "int64_t vector2[10][5];\n" ++
          "\n" ++
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        for (uint32_t __i1 = 0; __i1 < (uint32_t)5; __i1 = __i1 + (uint32_t)1) {\n" ++
          "            vector2[__i0][__i1] = vector1[__i0][__i1];\n" ++
          "        }\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement var vector3 : [u32; 10 : u32] = [0 : u32; 10 : u32];" $ do
      renderStatement vector3 `shouldBe`
        pack (
          "uint32_t vector3[10];\n" ++
          "\n" ++
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        vector3[__i0] = (uint32_t)0;\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement var vector4 : [[u32; 5 : u32]; 10 : u32] = [[0 : u32; 5 : u32]; 10 : u32];" $ do
      renderStatement vector4 `shouldBe`
        pack (
          "int64_t vector4[10][5];\n" ++
          "\n" ++
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        for (uint32_t __i1 = 0; __i1 < (uint32_t)5; __i1 = __i1 + (uint32_t)1) {\n" ++
          "            vector4[__i0][__i1] = (uint32_t)0;\n" ++
          "        }\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement var vector4 : [[u32; 5 : u32]; 10 : u32] = [vector_row; 10 : u32];" $ do
      renderStatement vector5 `shouldBe`
        pack (
          "int64_t vector5[10][5];\n" ++
          "\n" ++
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        for (uint32_t __i1 = 0; __i1 < (uint32_t)5; __i1 = __i1 + (uint32_t)1) {\n" ++
          "            vector5[__i0][__i1] = vector_row[__i1];\n" ++
          "        }\n" ++
          "    }\n" ++
          "}")
    it ("Prints the statement var vector6 : [TMDescriptor; 20 : u32] = " ++
        "[{field0 = 0 : u32; field1 = {field_a = 0; field_b = 0xFFFF0000} : StructA} : TMDescriptor; 20 : u32];") $ do
      renderStatement vector6 `shouldBe`
        pack (
          "TMDescriptor vector6[20];\n" ++
          "\n" ++
          "{\n" ++
          "    for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
          "        vector6[__i0].field0 = (uint32_t)0;\n" ++
          "        vector6[__i0].field1.field_a = (uint32_t)0;\n" ++
          "        for (uint32_t __i1 = 0; __i1 < (uint32_t)10; __i1 = __i1 + (uint32_t)1) {\n" ++
          "            vector6[__i0].field1.field_b[__i1] = (uint32_t)0;\n" ++
          "        }\n" ++
          "        vector6[__i0].field1.field_c = (uint32_t)4294901760;\n" ++
          "    }\n" ++
          "}")
