module UT.PPrinter.FunctionSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import Semantic.Monad
import UT.PPrinter.Expression.Common
import PPrinter.Function

tmDescriptorTS :: TypeSpecifier
tmDescriptorTS = DefinedType "TMDescriptor"

constUInt32 :: Expression SemanticAnns
-- | 1024 : u32
constUInt32 = Constant (I UInt32 1024) uint32SemAnn

vectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn Mutable UInt32 (K 10)

refVectorAnn :: SemanticAnns
refVectorAnn = refVectorSemAnn UInt32 (K 10)

structASemAnn, tmDescriptorSemAnn :: SemanticAnns
structASemAnn = definedTypeSemAnn Mutable "StructA"
tmDescriptorSemAnn = definedTypeSemAnn Mutable "TMDescriptor"

uint32Const0, uint32Const0xFFFF0000 :: Expression SemanticAnns
uint32Const0 = Constant (I UInt32 0) uint32SemAnn
uint32Const0xFFFF0000 = Constant (I UInt32 4294901760) uint32SemAnn

-- | Initialization expression:
-- { field_a = 0 : u32, field_b = 0xFFFF0000 : u32 } : StructA
structAFieldsInit0 :: Expression SemanticAnns
structAFieldsInit0 = 
    FieldAssignmentsExpression "StructA"
        [FieldValueAssignment "field_a" uint32Const0,
         FieldValueAssignment "field_b" (VectorInitExpression uint32Const0 (K 10) vectorAnn),
         FieldValueAssignment "field_c" uint32Const0xFFFF0000] structASemAnn

structAFieldsInit1 :: Expression SemanticAnns
structAFieldsInit1 = 
    FieldAssignmentsExpression "StructA"
        [FieldValueAssignment "field_a" (AccessObject (Variable "param0" (objSemAnn Mutable UInt32))),
         FieldValueAssignment "field_b" (VectorInitExpression uint32Const0 (K 10) vectorAnn),
         FieldValueAssignment "field_c" uint32Const0xFFFF0000] structASemAnn

structAFieldsInit2 :: Expression SemanticAnns
structAFieldsInit2 = 
    FieldAssignmentsExpression "StructA"
        [FieldValueAssignment "field_a" (AccessObject (Variable "param0" (objSemAnn Mutable UInt32))),
         FieldValueAssignment "field_b" (AccessObject (Variable "param1" vectorAnn)),
         FieldValueAssignment "field_c" uint32Const0xFFFF0000] structASemAnn

structAFieldsInit3 :: Expression SemanticAnns
structAFieldsInit3 = 
    FieldAssignmentsExpression "StructA"
        [FieldValueAssignment "field_a" (AccessObject (Variable "param0" (objSemAnn Mutable UInt32))),
         FieldValueAssignment "field_b" (AccessObject (Dereference (Variable "param1" refVectorAnn) vectorAnn)),
         FieldValueAssignment "field_c" uint32Const0xFFFF0000] structASemAnn

tmDescriptorFieldsInit0 :: Expression SemanticAnns
tmDescriptorFieldsInit0 = 
    FieldAssignmentsExpression "TMDescriptor"
        [FieldValueAssignment "field0" uint32Const0,
         FieldValueAssignment "field1" structAFieldsInit0] tmDescriptorSemAnn

tmDescriptorFieldsInit1 :: Expression SemanticAnns
tmDescriptorFieldsInit1 = 
    FieldAssignmentsExpression "TMDescriptor"
        [FieldValueAssignment "field0" uint32Const0,
         FieldValueAssignment "field1" structAFieldsInit1] tmDescriptorSemAnn

tmDescriptorFieldsInit2 :: Expression SemanticAnns
tmDescriptorFieldsInit2 = 
    FieldAssignmentsExpression "TMDescriptor"
        [FieldValueAssignment "field0" uint32Const0,
         FieldValueAssignment "field1" structAFieldsInit2] tmDescriptorSemAnn

tmDescriptorFieldsInit3 :: Expression SemanticAnns
tmDescriptorFieldsInit3 = 
    FieldAssignmentsExpression "TMDescriptor"
        [FieldValueAssignment "field0" uint32Const0,
         FieldValueAssignment "field1" structAFieldsInit3] tmDescriptorSemAnn

struct0Declaration0, struct0Declaration1, struct0Declaration2, struct0Declaration3, struct1Declaration :: Statement SemanticAnns
struct0Declaration0 = Declaration "struct0" Mutable tmDescriptorTS tmDescriptorFieldsInit0 undefined
struct0Declaration1 = Declaration "struct0" Mutable tmDescriptorTS tmDescriptorFieldsInit1 undefined
struct0Declaration2 = Declaration "struct0" Mutable tmDescriptorTS tmDescriptorFieldsInit2 undefined
struct0Declaration3 = Declaration "struct0" Mutable tmDescriptorTS tmDescriptorFieldsInit3 undefined
struct1Declaration = Declaration "struct1" Mutable tmDescriptorTS (AccessObject (Variable "struct0" tmDescriptorSemAnn)) undefined

struct0 :: Object SemanticAnns
struct0 = Variable "struct0" tmDescriptorSemAnn

struct0field0 :: Expression SemanticAnns
struct0field0 = AccessObject (MemberAccess struct0 "field0" (objSemAnn Mutable UInt32))

struct0Assignment0 :: Statement SemanticAnns
struct0Assignment0 = AssignmentStmt (MemberAccess (Variable "struct0" tmDescriptorSemAnn) "field0" (objSemAnn Mutable UInt32)) (BinOp Addition struct0field0 constUInt32 uint32SemAnn) unitSemAnn

returnVoid :: ReturnStmt SemanticAnns
returnVoid = ReturnStmt Nothing unitSemAnn

returnStructField0 :: ReturnStmt SemanticAnns
returnStructField0 = ReturnStmt (Just struct0field0) uint32SemAnn

function0 :: AnnASTElement SemanticAnns
function0 = Function "function0" [] Nothing (BlockRet [struct0Declaration0, struct1Declaration, struct0Assignment0] returnVoid) [] unitSemAnn

function1 :: AnnASTElement SemanticAnns
function1 = Function "function1" [] (Just UInt32) (BlockRet [struct0Declaration0, struct1Declaration, struct0Assignment0] returnStructField0) [] unitSemAnn

function2 :: AnnASTElement SemanticAnns
function2 = Function "function2" [Parameter "param0" UInt32] (Just UInt32)
  (BlockRet [
    struct0Declaration1, 
    struct1Declaration, 
    struct0Assignment0] returnStructField0) [] unitSemAnn

function3 :: AnnASTElement SemanticAnns
function3 = Function "function3" [Parameter "param0" UInt32, Parameter "param1" (Vector UInt32 (K 10))] (Just UInt32) 
  (BlockRet [
    struct0Declaration2, 
    struct1Declaration, 
    struct0Assignment0
    ] returnStructField0) [] unitSemAnn

function4 :: AnnASTElement SemanticAnns
function4 = Function "function4" [Parameter "param0" UInt32, Parameter "param1" (Reference Mutable (Vector UInt32 (K 10)))] (Just UInt32) 
  (BlockRet [
    struct0Declaration3, 
    struct1Declaration, 
    struct0Assignment0
    ] returnStructField0) [] unitSemAnn

renderFunctionDeclaration :: AnnASTElement SemanticAnns -> Text
renderFunctionDeclaration = render . ppFunctionDeclaration

renderFunction :: AnnASTElement SemanticAnns -> Text
renderFunction = render . ppFunction

spec :: Spec
spec = do
  describe "Pretty printing function declarations" $ do
    it "Prints fuction0 declaration" $ do
      renderFunctionDeclaration function0 `shouldBe`
        pack "void function0();\n"
    it "Prints fuction1 declaration" $ do
      renderFunctionDeclaration function1 `shouldBe`
        pack "uint32_t function1();\n"
    it "Prints fuction2 declaration" $ do
      renderFunctionDeclaration function2 `shouldBe`
        pack "uint32_t function2(uint32_t param0);\n"
    it "Prints fuction3 declaration" $ do
      renderFunctionDeclaration function3 `shouldBe`
        pack ("typedef struct {\n" ++
              "    uint32_t array[10];\n" ++
              "} __param_function3_param1_t;\n" ++
              "\n" ++
              "uint32_t function3(uint32_t param0, __param_function3_param1_t param1);\n")
    it "Prints fuction4 declaration" $ do
      renderFunctionDeclaration function4 `shouldBe`
        pack "uint32_t function4(uint32_t param0, uint32_t param1[10]);\n"
  describe "Pretty printing function definitions" $ do
    it "Prints fuction0 definition" $ do
      renderFunction function0 `shouldBe`
        pack ("void function0() {\n" ++
              "\n" ++
              "    TMDescriptor struct0;\n" ++
              "\n" ++
              "    {\n" ++
              "        struct0.field0 = 0;\n" ++
              "        struct0.field1.field_a = 0;\n" ++
              "        for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            struct0.field1.field_b[__i0] = 0;\n" ++
              "        }\n" ++
              "        struct0.field1.field_c = 4294901760;\n" ++
              "    }\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}\n")
    it "Prints fuction1 definition" $ do
      renderFunction function1 `shouldBe`
        pack ("uint32_t function1() {\n" ++
              "\n" ++
              "    TMDescriptor struct0;\n" ++
              "\n" ++
              "    {\n" ++
              "        struct0.field0 = 0;\n" ++
              "        struct0.field1.field_a = 0;\n" ++
              "        for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            struct0.field1.field_b[__i0] = 0;\n" ++
              "        }\n" ++
              "        struct0.field1.field_c = 4294901760;\n" ++
              "    }\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}\n")
    it "Prints fuction2 definition" $ do
      renderFunction function2 `shouldBe`
        pack ("uint32_t function2(uint32_t param0) {\n" ++
              "\n" ++
              "    TMDescriptor struct0;\n" ++
              "\n" ++
              "    {\n" ++
              "        struct0.field0 = 0;\n" ++
              "        struct0.field1.field_a = param0;\n" ++
              "        for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            struct0.field1.field_b[__i0] = 0;\n" ++
              "        }\n" ++
              "        struct0.field1.field_c = 4294901760;\n" ++
              "    }\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}\n")
    it "Prints fuction3 definition" $ do
      renderFunction function3 `shouldBe`
        pack ("uint32_t function3(uint32_t param0, __param_function3_param1_t param1) {\n" ++
              "\n" ++
              "    TMDescriptor struct0;\n" ++
              "\n" ++
              "    {\n" ++
              "        struct0.field0 = 0;\n" ++
              "        struct0.field1.field_a = param0;\n" ++
              "        for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            struct0.field1.field_b[__i0] = param1.array[__i0];\n" ++
              "        }\n" ++
              "        struct0.field1.field_c = 4294901760;\n" ++
              "    }\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}\n")
    it "Prints fuction4 definition" $ do
      renderFunction function4 `shouldBe`
        pack ("uint32_t function4(uint32_t param0, uint32_t param1[10]) {\n" ++
              "\n" ++
              "    TMDescriptor struct0;\n" ++
              "\n" ++
              "    {\n" ++
              "        struct0.field0 = 0;\n" ++
              "        struct0.field1.field_a = param0;\n" ++
              "        for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "            struct0.field1.field_b[__i0] = param1[__i0];\n" ++
              "        }\n" ++
              "        struct0.field1.field_c = 4294901760;\n" ++
              "    }\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}\n")
