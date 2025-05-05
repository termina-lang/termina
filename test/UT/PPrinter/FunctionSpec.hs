module UT.PPrinter.FunctionSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text
import Generator.Monadic
import Semantic.Types
import UT.PPrinter.Common

tmDescriptorTS :: TerminaType SemanticAnn
tmDescriptorTS = TStruct "TMDescriptor"

constUInt32 :: Expression SemanticAnn
-- | 1024 : u32
constUInt32 = Constant (I (TInteger 1024 DecRepr) (Just TUInt32)) uint32ExprSemAnn

arrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable TUInt32 (buildConstExprTUSize 10)

arrayExprAnn :: SemanticAnn
arrayExprAnn = arrayExprSemAnn TUInt32 (buildConstExprTUSize 10)

refArrayAnn :: SemanticAnn
refArrayAnn = refArraySemAnn TUInt32 (buildConstExprTUSize 10)

tmDescriptorObjSemAnn :: SemanticAnn
tmDescriptorObjSemAnn = structObjSemAnn Mutable "TMDescriptor"

structAExprSemAnn, tmDescriptorExprSemAnn :: SemanticAnn
structAExprSemAnn = structExprSemAnn "StructA"
tmDescriptorExprSemAnn = structExprSemAnn "TMDescriptor"

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

structAFieldsInit1 :: Expression SemanticAnn
structAFieldsInit1 = 
    StructInitializer
        [FieldValueAssignment "field_a" (AccessObject (Variable "param0" (objSemAnn Mutable TUInt32))) stmtSemAnn,
         FieldValueAssignment "field_b" (ArrayInitializer uint32Const0 (buildConstExprTUSize 10) arrayExprAnn) stmtSemAnn,
         FieldValueAssignment "field_c" uint32Const0xFFFF0000 stmtSemAnn] structAExprSemAnn

structAFieldsInit3 :: Expression SemanticAnn
structAFieldsInit3 = 
    StructInitializer
        [FieldValueAssignment "field_a" (AccessObject (Variable "param0" (objSemAnn Mutable TUInt32))) stmtSemAnn,
         FieldValueAssignment "field_b" (AccessObject (Dereference (Variable "param1" refArrayAnn) arrayObjAnn)) stmtSemAnn,
         FieldValueAssignment "field_c" uint32Const0xFFFF0000 stmtSemAnn] structAExprSemAnn

tmDescriptorFieldsInit0 :: Expression SemanticAnn
tmDescriptorFieldsInit0 = 
    StructInitializer
        [FieldValueAssignment "field0" uint32Const0 stmtSemAnn,
         FieldValueAssignment "field1" structAFieldsInit0 stmtSemAnn] tmDescriptorExprSemAnn

tmDescriptorFieldsInit1 :: Expression SemanticAnn
tmDescriptorFieldsInit1 = 
    StructInitializer
        [FieldValueAssignment "field0" uint32Const0 stmtSemAnn,
         FieldValueAssignment "field1" structAFieldsInit1 stmtSemAnn] tmDescriptorExprSemAnn

tmDescriptorFieldsInit3 :: Expression SemanticAnn
tmDescriptorFieldsInit3 = 
    StructInitializer
        [FieldValueAssignment "field0" uint32Const0 stmtSemAnn,
         FieldValueAssignment "field1" structAFieldsInit3 stmtSemAnn] tmDescriptorExprSemAnn

struct0Declaration0, struct0Declaration1, struct0Declaration2, struct1Declaration :: Statement SemanticAnn
struct0Declaration0 = Declaration "struct0" Mutable tmDescriptorTS tmDescriptorFieldsInit0 stmtSemAnn
struct0Declaration1 = Declaration "struct0" Mutable tmDescriptorTS tmDescriptorFieldsInit1 stmtSemAnn
struct0Declaration2 = Declaration "struct0" Mutable tmDescriptorTS tmDescriptorFieldsInit3 stmtSemAnn
struct1Declaration = Declaration "struct1" Mutable tmDescriptorTS (AccessObject (Variable "struct0" tmDescriptorObjSemAnn)) stmtSemAnn

struct0 :: Object SemanticAnn
struct0 = Variable "struct0" tmDescriptorObjSemAnn

struct0field0 :: Expression SemanticAnn
struct0field0 = AccessObject (MemberAccess struct0 "field0" (objSemAnn Mutable TUInt32))

struct0Assignment0 :: Statement SemanticAnn
struct0Assignment0 = AssignmentStmt (MemberAccess struct0 "field0" (objSemAnn Mutable TUInt32)) (BinOp Addition struct0field0 constUInt32 uint32ExprSemAnn) unitSemAnn

returnVoid :: Statement SemanticAnn
returnVoid = ReturnStmt Nothing unitSemAnn

returnStructField0 :: Statement SemanticAnn
returnStructField0 = ReturnStmt (Just struct0field0) uint32ExprSemAnn

function0 :: AnnASTElement SemanticAnn
function0 = Function "function0" [] Nothing (Block [struct0Declaration0, struct1Declaration, struct0Assignment0, returnVoid] stmtSemAnn) [] unitSemAnn

function1 :: AnnASTElement SemanticAnn
function1 = Function "function1" [] (Just TUInt32) (Block [struct0Declaration0, struct1Declaration, struct0Assignment0, returnStructField0] stmtSemAnn) [] unitSemAnn

function2 :: AnnASTElement SemanticAnn
function2 = Function "function2" [Parameter "param0" TUInt32] (Just TUInt32)
  (Block [
    struct0Declaration1, 
    struct1Declaration, 
    struct0Assignment0, 
    returnStructField0] stmtSemAnn) [] unitSemAnn

function3 :: AnnASTElement SemanticAnn
function3 = Function "function3" [Parameter "param0" TUInt32, Parameter "param1" (TReference Mutable (TArray TUInt32 (buildConstExprTUSize 10)))] (Just TUInt32) 
  (Block [
    struct0Declaration2, 
    struct1Declaration, 
    struct0Assignment0,
    returnStructField0] stmtSemAnn) [] unitSemAnn

spec :: Spec
spec = do
  describe "Pretty printing function declarations" $ do
    it "Prints fuction0 declaration" $ do
      renderFunctionDecl emptyMonadicTypes function0 `shouldBe`
        pack "\nvoid function0();"
    it "Prints fuction1 declaration" $ do
      renderFunctionDecl emptyMonadicTypes function1 `shouldBe`
        pack "\nuint32_t function1();"
    it "Prints fuction2 declaration" $ do
      renderFunctionDecl emptyMonadicTypes function2 `shouldBe`
        pack "\nuint32_t function2(uint32_t param0);"
    it "Prints fuction4 declaration" $ do
      renderFunctionDecl emptyMonadicTypes function3 `shouldBe`
        pack "\nuint32_t function3(uint32_t param0, uint32_t param1[10U]);"
  describe "Pretty printing function definitions" $ do
    it "Prints fuction0 definition" $ do
      renderFunction function0 `shouldBe`
        pack ("\nvoid function0() {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "    struct0.field0 = 0U;\n" ++
              "    struct0.field1.field_a = 0U;\n" ++
              "    for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
              "        struct0.field1.field_b[__i0] = 0U;\n" ++
              "    }\n" ++
              "    struct0.field1.field_c = 4294901760U;\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024U;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}")
    it "Prints fuction1 definition" $ do
      renderFunction function1 `shouldBe`
        pack ("\nuint32_t function1() {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "    struct0.field0 = 0U;\n" ++
              "    struct0.field1.field_a = 0U;\n" ++
              "    for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
              "        struct0.field1.field_b[__i0] = 0U;\n" ++
              "    }\n" ++
              "    struct0.field1.field_c = 4294901760U;\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024U;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}")
    it "Prints fuction2 definition" $ do
      renderFunction function2 `shouldBe`
        pack ("\nuint32_t function2(uint32_t param0) {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "    struct0.field0 = 0U;\n" ++
              "    struct0.field1.field_a = param0;\n" ++
              "    for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
              "        struct0.field1.field_b[__i0] = 0U;\n" ++
              "    }\n" ++
              "    struct0.field1.field_c = 4294901760U;\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024U;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}")
    it "Prints fuction4 definition" $ do
      renderFunction function3 `shouldBe`
        pack ("\nuint32_t function3(uint32_t param0, uint32_t param1[10U]) {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "    struct0.field0 = 0U;\n" ++
              "    struct0.field1.field_a = param0;\n" ++
              "    for (size_t __i0 = 0U; __i0 < 10U; __i0 = __i0 + 1U) {\n" ++
              "        struct0.field1.field_b[__i0] = param1[__i0];\n" ++
              "    }\n" ++
              "    struct0.field1.field_c = 4294901760U;\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024U;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}")
