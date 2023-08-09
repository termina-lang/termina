module UT.PPrinter.FunctionSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Statement
import UT.PPrinter.Expression.Common
import PPrinter.Function

tmDescriptorTS, messageTS :: TypeSpecifier
tmDescriptorTS = DefinedType "TMDescriptor"
messageTS = DefinedType "Message"

constUInt32 :: Expression SemanticAnns
-- | 1024 : u32
constUInt32 = Constant (I UInt32 1024) uint32SemAnn

vectorAnn, vectorTMDescriptorAnn, twoDymVectorAnn, twoDymVectorRowAnn :: SemanticAnns
vectorAnn = vectorSemAnn UInt32 (I UInt32 10)
vectorTMDescriptorAnn = vectorSemAnn tmDescriptorTS (I UInt32 20)
twoDymVectorRowAnn = vectorSemAnn Int64 (I UInt32 5)
twoDymVectorAnn = twoDymVectorSemAnn Int64 (I UInt32 5) (I UInt32 10)

foo0 :: Expression SemanticAnns
foo0 = AccessObject (RHS (Variable "foo0" uint32SemAnn))

foo1, foo2 :: Statement SemanticAnns
foo1 = Declaration "foo1" UInt32 foo0 undefined
foo2 = Declaration "foo2" UInt32 uint32Const0 undefined

structASemAnn, tmDescriptorSemAnn :: SemanticAnns
structASemAnn = definedTypeSemAnn "StructA"
tmDescriptorSemAnn = definedTypeSemAnn "TMDescriptor"

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

struct0Declaration, struct1Declaration :: Statement SemanticAnns
struct0Declaration = Declaration "struct0" tmDescriptorTS tmDescriptorFieldsInit0 undefined
struct1Declaration = Declaration "struct1" tmDescriptorTS (AccessObject (RHS (Variable "struct0" tmDescriptorSemAnn))) undefined

struct0 :: Object' Expression SemanticAnns
struct0 = Variable "struct0" tmDescriptorSemAnn

struct0field0 :: Expression SemanticAnns
struct0field0 = AccessObject (RHS (MemberAccess struct0 "field0" uint32SemAnn))

struct0Assignment :: Statement SemanticAnns
struct0Assignment = AssignmentStmt (LHS (MemberAccess (Variable "struct0" tmDescriptorSemAnn) "field0" uint32SemAnn)) (BinOp Addition struct0field0 constUInt32 uint32SemAnn) unitSemAnn

returnVoid :: ReturnStmt SemanticAnns
returnVoid = ReturnStmt Nothing unitSemAnn

returnStructField0 :: ReturnStmt SemanticAnns
returnStructField0 = ReturnStmt (Just struct0field0) uint32SemAnn

function0 :: AnnASTElement SemanticAnns
function0 = Function "function0" [] Nothing (BlockRet [struct0Declaration, struct1Declaration, struct0Assignment] returnVoid) [] unitSemAnn

function1 :: AnnASTElement SemanticAnns
function1 = Function "function1" [] (Just UInt32) (BlockRet [struct0Declaration, struct1Declaration, struct0Assignment] returnStructField0) [] unitSemAnn


renderFunctionDeclaration :: AnnASTElement SemanticAnns -> Text
renderFunctionDeclaration = render . ppFunctionDeclaration

renderFunction :: AnnASTElement SemanticAnns -> Text
renderFunction = render . ppFunction

spec :: Spec
spec = do
  describe "Pretty printing function declarations" $ do
    it "Prints fuction0 declaration" $ do
      renderFunctionDeclaration function0 `shouldBe`
        pack "void function0();"
    it "Prints fuction1 declaration" $ do
      renderFunctionDeclaration function1 `shouldBe`
        pack "uint32_t function1();"
  describe "Pretty printing function definitions" $ do
    it "Prints fuction0 definition" $ do
      renderFunction function0 `shouldBe`
        pack ("void function0() {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "\n" ++
              "    {\n" ++
              "        struct0.field0 = (uint32_t)0;\n" ++
              "        struct0.field1.field_a = (uint32_t)0;\n" ++
              "        for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
              "            struct0.field1.field_b[__i0] = (uint32_t)0;\n" ++
              "        }\n" ++
              "        struct0.field1.field_c = (uint32_t)4294901760;\n" ++
              "    }\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + (uint32_t)1024;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}")
    it "Prints fuction1 definition" $ do
      renderFunction function1 `shouldBe`
        pack ("uint32_t function1() {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "\n" ++
              "    {\n" ++
              "        struct0.field0 = (uint32_t)0;\n" ++
              "        struct0.field1.field_a = (uint32_t)0;\n" ++
              "        for (uint32_t __i0 = 0; __i0 < (uint32_t)10; __i0 = __i0 + (uint32_t)1) {\n" ++
              "            struct0.field1.field_b[__i0] = (uint32_t)0;\n" ++
              "        }\n" ++
              "        struct0.field1.field_c = (uint32_t)4294901760;\n" ++
              "    }\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + (uint32_t)1024;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}")