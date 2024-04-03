module UT.PPrinter.FunctionSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text hiding (empty)
import qualified Data.Map as M
import Semantic.Monad
import UT.PPrinter.Expression.Common
import Prettyprinter
import Control.Monad.Reader
import Generator.LanguageC.Printer
import Generator.Function
import Generator.Common

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
        [FieldValueAssignment "field_a" uint32Const0 undefined,
         FieldValueAssignment "field_b" (VectorInitExpression uint32Const0 (K 10) vectorAnn) undefined,
         FieldValueAssignment "field_c" uint32Const0xFFFF0000 undefined] structASemAnn

structAFieldsInit1 :: Expression SemanticAnns
structAFieldsInit1 = 
    FieldAssignmentsExpression "StructA"
        [FieldValueAssignment "field_a" (AccessObject (Variable "param0" (objSemAnn Mutable UInt32))) undefined,
         FieldValueAssignment "field_b" (VectorInitExpression uint32Const0 (K 10) vectorAnn) undefined,
         FieldValueAssignment "field_c" uint32Const0xFFFF0000 undefined] structASemAnn

structAFieldsInit2 :: Expression SemanticAnns
structAFieldsInit2 = 
    FieldAssignmentsExpression "StructA"
        [FieldValueAssignment "field_a" (AccessObject (Variable "param0" (objSemAnn Mutable UInt32))) undefined,
         FieldValueAssignment "field_b" (AccessObject (Variable "param1" vectorAnn)) undefined,
         FieldValueAssignment "field_c" uint32Const0xFFFF0000 undefined] structASemAnn

structAFieldsInit3 :: Expression SemanticAnns
structAFieldsInit3 = 
    FieldAssignmentsExpression "StructA"
        [FieldValueAssignment "field_a" (AccessObject (Variable "param0" (objSemAnn Mutable UInt32))) undefined,
         FieldValueAssignment "field_b" (AccessObject (Dereference (Variable "param1" refVectorAnn) vectorAnn)) undefined,
         FieldValueAssignment "field_c" uint32Const0xFFFF0000 undefined] structASemAnn

tmDescriptorFieldsInit0 :: Expression SemanticAnns
tmDescriptorFieldsInit0 = 
    FieldAssignmentsExpression "TMDescriptor"
        [FieldValueAssignment "field0" uint32Const0 undefined,
         FieldValueAssignment "field1" structAFieldsInit0 undefined] tmDescriptorSemAnn

tmDescriptorFieldsInit1 :: Expression SemanticAnns
tmDescriptorFieldsInit1 = 
    FieldAssignmentsExpression "TMDescriptor"
        [FieldValueAssignment "field0" uint32Const0 undefined,
         FieldValueAssignment "field1" structAFieldsInit1 undefined] tmDescriptorSemAnn

tmDescriptorFieldsInit2 :: Expression SemanticAnns
tmDescriptorFieldsInit2 = 
    FieldAssignmentsExpression "TMDescriptor"
        [FieldValueAssignment "field0" uint32Const0 undefined,
         FieldValueAssignment "field1" structAFieldsInit2 undefined] tmDescriptorSemAnn

tmDescriptorFieldsInit3 :: Expression SemanticAnns
tmDescriptorFieldsInit3 = 
    FieldAssignmentsExpression "TMDescriptor"
        [FieldValueAssignment "field0" uint32Const0 undefined,
         FieldValueAssignment "field1" structAFieldsInit3 undefined] tmDescriptorSemAnn

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

renderFunctionDecl :: OptionTypes -> AnnASTElement SemanticAnns -> Text
renderFunctionDecl opts decl = 
  case runReaderT (genFunctionDecl decl) opts of
    Left err -> pack $ show err
    Right cDecls -> render $ vsep $ runReader (mapM pprint cDecls) (CPrinterConfig False False)

renderFunction :: AnnASTElement SemanticAnns -> Text
renderFunction func = 
  case runReaderT (genFunction func) M.empty of
    Left err -> pack $ show err
    Right cDecls -> render $ vsep $ runReader (mapM pprint cDecls) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing function declarations" $ do
    it "Prints fuction0 declaration" $ do
      renderFunctionDecl M.empty function0 `shouldBe`
        pack "\nvoid function0();"
    it "Prints fuction1 declaration" $ do
      renderFunctionDecl M.empty function1 `shouldBe`
        pack "\nuint32_t function1();"
    it "Prints fuction2 declaration" $ do
      renderFunctionDecl M.empty function2 `shouldBe`
        pack "\nuint32_t function2(uint32_t param0);"
    it "Prints fuction3 declaration" $ do
      renderFunctionDecl M.empty function3 `shouldBe`
        pack ("\nuint32_t function3(uint32_t param0, __wrapper_uint32__10_t param1);")
    it "Prints fuction4 declaration" $ do
      renderFunctionDecl M.empty function4 `shouldBe`
        pack "\nuint32_t function4(uint32_t param0, uint32_t param1[10]);"
  describe "Pretty printing function definitions" $ do
    it "Prints fuction0 definition" $ do
      renderFunction function0 `shouldBe`
        pack ("\nvoid function0() {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "    struct0.field0 = 0;\n" ++
              "    struct0.field1.field_a = 0;\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        struct0.field1.field_b[__i0] = 0;\n" ++
              "    }\n" ++
              "    struct0.field1.field_c = 4294901760;\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024;\n" ++
              "\n" ++
              "    return;\n" ++
              "\n" ++
              "}")
    it "Prints fuction1 definition" $ do
      renderFunction function1 `shouldBe`
        pack ("\nuint32_t function1() {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "    struct0.field0 = 0;\n" ++
              "    struct0.field1.field_a = 0;\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        struct0.field1.field_b[__i0] = 0;\n" ++
              "    }\n" ++
              "    struct0.field1.field_c = 4294901760;\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}")
    it "Prints fuction2 definition" $ do
      renderFunction function2 `shouldBe`
        pack ("\nuint32_t function2(uint32_t param0) {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "    struct0.field0 = 0;\n" ++
              "    struct0.field1.field_a = param0;\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        struct0.field1.field_b[__i0] = 0;\n" ++
              "    }\n" ++
              "    struct0.field1.field_c = 4294901760;\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}")
    it "Prints fuction3 definition" $ do
      renderFunction function3 `shouldBe`
        pack ("\nuint32_t function3(uint32_t param0, __wrapper_uint32__10_t param1) {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "    struct0.field0 = 0;\n" ++
              "    struct0.field1.field_a = param0;\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        struct0.field1.field_b[__i0] = param1.array[__i0];\n" ++
              "    }\n" ++
              "    struct0.field1.field_c = 4294901760;\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}")
    it "Prints fuction4 definition" $ do
      renderFunction function4 `shouldBe`
        pack ("\nuint32_t function4(uint32_t param0, uint32_t param1[10]) {\n" ++
              "    \n" ++
              "    TMDescriptor struct0;\n" ++
              "    struct0.field0 = 0;\n" ++
              "    struct0.field1.field_a = param0;\n" ++
              "    for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
              "        struct0.field1.field_b[__i0] = param1[__i0];\n" ++
              "    }\n" ++
              "    struct0.field1.field_c = 4294901760;\n" ++
              "\n" ++
              "    TMDescriptor struct1 = struct0;\n" ++
              "\n" ++
              "    struct0.field0 = struct0.field0 + 1024;\n" ++
              "\n" ++
              "    return struct0.field0;\n" ++
              "\n" ++
              "}")
