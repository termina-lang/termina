module UT.PPrinter.Statement.AssignmentSpec (spec) where

import Test.Hspec
import AST.Seman
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import Prettyprinter
import UT.PPrinter.Expression.Common
import Control.Monad.Reader
import Generator.CodeGen.Statement
import Generator.LanguageC.Printer


tmDescriptorTS :: TypeSpecifier
tmDescriptorTS = DefinedType "TMDescriptor"

optionBoxUInt32ObjSemAnn :: SemanticAnn
optionBoxUInt32ObjSemAnn = optionBoxObjSemAnn Mutable UInt32

optionBoxUInt32ExprSemAnn :: SemanticAnn
optionBoxUInt32ExprSemAnn = optionBoxExprSemAnn UInt32

vectorObjAnn, vectorTMDescriptorObjAnn, twoDymArrayRowObjAnn, twoDymArrayObjAnn :: SemanticAnn
vectorObjAnn = vectorObjSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))
vectorTMDescriptorObjAnn = vectorObjSemAnn Mutable tmDescriptorTS (K (TInteger 20 DecRepr))
twoDymArrayRowObjAnn = vectorObjSemAnn Mutable Int64 (K (TInteger 5 DecRepr))
twoDymArrayObjAnn = twoDymArrayObjSemAnn Mutable Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

vectorExprAnn, vectorTMDescriptorExprAnn, twoDymArrayRowExprAnn, twoDymArrayExprAnn  :: SemanticAnn
vectorExprAnn = vectorExprSemAnn UInt32 (K (TInteger 10 DecRepr))
vectorTMDescriptorExprAnn = vectorExprSemAnn tmDescriptorTS (K (TInteger 20 DecRepr))
twoDymArrayRowExprAnn = vectorExprSemAnn Int64 (K (TInteger 5 DecRepr))
twoDymArrayExprAnn = twoDymArrayExprSemAnn Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

vector1, vector2, vector3, vector4, vector5, vector6 :: Object SemanticAnn
vector1 = Variable "vector1" vectorObjAnn
vector2 = Variable "vector2" twoDymArrayObjAnn
vector3 = Variable "vector3" vectorObjAnn
vector4 = Variable "vector4" twoDymArrayObjAnn
vector5 = Variable "vector5" twoDymArrayObjAnn
vector6 = Variable "vector6" vectorTMDescriptorObjAnn

vector1Assign, vector2Assign, vector3Assign, vector4Assign, vector5Assign, vector6Assign :: Statement SemanticAnn
vector1Assign = AssignmentStmt vector1 (AccessObject (Variable "vector0" vectorObjAnn)) stmtSemAnn
vector2Assign = AssignmentStmt vector2 (AccessObject (Variable "vector1" twoDymArrayObjAnn)) stmtSemAnn
vector3Assign = AssignmentStmt vector3 (ArrayInitializer uint32Const0 (K (TInteger 10 DecRepr)) vectorExprAnn) stmtSemAnn
vector4Assign = AssignmentStmt vector4 (ArrayInitializer (ArrayInitializer uint32Const0 (K (TInteger 5 DecRepr)) twoDymArrayRowExprAnn) (K (TInteger 10 DecRepr)) twoDymArrayExprAnn) stmtSemAnn
vector5Assign = AssignmentStmt vector5 (ArrayInitializer (AccessObject (Variable "vector_row" twoDymArrayRowObjAnn)) (K (TInteger 10 DecRepr)) twoDymArrayExprAnn) stmtSemAnn
vector6Assign = AssignmentStmt vector6 (ArrayInitializer tmDescriptorFieldsInit0 (K (TInteger 10 DecRepr)) vectorTMDescriptorExprAnn) stmtSemAnn

foo1, foo2 :: Object SemanticAnn
foo1 = Variable "foo1" (objSemAnn Mutable UInt32)
foo2 = Variable "foo2" (objSemAnn Mutable UInt32)

foo1Assign, foo2Assign :: Statement SemanticAnn
foo1Assign = AssignmentStmt foo1 (AccessObject (Variable "foo0" (objSemAnn Mutable UInt32))) undefined
foo2Assign = AssignmentStmt foo2 uint32Const0 undefined

tmDescriptorObjSemAnn, messageObjSemAnn :: SemanticAnn
tmDescriptorObjSemAnn = definedTypeObjSemAnn Mutable "TMDescriptor"
messageObjSemAnn = definedTypeObjSemAnn Mutable "Message"

structAExprSemAnn, tmDescriptorExprSemAnn, messageExprSemAnn :: SemanticAnn
structAExprSemAnn = definedTypeExprSemAnn "StructA"
tmDescriptorExprSemAnn = definedTypeExprSemAnn "TMDescriptor"
messageExprSemAnn = definedTypeExprSemAnn "Message"

uint32Const0, uint32Const0xFFFF0000 :: Expression SemanticAnn
uint32Const0 = Constant (I (TInteger 0 DecRepr) (Just UInt32)) uint32ExprSemAnn
uint32Const0xFFFF0000 = Constant (I (TInteger 4294901760 DecRepr) (Just UInt32)) uint32ExprSemAnn

-- | Initialization expression:
-- { field_a = 0 : u32, field_b = 0xFFFF0000 : u32 } : StructA
structAFieldsInit0 :: Expression SemanticAnn
structAFieldsInit0 = 
    StructInitializer 
        [FieldValueAssignment "field_a" uint32Const0 stmtSemAnn,
         FieldValueAssignment "field_b" (ArrayInitializer uint32Const0 (K (TInteger 10 DecRepr)) vectorExprAnn) stmtSemAnn,
         FieldValueAssignment "field_c" uint32Const0xFFFF0000 stmtSemAnn] (Just "StructA") structAExprSemAnn

tmDescriptorFieldsInit0 :: Expression SemanticAnn
tmDescriptorFieldsInit0 = 
    StructInitializer
        [FieldValueAssignment "field0" uint32Const0 stmtSemAnn,
         FieldValueAssignment "field1" structAFieldsInit0 stmtSemAnn] (Just "TMDescriptor") tmDescriptorExprSemAnn


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
unboxVar0 = Unbox boxVar0 (objSemAnn Mutable UInt32)

unboxVar0AssignFoo1, unboxVar0AssignConst :: Statement SemanticAnn
unboxVar0AssignFoo1 = AssignmentStmt unboxVar0 (AccessObject foo1) stmtSemAnn
unboxVar0AssignConst = AssignmentStmt unboxVar0 (Constant (I (TInteger 1024 DecRepr) (Just UInt32)) uint32ExprSemAnn) stmtSemAnn

option0Assign, option1Assign :: Statement SemanticAnn
option0Assign = AssignmentStmt option0 (OptionVariantInitializer (Some (AccessObject boxVar0)) optionBoxUInt32ExprSemAnn) stmtSemAnn
option1Assign = AssignmentStmt option1 (OptionVariantInitializer None optionBoxUInt32ExprSemAnn) stmtSemAnn

renderStatement :: Statement SemanticAnn -> Text
renderStatement stmt = 
  case runReaderT (genBlockItem stmt) empty of
    Left err -> pack $ show err
    Right cStmts -> render $ vsep $ runReader (mapM pprint cStmts) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing integer variable declarations" $ do
    it "Prints the statement foo1 = foo0; where foo1 : u32" $ do
      renderStatement foo1Assign `shouldBe`
        pack "\nfoo1 = foo0;"
    it "Prints the statement foo2 = 0 : u32; where foo2 : u32" $ do
      renderStatement foo2Assign `shouldBe`
        pack "\nfoo2 = 0;"
  describe "Pretty printing option variable declarations" $ do
    it "Prints the statement option0 = Some(box_var0); where option0 : Option <'box u32>" $ do
      renderStatement option0Assign `shouldBe`
        pack (
          "\noption0.__variant = Some;\n" ++
          "option0.Some.__0 = box_var0;")
    it "Prints the statement option1 = None; where option1 : Option <'box u32>" $ do
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
          "enum1.In.__0 = 0;\n" ++
          "enum1.In.__1 = 0;")
  describe "Pretty printing struct variable declarations" $ do
    it "Prints the statement struct0 = {field0 = 0 : u32; field1 = {field_a = 0; field_b = 0xFFFF0000} : StructA} : TMDescriptor; where struct0 : TMDescriptor" $ do
      renderStatement struct0Assign `shouldBe`
        pack (
        "\nstruct0.field0 = 0;\n" ++
        "struct0.field1.field_a = 0;\n" ++
        "for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
        "    struct0.field1.field_b[__i0] = 0;\n" ++
        "}\n" ++
        "struct0.field1.field_c = 4294901760;")
    it "Prints the statement struct1 = struct0; where struct1 : TMDescriptor" $ do
      renderStatement struct1Assign `shouldBe`
        pack "\nstruct1 = struct0;"
  describe "Pretty printing vector variable declarations" $ do
    it "Prints the statement vector1 = vector0; where vector1 : [u32; 10 : u32]" $ do
      renderStatement vector1Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    vector1[__i0] = vector0[__i0];\n" ++
          "}")
    it "Prints the statement vector2 = vector0; where vector2 : [[u32; 5 : u32]; 10 : u32]" $ do
      renderStatement vector2Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
          "        vector2[__i0][__i1] = vector1[__i0][__i1];\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement vector3 = [0 : u32; 10 : u32]; where vector3 : [u32; 10 : u32]" $ do
      renderStatement vector3Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    vector3[__i0] = 0;\n" ++
          "}")
    it "Prints the statement vector4 = [[0 : u32; 5 : u32]; 10 : u32]; where vector4 : [[u32; 5 : u32]; 10 : u32]s" $ do
      renderStatement vector4Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
          "        vector4[__i0][__i1] = 0;\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement vector4 = [vector_row; 10 : u32]; where vector4 : [[u32; 5 : u32]; 10 : u32]" $ do
      renderStatement vector5Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
          "        vector5[__i0][__i1] = vector_row[__i1];\n" ++
          "    }\n" ++
          "}")
    it ("Prints the statement vector6 = " ++
        "[{field0 = 0 : u32; field1 = {field_a = 0; field_b = 0xFFFF0000} : StructA} : TMDescriptor; 20 : u32];") $ do
      renderStatement vector6Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    vector6[__i0].field0 = 0;\n" ++
          "    vector6[__i0].field1.field_a = 0;\n" ++
          "    for (size_t __i1 = 0; __i1 < 10; __i1 = __i1 + 1) {\n" ++
          "        vector6[__i0].field1.field_b[__i1] = 0;\n" ++
          "    }\n" ++
          "    vector6[__i0].field1.field_c = 4294901760;\n" ++
          "}")
  describe "Pretty printing unbox assignments" $ do
    it "Prints the statement box_var0 = foo1" $ do
      renderStatement unboxVar0AssignFoo1 `shouldBe`
        pack "\n*(uint32_t *)box_var0.data = foo1;"
    it "Prints the statement box_var0 = 1024 : u32" $ do
      renderStatement unboxVar0AssignConst `shouldBe`
        pack "\n*(uint32_t *)box_var0.data = 1024;"