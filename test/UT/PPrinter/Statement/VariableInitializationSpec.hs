module UT.PPrinter.Statement.VariableInitializationSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Types
import Prettyprinter
import Control.Monad.Reader
import Generator.CodeGen.Statement
import Generator.LanguageC.Printer
import UT.PPrinter.Expression.Common

tmDescriptorTS, messageTS :: TypeSpecifier
tmDescriptorTS = DefinedType "TMDescriptor"
messageTS = DefinedType "Message"

optionBoxUInt32TS :: TypeSpecifier
optionBoxUInt32TS = Option (BoxSubtype UInt32)

vectorTS, vectorTMDescriptorTS, twoDimArrayTS :: TypeSpecifier
vectorTS = Array UInt32 (K (TInteger 10 DecRepr))
vectorTMDescriptorTS = Array tmDescriptorTS (K (TInteger 20 DecRepr))
twoDimArrayTS = Array (Array Int64 (K (TInteger 5 DecRepr))) (K (TInteger 10 DecRepr))

optionBoxUInt32ExprSemAnn :: SemanticAnn
optionBoxUInt32ExprSemAnn = optionBoxExprSemAnn UInt32

vectorObjAnn, twoDymArrayObjAnn, twoDymArrayRowObjAnn :: SemanticAnn
vectorObjAnn = vectorObjSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))
twoDymArrayRowObjAnn = vectorObjSemAnn Mutable Int64 (K (TInteger 5 DecRepr))
twoDymArrayObjAnn = twoDymArrayObjSemAnn Mutable Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

vectorExprAnn, vectorTMDescriptorExprAnn, twoDymArrayExprAnn, twoDymArrayRowExprAnn :: SemanticAnn
vectorExprAnn = vectorExprSemAnn UInt32 (K (TInteger 10 DecRepr))
vectorTMDescriptorExprAnn = vectorExprSemAnn tmDescriptorTS (K (TInteger 20 DecRepr))
twoDymArrayRowExprAnn = vectorExprSemAnn Int64 (K (TInteger 5 DecRepr))
twoDymArrayExprAnn = twoDymArrayExprSemAnn Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

vector0 :: Expression SemanticAnn
vector0 = AccessObject (Variable "vector0" vectorObjAnn)

vector1, vector2, vector3, vector4, vector5, vector6 :: Statement SemanticAnn
vector1 = Declaration "vector1" Mutable vectorTS vector0 stmtSemAnn
vector2 = Declaration "vector2" Mutable twoDimArrayTS (AccessObject (Variable "vector1" twoDymArrayObjAnn)) stmtSemAnn
vector3 = Declaration "vector3" Mutable vectorTS (ArrayInitializer uint32Const0 (K (TInteger 10 DecRepr)) vectorExprAnn) stmtSemAnn
vector4 = Declaration "vector4" Mutable twoDimArrayTS (ArrayInitializer (ArrayInitializer uint32Const0 (K (TInteger 5 DecRepr)) twoDymArrayRowExprAnn) (K (TInteger 10 DecRepr)) twoDymArrayExprAnn) stmtSemAnn
vector5 = Declaration "vector5" Mutable twoDimArrayTS (ArrayInitializer (AccessObject (Variable "vector_row" twoDymArrayRowObjAnn)) (K (TInteger 10 DecRepr)) twoDymArrayExprAnn) stmtSemAnn
vector6 = Declaration "vector6" Mutable vectorTMDescriptorTS (ArrayInitializer tmDescriptorFieldsInit0 (K (TInteger 10 DecRepr)) vectorTMDescriptorExprAnn) stmtSemAnn

foo0 :: Expression SemanticAnn
foo0 = AccessObject (Variable "foo0" (objSemAnn Mutable UInt32))

foo1, foo2 :: Statement SemanticAnn
foo1 = Declaration "foo1" Mutable UInt32 foo0 stmtSemAnn
foo2 = Declaration "foo2" Mutable UInt32 uint32Const0 stmtSemAnn

tmDescriptorObjSemAnn :: SemanticAnn
tmDescriptorObjSemAnn = definedTypeObjSemAnn Mutable "TMDescriptor"

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
        [FieldValueAssignment "field_a" uint32Const0 undefined,
         FieldValueAssignment "field_b" (ArrayInitializer uint32Const0 (K (TInteger 10 DecRepr)) vectorExprAnn) stmtSemAnn,
         FieldValueAssignment "field_c" uint32Const0xFFFF0000 stmtSemAnn] (Just "StructA") structAExprSemAnn

tmDescriptorFieldsInit0 :: Expression SemanticAnn
tmDescriptorFieldsInit0 =
    StructInitializer
        [FieldValueAssignment "field0" uint32Const0 undefined,
         FieldValueAssignment "field1" structAFieldsInit0 undefined] (Just "TMDescriptor") tmDescriptorExprSemAnn

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

renderStatement :: Statement SemanticAnn -> Text
renderStatement stmt = 
  case runReaderT (genBlockItem stmt) empty of
    Left err -> pack $ show err
    Right cStmts -> render $ vsep $ runReader (mapM pprint cStmts) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing integer variable declarations" $ do
    it "Prints the statement var foo1 : u32 = foo0;" $ do
      renderStatement foo1 `shouldBe`
        pack "\nuint32_t foo1 = foo0;"
    it "Prints the statement var foo2 : u32 = 0 : u32;" $ do
      renderStatement foo2 `shouldBe`
        pack "\nuint32_t foo2 = 0;"
  describe "Pretty printing option variable declarations" $ do
    it "Prints the statement var option0 : Option <'box u32> = Some(box_var0);" $ do
      renderStatement option0 `shouldBe`
        pack (
          "\n__option_box_t option0;\n" ++
          "option0.__variant = Some;\n" ++
          "option0.Some.__0 = box_var0;")
    it "Prints the statement var option1 : Option <'box u32> = None;" $ do
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
          "enum1.In.__0 = 0;\n" ++
          "enum1.In.__1 = 0;")
  describe "Pretty printing struct variable declarations" $ do
    it "Prints the statement var struct0 : TMDescriptor = {field0 = 0 : u32; field1 = {field_a = 0; field_b = 0xFFFF0000} : StructA} : TMDescriptor;" $ do
      renderStatement struct0 `shouldBe`
        pack (
        "\nTMDescriptor struct0;\n" ++
        "struct0.field0 = 0;\n" ++
        "struct0.field1.field_a = 0;\n" ++
        "for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
        "    struct0.field1.field_b[__i0] = 0;\n" ++
        "}\n" ++
        "struct0.field1.field_c = 4294901760;")
    it "Prints the statement var struct1 : TMDescriptor = struct0;" $ do
      renderStatement struct1 `shouldBe`
        pack "\nTMDescriptor struct1 = struct0;"
  describe "Pretty printing vector variable declarations" $ do
    it "Prints the statement var vector1 : [u32; 10 : u32] = vector0;" $ do
      renderStatement vector1 `shouldBe`
        pack (
          "\nuint32_t vector1[10];\n" ++
          "for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    vector1[__i0] = vector0[__i0];\n" ++
          "}")
    it "Prints the statement var vector2 : [[u32; 5 : u32]; 10 : u32] = vector0;" $ do
      renderStatement vector2 `shouldBe`
        pack (
          "\nint64_t vector2[10][5];\n" ++
          "for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
          "        vector2[__i0][__i1] = vector1[__i0][__i1];\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement var vector3 : [u32; 10 : u32] = [0 : u32; 10 : u32];" $ do
      renderStatement vector3 `shouldBe`
        pack (
          "\nuint32_t vector3[10];\n" ++
          "for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    vector3[__i0] = 0;\n" ++
          "}")
    it "Prints the statement var vector4 : [[u32; 5 : u32]; 10 : u32] = [[0 : u32; 5 : u32]; 10 : u32];" $ do
      renderStatement vector4 `shouldBe`
        pack (
          "\nint64_t vector4[10][5];\n" ++
          "for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
          "        vector4[__i0][__i1] = 0;\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement var vector4 : [[u32; 5 : u32]; 10 : u32] = [vector_row; 10 : u32];" $ do
      renderStatement vector5 `shouldBe`
        pack (
          "\nint64_t vector5[10][5];\n" ++
          "for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
          "        vector5[__i0][__i1] = vector_row[__i1];\n" ++
          "    }\n" ++
          "}")
    it ("Prints the statement var vector6 : [TMDescriptor; 20 : u32] = " ++
        "[{field0 = 0 : u32; field1 = {field_a = 0; field_b = 0xFFFF0000} : StructA} : TMDescriptor; 20 : u32];") $ do
      renderStatement vector6 `shouldBe`
        pack (
          "\nTMDescriptor vector6[20];\n" ++
          "for (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    vector6[__i0].field0 = 0;\n" ++
          "    vector6[__i0].field1.field_a = 0;\n" ++
          "    for (size_t __i1 = 0; __i1 < 10; __i1 = __i1 + 1) {\n" ++
          "        vector6[__i0].field1.field_b[__i1] = 0;\n" ++
          "    }\n" ++
          "    vector6[__i0].field1.field_c = 4294901760;\n" ++
          "}")
