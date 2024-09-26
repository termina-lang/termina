module UT.PPrinter.Statement.AssignmentSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Types
import Prettyprinter
import UT.PPrinter.Expression.Common
import Control.Monad.Reader
import Generator.CodeGen.Statement
import Generator.LanguageC.Printer
import ControlFlow.Common
import Control.Monad.Except

tmDescriptorTS :: TerminaType
tmDescriptorTS = DefinedType "TMDescriptor"

optionBoxUInt32ObjSemAnn :: SemanticAnn
optionBoxUInt32ObjSemAnn = optionBoxObjSemAnn Mutable UInt32

optionBoxUInt32ExprSemAnn :: SemanticAnn
optionBoxUInt32ExprSemAnn = optionBoxExprSemAnn UInt32

arrayObjAnn, arrayTMDescriptorObjAnn, twoDymArrayRowObjAnn, twoDymArrayObjAnn :: SemanticAnn
arrayObjAnn = arrayObjSemAnn Mutable UInt32 (K (TInteger 10 DecRepr))
arrayTMDescriptorObjAnn = arrayObjSemAnn Mutable tmDescriptorTS (K (TInteger 20 DecRepr))
twoDymArrayRowObjAnn = arrayObjSemAnn Mutable Int64 (K (TInteger 5 DecRepr))
twoDymArrayObjAnn = twoDymArrayObjSemAnn Mutable Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

arrayExprAnn, arrayTMDescriptorExprAnn, twoDymArrayRowExprAnn, twoDymArrayExprAnn  :: SemanticAnn
arrayExprAnn = arrayExprSemAnn UInt32 (K (TInteger 10 DecRepr))
arrayTMDescriptorExprAnn = arrayExprSemAnn tmDescriptorTS (K (TInteger 20 DecRepr))
twoDymArrayRowExprAnn = arrayExprSemAnn Int64 (K (TInteger 5 DecRepr))
twoDymArrayExprAnn = twoDymArrayExprSemAnn Int64 (K (TInteger 5 DecRepr)) (K (TInteger 10 DecRepr))

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
array3Assign = AssignmentStmt array3 (ArrayInitializer uint32Const0 (K (TInteger 10 DecRepr)) arrayExprAnn) stmtSemAnn
array4Assign = AssignmentStmt array4 (ArrayInitializer (ArrayInitializer uint32Const0 (K (TInteger 5 DecRepr)) twoDymArrayRowExprAnn) (K (TInteger 10 DecRepr)) twoDymArrayExprAnn) stmtSemAnn
array5Assign = AssignmentStmt array5 (ArrayInitializer (AccessObject (Variable "array_row" twoDymArrayRowObjAnn)) (K (TInteger 10 DecRepr)) twoDymArrayExprAnn) stmtSemAnn
array6Assign = AssignmentStmt array6 (ArrayInitializer tmDescriptorFieldsInit0 (K (TInteger 10 DecRepr)) arrayTMDescriptorExprAnn) stmtSemAnn

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
         FieldValueAssignment "field_b" (ArrayInitializer uint32Const0 (K (TInteger 10 DecRepr)) arrayExprAnn) stmtSemAnn,
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
  case runExcept (genBBlocks [] [stmt]) of
    Left err -> pack $ show err
    Right bBlocks ->
      case runReaderT (Prelude.concat <$> mapM genBlocks bBlocks) empty of
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
  describe "Pretty printing array variable declarations" $ do
    it "Prints the statement array1 = array0; where array1 : [u32; 10 : u32]" $ do
      renderStatement array1Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    array1[__i0] = array0[__i0];\n" ++
          "}")
    it "Prints the statement array2 = array0; where array2 : [[u32; 5 : u32]; 10 : u32]" $ do
      renderStatement array2Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
          "        array2[__i0][__i1] = array1[__i0][__i1];\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement array3 = [0 : u32; 10 : u32]; where array3 : [u32; 10 : u32]" $ do
      renderStatement array3Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    array3[__i0] = 0;\n" ++
          "}")
    it "Prints the statement array4 = [[0 : u32; 5 : u32]; 10 : u32]; where array4 : [[u32; 5 : u32]; 10 : u32]s" $ do
      renderStatement array4Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
          "        array4[__i0][__i1] = 0;\n" ++
          "    }\n" ++
          "}")
    it "Prints the statement array4 = [array_row; 10 : u32]; where array4 : [[u32; 5 : u32]; 10 : u32]" $ do
      renderStatement array5Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    for (size_t __i1 = 0; __i1 < 5; __i1 = __i1 + 1) {\n" ++
          "        array5[__i0][__i1] = array_row[__i1];\n" ++
          "    }\n" ++
          "}")
    it ("Prints the statement array6 = " ++
        "[{field0 = 0 : u32; field1 = {field_a = 0; field_b = 0xFFFF0000} : StructA} : TMDescriptor; 20 : u32];") $ do
      renderStatement array6Assign `shouldBe`
        pack (
          "\nfor (size_t __i0 = 0; __i0 < 10; __i0 = __i0 + 1) {\n" ++
          "    array6[__i0].field0 = 0;\n" ++
          "    array6[__i0].field1.field_a = 0;\n" ++
          "    for (size_t __i1 = 0; __i1 < 10; __i1 = __i1 + 1) {\n" ++
          "        array6[__i0].field1.field_b[__i1] = 0;\n" ++
          "    }\n" ++
          "    array6[__i0].field1.field_c = 4294901760;\n" ++
          "}")
  describe "Pretty printing unbox assignments" $ do
    it "Prints the statement box_var0 = foo1" $ do
      renderStatement unboxVar0AssignFoo1 `shouldBe`
        pack "\n*(uint32_t *)box_var0.data = foo1;"
    it "Prints the statement box_var0 = 1024 : u32" $ do
      renderStatement unboxVar0AssignConst `shouldBe`
        pack "\n*(uint32_t *)box_var0.data = 1024;"