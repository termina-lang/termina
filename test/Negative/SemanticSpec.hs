module Negative.SemanticSpec (spec) where

import Test.Hspec
import Semantic.AST
import Semantic.Errors
import Negative.Common
import Semantic.Types

testSE001 :: String
testSE001 = "function test0() {\n" ++
        "\n" ++
        "    var foo : u32 = 0;\n" ++
        "\n" ++
        "    foo[0] = 1024;\n" ++
        "\n" ++
        "    return;\n" ++
        "\n" ++
        "}\n"

testSE002 :: String
testSE002 = "function test0() {\n" ++
       "    foo = foo + 1024 : u16;\n" ++
       "    return;\n" ++
       "}\n"

testSE003 :: String
testSE003 = "function f (n : u8) -> u8 {\n" ++
       "    let ntimes : u8 = n;\n" ++
       "    var y : u8 = 0 : u8;\n" ++
       "\n" ++
       "    for i : u8 in 0 .. ntimes {\n" ++
       "        y = i + y;\n" ++
       "    }\n" ++
       "\n" ++
       "    return y;\n" ++
       "\n" ++
       "}\n" ++
       "\n"

testSE004 :: String
testSE004 = "function f (x : u8) -> u8 {\n" ++
       "     x = x * 2 : u8;\n" ++
       "    return x;\n" ++
       "}\n" ++
       "\n" ++
       "\n"

testSE005 :: String
testSE005 = "function is_inside(x : u8, range: &[u8;2]) {\n" ++
       "\n" ++
       "    let lower_limit : u8 = range[0];\n" ++
       "    let upper_limit : u8 = range[1];\n" ++
       "\n" ++
       "    if ((x > lower_limit) && (x < upper_limit)){\n" ++
       "\n" ++
       "    } else if (x > lower_limit) && (x - lower_limit < 5) {\n" ++
       "\n" ++
       "    } else if (x > upper_limit) && (x - upper_limit < 5) {\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "    return;\n" ++
       "\n" ++
       "}\n"

testSE006 :: String
testSE006 = "function f() {\n" ++
       "\n" ++
       "    let foo : bool = true;\n" ++
       "\n" ++
       "    var casting : u32 = foo as u32;\n" ++
       "\n" ++
       "    return;\n" ++
       "\n" ++
       "}\n"

testSE007 :: String
testSE007 = "function foo(param0 : box u8) -> u8 {\n" ++
       "\n" ++
       "    let x : u8 = param0 + 1;\n" ++
       "\n" ++
       "    return x;\n" ++
       "\n" ++
       "}\n"

testSE007_2 :: String
testSE007_2 = "function foo(param0 : [u8; 10]) -> u8 {\n" ++
        "\n" ++
        "    let x : u8 = param0[0] + 1;\n" ++
        "\n" ++
        "    return x;\n" ++
        "\n" ++
        "}"

testSE008 :: String
testSE008 = "function foo(param0 : u8) -> [u8; 10] {\n" ++
        "\n" ++
        "    var x : [u8; 10] = [0; 10];\n" ++
        "\n" ++
        "    x[0] = param0;\n" ++
        "\n" ++
        "    return x;\n" ++
        "\n" ++
        "}"

testSE009 :: String
testSE009 = "\n" ++
       "interface Interface0 {\n" ++
       "\n" ++
       "    procedure procedure0(&mut self, param0 : u32, param1 : u16);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "task class TaskClass0 {\n" ++
       "\n" ++
       "\n" ++
       "    ap : access Interface0;\n" ++
       "\n" ++
       "    input : sink u32 triggers action0;\n" ++
       "\n" ++
       "    action action0(&priv self, data : u32) -> Status<i32> {\n" ++
       "\n" ++
       "\n" ++
       "        var status : Status<i32> = Success;\n" ++
       "\n" ++
       "        self->ap.procedure0(data, 100, 200);\n" ++
       "\n" ++
       "        return status;\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testSE010 :: String
testSE010 = "\n" ++
       "interface Interface0 {\n" ++
       "\n" ++
       "    procedure procedure0(&mut self, param0 : u32, param1 : u16);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "task class TaskClass0 {\n" ++
       "\n" ++
       "\n" ++
       "    ap : access Interface0;\n" ++
       "\n" ++
       "    input : sink u32 triggers action0;\n" ++
       "\n" ++
       "    action action0(&priv self, data : u32) -> Status<i32> {\n" ++
       "\n" ++
       "\n" ++
       "        var status : Status<i32> = Success;\n" ++
       "\n" ++
       "        self->ap.procedure0(data);\n" ++
       "\n" ++
       "        return status;\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testSE011 :: String
testSE011 = "\n" ++
       "interface Interface0 {\n" ++
       "\n" ++
       "    procedure procedure0(&mut self, param0 : u32, param1 : u16);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "task class TaskClass0 {\n" ++
       "\n" ++
       "\n" ++
       "    ap : access Interface0;\n" ++
       "\n" ++
       "    input : sink u32 triggers action0;\n" ++
       "\n" ++
       "    action action0(&priv self, data : u32) -> Status<i32> {\n" ++
       "\n" ++
       "\n" ++
       "        var status : Status<i32> = Success;\n" ++
       "\n" ++
       "        self->ap.procedure0(100, data);\n" ++
       "\n" ++
       "        return status;\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testSE012 :: String
testSE012 = "\n" ++
       "interface Interface0 {\n" ++
       "\n" ++
       "    procedure procedure0(&mut self, param0 : u32, param1 : u16);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "task class TaskClass0 {\n" ++
       "\n" ++
       "\n" ++
       "    ap : access Interface0;\n" ++
       "\n" ++
       "    input : sink u32 triggers action0;\n" ++
       "\n" ++
       "    action action0(&priv self, data : u32) -> Status<i32> {\n" ++
       "\n" ++
       "        var status : Status<i32> = Success;\n" ++
       "\n" ++
       "        self->ap.procedure1(data);\n" ++
       "\n" ++
       "        return status;\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testSE013 :: String
testSE013 = "resource class id0 {\n" ++
       "    procedure assignment_test1(&mut self, box_var0 : box u32) {\n" ++
       "        var opt : Option<box u32> = None;\n" ++
       "        opt = Some(box_var0);\n" ++
       "        return;\n" ++
       "    }\n" ++
       "};\n"

testSE014 :: String
testSE014 = "interface TMChannelInterface {\n" ++
       "\n" ++
       "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
       "\n" ++
       "};\n" ++
       "\n" ++
       "resource class TMChannel provides TMChannelInterface {\n" ++
       "    \n" ++
       "    tm_sent_packets : u32;\n" ++
       "    \n" ++
       "    action send_packet(&priv self, input : u32) -> Status<i32> {\n" ++
       "\n" ++
       "        var status : Status<i32> = Success;\n" ++
       "\n" ++
       "        return status;\n" ++
       "\n" ++
       "    }\n" ++
       "    \n" ++
       "    procedure get_tm_sent_packets(&mut self, packets : &mut u32) {\n" ++
       "\n" ++
       "        *packets = self->tm_sent_packets;\n" ++
       "\n" ++
       "        return;\n" ++
       "\n" ++
       "    }\n" ++
       "\n" ++
       "};\n"

testSE015 :: String
testSE015 = "interface TMChannelInterface {\n" ++
       "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
       "};\n" ++
       "\n" ++
       "resource class TMChannel provides TMChannelInterface {\n" ++
       "    \n" ++
       "  tm_sent_packets : u32;\n" ++
       "\n" ++
       "  input_msg : in u32 triggers get_tm_sent_packets;\n" ++
       "\n" ++
       "  procedure get_tm_sent_packets(&mut self, packets : &mut u32) {\n" ++
       "    *packets = self->tm_sent_packets;\n" ++
       "    return;\n" ++
       "  }\n" ++
       "\n" ++
       "};\n"

testSE016 :: String
testSE016 = "interface TMChannelInterface {\n" ++
       "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
       "};\n" ++
       "\n" ++
       "resource class TMChannel provides TMChannelInterface {\n" ++
       "    \n" ++
       "  tm_sent_packets : u32;\n" ++
       "\n" ++
       "  output_msg : out u32;\n" ++
       "\n" ++
       "  procedure get_tm_sent_packets(&mut self, packets : &mut u32) {\n" ++
       "    *packets = self->tm_sent_packets;\n" ++
       "    return;\n" ++
       "  }\n" ++
       "\n" ++
       "};\n"

spec :: Spec
spec = do
  describe "Semantic Errors" $ do
    it "SE-001: invalid array indexing" $ do
     runNegativeTestTypeCheck testSE001
       `shouldSatisfy`
        isEInvalidArrayIndexing TUInt32
    it "SE-002: object not found" $ do
        runNegativeTestTypeCheck testSE002
        `shouldSatisfy`
            isENotNamedObject "foo"
    it "SE-003: expected constant expression" $ do
     runNegativeTestTypeCheck testSE003
       `shouldSatisfy`
        isEExpressionNotConstant
    it "SE-004: assignment to immutable variable" $ do
     runNegativeTestTypeCheck testSE004
       `shouldSatisfy`
        isEAssignmentToImmutable
    it "SE-005: missing else clause" $ do
     runNegativeTestTypeCheck testSE005
       `shouldSatisfy`
        isEIfElseNoOtherwise
    it "SE-006: invalid cast" $ do
     runNegativeTestTypeCheck testSE006
       `shouldSatisfy`
        isENotCasteableTBoolTUInt32 
    it "SE-007: invalid parameter type (defining a box parameter)" $ do
     runNegativeTestTypeCheck testSE007
       `shouldSatisfy`
        isEInvalidParameterTypeBox
    it "SE-007: invalid parameter type (defining a fixed size array parameter)" $ do
     runNegativeTestTypeCheck testSE007_2
       `shouldSatisfy`
        isEInvalidParameterTypeArray
    it "SE-008: invalid return type" $ do
     runNegativeTestTypeCheck testSE008
       `shouldSatisfy`
        isEInvalidReturnType 
    it "SE-009: extra arguments in procedure call" $ do
     runNegativeTestTypeCheck testSE009
       `shouldSatisfy`
        isEProcedureCallExtraArgs 
    it "SE-010: missing arguments in procedure call" $ do
     runNegativeTestTypeCheck testSE010
       `shouldSatisfy`
        isEProcedureCallMissingArgs 
    it "SE-011: argument type mismatch in procedure call" $ do
     runNegativeTestTypeCheck testSE011
       `shouldSatisfy`
        isEProcedureCallArgTypeMismatch 
    it "SE-012: unknown procedure" $ do
     runNegativeTestTypeCheck testSE012
       `shouldSatisfy`
        isEUnknownProcedure "procedure1"
    it "SE-013: resource class does not provide any interface" $ do
     runNegativeTestTypeCheck testSE013
       `shouldSatisfy`
        isEResourceClassNoProvides "id0"
    it "SE-014: resource class defines an action" $ do
     runNegativeTestTypeCheck testSE014
       `shouldSatisfy`
        isEResourceClassAction "send_packet"
    it "SE-015: resource class defines an in port" $ do
     runNegativeTestTypeCheck testSE015
       `shouldSatisfy`
        isEResourceClassInPort "input_msg"
    it "SE-015: resource class defines an out port" $ do
     runNegativeTestTypeCheck testSE016
       `shouldSatisfy`
        isEResourceClassOutPort "output_msg"

  where

    isEInvalidArrayIndexing :: TerminaType SemanticAnn -> Maybe Error -> Bool
    isEInvalidArrayIndexing inTy = \case Just (EInvalidArrayIndexing ty) -> (inTy == ty); _ -> False
 
    isENotNamedObject :: Identifier -> Maybe Error -> Bool
    isENotNamedObject inIdent = \case Just (ENotNamedObject ident) -> (inIdent == ident); _ -> False

    isEExpressionNotConstant :: Maybe Error -> Bool
    isEExpressionNotConstant = \case Just EExpressionNotConstant -> True; _ -> False

    isEAssignmentToImmutable :: Maybe Error -> Bool
    isEAssignmentToImmutable = \case Just EAssignmentToImmutable -> True; _ -> False

    isEIfElseNoOtherwise :: Maybe Error -> Bool
    isEIfElseNoOtherwise = \case Just EIfElseNoOtherwise -> True; _ -> False

    isENotCasteableTBoolTUInt32 :: Maybe Error -> Bool
    isENotCasteableTBoolTUInt32 = \case Just (ENotCasteable TBool TUInt32) -> True; _ -> False

    isEInvalidParameterTypeBox :: Maybe Error -> Bool
    isEInvalidParameterTypeBox = \case Just (EInvalidParameterType (Parameter _ (TBoxSubtype _))) -> True; _ -> False

    isEInvalidParameterTypeArray :: Maybe Error -> Bool
    isEInvalidParameterTypeArray = \case Just (EInvalidParameterType (Parameter _ (TArray _ _))) -> True; _ -> False

    isEInvalidReturnType :: Maybe Error -> Bool
    isEInvalidReturnType = \case Just (EInvalidReturnType _) -> True; _ -> False

    isEProcedureCallExtraArgs :: Maybe Error -> Bool
    isEProcedureCallExtraArgs = \case Just (EProcedureCallExtraArgs _ _) -> True; _ -> False

    isEProcedureCallMissingArgs :: Maybe Error -> Bool
    isEProcedureCallMissingArgs = \case Just (EProcedureCallMissingArgs _ _) -> True; _ -> False

    isEProcedureCallArgTypeMismatch :: Maybe Error -> Bool
    isEProcedureCallArgTypeMismatch = \case Just (EProcedureCallArgTypeMismatch _ _ _) -> True; _ -> False

    isEUnknownProcedure :: Identifier -> Maybe Error -> Bool
    isEUnknownProcedure inIdent = \case Just (EUnknownProcedure ident) -> (inIdent == ident); _ -> False

    isEResourceClassNoProvides :: Identifier -> Maybe Error -> Bool
    isEResourceClassNoProvides inIdent = \case Just (EResourceClassNoProvides ident) -> (inIdent == ident); _ -> False

    isEResourceClassAction :: Identifier -> Maybe Error -> Bool
    isEResourceClassAction inIdent = \case Just (EResourceClassAction _ ident) -> (inIdent == ident); _ -> False

    isEResourceClassInPort :: Identifier -> Maybe Error -> Bool
    isEResourceClassInPort inIdent = \case Just (EResourceClassInPort _ ident) -> (inIdent == ident); _ -> False

    isEResourceClassOutPort :: Identifier -> Maybe Error -> Bool
    isEResourceClassOutPort inIdent = \case Just (EResourceClassOutPort _ ident) -> (inIdent == ident); _ -> False