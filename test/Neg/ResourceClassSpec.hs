module Neg.ResourceClassSpec (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Monad
import Semantic.Errors
import AST.Seman

runNegativeTest :: String -> Maybe (Errors Annotation)
runNegativeTest input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking initialExpressionSt (typeTerminaModule ast) of
      Left err -> Just $ semError err
      Right _ -> Nothing

test0 :: String
test0 = "resource class id0 {\n" ++
        "    procedure assignment_test1(&mut self, dyn_var0 : dyn u32) {\n" ++
        "        var opt : Option<dyn u32> = None;\n" ++
        "        opt = Some(dyn_var0);\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test1 :: String
test1 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  action send_packet(&mut self, input : u32) -> Result {\n" ++
        "    var ret : Result = Result::Ok;" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&mut self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test2 :: String
test2 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
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

test3 :: String
test3 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
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

test4 :: String
test4 = "resource class TMChannel provides TMChannelInterface {\n" ++
        "\n"++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&mut self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test5 :: String
test5 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&mut self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "  procedure send_packet(&mut self, input : u32) {\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test6 :: String
test6 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
        "    procedure send_packet(&mut self, input : u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&mut self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test7 :: String
test7 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
        "    procedure send_packet(&mut self);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&mut self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "  procedure send_packet(&mut self, input : u32) {\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test8 :: String
test8 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
        "    procedure send_packet(&mut self, input : u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&mut self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "  procedure send_packet(&mut self) {\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test9 :: String
test9 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&mut self, packets : &mut u32);\n" ++
        "    procedure send_packet(&mut self);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&mut self, packets : &mut u16) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "  procedure send_packet(&mut self) {\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

spec :: Spec
spec = do
  describe "Resource class definition" $ do
    it "Resource class without provided interfaces" $ do
     runNegativeTest test0
       `shouldSatisfy`
        isEResourceClassNoProvides
    it "Resource class defines an action" $ do
     runNegativeTest test1
       `shouldSatisfy`
        isEResourceClassAction
    it "Resource class defines an in port" $ do
     runNegativeTest test2
       `shouldSatisfy`
        isEResourceClassInPort
    it "Resource class defines an out port" $ do
     runNegativeTest test3
       `shouldSatisfy`
        isEResourceClassOutPort
    it "Resource class provides an undefined interface" $ do
     runNegativeTest test4
       `shouldSatisfy`
        isEInterfaceNotFound
    it "Resource class implements a procedure that does not belong to an interface" $ do
     runNegativeTest test5
       `shouldSatisfy`
        isEProcedureNotFromProvidedInterfaces
    it "Resource class does not implement a procedure from the provided interfaces" $ do
     runNegativeTest test6
       `shouldSatisfy`
        isEMissingProcedure
    it "A procedure defines an extra parameter" $ do
     runNegativeTest test7
       `shouldSatisfy`
        isEProcedureExtraParams
    it "A procedure has a missing parameter" $ do
     runNegativeTest test8
       `shouldSatisfy`
        isEProcedureMissingParams
    it "A procedure defines a parameter different from the one specified by the interface" $ do
     runNegativeTest test9
       `shouldSatisfy`
        isEProcedureParamMismatch
    
  
  where

    isEResourceClassNoProvides :: Maybe (Errors Annotation) -> Bool
    isEResourceClassNoProvides = \case Just (EResourceClassNoProvides "id0") -> True; _ -> False

    isEResourceClassAction :: Maybe (Errors Annotation) -> Bool
    isEResourceClassAction = \case Just (EResourceClassAction ("TMChannel", Position _pos) "send_packet") -> True; _ -> False

    isEResourceClassInPort :: Maybe (Errors Annotation) -> Bool
    isEResourceClassInPort = \case Just (EResourceClassInPort ("TMChannel", Position _pos) "input_msg") -> True; _ -> False

    isEResourceClassOutPort :: Maybe (Errors Annotation) -> Bool
    isEResourceClassOutPort = \case Just (EResourceClassOutPort ("TMChannel", Position _pos) "output_msg") -> True; _ -> False

    isEInterfaceNotFound :: Maybe (Errors Annotation) -> Bool
    isEInterfaceNotFound = \case Just (EInterfaceNotFound "TMChannelInterface") -> True; _ -> False

    isEProcedureNotFromProvidedInterfaces :: Maybe (Errors Annotation) -> Bool
    isEProcedureNotFromProvidedInterfaces = \case Just (EProcedureNotFromProvidedInterfaces ("TMChannel", Position _pos) "send_packet") -> True; _ -> False
  
    isEMissingProcedure :: Maybe (Errors Annotation) -> Bool
    isEMissingProcedure = \case Just (EMissingProcedure "TMChannelInterface" "send_packet") -> True; _ -> False

    isEProcedureExtraParams :: Maybe (Errors Annotation) -> Bool
    isEProcedureExtraParams = \case Just (EProcedureExtraParams ("TMChannelInterface", "send_packet", [], Position _pos) 1) -> True; _ -> False

    isEProcedureMissingParams :: Maybe (Errors Annotation) -> Bool
    isEProcedureMissingParams = \case Just (EProcedureMissingParams ("TMChannelInterface", "send_packet", [Parameter "input" UInt32], Position _pos) 0) -> True; _ -> False

    isEProcedureParamMismatch :: Maybe (Errors Annotation) -> Bool
    isEProcedureParamMismatch = \case Just (EProcedureParamTypeMismatch ("TMChannelInterface", "get_tm_sent_packets", Parameter "packets" (Reference Mutable UInt32), Position _pos) (Reference Mutable UInt16)) -> True; _ -> False