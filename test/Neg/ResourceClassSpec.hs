module Neg.ResourceClassSpec (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Monad
import Semantic.Errors.Errors
import Semantic.AST
import Utils.Annotations

runNegativeTest :: String -> Maybe (Error Location)
runNegativeTest input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case runTypeChecking (makeInitialGlobalEnv []) (typeTerminaModule ast) of
      Left err -> Just $ getError err
      Right _ -> Nothing

test0 :: String
test0 = "resource class id0 {\n" ++
        "    procedure assignment_test1(&mut self, box_var0 : box u32) {\n" ++
        "        var opt : Option<box u32> = None;\n" ++
        "        opt = Some(box_var0);\n" ++
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

    isEResourceClassNoProvides :: Maybe (Error Location) -> Bool
    isEResourceClassNoProvides = \case Just (EResourceClassNoProvides "id0") -> True; _ -> False

    isEResourceClassAction :: Maybe (Error Location) -> Bool
    isEResourceClassAction = \case Just (EResourceClassAction ("TMChannel", Position {}) "send_packet") -> True; _ -> False

    isEResourceClassInPort :: Maybe (Error Location) -> Bool
    isEResourceClassInPort = \case Just (EResourceClassInPort ("TMChannel", Position {}) "input_msg") -> True; _ -> False

    isEResourceClassOutPort :: Maybe (Error Location) -> Bool
    isEResourceClassOutPort = \case Just (EResourceClassOutPort ("TMChannel", Position {}) "output_msg") -> True; _ -> False

    isEInterfaceNotFound :: Maybe (Error Location) -> Bool
    isEInterfaceNotFound = \case Just (EInterfaceNotFound "TMChannelInterface") -> True; _ -> False

    isEProcedureNotFromProvidedInterfaces :: Maybe (Error Location) -> Bool
    isEProcedureNotFromProvidedInterfaces = \case Just (EProcedureNotFromProvidedInterfaces ("TMChannel", Position {}) "send_packet") -> True; _ -> False
  
    isEMissingProcedure :: Maybe (Error Location) -> Bool
    isEMissingProcedure = \case Just (EMissingProcedure "TMChannelInterface" "send_packet") -> True; _ -> False

    isEProcedureExtraParams :: Maybe (Error Location) -> Bool
    isEProcedureExtraParams = \case Just (EProcedureExtraParams ("TMChannelInterface", "send_packet", [], Position {}) 1) -> True; _ -> False

    isEProcedureMissingParams :: Maybe (Error Location) -> Bool
    isEProcedureMissingParams = \case Just (EProcedureMissingParams ("TMChannelInterface", "send_packet", [UInt32], Position {}) 0) -> True; _ -> False

    isEProcedureParamMismatch :: Maybe (Error Location) -> Bool
    isEProcedureParamMismatch = \case Just (EProcedureParamTypeMismatch ("TMChannelInterface", "get_tm_sent_packets", Reference Mutable UInt32, Position {}) (Reference Mutable UInt16)) -> True; _ -> False