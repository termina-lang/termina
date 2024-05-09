module Neg.ResourceClassSpec (spec) where

import Test.Hspec

import Text.Parsec
import Parser.Parsing

import Semantic.TypeChecking
import Semantic.Errors

runNegativeTest :: String -> Maybe (Errors Annotation)
runNegativeTest input = case parse (contents topLevel) "" input of
  Left err -> error $ "Parser Error: " ++ show err
  Right ast -> 
    case typeCheckRun ast of
      Left err -> Just $ semError err
      Right _ -> Nothing

test0 :: String
test0 = "resource class id0 {\n" ++
        "    procedure assignment_test1(&priv self, dyn_var0 : dyn u32) {\n" ++
        "        var opt : Option<dyn u32> = None;\n" ++
        "        opt = Some(dyn_var0);\n" ++
        "        return;\n" ++
        "    }\n" ++
        "};"

test1 :: String
test1 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&priv self, packets : &mut u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  action send_packet(&priv self, input : u32) -> Result {\n" ++
        "    var ret : Result = Result::Ok;" ++
        "    return ret;\n" ++
        "  }\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&priv self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test2 :: String
test2 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&priv self, packets : &mut u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  input_msg : in u32 triggers get_tm_sent_packets;\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&priv self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
        "    return;\n" ++
        "  }\n" ++
        "\n" ++
        "};\n"

test3 :: String
test3 = "interface TMChannelInterface {\n" ++
        "    procedure get_tm_sent_packets(&priv self, packets : &mut u32);\n" ++
        "};\n" ++
        "\n"++
        "resource class TMChannel provides TMChannelInterface {\n" ++
        "  tm_sent_packets : u32;\n" ++
        "\n" ++
        "  output_msg : out u32;\n" ++
        "\n" ++
        "  procedure get_tm_sent_packets(&priv self, packets : &mut u32) {\n" ++
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
        "  procedure get_tm_sent_packets(&priv self, packets : &mut u32) {\n" ++
        "    *packets = self->tm_sent_packets;\n" ++
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