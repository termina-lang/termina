-- | Positive parser tests for the type-specifier grammar
-- ('typeSpecifierParser'): every primitive keyword plus the composite forms.
module Parser.Positive.TypesSpec (spec) where

import Parser.Common (parseWith, ctorName)
import Parser.Parsing (typeSpecifierParser)

import Test.Hspec

-- | The constructor a type fragment parses to.
ty :: String -> Either String String
ty s = either (Left . show) (Right . ctorName) (parseWith typeSpecifierParser s)

spec :: Spec
spec = describe "Parser: type specifiers" $ do

  describe "primitive types" $
    mapM_ (\(src, expected) -> it ("parses " ++ src) $ ty src `shouldBe` Right expected)
      [ ("u8", "TSUInt8"), ("u16", "TSUInt16"), ("u32", "TSUInt32")
      , ("u64", "TSUInt64")
      , ("i8", "TSInt8"), ("i16", "TSInt16"), ("i32", "TSInt32"), ("i64", "TSInt64")
      , ("usize", "TSUSize"), ("bool", "TSBool"), ("char", "TSChar")
      , ("f32", "TSFloat32"), ("f64", "TSFloat64")
      , ("unit", "TSUnit")
      ]

  describe "composite types" $ do
    it "parses a fixed-size array type" $
      ty "[u8; 4]" `shouldBe` Right "TSArray"
    it "parses an immutable reference type" $
      ty "&u32" `shouldBe` Right "TSReference"
    it "parses a mutable reference type" $
      ty "&mut u32" `shouldBe` Right "TSReference"
    it "parses a box subtype" $
      ty "box u32" `shouldBe` Right "TSBoxSubtype"
    it "parses a location subtype" $
      ty "loc u32" `shouldBe` Right "TSLocation"
    it "parses a user-defined type" $
      ty "MyStruct" `shouldBe` Right "TSDefinedType"
    it "parses a generic user-defined type" $
      ty "Option<u32>" `shouldBe` Right "TSDefinedType"
    it "parses a nested array type" $
      ty "[[u8; 2]; 3]" `shouldBe` Right "TSArray"
    it "parses a reference to an array" $
      ty "&[u8; 4]" `shouldBe` Right "TSReference"
    it "parses a box of a user-defined type" $
      ty "box MyStruct" `shouldBe` Right "TSBoxSubtype"
    it "parses a nested generic (Option of box)" $
      ty "Option<box u32>" `shouldBe` Right "TSDefinedType"
    it "parses a two-parameter generic" $
      ty "MsgQueue<u32; 10>" `shouldBe` Right "TSDefinedType"

  describe "port types" $ do
    it "parses a sink port type" $ ty "sink u32 triggers on_tick" `shouldBe` Right "TSSinkPort"
    it "parses an in port type" $ ty "in u32 triggers process" `shouldBe` Right "TSInPort"
    it "parses an out port type" $ ty "out u32" `shouldBe` Right "TSOutPort"
    it "parses an access port type" $ ty "access IFoo" `shouldBe` Right "TSAccessPort"
