-- | The packed-member reference rule (SE-218), which fires only on
-- strict-alignment targets. Taking a reference to a location reached *through* a
-- member of a packed struct yields an under-aligned pointer whose packed
-- provenance is lost at the call boundary, so it is rejected. The subtle part is
-- the asymmetry with arrays: a reference to an element of an array *of* packed
-- structs is fine (the pointee type is itself packed and carries alignment 1),
-- whereas a reference into a field or array *inside* a packed struct crosses a
-- packed member and is not. Both directions, plus the platform gating, are
-- pinned here.
module Semantic.Positive.PackedReferenceSpec (spec) where

import Semantic.Common (typeCheckErrorOn)

import Configuration.Platform (Platform(TestPlatform, POSIXGCC))
import Data.Text (pack)
import Test.Hspec

-- | A packed struct, a normal struct, a packed struct wrapping a normal one,
-- and sink functions that take references. Each scenario appends the reference
-- it exercises.
prelude :: String
prelude =
    "#[packed]\n" ++
    "struct Packet {\n" ++
    "    header : u32;\n" ++
    "    flags : u8;\n" ++
    "};\n" ++
    "struct Inner {\n" ++
    "    x : u32;\n" ++
    "    y : u8;\n" ++
    "};\n" ++
    "#[packed]\n" ++
    "struct Outer {\n" ++
    "    inner : Inner;\n" ++
    "    z : u8;\n" ++
    "};\n" ++
    "#[packed]\n" ++
    "struct Buf {\n" ++
    "    data : [u32; 4];\n" ++
    "    tag : u8;\n" ++
    "};\n" ++
    "constexpr zero_pkt : Packet = {header = 0 : u32, flags = 0 : u8};\n" ++
    "function sink_u32(value : &mut u32) {\n" ++
    "    *value = 1 : u32;\n" ++
    "    return;\n" ++
    "}\n" ++
    "function sink_pkt(p : &mut Packet) {\n" ++
    "    return;\n" ++
    "}\n"

-- Scenarios: each is a function body wrapped by 'prelude'.

-- Reference to a scalar member of a packed struct: unsafe.
packedFieldRef :: String
packedFieldRef = prelude ++
    "function trigger() {\n" ++
    "    var pkt : Packet = {header = 0 : u32, flags = 0 : u8};\n" ++
    "    sink_u32(&mut pkt.header);\n" ++
    "    return;\n" ++
    "}\n"

-- Reference to a scalar reached through a packed struct whose member is a
-- (non-packed) nested struct: unsafe, and only caught by walking the chain.
nestedPackedRef :: String
nestedPackedRef = prelude ++
    "function trigger() {\n" ++
    "    var o : Outer = {inner = {x = 0 : u32, y = 0 : u8}, z = 0 : u8};\n" ++
    "    sink_u32(&mut o.inner.x);\n" ++
    "    return;\n" ++
    "}\n"

-- Reference to an element of an array *inside* a packed struct: unsafe.
arrayInsidePackedRef :: String
arrayInsidePackedRef = prelude ++
    "function trigger() {\n" ++
    "    var b : Buf = {data = [0 : u32; 4], tag = 0 : u8};\n" ++
    "    sink_u32(&mut b.data[1 : usize]);\n" ++
    "    return;\n" ++
    "}\n"

-- Reference to an element of an array *of* packed structs: safe (the pointee
-- type is the packed struct itself).
arrayOfPackedRef :: String
arrayOfPackedRef = prelude ++
    "function trigger() {\n" ++
    "    var arr : [Packet; 4] = [zero_pkt; 4];\n" ++
    "    sink_pkt(&mut arr[1 : usize]);\n" ++
    "    return;\n" ++
    "}\n"

-- Reference to a whole packed struct: safe (the struct is aligned to 1; the
-- pointer is a packed-struct pointer).
wholePackedRef :: String
wholePackedRef = prelude ++
    "function trigger() {\n" ++
    "    var pkt : Packet = {header = 0 : u32, flags = 0 : u8};\n" ++
    "    sink_pkt(&mut pkt);\n" ++
    "    return;\n" ++
    "}\n"

spec :: Spec
spec = do
  describe "SE-218: reference to a member of a packed struct (strict-alignment target)" $ do
    it "rejects a reference to a scalar member" $
      typeCheckErrorOn TestPlatform packedFieldRef `shouldBe` Just (pack "SE-218")
    it "rejects a reference reached through a nested packed member" $
      typeCheckErrorOn TestPlatform nestedPackedRef `shouldBe` Just (pack "SE-218")
    it "rejects a reference to an element of an array inside a packed struct" $
      typeCheckErrorOn TestPlatform arrayInsidePackedRef `shouldBe` Just (pack "SE-218")

  describe "SE-218: references that stay well-aligned are accepted" $ do
    it "accepts a reference to an element of an array of packed structs" $
      typeCheckErrorOn TestPlatform arrayOfPackedRef `shouldBe` Nothing
    it "accepts a reference to a whole packed struct" $
      typeCheckErrorOn TestPlatform wholePackedRef `shouldBe` Nothing

  describe "SE-218: the rule is gated on the platform being strict-alignment" $ do
    it "accepts the same packed-member reference on a non-strict target" $
      typeCheckErrorOn POSIXGCC packedFieldRef `shouldBe` Nothing
