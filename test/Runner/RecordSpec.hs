-- | The decoding of the record of a test, from records built by hand on a
-- posix host of 64 bits and on the LEON3, big-endian and of 32 bits.
module Runner.RecordSpec (spec) where

import Configuration.Platform (Platform(..))
import Runner.Record

import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Hspec

posix :: Layout
posix = platformLayout POSIXGCC

leon :: Layout
leon = platformLayout RTEMS5LEON3SFNEXYSA7

-- | An unsigned integer of n bytes in the byte order of a layout
field :: Layout -> Int -> Integer -> [Word8]
field layout n value =
    let bytes = [ fromIntegral ((value `shiftR` (8 * i)) .&. 0xFF) | i <- [0 .. n - 1] ] in
    if layoutLittleEndian layout then bytes else reverse bytes

-- | A signed integer of n bytes in two's complement
signedField :: Layout -> Int -> Integer -> [Word8]
signedField layout n value = field layout n (if value < 0 then value + 2 ^ (8 * n) else value)

header :: Layout -> [Word8]
header layout =
    let usize = layoutUsize layout
        padding = replicate (headerSize layout - 40 - usize - layoutPointer layout) 0
    in map (fromIntegral . fromEnum) "TRMTEST" ++ [0]
        ++ field layout 2 1 ++ field layout 2 0
        ++ concatMap (field layout 4) [0xd4344c57, 8192, 94, 0, 5, 1, 14]
        ++ padding ++ field layout usize 3 ++ field layout (layoutPointer layout) 0x55555556b080

spec :: Spec
spec = describe "Runner: decoding of the record" $ do

  it "gives the size of the header with its padding" $ do
    headerSize posix `shouldBe` 56
    headerSize leon `shouldBe` 48

  it "decodes the header" $
    decodeHeader posix (BS.pack (header posix)) `shouldBe` Right Header {
        headerMagic = BS.pack (map (fromIntegral . fromEnum) "TRMTEST" ++ [0]),
        headerVersion = 1, headerFlags = 0, headerStamp = 0xd4344c57, headerCapacity = 8192,
        headerWriteOffset = 94, headerSelectedCase = 0, headerCasesEnded = 5,
        headerChecksFailed = 1, headerChecksPassed = 14, headerLastStep = 3,
        headerAnchor = 0x55555556b080
      }

  it "rejects a header that is cut" $
    decodeHeader posix (BS.pack (take 40 (header posix))) `shouldBe` Left "the record is shorter than its header"

  it "decodes the records of a case that passes and one that fails" $ do
    let area = [1] ++ field posix 2 1 ++ [2] ++ field posix 8 4
            ++ [1] ++ field posix 2 5
            ++ [15] ++ field posix 8 0 ++ field posix 8 0x55555555c6e2 ++ field posix 8 0 ++ field posix 8 1
            ++ [2] ++ field posix 8 2 ++ [4]
    decodeRecords posix (BS.pack area) `shouldBe` Right [
        CaseBegin 1, CaseEnd 4, CaseBegin 5,
        Failure "check_eq_usize" (Check 0 0x55555555c6e2 (Just (Scalars 0 1))),
        CaseEnd 2, End
      ]

  it "reads the values of a signed check with their sign" $ do
    let area = [11] ++ field posix 8 7 ++ field posix 8 0x1000 ++ signedField posix 1 (-3) ++ signedField posix 1 4
    decodeRecords posix (BS.pack area) `shouldBe` Right [Failure "check_eq_i8" (Check 7 0x1000 (Just (Scalars (-3) 4)))]

  it "decodes a check and a fail without values" $ do
    let area = [6] ++ field posix 8 0 ++ field posix 8 0x10 ++ [19] ++ field posix 8 2 ++ field posix 8 0x20
    decodeRecords posix (BS.pack area) `shouldBe` Right [
        Failure "check" (Check 0 0x10 Nothing), Failure "fail" (Check 2 0x20 Nothing)
      ]

  it "decodes the two TimeVal of check_elapsed" $ do
    let area = [18] ++ field posix 8 0 ++ field posix 8 0x30 ++ concatMap (field posix 4) [1, 500000, 0, 900000]
    decodeRecords posix (BS.pack area) `shouldBe` Right [
        Failure "check_elapsed" (Check 0 0x30 (Just (TimeVals (1, 500000) (0, 900000))))
      ]

  it "decodes an exception with the fields of its variant" $ do
    let area = [5] ++ field posix 8 0 ++ [9] ++ field posix 8 0x4000 ++ field posix 4 10 ++ signedField posix 4 (-2)
    decodeRecords posix (BS.pack area) `shouldBe` Right [
        Exception (ExceptionInfo 0 "ERuntimeFailure" [("address", 0x4000), ("operation", 10), ("error", -2)])
      ]

  it "decodes a failure in the byte order and the sizes of the LEON3" $ do
    let area = [9] ++ field leon 4 2 ++ field leon 4 0x40001234 ++ field leon 4 95 ++ field leon 4 0
    decodeRecords leon (BS.pack area) `shouldBe` Right [Failure "check_eq_u32" (Check 2 0x40001234 (Just (Scalars 95 0)))]

  it "rejects a record that is cut" $
    decodeRecords posix (BS.pack ([1] ++ field posix 2 1 ++ [15] ++ field posix 8 0)) `shouldBe` Left "the record at byte 3 is cut"

  it "rejects an unknown tag and an unknown variant of Exception" $ do
    decodeRecords posix (BS.pack [99]) `shouldBe` Left "unknown tag 99 at byte 0"
    decodeRecords posix (BS.pack ([5] ++ field posix 8 0 ++ [40])) `shouldBe` Left "unknown variant 40 of Exception at byte 0"
