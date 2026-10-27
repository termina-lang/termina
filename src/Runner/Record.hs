-- | The record that the runtime of a test writes: a header and records packed
-- one after another, decoded with the byte order and the sizes of the platform.
module Runner.Record (
    Layout(..), platformLayout,
    Header(..), Record(..), Check(..), Values(..), ExceptionInfo(..),
    headerSize, decodeHeader, decodeRecords
) where

import Configuration.Platform (Platform(..), usizeWidth)

import Data.Bits (shiftL, (.|.), testBit)
import qualified Data.ByteString as BS
import Data.Word (Word8)

-- | Byte order and sizes of usize and of a pointer, in bytes
data Layout = Layout {
    layoutLittleEndian :: Bool,
    layoutUsize :: Int,
    layoutPointer :: Int
  } deriving (Eq, Show)

-- | The SPARC of the LEON3 is big-endian, the ARM cores and the posix hosts little-endian
platformLayout :: Platform -> Layout
platformLayout plt =
    let bytes = fromIntegral (usizeWidth plt) `div` 8 in
    case plt of
        RTEMS5LEON3SFNEXYSA7 -> Layout False bytes bytes
        _ -> Layout True bytes bytes

data Header = Header {
    headerMagic :: BS.ByteString,
    headerVersion :: Integer,
    headerFlags :: Integer,
    headerStamp :: Integer,
    headerCapacity :: Integer,
    headerWriteOffset :: Integer,
    headerSelectedCase :: Integer,
    headerCasesEnded :: Integer,
    headerChecksFailed :: Integer,
    headerChecksPassed :: Integer,
    headerLastStep :: Integer,
    headerAnchor :: Integer
  } deriving (Eq, Show)

-- | A failed check: the last step, the return address and the values obtained and expected
data Check = Check {
    checkStep :: Integer,
    checkWhere :: Integer,
    checkValues :: Maybe Values
  } deriving (Eq, Show)

-- | Two integers, already signed if the type is, or two TimeVal as seconds and microseconds
data Values =
    Scalars Integer Integer
    | TimeVals (Integer, Integer) (Integer, Integer)
  deriving (Eq, Show)

-- | A variant of Exception with the name and the value of each of its fields
data ExceptionInfo = ExceptionInfo {
    exceptionTask :: Integer,
    exceptionVariant :: String,
    exceptionFields :: [(String, Integer)]
  } deriving (Eq, Show)

data Record =
    CaseBegin Integer
    | CaseEnd Integer -- ^ checks run by the case
    | Finish
    | End
    | Exception ExceptionInfo
    | Failure String Check -- ^ the procedure that failed
    | Elapsed Check -- ^ a measurement of check_elapsed, without verdict
  deriving (Eq, Show)

-- | The size of the header, with the padding of termina__test__header_t
headerSize :: Layout -> Int
headerSize layout =
    let afterCounters = alignTo (layoutUsize layout) countersEnd
        size = afterCounters + layoutUsize layout + layoutPointer layout
    in alignTo (maximum [4, layoutUsize layout, layoutPointer layout]) size

-- | The end of the counters: the magic, version, flags and seven fields of 32 bits
countersEnd :: Int
countersEnd = 8 + 2 + 2 + 7 * 4

alignTo :: Int -> Int -> Int
alignTo n x = ((x + n - 1) `div` n) * n

-- | Reads an unsigned integer of the given number of bytes at an offset
readUnsigned :: Layout -> BS.ByteString -> Int -> Int -> Integer
readUnsigned layout bytes offset size =
    let field = BS.unpack (BS.take size (BS.drop offset bytes))
        ordered = if layoutLittleEndian layout then reverse field else field
    in foldl (\acc b -> (acc `shiftL` 8) .|. toInteger b) 0 ordered

-- | Reads a signed integer of the given number of bytes in two's complement
readSigned :: Layout -> BS.ByteString -> Int -> Int -> Integer
readSigned layout bytes offset size =
    let value = readUnsigned layout bytes offset size in
    if testBit value (8 * size - 1) then value - 2 ^ (8 * size) else value

decodeHeader :: Layout -> BS.ByteString -> Either String Header
decodeHeader layout bytes =
    if BS.length bytes < headerSize layout then
        Left "the record is shorter than its header"
    else
        let u32 offset = readUnsigned layout bytes offset 4
            stepOffset = alignTo (layoutUsize layout) countersEnd
        in Right Header {
            headerMagic = BS.take 8 bytes,
            headerVersion = readUnsigned layout bytes 8 2,
            headerFlags = readUnsigned layout bytes 10 2,
            headerStamp = u32 12,
            headerCapacity = u32 16,
            headerWriteOffset = u32 20,
            headerSelectedCase = u32 24,
            headerCasesEnded = u32 28,
            headerChecksFailed = u32 32,
            headerChecksPassed = readUnsigned layout bytes 36 4,
            headerLastStep = readUnsigned layout bytes stepOffset (layoutUsize layout),
            headerAnchor = readUnsigned layout bytes (stepOffset + layoutUsize layout) (layoutPointer layout)
        }

-- | The procedure of each tag of a failure, the size of its values and whether they are signed
failureOf :: Layout -> Word8 -> Maybe (String, Int, Bool)
failureOf layout tag =
    case tag of
        6 -> Just ("check", 0, False)
        7 -> Just ("check_eq_u8", 1, False)
        8 -> Just ("check_eq_u16", 2, False)
        9 -> Just ("check_eq_u32", 4, False)
        10 -> Just ("check_eq_u64", 8, False)
        11 -> Just ("check_eq_i8", 1, True)
        12 -> Just ("check_eq_i16", 2, True)
        13 -> Just ("check_eq_i32", 4, True)
        14 -> Just ("check_eq_i64", 8, True)
        15 -> Just ("check_eq_usize", layoutUsize layout, False)
        16 -> Just ("check_eq_bool", 1, False)
        17 -> Just ("check_eq_char", 1, False)
        18 -> Just ("check_elapsed", 8, False)
        19 -> Just ("fail", 0, False)
        _ -> Nothing

-- | The variants of Exception in the order of prelude.h, with the name, the size and the sign of each field
exceptionVariants :: Layout -> [(String, [(String, Int, Bool)])]
exceptionVariants layout =
    let usize = layoutUsize layout in [
        ("EMsgQueueSendError", [("address", usize, False), ("queue", usize, False), ("error", 4, True)]),
        ("EMsgQueueRecvError", [("queue", usize, False), ("error", 4, True)]),
        ("EArrayIndexOutOfBounds", [("address", usize, False), ("size", usize, False), ("index", usize, False)]),
        ("EArraySliceOutOfBounds", [("address", usize, False), ("size", usize, False), ("upper", usize, False)]),
        ("EArraySliceNegativeRange", [("address", usize, False), ("lower", usize, False), ("upper", usize, False)]),
        ("EArraySliceInvalidRange", [("address", usize, False), ("expected", usize, False), ("lower", usize, False), ("upper", usize, False)]),
        ("EShiftAmountOutOfBounds", [("address", usize, False), ("width", usize, False), ("amount", usize, False)]),
        ("EArithmeticOverflow", [("address", usize, False)]),
        ("EDivisionByZero", [("address", usize, False)]),
        ("ERuntimeFailure", [("address", usize, False), ("operation", 4, False), ("error", 4, True)]),
        ("EProcessorFault", [("exception", 4, False), ("status", 4, False), ("address", usize, False), ("fault_address", usize, False)]),
        ("EKernelFailure", [("source", 4, False), ("code", usize, False)]),
        ("EStackOverflow", [("task", usize, False)])
    ]

-- | The records of an area, in order; a record that does not fit in it is an error
decodeRecords :: Layout -> BS.ByteString -> Either String [Record]
decodeRecords layout area = go 0

  where

    total = BS.length area
    usize = layoutUsize layout
    pointer = layoutPointer layout

    need offset size continue =
        if offset + size <= total then continue
        else Left ("the record at byte " ++ show (offset - 1) ++ " is cut")

    go offset =
        if offset >= total then Right []
        else
            let tag = BS.index area offset
                next = offset + 1
            in case tag of
                1 -> need next 2 $ (CaseBegin (readUnsigned layout area next 2) :) <$> go (next + 2)
                2 -> need next usize $ (CaseEnd (readUnsigned layout area next usize) :) <$> go (next + usize)
                3 -> (Finish :) <$> go next
                4 -> (End :) <$> go next
                5 -> exception next
                20 -> check next 8 False Elapsed
                _ -> case failureOf layout tag of
                    Just (procedure, size, signed) -> check next size signed (Failure procedure)
                    Nothing -> Left ("unknown tag " ++ show tag ++ " at byte " ++ show offset)

    readValue signed at size =
        if signed then readSigned layout area at size else readUnsigned layout area at size

    timeVal at = (readUnsigned layout area at 4, readUnsigned layout area (at + 4) 4)

    -- | check_elapsed and ELAPSED carry two TimeVal, the rest two integers of the size of the type
    valuesOf tag at size signed =
        if size == 0 then Nothing
        else if tag == 18 || tag == 20 then Just (TimeVals (timeVal at) (timeVal (at + size)))
        else Just (Scalars (readValue signed at size) (readValue signed (at + size) size))

    check offset size signed build =
        let valuesAt = offset + usize + pointer
            tag = BS.index area (offset - 1)
            values = valuesOf tag valuesAt size signed
            record = build (Check (readUnsigned layout area offset usize) (readUnsigned layout area (offset + usize) pointer) values)
        in need offset (usize + pointer + 2 * size) ((record :) <$> go (valuesAt + 2 * size))

    exception offset =
        need offset (usize + 1) $
            let task = readUnsigned layout area offset usize
                variant = fromIntegral (BS.index area (offset + usize)) :: Int
                fieldsAt = offset + usize + 1
                variants = exceptionVariants layout
            in if variant < length variants then
                let (name, fields) = variants !! variant
                    sizes = [ size | (_, size, _) <- fields ]
                    offsets = scanl (+) fieldsAt sizes
                    values = [ (field, readValue signed at size) | ((field, size, signed), at) <- zip fields offsets ]
                in need fieldsAt (sum sizes) ((Exception (ExceptionInfo task name values) :) <$> go (fieldsAt + sum sizes))
            else Left ("unknown variant " ++ show variant ++ " of Exception at byte " ++ show (offset - 1))
