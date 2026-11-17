-- | The data objects of an ELF binary, read with the library elf.
module Runner.Elf (
    readObjects
) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Elf (ElfSectionIndex(..), ElfSymbolTableEntry(..), ElfSymbolType(..), parseElf, parseSymbolTables)
import qualified Data.Map.Strict as M

-- | The data objects that the binary defines, each with its address, or why they cannot be read
readObjects :: FilePath -> IO (Either String (M.Map String Integer))
readObjects binary = do
    read' <- try (do
        bytes <- BS.readFile binary
        if BS.take 4 bytes `elem` machOMagics
            then return (Left (binary ++ " is a Mach-O binary, and only ELF binaries are read"))
            else Right <$> evaluate (M.fromList
            [ (BC.unpack name, fromIntegral (steValue entry))
            | table <- parseSymbolTables (parseElf bytes)
            , entry <- table
            , steType entry == STTObject
            , steIndex entry /= SHNUndef
            , Just name <- [snd (steName entry)] ])) :: IO (Either SomeException (Either String (M.Map String Integer)))
    return (either (\err -> Left ("cannot read the symbols of " ++ binary ++ ": " ++ show err)) id read')

  where

    -- | The magic numbers of a 64-bit Mach-O file, in either byte order
    machOMagics = [BS.pack [0xcf, 0xfa, 0xed, 0xfe], BS.pack [0xfe, 0xed, 0xfa, 0xcf]]
