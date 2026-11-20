-- | The translation of the return addresses of a record to the file and the
-- line of the .fin, with the tools of the toolchain of the test.
module Runner.Symbols (
    Location(..), locateAddresses, stampAddress, addr2lineLocation, definedSymbols
) where

import Control.Exception (IOException, try)
import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import Numeric (showHex)
import Runner.Elf (readObjects)
import System.Process (readProcessWithExitCode)

-- | A file and a line of the source
data Location = Location {
    locationFile :: FilePath,
    locationLine :: Integer
  } deriving (Eq, Show)

-- | The location of each address of a binary, already without its load offset,
-- with the addr2line of the toolchain whose programs start with the prefix.
-- Each address comes with the one to look up, which for a return address is
-- inside its call. It gives the addresses it could not translate as nothing,
-- and a warning when a tool is missing.
locateAddresses :: String -> FilePath -> [(Integer, Integer)] -> IO (M.Map Integer (Maybe Location), Maybe String)
locateAddresses _ _ [] = return (M.empty, Nothing)
locateAddresses prefix binary addresses = do
    let queries = [ hex query | (_, query) <- addresses ]
        keys = map fst addresses
    located <- runTool (prefix ++ "addr2line") (["-e", binary] ++ queries) addr2lineLocation
    return $ case located of
        Left warning -> (M.fromList [ (a, Nothing) | a <- keys ], Just warning)
        Right locations -> (M.fromList (zip keys (locations ++ repeat Nothing)), Nothing)

  where

    hex value = "0x" ++ showHex value ""

-- | Runs a tool of the system and reads one location per line of its output
runTool :: String -> [String] -> (String -> Maybe Location) -> IO (Either String [Maybe Location])
runTool tool args readLine = do
    result <- try (readProcessWithExitCode tool args "")
    return $ case result of
        Left err -> Left (tool ++ " not found: the failures are given without their line (" ++ show (err :: IOException) ++ ")")
        Right (_, out, _) -> Right (map readLine (lines out))

-- | The symbols of the list that the symbol table of the binary defines
definedSymbols :: FilePath -> [String] -> IO (Either String [String])
definedSymbols binary wanted = fmap (\symbols -> filter (`M.member` symbols) wanted) <$> readObjects binary

-- | The address of termina__test__stamp in the symbol table of the binary
stampAddress :: FilePath -> IO (Either String Integer)
stampAddress binary = do
    symbols <- readObjects binary
    return $ case symbols of
        Left err -> Left (err ++ ": the failures are given without their line")
        Right table -> maybe (Left ("the binary " ++ binary ++ " has no termina__test__stamp")) Right
            (M.lookup "termina__test__stamp" table)

-- | "./tests/hk_fdir/test.fin:228", maybe followed by a discriminator; "??:0" when it does not know
addr2lineLocation :: String -> Maybe Location
addr2lineLocation text =
    case words text of
        (first : _) -> fileAndLine first
        [] -> Nothing

fileAndLine :: String -> Maybe Location
fileAndLine text =
    let (lineDigits, rest) = span isDigit (reverse text) in
    case rest of
        ':' : file ->
            if not (null lineDigits) && reverse file /= "??"
                then Just (Location (dropDot (reverse file)) (read (reverse lineDigits)))
                else Nothing
        _ -> Nothing

  where

    dropDot path = case path of
        '.' : '/' : remaining -> remaining
        _ -> path
