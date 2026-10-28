-- | The translation of the return addresses of a record to the file and the
-- line of the .fin, with the tool of the system where the test ran.
module Runner.Symbols (
    Location(..), locateAddresses, stampAddress, atosLocation, addr2lineLocation
) where

import Control.Exception (IOException, try)
import Data.Char (isDigit)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import Numeric (readHex, showHex)
import System.Info (os)
import System.Process (readProcessWithExitCode)

-- | A file and a line of the source
data Location = Location {
    locationFile :: FilePath,
    locationLine :: Integer
  } deriving (Eq, Show)

-- | The location of each address of a binary, already without its load offset;
-- one is subtracted from each return address, which points after its call.
-- It gives the addresses it could not translate as nothing, and a warning when
-- a tool is missing.
locateAddresses :: FilePath -> [Integer] -> IO (M.Map Integer (Maybe Location), Maybe String)
locateAddresses _ [] = return (M.empty, Nothing)
locateAddresses binary addresses = do
    let queries = [ hex (a - 1) | a <- addresses ]
    located <- if os == "darwin"
        then runTool "atos" (["-o", binary] ++ queries) atosLocation
        else runTool "addr2line" (["-e", binary] ++ queries) addr2lineLocation
    return $ case located of
        Left warning -> (M.fromList [ (a, Nothing) | a <- addresses ], Just warning)
        Right locations -> (M.fromList (zip addresses (locations ++ repeat Nothing)), Nothing)

  where

    hex value = "0x" ++ showHex value ""

-- | Runs a tool of the system and reads one location per line of its output
runTool :: String -> [String] -> (String -> Maybe Location) -> IO (Either String [Maybe Location])
runTool tool args readLine = do
    result <- try (readProcessWithExitCode tool args "")
    return $ case result of
        Left err -> Left (tool ++ " not found: the failures are given without their line (" ++ show (err :: IOException) ++ ")")
        Right (_, out, _) -> Right (map readLine (lines out))

-- | The address of termina__test__stamp in the symbol table, which macOS
-- writes with an underscore; the anchor of the header minus it is the load offset
stampAddress :: FilePath -> IO (Either String Integer)
stampAddress binary = do
    result <- try (readProcessWithExitCode "nm" [binary] "")
    return $ case result of
        Left err -> Left ("nm not found: the failures are given without their line (" ++ show (err :: IOException) ++ ")")
        Right (_, out, _) ->
            case [ address | [address, _, name] <- map words (lines out), name `elem` ["termina__test__stamp", "_termina__test__stamp"] ] of
                (address : _) -> case readHex address of
                    [(value, "")] -> Right value
                    _ -> Left ("nm gives an address that is not hexadecimal: " ++ address)
                [] -> Left ("the binary " ++ binary ++ " has no termina__test__stamp")

-- | "function (in binary) (test.fin:228)": atos gives the name of the file without its folder
atosLocation :: String -> Maybe Location
atosLocation text =
    case reverse (words text) of
        (lastWord : _) ->
            if "(" == take 1 lastWord && ")" `isSuffixOf` lastWord
                then fileAndLine (init (drop 1 lastWord))
                else Nothing
        [] -> Nothing

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
