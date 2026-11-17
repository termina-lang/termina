{-# LANGUAGE OverloadedStrings #-}

-- | The maps of an instrumented build, its arrays of flags, and the result of a run.
module Runner.Coverage (
    readMaps, coverageArrays, linkedArrays, gdbScripts, readDumps, writeTestResult, writeAppResult
) where

import Generator.CodeGen.Coverage (statementsArrayName, witnessesArrayName)
import Runner.CoverageModel (ModuleMap(..))
import Runner.Symbols (definedSymbols)

import Control.Exception (IOException, try)
import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>), (<.>), takeDirectory)
import Utils.Files (filesUnder)

-- | The maps under the folder map of an instrumented build
readMaps :: FilePath -> IO (Either String [ModuleMap])
readMaps folder = do
    let mapsFolder = folder </> "map"
    exists <- doesDirectoryExist mapsFolder
    if not exists
        then return (Left ("the build in " ++ folder ++ " has no coverage maps"))
        else do
            files <- filter (".json" `isSuffixOf`) <$> filesUnder mapsFolder
            read' <- forM files $ \file -> do
                bytes <- try (BL.readFile (mapsFolder </> file))
                return $ case bytes of
                    Left err -> Left (file ++ ": " ++ show (err :: IOException))
                    Right b -> either (\e -> Left (file ++ ": " ++ e)) Right (eitherDecode b)
            return (sequence read')

-- | The arrays that the symbol table of the binary defines, or none and a warning when it cannot be read
linkedArrays :: FilePath -> [(String, Integer)] -> IO ([(String, Integer)], Maybe String)
linkedArrays _ [] = return ([], Nothing)
linkedArrays binary arrays = do
    defined <- definedSymbols binary (map fst arrays)
    return $ case defined of
        Left err -> ([], Just ("the coverage flags are not read: " ++ err))
        Right names -> ([ a | a@(symbol, _) <- arrays, symbol `elem` names ], Nothing)

-- | The GDB scripts of a binary: the one that clears its arrays after the
-- load, and the one that dumps each of them to its file in the current folder
gdbScripts :: [(String, Integer)] -> (String, String)
gdbScripts arrays =
    ( unlines ("# Clears the coverage flags, after the load and before the start"
        : [ "interpreter-exec mi \"-data-write-memory-bytes &" ++ symbol ++ " 00 " ++ show size ++ "\"" | (symbol, size) <- arrays ])
    , unlines ("# Dumps the coverage flags to the current folder, for termina coverage --record"
        : [ "dump binary memory " ++ symbol <.> "bin" ++ " (char*)&" ++ symbol ++ " (char*)&" ++ symbol ++ "+" ++ show size
          | (symbol, size) <- arrays ]) )

-- | The dumps of the arrays in a folder, each with the size of its array; an
-- array without a file was not in the binary
readDumps :: FilePath -> [(String, Integer)] -> IO (Either String [(String, BS.ByteString)])
readDumps folder arrays = do
    found <- forM arrays $ \(symbol, size) -> do
        let file = folder </> symbol <.> "bin"
        exists <- doesFileExist file
        if not exists then return (Right []) else do
            bytes <- BS.readFile file
            return $ if fromIntegral (BS.length bytes) == size
                then Right [(symbol, bytes)]
                else Left (file ++ " has " ++ show (BS.length bytes) ++ " bytes, and " ++ symbol ++ " has " ++ show size ++ " in the build")
    return (concat <$> sequence found)

-- | The symbol and the size in bytes of each array of the flags
coverageArrays :: [ModuleMap] -> [(String, Integer)]
coverageArrays maps = concat
    [ [ (statementsArrayName (mapModule m), mapStatements m) | mapStatements m > 0 ]
      ++ [ (witnessesArrayName (mapModule m), 2 * mapWitnesses m) | mapWitnesses m > 0 ]
    | m <- maps ]

-- | Writes the result of a run of a test over the one of its previous run
writeTestResult :: FilePath -> String -> String -> String -> String -> [ModuleMap] -> [(String, BS.ByteString)] -> IO FilePath
writeTestResult outputRoot testName plt started ended =
    writeResult (outputRoot </> "coverage" </> "runs" </> ("test-" ++ testName ++ "-" ++ plt) <.> "json")
        [ "kind" .= ("test" :: T.Text), "test" .= testName, "platform" .= plt, "started" .= started, "ended" .= ended ]

-- | Writes the result of a campaign of the application, named by its start time
writeAppResult :: FilePath -> String -> String -> String -> String -> Int -> [ModuleMap] -> [(String, BS.ByteString)] -> IO FilePath
writeAppResult outputRoot plt started ended end reboots =
    writeResult (outputRoot </> "coverage" </> "runs" </> ("app-" ++ plt ++ "-" ++ map noColon started) <.> "json")
        [ "kind" .= ("app" :: T.Text), "platform" .= plt, "started" .= started, "ended" .= ended,
          "end" .= end, "reboots" .= reboots ]

  where

    noColon c = if c == ':' then '-' else c

-- | Writes a result with its fields and, for each module, its stamp and its flags
writeResult :: FilePath -> [(Key, Value)] -> [ModuleMap] -> [(String, BS.ByteString)] -> IO FilePath
writeResult file fields maps dumps = do
    let modules = [ object [
                        "module" .= mapModule m,
                        "stamp" .= mapStamp m,
                        "linked" .= (null (coverageArrays [m])
                            || any (\(symbol, _) -> lookup symbol dumps /= Nothing) (coverageArrays [m])),
                        "statements" .= flagsOf (statementsArrayName (mapModule m)) (mapStatements m),
                        "witnesses" .= rowsOf (flagsOf (witnessesArrayName (mapModule m)) (2 * mapWitnesses m)) ]
                  | m <- maps ]
        result = object (fields ++ [ "modules" .= modules ])
        keys = ["kind", "test", "platform", "started", "ended", "end", "reboots", "modules",
                "module", "stamp", "linked", "statements", "witnesses"]
    createDirectoryIfMissing True (takeDirectory file)
    BL.writeFile file (encodePretty' defConfig { confCompare = keyOrder keys } result)
    return file

  where

    -- | The flags of an array, one number per flag, or none when it was not dumped
    flagsOf symbol size =
        case lookup symbol dumps of
            Just bytes -> map fromIntegral (BS.unpack (BS.take (fromIntegral size) bytes)) :: [Int]
            Nothing -> []

    -- | The flags of the witnesses in rows: [true, false]
    rowsOf flags = case flags of
        t : f : rest -> [t, f] : rowsOf rest
        _ -> []
