-- | The report of the coverage of a project: the .info of each view and its pages.
module Runner.CoverageReport (buildReport, infoOf) where

import Control.Exception (IOException, try)
import Control.Monad (forM, forM_, when)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>), (<.>), dropDrive, dropExtension, isRelative, joinPath, makeRelative, splitDirectories, takeDirectory, takeFileName)
import Runner.CoverageAssets (styleSheet, terminaMark)
import Runner.CoverageHtml
import Runner.CoverageModel
import Utils.Files (filesUnder)
import Utils.Hash (sha256File)

-- | Writes the report, given its provenance and the stylesheet of the project, from the results
-- whose stamps match a map built from the current source by the version of termina
buildReport :: Provenance -> Maybe FilePath -> FilePath -> IO ()
buildReport provenance projectStyle outputRoot = do
    let terminaVersion = provenanceTermina provenance
    let coverageRoot = outputRoot </> "coverage"
    projectRoot <- canonicalizePath =<< getCurrentDirectory
    mapFiles <- filter (\f -> ".json" `isSuffixOf` f && "/map/" `isInfixOf` ("/" ++ f)) <$> filesUnder coverageRoot
    allMaps <- fmap concat $ forM mapFiles $ \f -> do
        decoded <- decodeFile (coverageRoot </> f)
        case decoded of
            Just m -> (\source -> [m { mapSource = source }]) <$> canonicalizePath (projectRoot </> mapSource m)
            Nothing -> return []
    hashes <- M.fromList <$> mapM (\path -> (,) path <$> hashOf path) (nub (map mapSource allMaps))
    let valid m = mapTermina m == Just terminaVersion
            && mapSourceHash m /= Nothing && mapSourceHash m == M.findWithDefault Nothing (mapSource m) hashes
        maps = filter valid allMaps
    runFiles <- filter (".json" `isSuffixOf`) <$> filesUnder (coverageRoot </> "runs")
    runs <- fmap concat $ forM runFiles $ \file -> do
        decoded <- decodeFile (coverageRoot </> "runs" </> file)
        return $ case decoded of
            Just r -> [r { runName = dropExtension (takeFileName file) }]
            Nothing -> []
    texts <- M.fromList <$> mapM (\path -> (,) path <$> readLines path) (nub (map mapSource maps))
    let byStamp = M.fromList [ ((mapModule m, mapStamp m), m) | m <- maps ]
        current = [ (r, [ (mf, m) | mf <- runModules r, Just m <- [M.lookup (flagsModule mf, flagsStamp mf) byStamp] ]) | r <- sortOn runStarted runs ]
        views = [ ("Everything", "all", current, [])
                , ("Tests", "tests", [ c | c@(r, _) <- current, runKind r == "test" ], [])
                , ("Application", "app", [ c | c@(r, _) <- current, runKind r == "app" ], []) ]
                ++ [ ("Platform " ++ p, "platform" </> p, [ c | c@(r, _) <- current, runPlatform r == p ], [])
                   | p <- nub (map (runPlatform . fst) current) ]
                ++ [ (labelOf r, "runs" </> runName r, [c], factsOf r) | c@(r, _) <- current ]
        shownOf path = let relative = makeRelative projectRoot path
                       in if isRelative relative then relative else "external" </> dropDrive relative
        styles = styleFile : [ projectStyleFile | Just _ <- [projectStyle] ]
    -- | The views are written again from scratch
    createDirectoryIfMissing True coverageRoot
    let html = coverageRoot </> "html"
    exists <- doesDirectoryExist html
    when exists (removeDirectoryRecursive html)
    oldInfo <- filter (".info" `isSuffixOf`) <$> listDirectory coverageRoot
    mapM_ (removeFile . (coverageRoot </>)) oldInfo
    entries <- fmap concat $ forM views $ \(title, folder, selected, facts) -> do
        let name = map (\c -> if c == '/' then '-' else c) folder
        if null selected then return [] else do
            writeFile (coverageRoot </> name <.> "info") (infoOf texts selected)
            let sources = [ (shownOf (sourcePath s), s) | s <- withCode (gatherSources texts selected) ]
                viewRoot = html </> folder
                toLanding = upTo (1 + depth folder)
            createDirectoryIfMissing True viewRoot
            writeFile (viewRoot </> "index.html") (viewPage styles title toLanding facts sources)
            forM_ sources $ \(shown, s) -> do
                let file = viewRoot </> shown <.> "html"
                createDirectoryIfMissing True (takeDirectory file)
                writeFile file (sourcePage styles title (upTo (depth shown - 1)) (upTo (depth folder + depth shown)) shown s)
            return [(title, "html" </> folder, foldr (addTotals . sourceTotals . snd) noTotals sources)]
    let viewEntries = [ ViewEntry title folder totals | (title, folder, totals) <- entries, not ("html/runs" `isPrefixOf` folder) ]
        runEntries = [ RunEntry (labelOf r) (runPlatform r) (timeOf r (runStarted r)) (timeOf r (runEnded r)) (endOf r) (endTipOf r) (rebootsOf r)
                         (if null kept then Nothing else Just ("html" </> "runs" </> runName r))
                         (length kept, length (runModules r))
                     | (r, kept) <- current ]
        everything = withCode (gatherSources texts current)
        notLinked = length (filter (not . sourceLinked) everything)
    BS.writeFile (coverageRoot </> markFile) terminaMark
    BS.writeFile (coverageRoot </> styleFile) styleSheet
    case projectStyle of
        Just file -> copyFile file (coverageRoot </> projectStyleFile)
        Nothing -> do
            old <- doesFileExist (coverageRoot </> projectStyleFile)
            when old (removeFile (coverageRoot </> projectStyleFile))
    writeFile (coverageRoot </> "index.html")
        (landingPage styles provenance (foldr (addTotals . sourceTotals) noTotals everything) viewEntries runEntries notLinked)

  where

    labelOf r = (if runKind r == "test" then "test " else "application ") ++ runWhat r ++ " (" ++ runStarted r ++ ")"

    endOf r = fromMaybe "–" (runEnd r)

    rebootsOf r = maybe "–" show (runReboots r)

    -- | A time of a run, unknown in a recorded one
    timeOf r time = if runEnd r == Just "recorded" then "–" else time

    -- | The explanation of how a run ended
    endTipOf r = case runEnd r of
        Just "recorded" -> Just "Coverage imported from a run outside termina"
        Just _ | runKind r == "test" -> Just "Verdicts of the test run with coverage"
        Just "timeout" -> Just "Stopped when its time limit ran out"
        Just "interrupted" -> Just "Stopped by the user"
        Just e | "signal " `isPrefixOf` e -> Just "Stopped by a signal of the program"
        _ -> Nothing

    -- | The facts at the top of the view of one run
    factsOf r = [ ("Platform", runPlatform r, Nothing), ("Started", timeOf r (runStarted r), Nothing), ("Ended", timeOf r (runEnded r), Nothing)
                , ("End", endOf r, endTipOf r), ("Reboots", rebootsOf r, Nothing) ]

    depth path = length (splitDirectories path)

    upTo n = joinPath (replicate n "..") ++ (if n > 0 then "/" else "")

    -- | The SHA-256 of a file, or nothing when it cannot be read
    hashOf :: FilePath -> IO (Maybe String)
    hashOf file = either (\e -> const Nothing (e :: IOException)) Just <$> try (sha256File file)

    -- | The lines of a file, or none when it cannot be read
    readLines :: FilePath -> IO [String]
    readLines file = do
        content <- try (BL.readFile file) :: IO (Either IOException BL.ByteString)
        return (either (const []) (lines . T.unpack . TE.decodeUtf8With TEE.lenientDecode . BL.toStrict) content)

    decodeFile :: FromJSON a => FilePath -> IO (Maybe a)
    decodeFile file = do
        bytes <- try (BL.readFile file) :: IO (Either IOException BL.ByteString)
        return (either (const Nothing) decode bytes)

-- | The sources with some line of code
withCode :: [SourceCoverage] -> [SourceCoverage]
withCode = filter (not . M.null . sourceLines)

-- | The .info of the runs: a TN per run, an SF per source, values 0 or 1 and branches named by their text
infoOf :: M.Map FilePath [String] -> [(RunResult, [(ModuleFlags, ModuleMap)])] -> String
infoOf texts runs = concat [ concatMap (section r) (withCode (gatherSources texts [c])) | c@(r, _) <- runs ]

  where

    section r s =
        let functions = sourceFunctions s
            blocks = zip [0 :: Int ..] (concat [ (fst (decisionStart d), decisionText d, decisionOutcomes d)
                                                   : [ (fst (decisionStart d), conditionText c, conditionOutcomes c) | c <- decisionConditions d ]
                                                 | d <- sourceDecisions s ])
            branches = [ (line, block, text ++ ": " ++ value, taken)
                       | (block, (line, text, o)) <- blocks
                       , let evaluated = seenTrue o || seenFalse o
                       , (value, seen) <- [("true", seenTrue o), ("false", seenFalse o)]
                       , let taken = if evaluated then (if seen then "1" else "0") else "-" ]
            lines' = M.toList (sourceLines s)
        in unlines $
            ["TN:" ++ map clean (runName r), "SF:" ++ sourcePath s]
            ++ concat [ ["FN:" ++ show (functionLine f) ++ "," ++ functionName f, "FNDA:" ++ flag (functionCovered f) ++ "," ++ functionName f] | f <- functions ]
            ++ ["FNF:" ++ show (length functions), "FNH:" ++ show (length (filter functionCovered functions))]
            ++ [ "BRDA:" ++ show l ++ "," ++ show b ++ "," ++ n ++ "," ++ t | (l, b, n, t) <- branches ]
            ++ ["BRF:" ++ show (length branches), "BRH:" ++ show (length [ () | (_, _, _, "1") <- branches ])]
            ++ [ "DA:" ++ show l ++ "," ++ flag v | (l, v) <- lines' ]
            ++ ["LF:" ++ show (length lines'), "LH:" ++ show (length (filter snd lines')), "end_of_record"]

    flag v = if v then "1" else "0"

    clean c = if c `elem` ("-." :: String) then '_' else c
