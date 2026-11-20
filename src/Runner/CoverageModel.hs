{-# LANGUAGE OverloadedStrings #-}

-- | The coverage of the sources of a project, from the maps and the results of the runs.
module Runner.CoverageModel (
    Point(..), pointLine, ModuleMap(..), ModuleFlags(..), RunResult(..),
    Outcomes(..), Condition(..), Decision(..), Case(..), Function(..), SourceCoverage(..),
    gatherSources,
    Totals(..), noTotals, addTotals, sourceTotals
) where

import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

-- | A point of a map
data Point = Point {
    -- | Its index in its array, none for a case without a test
    pointIndex :: Int,
    pointKind :: String,
    pointConstruct :: Maybe String,
    pointFunction :: String,
    -- | The line and the column where it starts and where it ends
    pointStart :: (Int, Int),
    pointEnd :: (Int, Int),
    -- | The decision of a condition
    pointDecision :: Maybe Int,
    -- | The test of the case before a case without a test
    pointAfter :: Maybe Int,
    -- | The statement of the match of its only case
    pointStatement :: Maybe Int
  }

pointLine :: Point -> Int
pointLine = fst . pointStart

instance FromJSON Point where
    parseJSON = withObject "point" $ \o -> do
        start <- o .: "start"
        end <- o .: "end"
        Point <$> (fromMaybe (-1) <$> o .:? "index") <*> o .: "kind" <*> o .:? "construct" <*> o .: "function"
              <*> return (position start) <*> return (position end)
              <*> o .:? "decision" <*> o .:? "after" <*> o .:? "statement"

      where

        position p = case p of { [l, c] -> (l, c); _ -> (0, 0) }

-- | The map of a module
data ModuleMap = ModuleMap {
    mapModule :: String,
    mapStamp :: String,
    -- | The version of termina that built it, none in a map of an older one
    mapTermina :: Maybe String,
    mapSource :: FilePath,
    -- | The SHA-256 of its source when it was built
    mapSourceHash :: Maybe String,
    mapPoints :: [Point],
    -- | The flags of its array of the statements, and the rows of its array of the witnesses
    mapStatements :: Integer,
    mapWitnesses :: Integer
  }

instance FromJSON ModuleMap where
    parseJSON = withObject "map" $ \o ->
        ModuleMap <$> o .: "module" <*> o .: "stamp" <*> o .:? "termina" <*> o .: "source"
                  <*> o .:? "source-sha256" <*> o .: "points" <*> o .: "statements" <*> o .: "witnesses"

-- | The flags of a module in a result
data ModuleFlags = ModuleFlags {
    flagsModule :: String,
    flagsStamp :: String,
    flagsLinked :: Bool,
    flagsStatements :: [Int],
    flagsWitnesses :: [[Int]]
  }

instance FromJSON ModuleFlags where
    parseJSON = withObject "module" $ \o ->
        ModuleFlags <$> o .: "module" <*> o .: "stamp" <*> (fromMaybe True <$> o .:? "linked")
                    <*> o .: "statements" <*> o .: "witnesses"

-- | The result of a run
data RunResult = RunResult {
    runName :: String, -- ^ the name of its file, without the extension
    runKind :: String, -- ^ test or app
    runWhat :: String, -- ^ the test, or the application
    runPlatform :: String,
    runStarted :: String,
    runEnded :: String,
    runEnd :: Maybe String, -- ^ how it ended, none in a result of an older termina
    runReboots :: Maybe Int, -- ^ the reboots of a run of the application
    runModules :: [ModuleFlags]
  }

-- | The name of the run is the one of its file, which the reader sets
instance FromJSON RunResult where
    parseJSON = withObject "result" $ \o -> do
        kind <- o .: "kind"
        what <- if kind == "test" then o .: "test" else fromMaybe "app" <$> o .:? "application"
        plt <- o .: "platform"
        started <- o .: "started"
        ended <- o .: "ended"
        end <- o .:? "end"
        reboots <- o .:? "reboots"
        modules <- o .: "modules"
        return (RunResult "" kind what plt started ended end reboots modules)

-- | Whether a decision was seen true and false, or a condition with each value
data Outcomes = Outcomes {
    seenTrue :: !Bool,
    seenFalse :: !Bool
  } deriving (Eq, Show)

data Condition = Condition {
    conditionText :: String,
    conditionStart :: (Int, Int),
    conditionEnd :: (Int, Int),
    -- | The range of a for … while
    conditionRange :: Bool,
    conditionOutcomes :: Outcomes
  } deriving (Eq, Show)

data Decision = Decision {
    -- | if, for or case, or none for an expression
    decisionConstruct :: Maybe String,
    decisionText :: String,
    decisionStart :: (Int, Int),
    decisionEnd :: (Int, Int),
    decisionOutcomes :: Outcomes,
    decisionConditions :: [Condition]
  } deriving (Eq, Show)

-- | A case of a match without a test of its own
data Case = Case {
    caseText :: String,
    caseStart :: (Int, Int),
    caseEntered :: Bool
  } deriving (Eq, Show)

data Function = Function {
    functionName :: String,
    functionLine :: Int,
    functionCovered :: Bool,
    -- | Its lines with code that some run went through, and its lines with code
    functionLines :: (Int, Int)
  } deriving (Eq, Show)

-- | The coverage of a source file in a set of runs
data SourceCoverage = SourceCoverage {
    sourcePath :: FilePath,
    sourceText :: [String],
    -- | Whether some run had a module of the file in its binary
    sourceLinked :: Bool,
    -- | The lines with code, and whether some run went through each
    sourceLines :: M.Map Int Bool,
    sourceFunctions :: [Function],
    sourceDecisions :: [Decision],
    sourceCases :: [Case]
  } deriving (Eq, Show)

-- | A point of a source, by its kind and its place
data Key = Key String (Int, Int) (Int, Int)
  deriving (Eq, Ord, Show)

-- | How a case without a test is entered
data Entry = AfterTest Key | WithStatement Key
  deriving (Eq, Show)

data Value =
    StatementValue String Bool -- ^ its function, and whether it ran
    | WitnessValue (Maybe String) (Maybe Key) Bool Bool -- ^ its construct, its decision, and its two flags
    | CaseValue (Maybe Entry)

-- | The coverage of each source in any of the runs, its points matched by kind and place
gatherSources :: M.Map FilePath [String] -> [(RunResult, [(ModuleFlags, ModuleMap)])] -> [SourceCoverage]
gatherSources texts runs =
    [ sourceOf path values (M.findWithDefault False path linked) | (path, values) <- M.toList bySource ]

  where

    bySource = M.fromListWith (M.unionWith combine)
        [ (mapSource m, M.fromListWith combine (pointValues f m)) | (_, kept) <- runs, (f, m) <- kept ]

    linked = M.fromListWith (||) [ (mapSource m, flagsLinked f) | (_, kept) <- runs, (f, m) <- kept ]

    combine a b = case (a, b) of
        (StatementValue fn x, StatementValue _ y) -> StatementValue fn (x || y)
        (WitnessValue c d x y, WitnessValue _ _ x' y') -> WitnessValue c d (x || x') (y || y')
        _ -> a

    keyOf p = Key (pointKind p) (pointStart p) (pointEnd p)

    pointValues f m =
        let witnesses = M.fromList [ (pointIndex p, p) | p <- mapPoints m, pointKind p `elem` ["decision", "condition"] ]
            statements = M.fromList [ (pointIndex p, p) | p <- mapPoints m, pointKind p == "statement" ]
            flagOf i = case drop i (flagsStatements f) of { (v : _) -> v > 0; [] -> False }
            rowOf i = case drop i (flagsWitnesses f) of { [t, e] : _ -> (t > 0, e > 0); _ -> (False, False) }
            valueOf p = case pointKind p of
                "statement" -> StatementValue (pointFunction p) (flagOf (pointIndex p))
                "case" -> CaseValue $ case (pointAfter p >>= (`M.lookup` witnesses), pointStatement p >>= (`M.lookup` statements)) of
                    (Just d, _) -> Just (AfterTest (keyOf d))
                    (_, Just s) -> Just (WithStatement (keyOf s))
                    _ -> Nothing
                _ -> let (t, e) = rowOf (pointIndex p)
                     in WitnessValue (pointConstruct p) (keyOf <$> (pointDecision p >>= (`M.lookup` witnesses))) t e
        in [ (keyOf p, valueOf p) | p <- mapPoints m ]

    sourceOf path values isLinked =
        let source = M.findWithDefault [] path texts
            statements = [ (start, fn, ran) | (Key _ start _, StatementValue fn ran) <- M.toList values ]
            witnesses = [ (k, c, d, Outcomes t e) | (k, WitnessValue c d t e) <- M.toList values ]
            decisions = [ (k, c, o) | (k@(Key kind _ _), c, _, o) <- witnesses, kind == "decision" ]
            conditionsOf k@(Key _ ds de) =
                [ Condition (nameOf source c k) cs ce (cs == ds && ce == de) o
                | (c@(Key _ cs ce), _, Just d, o) <- witnesses, d == k ]
            evaluated o = seenTrue o || seenFalse o
            entered entry = case entry of
                Just (AfterTest k) -> case M.lookup k values of
                    Just (WitnessValue _ _ _ e) -> e
                    _ -> False
                Just (WithStatement k) -> case M.lookup k values of
                    Just (StatementValue _ ran) -> ran
                    _ -> False
                Nothing -> False
            -- | The line of a case is run when the case is entered
            lineMarks = [ (fst start, ran) | (start, _, ran) <- statements ]
                ++ [ (line, if c == Just "case" then seenTrue o else evaluated o) | (Key _ (line, _) _, c, _, o) <- witnesses ]
                ++ [ (line, entered entry) | (Key _ (line, _) _, CaseValue entry) <- M.toList values ]
            functions = M.toList (M.fromListWith (\(l, r) (l', r') -> (min l l', r || r'))
                [ (fn, (fst start, ran)) | (start, fn, ran) <- statements ])
            linesOf = M.map (\ls -> (M.size (M.filter id ls), M.size ls)) (M.fromListWith (M.unionWith (||))
                [ (fn, M.singleton (fst start) ran) | (start, fn, ran) <- statements ])
        in SourceCoverage {
            sourcePath = path,
            sourceText = source,
            sourceLinked = isLinked,
            sourceLines = M.fromListWith (||) lineMarks,
            sourceFunctions = sortOn functionLine
                [ Function fn l r (M.findWithDefault (0, 0) fn linesOf) | (fn, (l, r)) <- functions ],
            sourceDecisions = [ Decision c (textOf source k) s e o (conditionsOf k)
                              | (k@(Key _ s e), c, o) <- sortOn (\(k, _, _) -> k) decisions ],
            sourceCases = [ Case (textOf source k) s (entered entry) | (k@(Key _ s _), CaseValue entry) <- M.toList values ]
          }

    -- | The range of a for is a condition with the place of its decision
    nameOf source c@(Key _ cs ce) (Key _ ds de)
        | cs == ds && ce == de = "range " ++ trim (beforeWhile (textOf source c))
        | otherwise = textOf source c

-- | The text of a point in its source, on its first line
textOf :: [String] -> Key -> String
textOf source (Key _ (l1, c1) (l2, c2)) =
    let line = case drop (l1 - 1) source of { (s : _) -> s; [] -> "" }
        piece = if l1 == l2 then take (c2 - c1) (drop (c1 - 1) line) else drop (c1 - 1) line
    in trim (dropSuffix "=>" (trim (dropSuffix "{" (trim piece))))

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

dropSuffix :: String -> String -> String
dropSuffix suffix s = if suffix `isSuffixOf` s then take (length s - length suffix) s else s

beforeWhile :: String -> String
beforeWhile s = T.unpack (fst (T.breakOn (T.pack " while") (T.pack s)))

-- | The items covered and the items in all, of each kind
data Totals = Totals {
    linesHit, linesAll :: !Int,
    functionsHit, functionsAll :: !Int,
    -- | Two outcomes per decision
    outcomesHit, outcomesAll :: !Int,
    -- | Two values per condition
    valuesHit, valuesAll :: !Int
  } deriving (Eq, Show)

noTotals :: Totals
noTotals = Totals 0 0 0 0 0 0 0 0

addTotals :: Totals -> Totals -> Totals
addTotals (Totals a b c d e f g h) (Totals a' b' c' d' e' f' g' h') =
    Totals (a + a') (b + b') (c + c') (d + d') (e + e') (f + f') (g + g') (h + h')

sourceTotals :: SourceCoverage -> Totals
sourceTotals s = Totals {
    linesHit = M.size (M.filter id (sourceLines s)),
    linesAll = M.size (sourceLines s),
    functionsHit = length (filter functionCovered (sourceFunctions s)),
    functionsAll = length (sourceFunctions s),
    outcomesHit = sum (map (seen . decisionOutcomes) decisions),
    outcomesAll = 2 * length decisions,
    valuesHit = sum (map (seen . conditionOutcomes) conditions),
    valuesAll = 2 * length conditions
  }

  where

    decisions = sourceDecisions s
    conditions = concatMap decisionConditions decisions
    seen o = fromEnum (seenTrue o) + fromEnum (seenFalse o)
