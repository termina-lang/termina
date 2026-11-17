-- | The report of the coverage: the merge of the runs, the names of the
-- decisions and the conditions, the .info and the pages.
module Runner.CoverageReportSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Golden
import Runner.CoverageHtml
import Runner.CoverageModel
import Runner.CoverageReport (infoOf)
import Test.Hspec

source :: [String]
source =
    [ "function f(a : u32, b : u32) -> u32 {"
    , "    var y : u32 = 0;"
    , "    if (a > 1 && (b > 2)) {"
    , "        y = 1;"
    , "    }"
    , "    for i : u32 in 0 .. 8 while y < 4 {"
    , "        y = y + 1;"
    , "    }"
    , "    return y;"
    , "}"
    , ""
    , "function g() -> u32 {"
    , "    return 0;"
    , "}"
    ]

path :: FilePath
path = "/project/src/m.fin"

statement :: Int -> String -> (Int, Int) -> (Int, Int) -> Point
statement i fn start end = Point i "statement" Nothing fn start end Nothing Nothing Nothing

decision :: Int -> String -> (Int, Int) -> (Int, Int) -> Point
decision i construct start end = Point i "decision" (Just construct) "f" start end Nothing Nothing Nothing

condition :: Int -> (Int, Int) -> (Int, Int) -> Int -> Point
condition i start end d = Point i "condition" Nothing "f" start end (Just d) Nothing Nothing

-- | The map of the source, with the places the parser gives: an expression in
-- parentheses takes them in, and an end takes the spaces after it
moduleMap :: ModuleMap
moduleMap = ModuleMap "m" "sha256:0" (Just "0.0.0") path (Just "source-hash")
    [ statement 0 "f" (2, 5) (2, 21), statement 1 "f" (3, 5) (5, 6), statement 2 "f" (4, 9) (4, 15)
    , statement 3 "f" (6, 5) (8, 6), statement 4 "f" (7, 9) (7, 19), statement 5 "f" (9, 5) (9, 14)
    , statement 6 "g::run" (13, 5) (13, 14)
    , decision 0 "if" (3, 8) (3, 27)
    , condition 1 (3, 9) (3, 15) 0
    , condition 2 (3, 18) (3, 25) 0
    , decision 3 "for" (6, 5) (8, 6)
    , condition 4 (6, 5) (8, 6) 3
    , condition 5 (6, 33) (6, 39) 3
    ]
    7 6

run :: String -> [Int] -> [[Int]] -> (RunResult, [(ModuleFlags, ModuleMap)])
run name statements witnesses =
    let flags = ModuleFlags "m" "sha256:0" True statements witnesses
    in (RunResult name "test" name "posix-gcc" "2026-11-16T10:00:00" "2026-11-16T10:00:01" [flags], [(flags, moduleMap)])

-- | Two runs whose flags complete each other
first, second :: (RunResult, [(ModuleFlags, ModuleMap)])
first = run "first" [1, 1, 1, 1, 1, 1, 0] [[1, 0], [1, 0], [1, 0], [1, 1], [1, 0], [1, 0]]
second = run "second" [1, 1, 0, 1, 0, 1, 0] [[0, 1], [0, 1], [0, 0], [0, 1], [1, 0], [0, 1]]

texts :: M.Map FilePath [String]
texts = M.fromList [(path, source)]

merged :: SourceCoverage
merged = head (gatherSources texts [first, second])

spec :: Spec
spec = describe "Runner.CoverageReport" $ do

    it "takes an item as covered when some run covers it" $
        map decisionOutcomes (sourceDecisions merged) `shouldBe` [Outcomes True True, Outcomes True True]

    it "keeps a value of a condition that no run took as not covered" $
        map (map conditionOutcomes . decisionConditions) (sourceDecisions merged)
            `shouldBe` [[Outcomes True True, Outcomes True False], [Outcomes True False, Outcomes True True]]

    it "names each decision and condition by its text, the range of a for apart" $
        map (\d -> (decisionText d, map conditionText (decisionConditions d))) (sourceDecisions merged)
            `shouldBe` [ ("(a > 1 && (b > 2))", ["a > 1", "(b > 2)"])
                       , ("for i : u32 in 0 .. 8 while y < 4", ["range for i : u32 in 0 .. 8", "y < 4"]) ]

    it "counts the lines, the functions, the outcomes and the values" $
        sourceTotals merged `shouldBe` Totals 6 7 1 2 4 4 6 8

    it "writes a section of the .info per run" $
        goldenReport "info" "coverage_report" (T.pack (infoOf texts [first, second]))

    it "writes the page of a source" $
        goldenReport "html" "coverage_source" (T.pack (sourcePage [styleFile, projectStyleFile] "Everything" "../" "../../../" "src/m.fin" merged))
