-- | The pages of the report of the coverage: the landing page, the views and the sources.
module Runner.CoverageHtml (
    ViewEntry(..), RunEntry(..), Provenance(..), landingPage, viewPage, sourcePage, markFile, styleFile, projectStyleFile
) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf, sortOn, tails)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Runner.CoverageModel
import Utils.Markup (escape)

-- | The file of the Termina mark, in the folder of the landing page
markFile :: FilePath
markFile = "termina-mark.svg"

-- | The file of the style of the pages, in the folder of the landing page
styleFile :: FilePath
styleFile = "coverage.css"

-- | The file of the style of the project, in the folder of the landing page
projectStyleFile :: FilePath
projectStyleFile = "project.css"

-- | A view on the landing page: its title, its folder and its totals
data ViewEntry = ViewEntry {
    viewTitle :: String,
    viewFolder :: FilePath,
    viewTotals :: Totals
  }

-- | A run on the landing page
data RunEntry = RunEntry {
    runLabel :: String,
    runPlatform' :: String,
    runStarted' :: String,
    runEnded' :: String,
    runEnd' :: String, -- ^ how it ended, or the summary of the verdicts of a test
    runEndTip :: Maybe String, -- ^ the explanation of its end
    runReboots' :: String,
    runFolder :: Maybe FilePath, -- ^ the folder of its view, when it has one
    runTaken :: (Int, Int) -- ^ the modules taken of those in the result
  }

-- | Where a report comes from: the project with its version, the version of termina and when it was written
data Provenance = Provenance {
    provenanceProject :: String,
    provenanceTermina :: String,
    provenanceWritten :: String
  }

-- | The landing page, given its stylesheets and its provenance: the totals, the views, the runs and the number of sources not linked
landingPage :: [FilePath] -> Provenance -> Totals -> [ViewEntry] -> [RunEntry] -> Int -> String
landingPage styles provenance totals views runs notLinked = page styles "Termina coverage" "" [] $
    [ "<h1><small>Project</small>Coverage</h1>"
    , "<p class=\"meta\">" ++ escape (provenanceProject provenance) ++ " · termina " ++ escape (provenanceTermina provenance)
      ++ " · written " ++ escape (provenanceWritten provenance) ++ "</p>"
    , tiles totals
    , "<h2>Views</h2>"
    , "<table class=\"list\"><tr><th>View</th>" ++ concatMap metricHead metrics ++ "</tr>" ]
    ++ [ "<tr><td><a href=\"" ++ viewFolder v ++ "/index.html\">" ++ escape (viewTitle v) ++ "</a></td>" ++ metricCells (viewTotals v) ++ "</tr>"
       | v <- views ]
    ++ [ "</table>", "<h2>Runs</h2>"
       , "<table class=\"list\"><tr><th>Run</th><th>Platform</th><th>Started</th><th>Ended</th>"
         ++ "<th>" ++ help "How the application ended, or the verdicts of a test" "End" ++ "</th><th>Reboots</th>"
         ++ "<th>" ++ help "Modules of the run that match the current build" "Modules" ++ "</th></tr>" ]
    ++ [ "<tr><td>" ++ maybe (escape (runLabel r)) (\f -> "<a href=\"" ++ f ++ "/index.html\">" ++ escape (runLabel r) ++ "</a>") (runFolder r)
         ++ "</td><td>" ++ escape (runPlatform' r) ++ "</td><td>" ++ escape (runStarted' r) ++ "</td><td>" ++ escape (runEnded' r)
         ++ "</td><td>" ++ maybe (escape (runEnd' r)) (\tip -> help tip (escape (runEnd' r))) (runEndTip r)
         ++ "</td><td>" ++ escape (runReboots' r)
         ++ "</td><td>" ++ show taken ++ " of " ++ show total
         ++ (if taken < total
                then " <span class=\"tag\" data-tip=\"Built from another version of the source or of termina. Not counted.\">"
                     ++ show (total - taken) ++ " stale</span>"
                else "")
         ++ "</td></tr>"
       | r <- runs, let (taken, total) = runTaken r ]
    ++ [ "</table>" ]
    ++ (if notLinked == 0 then [] else
        [ "<p class=\"note\">" ++ sourcesWord notLinked ++ " not linked in any run, counted as not covered.</p>" ])

-- | The index of a view, given the stylesheets, its path to the landing page and the facts of its run when it has one
viewPage :: [FilePath] -> String -> FilePath -> [(String, String, Maybe String)] -> [(FilePath, SourceCoverage)] -> String
viewPage styles title toLanding facts sources = page styles ("Termina coverage: " ++ title) toLanding [(title, Nothing)] $
    [ "<h1><small>View</small>" ++ escape title ++ "</h1>" ]
    ++ (if null facts then [] else
        [ "<dl class=\"facts\">"
          ++ concat [ "<div><dt>" ++ escape k ++ "</dt><dd>" ++ maybe (escape v) (\tip -> help tip (escape v)) mTip ++ "</dd></div>"
                    | (k, v, mTip) <- facts ]
          ++ "</dl>" ])
    ++ [ tiles (foldr (addTotals . sourceTotals . snd) noTotals sources)
    , "<table class=\"list\"><tr><th>Source</th>" ++ concatMap metricHead metrics ++ "</tr>" ]
    ++ [ "<tr><td><a href=\"" ++ shown ++ ".html\">" ++ escape shown ++ "</a>"
         ++ (if sourceLinked s then "" else " <span class=\"tag\">not linked</span>")
         ++ "</td>" ++ metricCells (sourceTotals s) ++ "</tr>"
       | (shown, s) <- sources ]
    ++ [ "</table>" ]

-- | The page of a source in a view, given the stylesheets and its paths to the view and to the
-- landing page: totals, the lines with their marks, and the functions
sourcePage :: [FilePath] -> String -> FilePath -> FilePath -> FilePath -> SourceCoverage -> String
sourcePage styles viewName toView toLanding shown s = page styles (shown ++ ": " ++ viewName) toLanding [(viewName, Just (toView ++ "index.html")), (shown, Nothing)] $
    [ "<h1><small>Source</small>" ++ escape shown ++ "</h1>" ]
    ++ (if sourceLinked s then [] else
        [ "<p class=\"note\">Not linked in any run of this view, counted as not covered.</p>" ])
    ++ [ tiles (sourceTotals s), "<div class=\"layout\">"
       , "<aside><div class=\"outline\"><div class=\"outline-head\">" ++ help "Lines covered of each function" "Functions" ++ "</div><ul>" ]
    ++ [ "<li><span class=\"dot " ++ hitClass (functionCovered f) ++ "\"></span>"
         ++ "<a href=\"#L" ++ show (functionLine f) ++ "\">" ++ escape (functionName f) ++ "</a>"
         ++ "<span class=\"count\">" ++ show (fst (functionLines f)) ++ "/" ++ show (snd (functionLines f)) ++ "</span></li>"
       | f <- sourceFunctions s ]
    ++ [ "</ul></div></aside>", "<div class=\"panel\">", legend, "<table class=\"code\">" ]
    ++ [ "<tr" ++ maybe "" (\c -> " class=\"" ++ hitClass c ++ "\"") (M.lookup n (sourceLines s)) ++ ">"
         ++ "<td class=\"dec\">" ++ concatMap chip (chipsOn n) ++ "</td>"
         ++ "<td class=\"num\" id=\"L" ++ show n ++ "\">" ++ show n ++ "</td>"
         ++ "<td class=\"src\">" ++ marked text (marksOn n text) ++ "</td></tr>"
       | (n, text) <- zip [1 ..] (sourceText s) ]
    ++ [ "</table></div></div>" ]

  where

    decisionsOn n = sortOn (snd . decisionStart) [ d | d <- sourceDecisions s, fst (decisionStart d) == n ]

    chipsOn n = concat [ (col, c) `seq` [c] | (col, c) <- sortOn fst
        ([ (snd (decisionStart d), c) | d <- decisionsOn n, c <- decisionChips d ]
         ++ [ (snd (caseStart c), caseChip c) | c <- sourceCases s, fst (caseStart c) == n ]) ]

    -- | An if without && or || is its own single condition
    marksOn n text =
        [ conditionMark c n text | d <- sourceDecisions s, not (isLoop d && simpleLoop d)
                                 , c <- decisionConditions d, not (conditionRange c), fst (conditionStart c) == n ]
        ++ [ conditionMark (Condition (decisionText d) (decisionStart d) (decisionEnd d) False (decisionOutcomes d)) n text
           | d <- decisionsOn n, decisionConstruct d == Just "if", null (decisionConditions d) ]
        ++ concat [ loopMarks d text | d <- decisionsOn n, isLoop d ]

-- | A chip of a decision or a case: its letter, whether it was seen and its explanation
data Chip = Chip Char Bool String

chip :: Chip -> String
chip (Chip letter seen tip) =
    "<span class=\"chip " ++ hitClass seen ++ "\" data-tip=\"" ++ escape tip ++ "\">" ++ [letter] ++ "</span>"

isLoop :: Decision -> Bool
isLoop d = decisionConstruct d == Just "for"

-- | A loop whose while is a single condition, shown by its own chip
simpleLoop :: Decision -> Bool
simpleLoop d = length [ c | c <- decisionConditions d, not (conditionRange c) ] <= 1

rangeOf :: Decision -> Maybe Condition
rangeOf d = listToMaybe [ c | c <- decisionConditions d, conditionRange c ]

whileOf :: Decision -> Maybe Condition
whileOf d = case [ c | c <- decisionConditions d, not (conditionRange c) ] of
    [c] -> Just c
    _ -> Nothing

decisionChips :: Decision -> [Chip]
decisionChips d = case decisionConstruct d of
    Just "case" -> [ entered (decisionText d) (seenTrue o) ]
    Just "for" ->
        [ Chip 'I' (seenTrue o) (if seenTrue o then "body entered" else "body never entered")
        , Chip 'R' ranOut (if ranOut then "range exhausted" else "range never exhausted") ]
        ++ [ Chip 'W' exited ((if exited then "exit by while " else "no exit by while ") ++ conditionText w)
           | Just w <- [whileOf d], rangeOf d /= Nothing, let exited = seenFalse (conditionOutcomes w) ]
    _ ->
        [ Chip 'T' (seenTrue o) (decisionText d ++ ": outcome TRUE " ++ exercised (seenTrue o))
        , Chip 'F' (seenFalse o) (decisionText d ++ ": outcome FALSE " ++ exercised (seenFalse o)) ]

  where

    o = decisionOutcomes d
    ranOut = maybe (seenFalse o) (seenFalse . conditionOutcomes) (rangeOf d)

caseChip :: Case -> Chip
caseChip c = entered (caseText c) (caseEntered c)

entered :: String -> Bool -> Chip
entered text seen = Chip 'E' seen (text ++ (if seen then ": entered" else ": never entered"))

exercised :: Bool -> String
exercised seen = if seen then "exercised" else "never exercised"

sourcesWord :: Int -> String
sourcesWord n = if n == 1 then "1 source" else show n ++ " sources"

-- | A part of a line to mark: its columns, from the first to the one after the last, its class and its explanation
data Mark = Mark Int Int String String

conditionMark :: Condition -> Int -> String -> Mark
conditionMark c n text =
    let (l1, c1) = conditionStart c
        (l2, c2) = conditionEnd c
        stop = if l1 == l2 && l2 == n then c2 else length text + 1
        -- | Without the spaces that the parser takes after the end
        end = c1 + length (dropWhileEnd isSpace (take (stop - c1) (drop (c1 - 1) text)))
        o = conditionOutcomes c
        state | seenTrue o && seenFalse o = "full"
              | seenTrue o || seenFalse o = "half"
              | otherwise = "none"
        tip | seenTrue o && seenFalse o = conditionText c ++ ": TRUE and FALSE exercised"
            | seenTrue o = conditionText c ++ ": FALSE never exercised"
            | seenFalse o = conditionText c ++ ": TRUE never exercised"
            | otherwise = conditionText c ++ ": never evaluated"
    in Mark c1 end ("c " ++ state) tip

-- | The bounds of the range of a for and its while, marked as their chips
loopMarks :: Decision -> String -> [Mark]
loopMarks d text = case forParts text of
    Nothing -> []
    Just (from, to, while) ->
        [ part from 'I', part to 'R' ] ++ [ part w 'W' | Just w <- [while] ]

  where

    -- | A part marked as the chip of its letter, with its explanation
    part (a, b) letter = case [ (seen, tip) | Chip l seen tip <- decisionChips d, l == letter ] of
        (seen, tip) : _ -> Mark a b ("p " ++ hitClass seen) tip
        [] -> Mark a a "" ""

-- | The columns of the first bound, the last bound and the while of the head of a for
forParts :: String -> Maybe ((Int, Int), (Int, Int), Maybe (Int, Int))
forParts line = do
    inAt <- find " in " 0
    dots <- find ".." (inAt + 4)
    let brace = fromMaybe (length line) (listToMaybe (reverse [ i | (i, '{') <- zip [0 ..] line ]))
        while = find " while " dots
    return ( trimmed (inAt + 4) dots
           , trimmed (dots + 2) (fromMaybe brace while)
           , (\w -> trimmed (w + 7) brace) <$> while )

  where

    find needle from = listToMaybe [ i | (i, rest) <- drop from (zip [0 ..] (tails line)), needle `isPrefixOf` rest ]

    -- | From the first to the last character that is not a space, as columns counted from one
    trimmed a b =
        let inside = [ i | i <- [a .. b - 1], i < length line, not (isSpace (line !! i)) ]
        in case inside of
            [] -> (a + 1, a + 1)
            _ -> (head inside + 1, last inside + 2)

-- | A line with its marks, which nest when one is inside another
marked :: String -> [Mark] -> String
marked text marks = go 1 (length text + 1) (sortOn (\(Mark a b _ _) -> (a, negate b)) marks)

  where

    go from to ms = case ms of
        [] -> slice from to
        Mark a b cls tip : rest
            | a < from || b > to || a >= b -> go from to rest
            | otherwise ->
                let (inner, after) = span (\(Mark a' _ _ _) -> a' < b) rest
                    nested = [ m | m@(Mark a' b' _ _) <- inner, a' >= a, b' <= b ]
                in slice from a ++ "<span class=\"" ++ cls ++ "\" data-tip=\"" ++ escape tip ++ "\">"
                   ++ go a b nested ++ "</span>" ++ go b to after

    slice a b = escape (take (b - a) (drop (a - 1) text))

hitClass :: Bool -> String
hitClass covered = if covered then "hit" else "miss"

legend :: String
legend = concat
    [ "<div class=\"legend\">"
    , "<span><span class=\"chip hit\">T</span><span class=\"chip miss\">T</span> exercised, never exercised</span>"
    , "<span><span class=\"chip key\" data-tip=\"Outcome TRUE of an if, an else if, or an expression with &amp;&amp; or ||\">T</span>"
    , "<span class=\"chip key\" data-tip=\"Outcome FALSE of an if, an else if, or an expression with &amp;&amp; or ||\">F</span> decision</span>"
    , "<span><span class=\"chip key\" data-tip=\"Case of a match entered\">E</span> case</span>"
    , "<span><span class=\"chip key\" data-tip=\"Body of a for entered; marks the first bound\">I</span>"
    , "<span class=\"chip key\" data-tip=\"Range of a for exhausted; marks the last bound\">R</span>"
    , "<span class=\"chip key\" data-tip=\"Exit of a for … while by its while; marks the while\">W</span> loop</span>"
    , "<span>" ++ help "Operands of &amp;&amp; and ||, and the condition of an if without them" "Condition"
    , " <span class=\"c full\">TRUE and FALSE</span> <span class=\"c half\">one of them</span> <span class=\"c none\">never evaluated</span></span>"
    , "</div>" ]

-- | The kinds of items, each with its explanation
metrics :: [(String, String)]
metrics =
    [ ("Lines", "Lines with at least one statement")
    , ("Functions", "Functions, and the methods, procedures, viewers and actions of the classes")
    , ("Decision outcomes", "True and false of each decision: if, else if, the guard of a for,\neach case of a match with a test, and any expression with && or ||")
    , ("Condition values", "True and false of each operand of && and ||,\nand of the range and the while of a for … while") ]

metricHead :: (String, String) -> String
metricHead (name, tip) = "<th>" ++ help tip name ++ "</th>"

counts :: Totals -> [(Int, Int)]
counts t = [ (linesHit t, linesAll t), (functionsHit t, functionsAll t), (outcomesHit t, outcomesAll t), (valuesHit t, valuesAll t) ]

tiles :: Totals -> String
tiles t = "<div class=\"tiles\">" ++ concat
    [ "<div class=\"tile\"><div class=\"label\">" ++ help tip name ++ "</div><div class=\"value\">" ++ percent hit total
      ++ "</div><div class=\"count\">" ++ show hit ++ " of " ++ show total ++ "</div>" ++ bar hit total ++ "</div>"
    | ((name, tip), (hit, total)) <- zip metrics (counts t) ] ++ "</div>"

metricCells :: Totals -> String
metricCells t = concat
    [ "<td class=\"metric\">" ++ bar hit total ++ "<span class=\"pct\">" ++ percent hit total ++ "</span>"
      ++ "<span class=\"count\">" ++ show hit ++ "/" ++ show total ++ "</span></td>"
    | (hit, total) <- counts t ]

bar :: Int -> Int -> String
bar hit total = "<div class=\"bar\"><i style=\"width:" ++ show (if total == 0 then 0 else 100 * hit `div` total) ++ "%\"></i></div>"

percent :: Int -> Int -> String
percent hit total = if total == 0 then "-" else show (100 * hit `div` total) ++ " %"

help :: String -> String -> String
help tip text = "<span class=\"help\" data-tip=\"" ++ escape tip ++ "\">" ++ text ++ "</span>"

-- | A page with the header of Termina, given its stylesheets, its path to the landing page and its place under it
page :: [FilePath] -> String -> FilePath -> [(String, Maybe FilePath)] -> [String] -> String
page styles title toLanding crumbs body = unlines $
    [ "<!doctype html>"
    , "<html lang=\"en\"><head><meta charset=\"utf-8\">"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    , "<title>" ++ escape title ++ "</title>" ]
    ++ [ "<link rel=\"stylesheet\" href=\"" ++ toLanding ++ style ++ "\">" | style <- styles ]
    ++ [ "</head><body>"
    , "<header><img src=\"" ++ toLanding ++ markFile ++ "\" alt=\"\"><span class=\"brand\"><span class=\"wordmark\">Termina</span> coverage</span>"
      ++ "<nav><a href=\"" ++ toLanding ++ "index.html\">Coverage</a>"
      ++ concat [ "<span>›</span>" ++ maybe (escape name) (\href -> "<a href=\"" ++ href ++ "\">" ++ escape name ++ "</a>") link
                | (name, link) <- crumbs ]
      ++ "</nav></header>"
    , "<main>" ]
    ++ body
    ++ [ "</main></body></html>" ]
