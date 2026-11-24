-- | The page of the report of the metrics: the totals, the units grouped by
-- module, the architecture and the counting rules.
module Metrics.Html (
    styleFile, reportPage
) where

import Data.List (sortOn)
import qualified Data.Text as T
import Text.Printf (printf)

import Metrics.Coupling
import Metrics.Lines
import Metrics.Report
import Metrics.Unit
import Utils.HtmlReport (Provenance, help, provenanceLine, scriptFile)
import qualified Utils.HtmlReport as H
import Utils.Markup (escape)

-- | The file of the style of the page, in its folder
styleFile :: FilePath
styleFile = H.styleFile "metrics"

----------------------------------------
-- Cells

-- | A header cell with what the figure is on hover
th :: String -> String -> String
th tip name = "<th class=\"n\">" ++ help tip name ++ "</th>"

-- | A cell with a number
td :: Int -> String
td n = "<td class=\"n\">" ++ show n ++ "</td>"

-- | A cell without a figure
none :: String
none = "<td class=\"n\"></td>"

tile :: String -> String -> String -> String -> String
tile tip name value under =
    "<div class=\"tile\"><div class=\"label\">" ++ help tip name ++ "</div><div class=\"value\">" ++ value
    ++ "</div><div class=\"count\">" ++ under ++ "</div></div>"

cyclomatic :: UnitMetrics -> Int
cyclomatic = unitCyclomatic . umUnit

-- | The unit of highest cyclomatic complexity, the first in case of a tie
mostComplex :: [UnitMetrics] -> Maybe UnitMetrics
mostComplex units = case sortOn (negate . cyclomatic) units of
    u : _ -> Just u
    [] -> Nothing

mean :: [Int] -> String
mean xs = case xs of
    [] -> "-"
    _ -> printf "%.2f" (fromIntegral (sum xs) / fromIntegral (length xs) :: Double)

maxOf :: [Int] -> String
maxOf xs = case xs of
    [] -> none
    _ -> td (maximum xs)

-- | The cells of the lines of a module or of a unit
lineCells :: LineCounts -> String
lineCells l = concatMap td [countTotal l, countCode l, countComment l, countBlank l]

architectureLine :: ArchTotals -> String
architectureLine t = printf "%d tasks, %d handlers, %d resources, %d pools, %d atomics, %d deques, %d channels, %d emitters, %d connections"
    (totalTasks t) (totalHandlers t) (totalResources t) (totalPools t) (totalAtomics t) (totalDeques t)
    (totalChannels t) (totalEmitters t) (totalConnections t)

----------------------------------------
-- Page

-- | The page of the report
reportPage :: Provenance -> ProjectMetrics -> String
reportPage provenance pm = H.page "metrics" [styleFile] [scriptFile] "Termina metrics" "" [("Rules", Just "#rules")] $
    [ "<h1><small>Project</small>Metrics</h1>"
    , provenanceLine provenance
    , "<div class=\"tiles\">"
      ++ tile "Code lines" "Code" (show (countCode ls)) (printf "of %d lines, %d comment, %d blank" (countTotal ls) (countComment ls) (countBlank ls))
      ++ tile "Logical source lines" "lsloc" (show (sum (map mmLsloc (pmModules pm)))) ""
      ++ tile "Functions, and methods, procedures, viewers, actions and cases of classes" "Units" (show (length units)) (printf "in %d modules" (length (pmModules pm)))
      ++ tile "Cyclomatic complexity" "v(G), max" (maybe "-" (show . cyclomatic) top) (maybe "" (escape . unitLabel . umUnit) top)
      ++ tile "Cyclomatic complexity" "v(G), mean" (mean (map cyclomatic units)) ""
      ++ "</div>"
    , "<p class=\"meta\">Data: <a href=\"units.csv\">units.csv</a>, <a href=\"metrics.json\">metrics.json</a></p>" ]
    ++ unitsTable
    ++ (if null (pmInstances pm) then [] else architecture)
    ++ rules

  where

    units = allUnits pm
    ls = mconcat (map mmLines (pmModules pm))
    top = mostComplex units

    unitsTable =
        [ "<h2>Units</h2>"
        , "<p class=\"fold\"><button type=\"button\" data-fold=\"open\" hidden>Expand all</button>"
          ++ "<button type=\"button\" data-fold=\"close\" hidden>Collapse all</button></p>"
        , "<table class=\"list sortable\"><thead><tr><th>Name</th><th>Kind</th>"
          ++ th "Total lines" "Lines" ++ th "Code lines" "Code" ++ th "Comment lines" "Comment" ++ th "Blank lines" "Blank"
          ++ th "Logical source lines" "lsloc" ++ th "Cyclomatic complexity" "v(G)" ++ th "Maximum nesting depth" "Nesting"
          ++ th "Parameters, excluding self" "Parameters" ++ th "Distinct units called" "Calls" ++ th "Distinct calling units" "Callers"
          ++ th "Project modules imported" "Imports" ++ th "Project modules importing it" "Imported by" ++ "</tr></thead>" ]
        ++ map moduleGroup (pmModules pm)
        ++ [ "</table>" ]

    moduleGroup mm =
        let us = mmUnits mm in
        concat $
            [ "<tbody" ++ (if null us then "" else " class=\"group\"") ++ ">"
            , "<tr class=\"head\"><td data-sort=\"" ++ escape (mmName mm) ++ "\">" ++ escape (mmName mm)
              ++ " <span class=\"count\">" ++ escape (mmFile mm) ++ "</span></td><td>module</td>"
              ++ lineCells (mmLines mm) ++ td (mmLsloc mm)
              ++ maxOf (map cyclomatic us) ++ maxOf (map (unitNesting . umUnit) us)
              ++ none ++ none ++ none
              ++ td (length (mmImportsOut mm)) ++ td (length (mmImportsIn mm)) ++ "</tr>" ]
            ++ [ let u = umUnit um in
                 "<tr><td class=\"unit\" data-sort=\"" ++ escape (unitLabel u) ++ "\">" ++ escape (unitLabel u)
                 ++ " <span class=\"count\">" ++ show (unitFirstLine u) ++ "</span></td><td>" ++ kindLabel (unitKind u) ++ "</td>"
                 ++ lineCells (umLines um)
                 ++ concatMap td [ unitLsloc u, unitCyclomatic u, unitNesting u, unitParameters u
                                 , length (umCalls um), length (umCallers um) ]
                 ++ none ++ none ++ "</tr>"
               | um <- us ]
            ++ [ "</tbody>" ]

    architecture =
        [ "<h2>Architecture</h2>"
        , "<p class=\"meta\">" ++ architectureLine (pmTotals pm) ++ "</p>" ]
        ++ concat [ instanceTable title kind | (title, kind) <- [("Tasks", TaskInstance), ("Handlers", HandlerInstance), ("Resources", ResourceInstance)] ]
        ++ [ "<h3>Calls by instance</h3>"
           , "<table class=\"list sortable\"><thead><tr><th>Instance</th><th>Unit</th>"
             ++ th "Distinct units called when the instance runs the unit" "Calls" ++ "</tr></thead><tbody>" ]
        ++ [ "<tr><td>" ++ escape (instanceName i) ++ "</td><td>" ++ escape (instanceClass i ++ "::" ++ unit) ++ "</td>" ++ td n ++ "</tr>"
           | i <- pmInstances pm, (unit, n) <- instanceCalls i ]
        ++ [ "</tbody></table>" ]

    instanceTable title kind = case [ i | i <- pmInstances pm, instanceKind i == kind ] of
        [] -> []
        is ->
            [ "<h3>" ++ title ++ "</h3>"
            , "<table class=\"list sortable\"><thead><tr><th>Instance</th><th>Class</th>"
              ++ th "Input ports" "In" ++ th "Output ports" "Out" ++ th "Sink ports" "Sink" ++ th "Access ports" "Access"
              ++ th "Resources accessed" "Accesses" ++ th "Channels sent to" "Sends to"
              ++ th "Channels received from" "Receives from" ++ th "Emitters connected" "Emitters"
              ++ th "Instances connected to the resource" "Users" ++ "</tr></thead><tbody>" ]
            ++ [ "<tr><td>" ++ escape (instanceName i) ++ "</td><td>" ++ escape (instanceClass i) ++ "</td>"
                 ++ concatMap td [ portsIn p, portsOut p, portsSink p, portsAccess p
                                 , length (instanceAccesses i), length (instanceSendsTo i), length (instanceReceivesFrom i)
                                 , length (instanceEmitters i), instanceUsers i ]
                 ++ "</tr>"
               | i <- is, let p = instancePorts i ]
            ++ [ "</tbody></table>" ]

    rules =
        [ "<h2 id=\"rules\">How it is measured</h2>"
        , "<p class=\"meta\">Counting rules: " ++ escape (T.unpack rulesVersion) ++ ". Reports produced with different rule versions are not comparable.</p>"
        , "<table class=\"list rules\"><thead><tr><th>Indicator</th><th>Rule</th></tr></thead><tbody>" ]
        ++ [ "<tr><td>" ++ name ++ "</td><td>" ++ escape rule ++ "</td></tr>" | (name, rule) <- ruleRows ]
        ++ [ "</tbody></table>" ]

-- | The rule of each indicator
ruleRows :: [(String, String)]
ruleRows =
    [ ("Code", "Lines with at least one non-blank character outside a comment. A line with code and a trailing comment is a code line.")
    , ("Comment", "Lines without code that contain comment text or lie inside a block comment. Documentation comments are comment lines.")
    , ("Blank", "Lines with only white space outside a comment.")
    , ("Unit", "Function, or method, procedure, viewer, action or case of a class. Its lines range from its documentation comment, if any, to its closing brace.")
    , ("lsloc", "In a unit: 1 for the header, plus 1 for each declaration, assignment, expression statement, if, for, match, return, continue and reboot. In a module: the lsloc of its units, plus 1 for each import, global, constexpr, extern function, struct, struct field, enum, interface, interface procedure, class, class field, class port and instance.")
    , ("v(G)", "1, plus 1 for each if, else if, for and while clause, plus k \8722 1 for each match of k cases (k if it has a default case), plus 1 for each && and ||.")
    , ("Nesting", "Maximum depth of nested if, for and match statements. An else if does not increase the depth.")
    , ("Parameters", "Number of parameters, excluding self.")
    , ("Calls", "Number of distinct units and operations called by the unit, from all the instances of its class. A procedure invoked through an access port resolves to the procedures of the resources connected to that port in app.fin, or to the interface procedure in a library. All send operations count as one target.")
    , ("Callers", "Number of distinct units that call the unit, from all the instances of their classes.")
    , ("Imports", "Number of project modules imported by the module.")
    , ("Imported by", "Number of project modules that import the module.")
    , ("In, Out, Sink, Access", "Number of ports of each kind of a task or handler.")
    , ("Users", "Number of instances connected to a resource.")
    , ("Connections", "Number of connections declared in app.fin.")
    ]
