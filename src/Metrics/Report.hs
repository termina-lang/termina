{-# LANGUAGE OverloadedStrings #-}
-- | The metrics of a project and their three renderings: the text table
-- printed on the terminal, the CSV of the units and the JSON with every
-- figure.
module Metrics.Report
  ( UnitMetrics(..)
  , ModuleMetrics(..)
  , ProjectMetrics(..)
  , moduleMetrics
  , rulesVersion
  , unitLabel
  , keyLabel
  , targetLabel
  , kindLabel
  , allUnits
  , textReport
  , unitsCsv
  , metricsJson
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate, maximumBy)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Printf (printf)

import Metrics.Coupling
import Metrics.Lines
import Metrics.Unit
import Semantic.AST (AnnotatedProgram, Identifier)
import Semantic.Types (SemanticAnn)

-- | The version of the counting rules of the design. Two reports can only be
-- compared when they carry the same one.
rulesVersion :: T.Text
rulesVersion = "termina-metrics/1"

-- | A unit with its lines and its calls. The calls are marked when the
-- generated C reaches them through a function pointer.
data UnitMetrics = UnitMetrics
  { umUnit :: Unit
  , umLines :: LineCounts
  , umCalls :: [(Target, Call)]
  , umCallers :: [(UnitKey, S.Set Identifier)]
  } deriving (Show, Eq)

-- | A module with its name in dotted form, its file relative to the project
-- root, its lines, its logical size, its imports and its units in source
-- order.
data ModuleMetrics = ModuleMetrics
  { mmName :: String
  , mmFile :: FilePath
  , mmLines :: LineCounts
  , mmLsloc :: Int
  , mmImportsOut :: [String]
  , mmImportsIn :: [String]
  , mmUnits :: [UnitMetrics]
  } deriving (Show, Eq)

-- | The modules in name order and the architecture of the application.
data ProjectMetrics = ProjectMetrics
  { pmModules :: [ModuleMetrics]
  , pmInstances :: [Instance]
  , pmTotals :: ArchTotals
  } deriving (Show, Eq)

-- | The metrics of a module, given its dotted name, its file, its text, its
-- typed AST, the calls and callers of every unit of the program and its
-- imports. Each import adds one to the logical size, as UCC counts an
-- @#include@.
moduleMetrics :: String -> FilePath -> String -> AnnotatedProgram SemanticAnn
  -> M.Map UnitKey CallSet -> M.Map UnitKey (M.Map UnitKey (S.Set Identifier)) -> ([String], [String]) -> ModuleMetrics
moduleMetrics name file text program calls callers (importsOut, importsIn) =
  ModuleMetrics
    { mmName = name
    , mmFile = file
    , mmLines = totalCounts ls
    , mmLsloc = length importsOut + moduleLsloc program units
    , mmImportsOut = importsOut
    , mmImportsIn = importsIn
    , mmUnits = map measured units
    }

  where

    ls = scanLines text
    units = unitsOf text program
    measured u =
      let key = (unitClass u, unitName u) in
      UnitMetrics
        { umUnit = u
        , umLines = countRange ls (unitFirstLine u) (unitLastLine u)
        , umCalls = M.toList (M.findWithDefault M.empty key calls)
        , umCallers = M.toList (M.findWithDefault M.empty key callers)
        }

-- | The name of a unit, with its class in front if it has one.
unitLabel :: Unit -> String
unitLabel u = keyLabel (unitClass u, unitName u)

keyLabel :: UnitKey -> String
keyLabel (cls, name) = maybe "" (++ "::") cls ++ name

targetLabel :: Target -> String
targetLabel t =
  case t of
    UnitTarget cls name -> keyLabel (cls, name)
    OperationTarget name -> name

kindLabel :: UnitKind -> String
kindLabel k =
  case k of
    FunctionUnit -> "function"
    MethodUnit -> "method"
    ProcedureUnit -> "procedure"
    ViewerUnit -> "viewer"
    ActionUnit -> "action"
    CaseUnit -> "case"

portCalls :: UnitMetrics -> Int
portCalls = length . filter (callThroughPointer . snd) . umCalls

allUnits :: ProjectMetrics -> [UnitMetrics]
allUnits = concatMap mmUnits . pmModules

-- | The unit of highest cyclomatic complexity, the first in case of a tie.
mostComplex :: [UnitMetrics] -> Maybe UnitMetrics
mostComplex units =
  case units of
    [] -> Nothing
    _ -> Just (maximumBy (comparing (unitCyclomatic . umUnit)) (reverse units))

----------------------------------------
-- Text

-- | One table per module with a row per unit, the line of the module after
-- it, and the totals of the project and of the architecture at the end.
textReport :: ProjectMetrics -> String
textReport pm = unlines (concatMap moduleBlock (pmModules pm) ++ [projectLine, archLine])

  where

    nameWidth = maximum (12 : [ length (unitLabel (umUnit u)) | u <- allUnits pm ])
    kindWidth = 9
    header file = printf "%-*s  %5s  %5s  %4s  %4s  %3s  %5s  %7s" (nameWidth + kindWidth + 3) file
      ("code" :: String) ("lsloc" :: String) ("v(G)" :: String) ("nest" :: String) ("par" :: String) ("calls" :: String) ("callers" :: String)

    row um =
      let u = umUnit um in
      printf "  %-*s %-*s  %5d  %5d  %4d  %4d  %3d  %5d  %7d" nameWidth (unitLabel u) kindWidth (kindLabel (unitKind u))
        (countCode (umLines um)) (unitLsloc u) (unitCyclomatic u) (unitNesting u) (unitParameters u)
        (length (umCalls um)) (length (umCallers um))

    moduleBlock mm =
      [header (mmFile mm)] ++ map row (mmUnits mm) ++ [moduleLine mm, ""]

    moduleLine mm =
      let l = mmLines mm in
      printf "  module: %d lines (%d code, %d comment, %d blank), lsloc %d%s"
        (countTotal l) (countCode l) (countComment l) (countBlank l) (mmLsloc mm)
        (maybe "" (\u -> printf ", max v(G) %d" (unitCyclomatic (umUnit u)) :: String) (mostComplex (mmUnits mm)))

    projectLine =
      printf "project: %d modules, %d units, %d code lines, lsloc %d%s"
        (length (pmModules pm)) (length (allUnits pm)) (sum (map (countCode . mmLines) (pmModules pm)))
        (sum (map mmLsloc (pmModules pm)))
        (maybe "" (\u -> printf ", max v(G) %d (%s)" (unitCyclomatic (umUnit u)) (unitLabel (umUnit u)) :: String) (mostComplex (allUnits pm)))

    archLine =
      let t = pmTotals pm in
      printf "architecture: %d tasks, %d handlers, %d resources, %d channels, %d emitters, %d connections"
        (totalTasks t) (totalHandlers t) (totalResources t) (totalChannels t) (totalEmitters t) (totalConnections t)

----------------------------------------
-- CSV

csvColumns :: [String]
csvColumns =
  [ "module", "class", "unit", "kind", "first_line", "last_line"
  , "lines", "code", "comment", "blank", "eol_comment", "doc"
  , "statements", "lsloc", "statement_complexity", "expression_complexity", "cyclomatic"
  , "max_nesting", "parameters", "calls", "port_calls", "callers" ]

-- | One row per unit, modules in name order and units in source order.
unitsCsv :: ProjectMetrics -> String
unitsCsv pm = unlines (intercalate "," csvColumns : [ csvRow mm um | mm <- pmModules pm, um <- mmUnits mm ])

  where

    csvRow mm um =
      let u = umUnit um
          l = umLines um in
      intercalate "," (
        [mmName mm, maybe "" id (unitClass u), unitName u, kindLabel (unitKind u)]
        ++ map show
          [ unitFirstLine u, unitLastLine u
          , countTotal l, countCode l, countComment l, countBlank l, countEolComment l, countDoc l
          , unitStatements u, unitLsloc u, unitStatementComplexity u, unitExpressionComplexity u, unitCyclomatic u
          , unitNesting u, unitParameters u, length (umCalls um), portCalls um, length (umCallers um) ])

----------------------------------------
-- JSON

linesJson :: LineCounts -> Value
linesJson l =
  object
    [ "total" .= countTotal l, "code" .= countCode l, "comment" .= countComment l
    , "blank" .= countBlank l, "eol_comment" .= countEolComment l, "doc" .= countDoc l ]

-- | The maximum and the mean of a figure over some units.
spread :: (Unit -> Int) -> [UnitMetrics] -> Value
spread f units =
  case map (f . umUnit) units of
    [] -> object [ "max" .= (0 :: Int), "mean" .= (0 :: Double) ]
    xs -> object [ "max" .= maximum xs, "mean" .= (fromIntegral (round (100 * mean xs) :: Int) / 100 :: Double) ]

  where

    mean xs = fromIntegral (sum xs) / fromIntegral (length xs) :: Double

unitJson :: UnitMetrics -> Value
unitJson um =
  let u = umUnit um in
  object
    [ "name" .= unitLabel u, "kind" .= kindLabel (unitKind u)
    , "first_line" .= unitFirstLine u, "last_line" .= unitLastLine u
    , "lines" .= linesJson (umLines um)
    , "statements" .= unitStatements u, "lsloc" .= unitLsloc u
    , "statement_complexity" .= unitStatementComplexity u
    , "expression_complexity" .= unitExpressionComplexity u
    , "cyclomatic" .= unitCyclomatic u, "max_nesting" .= unitNesting u
    , "parameters" .= unitParameters u
    , "calls" .=
        [ object [ "target" .= targetLabel t, "through_pointer" .= callThroughPointer c, "by_instances" .= S.toList (callInstances c) ]
        | (t, c) <- umCalls um ]
    , "port_calls" .= portCalls um
    , "callers" .= [ object [ "unit" .= keyLabel k, "by_instances" .= S.toList insts ] | (k, insts) <- umCallers um ] ]

moduleJson :: ModuleMetrics -> Value
moduleJson mm =
  object
    [ "name" .= mmName mm, "file" .= mmFile mm, "lines" .= linesJson (mmLines mm)
    , "lsloc" .= mmLsloc mm, "imports_out" .= mmImportsOut mm, "imports_in" .= mmImportsIn mm
    , "cyclomatic" .= spread unitCyclomatic (mmUnits mm), "max_nesting" .= spread unitNesting (mmUnits mm)
    , "units" .= map unitJson (mmUnits mm) ]

instanceJson :: Instance -> Value
instanceJson i =
  let p = instancePorts i in
  object
    [ "name" .= instanceName i, "class" .= instanceClass i
    , "kind" .= (case instanceKind i of { TaskInstance -> "task"; HandlerInstance -> "handler"; ResourceInstance -> "resource" } :: String)
    , "ports" .= object [ "in" .= portsIn p, "out" .= portsOut p, "sink" .= portsSink p, "access" .= portsAccess p ]
    , "accesses" .= instanceAccesses i, "sends_to" .= instanceSendsTo i
    , "receives_from" .= instanceReceivesFrom i, "emitters" .= instanceEmitters i
    , "users" .= instanceUsers i
    , "unit_calls" .= [ object [ "unit" .= name, "calls" .= n ] | (name, n) <- instanceCalls i ] ]

totalsJson :: ArchTotals -> Value
totalsJson t =
  object
    [ "tasks" .= totalTasks t, "handlers" .= totalHandlers t, "resources" .= totalResources t
    , "pools" .= totalPools t, "atomics" .= totalAtomics t, "deques" .= totalDeques t
    , "channels" .= totalChannels t, "emitters" .= totalEmitters t, "connections" .= totalConnections t ]

-- | Every figure of the project, with the keys in a fixed order so that the
-- file is stable between runs.
metricsJson :: ProjectMetrics -> BL.ByteString
metricsJson pm = encodePretty' defConfig { confCompare = keyOrder keys } value

  where

    units = allUnits pm
    value =
      object
        [ "rules" .= rulesVersion
        , "project" .= object
            [ "lines" .= linesJson (mconcat (map mmLines (pmModules pm)))
            , "lsloc" .= sum (map mmLsloc (pmModules pm))
            , "modules" .= length (pmModules pm)
            , "units" .= length units
            , "cyclomatic" .= spread unitCyclomatic units
            , "max_nesting" .= spread unitNesting units
            , "architecture" .= object [ "totals" .= totalsJson (pmTotals pm), "instances" .= map instanceJson (pmInstances pm) ] ]
        , "modules" .= map moduleJson (pmModules pm) ]

    keys :: [T.Text]
    keys =
      [ "rules", "project", "name", "file", "class", "kind", "first_line", "last_line", "lines"
      , "total", "code", "comment", "blank", "eol_comment", "doc"
      , "statements", "lsloc", "statement_complexity", "expression_complexity", "cyclomatic", "max_nesting"
      , "max", "mean", "parameters", "unit", "calls", "target", "through_pointer", "by_instances", "port_calls", "callers"
      , "imports_out", "imports_in", "modules", "units", "architecture", "totals", "instances"
      , "tasks", "handlers", "resources", "pools", "atomics", "deques", "channels"
      , "ports", "in", "out", "sink", "access", "accesses", "sends_to", "receives_from", "emitters"
      , "connections", "users", "unit_calls" ]
