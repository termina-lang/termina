-- | The structural measures of the units of a module.
module Metrics.UnitSpec (spec) where

import qualified Data.Text as T

import Metrics.Unit
import Pipeline.Common (Failure(..), runTypedModule)

import Test.Hspec

failed :: Failure -> Either String b
failed f = Left (T.unpack (failCode f) ++ ": " ++ T.unpack (failMessage f))

-- | The units of a module, or the error that stops it.
unitsIn :: String -> Either String [Unit]
unitsIn source = either failed (Right . unitsOf source) (runTypedModule "test" [("test", source)])

-- | The logical size of a module.
lslocOf :: String -> Either String Int
lslocOf source =
  either failed (\program -> Right (moduleLsloc program (unitsOf source program))) (runTypedModule "test" [("test", source)])

-- | Statements, lsloc, statement and expression complexity, cyclomatic
-- complexity, nesting and parameters of a unit.
figures :: Unit -> (Int, Int, Int, Int, Int, Int, Int)
figures u =
  ( unitStatements u, unitLsloc u, unitStatementComplexity u, unitExpressionComplexity u
  , unitCyclomatic u, unitNesting u, unitParameters u )

-- | The figures of the only unit of a module.
only :: String -> Either String (Int, Int, Int, Int, Int, Int, Int)
only source = fmap (map figures) (unitsIn source) >>= single

  where

    single fs =
      case fs of
        [f] -> Right f
        _ -> Left ("expected one unit, got " ++ show (length fs))

function :: [String] -> String
function body = unlines (["function f(x : u32, y : u32) -> u32 {", "    var r : u32 = 0;"] ++ map ("    " ++) body ++ ["    return r;", "}"])

spec :: Spec
spec = do
  describe "Cyclomatic complexity" $ do
    it "A unit without decisions has complexity 1" $
      only (function []) `shouldBe` Right (2, 3, 0, 0, 1, 0, 2)
    it "An if adds one and its else adds nothing" $
      only (function ["if (x > 0) {", "    r = 1;", "} else {", "    r = 2;", "}"])
        `shouldBe` Right (5, 6, 1, 0, 2, 1, 2)
    it "Each else if adds one" $
      only (function ["if (x > 0) {", "    r = 1;", "} else if (x > 1) {", "    r = 2;", "} else if (x > 2) {", "    r = 3;", "} else {", "    r = 4;", "}"])
        `shouldBe` Right (7, 8, 3, 0, 4, 1, 2)
    it "A for adds one and its while clause one more" $
      only (function ["for i : usize in 0 .. 4 while (r < y) {", "    r = r + x;", "}"])
        `shouldBe` Right (4, 5, 2, 0, 3, 1, 2)
    it "Each && and || adds one" $
      only (function ["if (x > 0 && y > 0 || x == y) {", "    r = 1;", "}"])
        `shouldBe` Right (4, 5, 1, 2, 4, 1, 2)
    it "Boolean operators outside conditions also count" $
      only (unlines ["function f(x : bool, y : bool) -> bool {", "    let r : bool = x && y;", "    return r;", "}"])
        `shouldBe` Right (2, 3, 0, 1, 2, 0, 2)
    it "A match adds one per case but the last" $
      only (unlines
        [ "enum E { A, B, C };"
        , "function f(e : E) -> u32 {"
        , "    var r : u32 = 0;"
        , "    match e {"
        , "        case A => { r = 1; }"
        , "        case B => { r = 2; }"
        , "        case C => { r = 3; }"
        , "    }"
        , "    return r;"
        , "}" ])
        `shouldBe` Right (6, 7, 2, 0, 3, 1, 1)
    it "A match with a default adds one per named case" $
      only (unlines
        [ "enum E { A, B, C };"
        , "function f(e : E) -> u32 {"
        , "    var r : u32 = 0;"
        , "    match e {"
        , "        case A => { r = 1; }"
        , "        case _ => { r = 3; }"
        , "    }"
        , "    return r;"
        , "}" ])
        `shouldBe` Right (5, 6, 1, 0, 2, 1, 1)

  describe "Nesting" $ do
    it "An else if does not add a level" $
      fmap (map unitNesting) (unitsIn (function ["if (x > 0) {", "    r = 1;", "} else if (x > 1) {", "    if (y > 1) {", "        r = 2;", "    }", "} else {", "    r = 3;", "}"]))
        `shouldBe` Right [2]
    it "Consecutive statements do not add levels" $
      fmap (map unitNesting) (unitsIn (function ["if (x > 0) {", "    r = 1;", "}", "for i : usize in 0 .. 4 {", "    r = r + 1;", "}"]))
        `shouldBe` Right [1]

  describe "The example module" $ do
    it "Measures each unit" $ do
      source <- readFile "test/Metrics/examples/sensors.fin"
      fmap (map (\u -> (unitClass u, unitName u, unitKind u, unitFirstLine u, unitLastLine u, figures u))) (unitsIn source)
        `shouldBe` Right
          [ (Nothing, "mean", FunctionUnit, 15, 27, (7, 8, 3, 0, 4, 1, 2))
          , (Just "CSensor", "update", ProcedureUnit, 36, 52, (7, 8, 3, 2, 6, 2, 1))
          ]
    it "Measures the logical size of the module" $ do
      source <- readFile "test/Metrics/examples/sensors.fin"
      lslocOf source `shouldBe` Right 23
