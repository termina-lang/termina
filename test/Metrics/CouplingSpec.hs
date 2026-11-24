-- | The coupling of a program: imports, calls and wiring.
module Metrics.CouplingSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Metrics.Coupling
import ControlFlow.Architecture.Types (TerminaProgArch)
import Lowering.AST (AnnotatedProgram)
import Pipeline.Common (Failure(..), runProjectArchitecture)
import Semantic.Types (SemanticAnn)

import Test.Hspec

-- | The architecture of the example application, or the error that stops it.
exampleApp :: IO (Either String (TerminaProgArch SemanticAnn, M.Map String (AnnotatedProgram SemanticAnn)))
exampleApp = do
  sources <- mapM (\m -> (,) ("example/" ++ m) <$> readFile ("test/Metrics/examples/" ++ m ++ ".fin")) ["sensors", "control", "app"]
  return (either (\f -> Left (T.unpack (failCode f) ++ ": " ++ T.unpack (failMessage f))) Right
    (runProjectArchitecture sources))

-- | The targets of each unit, with the mark of the calls through a function
-- pointer and the instances that make them.
calls :: (TerminaProgArch SemanticAnn, M.Map String (AnnotatedProgram SemanticAnn)) -> M.Map UnitKey [(Target, Bool, [String])]
calls = M.map (\cs -> [ (t, callThroughPointer c, S.toList (callInstances c)) | (t, c) <- M.toList cs ]) . allCalls

allCalls :: (TerminaProgArch SemanticAnn, M.Map String (AnnotatedProgram SemanticAnn)) -> M.Map UnitKey CallSet
allCalls (arch, lowered) = unitCalls (Just arch) (M.elems lowered)

spec :: Spec
spec = do
  describe "Calls of the example application" $ do
    it "Resolves a procedure through the connections of each instance" $ do
      result <- exampleApp
      fmap (M.lookup (Just "CControl", "sample") . calls) result `shouldBe` Right (Just
        [ (UnitTarget Nothing "filter", False, ["ctrl_a", "ctrl_b"])
        , (UnitTarget (Just "CFakeSensor") "update", True, ["ctrl_b"])
        , (UnitTarget (Just "CSensor") "update", True, ["ctrl_a"])
        , (OperationTarget "send", False, ["ctrl_a", "ctrl_b"])
        ])
    it "Sends a call through a port to the interface when there is no architecture" $ do
      result <- exampleApp
      fmap (\(_, lowered) -> M.lookup (Just "CControl", "sample")
                (M.map (map (\(t, c) -> (t, callThroughPointer c, S.toList (callInstances c))) . M.toList)
                  (unitCalls Nothing (M.elems lowered)))) result
        `shouldBe` Right (Just
          [ (UnitTarget Nothing "filter", False, [])
          , (OperationTarget "ISensor::update", True, [])
          , (OperationTarget "send", False, [])
          ])
    it "Counts a call to a function from a function, which no instance makes" $ do
      result <- exampleApp
      fmap (M.lookup (Nothing, "filter") . calls) result
        `shouldBe` Right (Just [(UnitTarget Nothing "scale", False, [])])
    it "Counts the callers of each unit with the instances that call" $ do
      result <- exampleApp
      fmap (M.map (map (fmap S.toList) . M.toList) . unitCallers . allCalls) result `shouldBe` Right (M.fromList
        [ ((Nothing, "filter"), [((Just "CControl", "sample"), ["ctrl_a", "ctrl_b"])])
        , ((Nothing, "scale"), [((Nothing, "filter"), [])])
        , ((Just "CFakeSensor", "update"), [((Just "CControl", "sample"), ["ctrl_b"])])
        , ((Just "CSensor", "update"), [((Just "CControl", "sample"), ["ctrl_a"])])
        ])

  describe "Wiring of the example application" $ do
    it "Describes each instance" $ do
      result <- exampleApp
      fmap (\r -> instancesOf (fst r) (allCalls r)) result `shouldBe` Right
        [ Instance "ctrl_a" "CControl" TaskInstance (PortCounts 0 1 1 1) ["real"] ["data_a"] [] ["timer_a"] 0 [("sample", 3)]
        , Instance "ctrl_b" "CControl" TaskInstance (PortCounts 0 1 1 1) ["fake"] ["data_b"] [] ["timer_b"] 0 [("sample", 3)]
        , Instance "log_a" "CLogger" TaskInstance (PortCounts 1 0 0 0) [] [] ["data_a"] [] 0 [("log", 0)]
        , Instance "log_b" "CLogger" TaskInstance (PortCounts 1 0 0 0) [] [] ["data_b"] [] 0 [("log", 0)]
        , Instance "fake" "CFakeSensor" ResourceInstance (PortCounts 0 0 0 0) [] [] [] [] 1 [("update", 0)]
        , Instance "real" "CSensor" ResourceInstance (PortCounts 0 0 0 0) [] [] [] [] 1 [("update", 0)]
        ]
    it "Adds up the totals" $ do
      result <- exampleApp
      fmap (archTotals . fst) result `shouldBe` Right (ArchTotals 4 0 2 0 0 0 2 2 8)

  describe "Imports between modules" $
    it "Counts the modules imported and the modules that import" $
      moduleImports (M.fromList [("app", ["sensors", "control"]), ("control", ["sensors"]), ("sensors", [])])
        `shouldBe` M.fromList
          [ ("app", (["control", "sensors"], []))
          , ("control", (["sensors"], ["app"]))
          , ("sensors", ([], ["app", "control"])) ]
