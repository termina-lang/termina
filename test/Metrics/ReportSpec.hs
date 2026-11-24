-- | The renderings of the metrics of the example application.
module Metrics.ReportSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

import Metrics.Coupling
import Metrics.Html
import Metrics.Report
import Utils.HtmlReport (Provenance(..))
import Pipeline.Common (Failure(..), runProjectArchitecture, runTypedModule)
import Golden (goldenMetrics)

import Test.Hspec

modules :: [String]
modules = ["sensors", "control", "app"]

-- | The metrics of the example application, or the error that stops it.
exampleMetrics :: IO (Either String ProjectMetrics)
exampleMetrics = do
  texts <- mapM (\m -> readFile ("test/Metrics/examples/" ++ m ++ ".fin")) modules
  let sources = [ ("example/" ++ m, t) | (m, t) <- zip modules texts ]
      failed f = T.unpack (failCode f) ++ ": " ++ T.unpack (failMessage f)
      imports = moduleImports (M.fromList
        [ ("example.sensors", []), ("example.control", ["example.sensors"])
        , ("example.app", ["example.sensors", "example.control"]) ])
  return $ either (Left . failed) Right $ do
    (arch, lowered) <- runProjectArchitecture sources
    let calls = unitCalls (Just arch) (M.elems lowered)
        callers = unitCallers calls
    measured <- mapM (\(m, t) ->
      (\program -> moduleMetrics ("example." ++ m) ("src/example/" ++ m ++ ".fin") t program calls callers
         (M.findWithDefault ([], []) ("example." ++ m) imports))
        <$> runTypedModule ("example/" ++ m) sources) (zip modules texts)
    return (ProjectMetrics (M.elems (M.fromList [ (mmName mm, mm) | mm <- measured ])) (instancesOf arch calls) (archTotals arch))

rendered :: (ProjectMetrics -> T.Text) -> IO T.Text
rendered render = either (T.pack . ("error: " ++)) render <$> exampleMetrics

spec :: Spec
spec = describe "Reports of the example application" $ do
  it "Prints the text table" $
    rendered (T.pack . textReport) >>= goldenMetrics "txt" "example"
  it "Writes the CSV of the units" $
    rendered (T.pack . unitsCsv) >>= goldenMetrics "csv" "example_units"
  it "Writes the JSON" $
    rendered (TE.decodeUtf8 . BL.toStrict . metricsJson) >>= goldenMetrics "json" "example_metrics"
  it "Writes the page" $
    rendered (T.pack . reportPage provenance) >>= goldenMetrics "html" "example_index"

  where

    provenance = Provenance "example 0.1.0" "0.0.0" "2026-01-01 00:00"
