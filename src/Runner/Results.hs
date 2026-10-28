{-# LANGUAGE OverloadedStrings #-}

-- | The files of the result of a test: results.json, which ties the verdicts
-- to the binary that produced them, and report.xml, in JUnit XML.
module Runner.Results (
    Evidence(..), Provenance(..), readProvenance,
    resultsJson, renderResults, renderJUnit
) where

import Runner.Analysis
import Runner.Descriptor
import Runner.Symbols (Location(..))

import Data.Aeson
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString.Lazy as BL
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.String (fromString)
import qualified Data.Text as T
import Numeric (showHex)

-- | What ties a result to the build and the run that produced it
data Evidence = Evidence {
    evidenceName :: String,
    evidencePlatform :: String,
    evidenceHost :: String,
    evidenceProfile :: String,
    evidenceMode :: String, -- ^ all, case or isolated
    evidenceExecutor :: String, -- ^ process or gdb
    evidenceDebugger :: Maybe String, -- ^ the first line of --version of the GDB that ran the test
    evidenceTimeout :: Maybe Integer,
    evidenceCommit :: Maybe (String, Bool), -- ^ the commit of the project and whether its tree has changes
    evidenceBinarySha :: Maybe String,
    evidenceProvenance :: Maybe Provenance
  } deriving (Eq, Show)

-- | The hashes of the build without debugging information, from provenance.json
data Provenance = Provenance {
    provenanceBinary :: Maybe T.Text,
    provenanceModules :: [Value]
  } deriving (Eq, Show)

instance FromJSON Provenance where
  parseJSON = withObject "provenance" $ \o -> do
    binary <- o .: "binary"
    Provenance <$> withObject "binary" (.:? "stripped-sha256") binary <*> o .: "modules"

readProvenance :: FilePath -> IO (Maybe Provenance)
readProvenance path = either (const Nothing) Just . eitherDecode <$> BL.readFile path

verdictText :: Verdict -> T.Text
verdictText verdict =
    case verdict of
        Passed -> "passed"
        Failed -> "failed"
        Raised -> "exception"
        TimedOut -> "timeout"
        Interrupted -> "interrupted"
        NotRun -> "not run"
        Unrecorded -> "unrecorded"

-- | The verdict of a group of cases: it passes when all of them pass
groupVerdict :: [CaseResult] -> T.Text
groupVerdict results = if all ((== Passed) . resultVerdict) results then "passed" else "failed"

resultsJson :: Evidence -> Maybe Descriptor -> M.Map Integer (Maybe Location) -> TestResult -> Value
resultsJson evidence mDescriptor locations result =
    object [
        "version" .= (1 :: Int),
        "name" .= evidenceName evidence,
        "level" .= fmap descriptorLevel mDescriptor,
        "platform" .= evidencePlatform evidence,
        "host" .= evidenceHost evidence,
        "profile" .= evidenceProfile evidence,
        "verdict" .= maybe (groupVerdict (testCases result)) (const "invalid") (testInvalid result),
        "invalid" .= testInvalid result,
        "stamp" .= fmap (\d -> "0x" ++ showHex (descriptorStamp d) "") mDescriptor,
        "commit" .= fmap (\(sha, dirty) -> object ["sha" .= sha, "dirty" .= dirty]) (evidenceCommit evidence),
        "binary" .= object [
            "path" .= ("bin/" ++ evidenceName evidence),
            "sha256" .= evidenceBinarySha evidence,
            "stripped-sha256" .= (evidenceProvenance evidence >>= provenanceBinary)
          ],
        "modules" .= maybe [] provenanceModules (evidenceProvenance evidence),
        "run" .= object [
            "mode" .= evidenceMode evidence,
            "executor" .= evidenceExecutor evidence,
            "debugger" .= evidenceDebugger evidence,
            "timeout" .= evidenceTimeout evidence,
            "overflow" .= (testOverflow result /= Nothing)
          ],
        "cases" .= map caseJson (testCases result),
        "elapsed" .= ([] :: [Value]),
        "requirements" .= requirements
    ]

  where

    caseJson r =
        let c = resultCase r in
        object [
            "number" .= caseNumber c,
            "id" .= caseId c,
            "instance" .= caseInstance c,
            "case" .= caseName c,
            "file" .= caseFile c,
            "line" .= caseLine c,
            "verdict" .= verdictText (resultVerdict r),
            "checks" .= resultChecks r,
            "verifies" .= caseVerifies c,
            "failures" .= map failureJson (resultFailures r),
            "exception" .= fmap exceptionJson (resultException r),
            "last_step" .= resultLastStep r,
            "backtrace" .= resultBacktrace r
        ]

    failureJson failure =
        let detail = failureDetailOf failure
            (expected, obtained) = case detailValues detail of
                Just (ShownValues e o) -> (Just e, Just o)
                Just (ShownElapsed took limit) -> (Just limit, Just took)
                Nothing -> (Nothing, Nothing)
        in object [
            "procedure" .= detailProcedure detail,
            "file" .= fmap locationFile (detailPlace detail),
            "line" .= fmap locationLine (detailPlace detail),
            "text" .= detailText detail,
            "step" .= detailStep detail,
            "expected" .= expected,
            "obtained" .= obtained,
            "address" .= hex (detailAddress detail)
        ]

    exceptionJson info =
        let detail = exceptionDetailOf info in
        object [
            "variant" .= exceptionName detail,
            "task" .= exceptionTaskName detail,
            "file" .= fmap locationFile (exceptionPlace detail),
            "line" .= fmap locationLine (exceptionPlace detail),
            "address" .= fmap hex (exceptionAddress detail),
            "fields" .= object [ fromString field .= value | (field, value) <- exceptionValues detail ]
        ]

    failureDetailOf = failureDetail (descriptorOrEmpty mDescriptor) locations

    exceptionDetailOf = exceptionDetail (descriptorOrEmpty mDescriptor) locations

    requirements =
        let cases = testCases result
            ids = nub (concatMap (caseVerifies . resultCase) cases)
        in [ object [
                "id" .= req,
                "cases" .= [ caseId (resultCase r) | r <- verifying ],
                "verdict" .= groupVerdict verifying
              ]
           | req <- ids, let verifying = [ r | r <- cases, req `elem` caseVerifies (resultCase r) ] ]

    hex value = "0x" ++ showHex value ""

descriptorOrEmpty :: Maybe Descriptor -> Descriptor
descriptorOrEmpty = maybe (Descriptor 1 0 "" "unit" "" Nothing [] [] M.empty M.empty (Symbols "" "" "" "")) id

renderResults :: Value -> BL.ByteString
renderResults = encodePretty' defConfig { confCompare = keyOrder keys }

  where

    -- | One order for every level: the keys that two levels share are placed where both need them
    keys = [
        "version", "number", "id", "name", "instance", "case", "level", "platform", "host", "profile",
        "procedure", "variant", "task", "file", "line", "verdict", "invalid", "stamp", "commit", "sha", "dirty",
        "binary", "module", "origin", "source", "object", "path", "sha256", "stripped-sha256", "modules",
        "run", "mode", "executor", "debugger", "timeout", "overflow", "cases", "checks", "verifies", "failures",
        "text", "step", "expected", "obtained", "address", "exception", "last_step", "backtrace", "fields", "elapsed", "requirements"
      ]

-- | report.xml: the test is a testsuite and each case a testcase; a failed
-- check is a failure, an exception, a timeout or a case without a record an
-- error, and a case not run is skipped
renderJUnit :: Evidence -> Maybe Descriptor -> M.Map Integer (Maybe Location) -> TestResult -> String
renderJUnit evidence mDescriptor locations result =
    unlines $ ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<testsuites" ++ counts ++ ">",
               "  <testsuite name=\"" ++ escape (testLabel result) ++ "\"" ++ counts ++ ">",
               "    <properties>"]
      ++ [ "      <property name=\"" ++ key ++ "\" value=\"" ++ escape value ++ "\"/>" | (key, value) <- properties ]
      ++ ["    </properties>"]
      ++ body
      ++ ["  </testsuite>", "</testsuites>"]

  where

    descriptor = descriptorOrEmpty mDescriptor

    properties =
        [("platform", evidencePlatform evidence), ("profile", evidenceProfile evidence)]
        ++ [ ("stamp", "0x" ++ showHex (descriptorStamp d) "") | Just d <- [mDescriptor] ]

    body =
        case testInvalid result of
            Just reason -> [ "    <testcase classname=\"" ++ escape (evidenceName evidence) ++ "\" name=\"(run)\">",
                             "      <error type=\"invalid\" message=\"" ++ escape reason ++ "\"/>",
                             "    </testcase>" ]
            Nothing -> concatMap testcase (testCases result)

    tally verdicts = length [ r | r <- testCases result, resultVerdict r `elem` verdicts ]

    counts =
        case testInvalid result of
            Just _ -> " tests=\"1\" failures=\"0\" errors=\"1\" skipped=\"0\""
            Nothing -> " tests=\"" ++ show (length (testCases result)) ++ "\" failures=\"" ++ show (tally [Failed])
                       ++ "\" errors=\"" ++ show (tally [Raised, TimedOut, Interrupted, Unrecorded]) ++ "\" skipped=\"" ++ show (tally [NotRun]) ++ "\""

    testcase r =
        let c = resultCase r
            open = "    <testcase classname=\"" ++ escape (evidenceName evidence ++ "." ++ caseInstance c)
                   ++ "\" name=\"" ++ escape (caseName c) ++ "\" file=\"" ++ escape (caseFile c)
                   ++ "\" line=\"" ++ show (caseLine c) ++ "\""
            details = caseDetailLines descriptor locations r
            text = escape (unlines details)
            firstLine = case details of
                (l : _) -> l
                [] -> ""
            element tag kind = [ open ++ ">",
                                 "      <" ++ tag ++ " type=\"" ++ escape kind ++ "\" message=\"" ++ escape firstLine ++ "\">" ++ text ++ "</" ++ tag ++ ">",
                                 "    </testcase>" ]
        in case resultVerdict r of
            Passed -> [open ++ "/>"]
            Failed -> element "failure" (case resultFailures r of
                (f : _) -> fst f
                [] -> "check")
            Raised -> element "error" (maybe "exception" (exceptionName . exceptionDetail descriptor locations) (resultException r))
            TimedOut -> element "error" "timeout"
            Interrupted -> element "error" "interrupted"
            Unrecorded -> element "error" "unrecorded"
            NotRun -> [open ++ ">", "      <skipped/>", "    </testcase>"]

-- | The text of an attribute or of an element of XML; the control characters that XML 1.0 does not admit are dropped
escape :: String -> String
escape = concatMap char

  where

    char c =
        case c of
            '&' -> "&amp;"
            '<' -> "&lt;"
            '>' -> "&gt;"
            '"' -> "&quot;"
            '\'' -> "&apos;"
            '\n' -> "&#10;"
            _ -> if c < ' ' && c /= '\t' then "" else [c]
