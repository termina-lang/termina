-- | The verdict of each case of a test from its record, and the report of the
-- console.
module Runner.Analysis (
    Verdict(..), CaseResult(..), TestResult(..),
    analyse, addressesOf, renderReport, exitCodeOf
) where

import Runner.Descriptor
import Runner.Record
import Runner.Symbols (Location(..))

import Data.Char (chr)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Numeric (showHex)
import System.FilePath (takeFileName)

data Verdict = Passed | Failed | Raised | TimedOut | NotRun
  deriving (Eq, Show)

data CaseResult = CaseResult {
    resultCase :: DescriptorCase,
    resultVerdict :: Verdict,
    resultChecks :: Maybe Integer, -- ^ checks run, from the end of the case
    resultFailures :: [(String, Check)],
    resultException :: Maybe ExceptionInfo -- ^ nothing when the process ended without a record of it
  } deriving (Eq, Show)

data TestResult = TestResult {
    testLabel :: String,
    testInvalid :: Maybe String, -- ^ why the record cannot be read, if it cannot
    testCases :: [CaseResult],
    testOverflow :: Bool,
    testLastStep :: Integer
  } deriving (Eq, Show)

-- | The cases of the descriptor crossed with the records; the process was killed at the timeout when killed is true
analyse :: Descriptor -> [Record] -> Bool -> [CaseResult]
analyse descriptor records killed =
    let ended = walk Nothing records M.empty in
    [ M.findWithDefault (CaseResult c NotRun Nothing [] Nothing) (caseNumber c) ended | c <- descriptorCases descriptor ]

  where

    caseOf n = [ c | c <- descriptorCases descriptor, caseNumber c == n ]

    close n verdict checks failures exception results =
        case caseOf n of
            (c : _) -> M.insert n (CaseResult c verdict checks (reverse failures) exception) results
            [] -> results

    -- | The case begun and its failures, in reverse order
    walk :: Maybe (Integer, [(String, Check)]) -> [Record] -> M.Map Integer CaseResult -> M.Map Integer CaseResult
    walk current rest results =
        case (rest, current) of
            (CaseBegin n : more, _) -> walk (Just (n, [])) more results
            (Failure procedure check : more, Just (n, failures)) -> walk (Just (n, (procedure, check) : failures)) more results
            (CaseEnd checks : more, Just (n, failures)) ->
                walk Nothing more (close n (if null failures then Passed else Failed) (Just checks) failures Nothing results)
            (Exception info : _, Just (n, failures)) -> close n Raised Nothing failures (Just info) results
            (End : _, _) -> results
            (Finish : _, _) -> results
            (_ : more, _) -> walk current more results
            ([], Just (n, failures)) -> close n (if killed then TimedOut else Raised) Nothing failures Nothing results
            ([], Nothing) -> results

-- | The return addresses of the failures and the addresses of the exceptions, to translate them at once
addressesOf :: [CaseResult] -> [Integer]
addressesOf results =
    [ checkWhere check | r <- results, (_, check) <- resultFailures r ]
    ++ [ address | r <- results, Just info <- [resultException r], ("address", address) <- exceptionFields info, address /= 0 ]

-- | The report of the console, with the location of each address and the text of each check of the descriptor
renderReport :: Descriptor -> M.Map Integer (Maybe Location) -> TestResult -> [String]
renderReport descriptor locations result =
    case testInvalid result of
        Just reason -> [testLabel result, "  invalid: " ++ reason]
        Nothing ->
            [testLabel result]
            ++ concatMap caseLines (testCases result)
            ++ [ "  the record overflowed: the failures beyond it are counted but not detailed" | testOverflow result ]
            ++ ["  " ++ summary (testCases result)]

  where

    width = maximum (0 : [ length (caseId (resultCase r)) | r <- testCases result ])

    pad text = text ++ replicate (width - length text) ' '

    caseLines r =
        let name = pad (caseId (resultCase r)) in
        case resultVerdict r of
            Passed -> [ "  ok       " ++ name ++ "    " ++ checksText (resultChecks r) ]
                      ++ [ "           warning: the case ran no checks" | resultChecks r == Just 0 ]
            Failed -> ("  FAIL     " ++ name ++ "    " ++ checksText (resultChecks r)) : map failureLine (resultFailures r)
            Raised -> ("  EXCEPT   " ++ name) : map failureLine (resultFailures r) ++ [exceptionLine (resultException r)]
            TimedOut -> ("  TIMEOUT  " ++ name) : map failureLine (resultFailures r)
                        ++ ["           the case did not end before the timeout; last step " ++ show (testLastStep result)]
            NotRun -> ["  --       " ++ name ++ "    not run"]

    checksText checks =
        case checks of
            Just 1 -> "1 check"
            Just n -> show n ++ " checks"
            Nothing -> ""

    failureLine (procedure, check) =
        let place = locate (checkWhere check)
            known = place >>= checkAt
            -- | The file of the descriptor, with its folder, when the check is in it
            shown = case known of
                Just c -> Just (Location (checkFile c) (checkLine c))
                Nothing -> place
            text = maybe procedure checkText known
            step = if checkStep check /= 0 then "  step " ++ show (checkStep check) else ""
        in "           " ++ placeText shown (checkWhere check) ++ "  " ++ text ++ valuesText procedure (checkValues check) ++ step

    exceptionLine exception =
        case exception of
            Nothing -> "           the process ended in the middle of the case, without a record of an exception"
            Just info ->
                let task = M.findWithDefault (show (exceptionTask info)) (exceptionTask info) (descriptorTasks descriptor)
                    fields = [ field ++ " " ++ fieldText field value | (field, value) <- exceptionFields info, field /= "address" ]
                    address = [ value | ("address", value) <- exceptionFields info, value /= 0 ]
                    place = case address of
                        (a : _) -> " at " ++ placeText (locate a) a
                        [] -> ""
                in "           " ++ exceptionVariant info ++ place ++ " in the task " ++ task
                   ++ (if null fields then "" else ": " ++ intercalate ", " fields)

    fieldText field value =
        if field `elem` ["queue", "task", "operation", "error", "size", "index", "lower", "upper", "expected", "width", "amount", "source", "exception"]
            then show value else "0x" ++ showHex value ""

    locate address = M.findWithDefault Nothing address locations

    placeText place address =
        case place of
            Just (Location file line) -> file ++ ":" ++ show line
            Nothing -> "0x" ++ showHex address ""

    -- | atos gives the name of the file without its folder, so the file matches by its name
    checkAt (Location file line) =
        case [ c | c <- descriptorChecks descriptor, checkLine c == line,
                   checkFile c == file || takeFileName (checkFile c) == takeFileName file ] of
            (c : _) -> Just c
            [] -> Nothing

    valuesText procedure values =
        case values of
            Nothing -> ""
            Just (Scalars got expected) -> "  expected " ++ valueText procedure expected ++ ", got " ++ valueText procedure got
            Just (TimeVals (gotSec, gotUsec) (maxSec, maxUsec)) ->
                "  took " ++ timeText gotSec gotUsec ++ ", more than " ++ timeText maxSec maxUsec

    valueText procedure value =
        case procedure of
            "check_eq_bool" -> if value == 0 then "false" else "true"
            "check_eq_char" -> show (chr (fromIntegral value))
            _ -> show value

    timeText sec usec = show sec ++ " s " ++ show usec ++ " us"

    summary results =
        let count verdict = length [ r | r <- results, resultVerdict r == verdict ]
            parts = [ show (count Passed) ++ " passed" ]
                ++ [ show (count Failed) ++ " failed" | count Failed > 0 ]
                ++ [ show (count Raised) ++ " with an exception" | count Raised > 0 ]
                ++ [ show (count TimedOut) ++ " timed out" | count TimedOut > 0 ]
                ++ [ show (count NotRun) ++ " not run" | count NotRun > 0 ]
        in intercalate ", " parts

-- | 0 when every case passes, 1 when one does not, 2 when the test cannot be read
exitCodeOf :: TestResult -> Int
exitCodeOf result =
    case testInvalid result of
        Just _ -> 2
        Nothing -> if all ((== Passed) . resultVerdict) (testCases result) then 0 else 1
