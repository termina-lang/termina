-- | The verdict of each case of a test from its record, and the report of the
-- console.
module Runner.Analysis (
    Verdict(..), CaseResult(..), TestResult(..), Run(..),
    FailureDetail(..), Shown(..), ExceptionDetail(..), AddressKind(..),
    analyse, addressesOf, failureDetail, exceptionDetail, caseDetailLines,
    renderReport, exitCodeOf
) where

import Runner.Descriptor
import Runner.Record
import Runner.Symbols (Location(..))

import Data.Char (chr)
import Data.List (dropWhileEnd, intercalate)
import qualified Data.Map.Strict as M
import Numeric (showHex)
import System.FilePath (takeFileName)

-- | Unrecorded is a case whose records were lost when the record overflowed
data Verdict = Passed | Failed | Raised | TimedOut | NotRun | Unrecorded
  deriving (Eq, Show)

data CaseResult = CaseResult {
    resultCase :: DescriptorCase,
    resultVerdict :: Verdict,
    resultChecks :: Maybe Integer, -- ^ checks run, from the end of the case
    resultFailures :: [(String, Check)],
    resultException :: Maybe ExceptionInfo, -- ^ nothing when the process ended without a record of it
    resultLastStep :: Maybe Integer, -- ^ the last step of a case that timed out
    resultBacktrace :: [String] -- ^ the frames of a case with an exception, when GDB gave them
  } deriving (Eq, Show)

data TestResult = TestResult {
    testLabel :: String,
    testInvalid :: Maybe String, -- ^ why the record cannot be read, if it cannot
    testCases :: [CaseResult],
    testOverflow :: Maybe (Integer, Integer) -- ^ cases ended and checks failed in all, when the record overflowed
  } deriving (Eq, Show)

-- | One run of the binary: the cases it runs, its records, whether the runner
-- killed it at the timeout, whether its record overflowed, its last step and
-- the frames where it stopped at an exception
data Run = Run {
    runCases :: [DescriptorCase],
    runRecords :: [Record],
    runKilled :: Bool,
    runOverflow :: Bool,
    runLastStep :: Integer,
    runBacktrace :: [String]
  } deriving (Eq, Show)

-- | The cases of a run crossed with its records
analyse :: Run -> [CaseResult]
analyse run =
    let ended = walk Nothing (runRecords run) M.empty
        missing c = CaseResult c (if runOverflow run then Unrecorded else NotRun) Nothing [] Nothing Nothing []
    in [ M.findWithDefault (missing c) (caseNumber c) ended | c <- runCases run ]

  where

    caseOf n = [ c | c <- runCases run, caseNumber c == n ]

    -- | The verdict of the case that was running when the records end
    interrupted =
        if runKilled run then TimedOut
        else if runOverflow run then Unrecorded
        else Raised

    close n verdict checks failures exception results =
        let lastStep = if verdict == TimedOut then Just (runLastStep run) else Nothing
            frames = if verdict == Raised then runBacktrace run else []
        in case caseOf n of
            (c : _) -> M.insert n (CaseResult c verdict checks (reverse failures) exception lastStep frames) results
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
            ([], Just (n, failures)) -> close n interrupted Nothing failures Nothing results
            ([], Nothing) -> results

-- | What an address of a record points to
data AddressKind =
    ReturnAddress -- ^ after the call that recorded it
    | FaultPc -- ^ the instruction that failed
  deriving (Eq, Show)

-- | The addresses of the failures and of the exceptions, to translate them at
-- once; the one of a processor fault is its program counter, and every other
-- one is the return address of a function of the OSAL
addressesOf :: [CaseResult] -> [(Integer, AddressKind)]
addressesOf results =
    [ (checkWhere check, ReturnAddress) | r <- results, (_, check) <- resultFailures r ]
    ++ [ (address, if exceptionVariant info == "EProcessorFault" then FaultPc else ReturnAddress)
       | r <- results, Just info <- [resultException r], ("address", address) <- exceptionFields info, address /= 0 ]

-- | A failed check as the reports give it
data FailureDetail = FailureDetail {
    detailProcedure :: String,
    detailPlace :: Maybe Location,
    detailAddress :: Integer,
    detailText :: String, -- ^ the text of the check, or its procedure when the descriptor does not have it
    detailStep :: Integer,
    detailValues :: Maybe Shown
  } deriving (Eq, Show)

-- | The values of a failure as text
data Shown =
    ShownValues String String -- ^ expected and obtained
    | ShownElapsed String String -- ^ the time taken and the limit
  deriving (Eq, Show)

-- | An exception as the reports give it
data ExceptionDetail = ExceptionDetail {
    exceptionName :: String,
    exceptionPlace :: Maybe Location,
    exceptionAddress :: Maybe Integer,
    exceptionTaskName :: String,
    exceptionValues :: [(String, String)] -- ^ the fields but the address
  } deriving (Eq, Show)

-- | A failed check with its location, translated from its address, and its text, from the descriptor
failureDetail :: Descriptor -> M.Map Integer (Maybe Location) -> (String, Check) -> FailureDetail
failureDetail descriptor locations (procedure, check) =
    let place = M.findWithDefault Nothing (checkWhere check) locations
        known = place >>= checkAt
        -- | The file of the descriptor, with its folder, when the check is in it
        shown = case known of
            Just c -> Just (Location (checkFile c) (checkLine c))
            Nothing -> place
    in FailureDetail procedure shown (checkWhere check) (maybe procedure checkText known) (checkStep check) (valuesOf <$> checkValues check)

  where

    -- | The file matches by its name too, for a tool that gives it without its folder
    checkAt (Location file line) =
        case [ c | c <- descriptorChecks descriptor, checkLine c == line,
                   checkFile c == file || takeFileName (checkFile c) == takeFileName file ] of
            (c : _) -> Just c
            [] -> Nothing

    valuesOf values =
        case values of
            Scalars got expected -> ShownValues (valueText expected) (valueText got)
            TimeVals (gotSec, gotUsec) (maxSec, maxUsec) -> ShownElapsed (timeText gotSec gotUsec) (timeText maxSec maxUsec)

    valueText value =
        case procedure of
            "check_eq_bool" -> if value == 0 then "false" else "true"
            "check_eq_char" -> show (chr (fromIntegral value))
            _ -> show value

    timeText sec usec = show sec ++ " s " ++ show usec ++ " us"

-- | An exception with the name of its task and the location of its address
exceptionDetail :: Descriptor -> M.Map Integer (Maybe Location) -> ExceptionInfo -> ExceptionDetail
exceptionDetail descriptor locations info =
    let task = M.findWithDefault (show (exceptionTask info)) (exceptionTask info) (descriptorTasks descriptor)
        address = case [ value | ("address", value) <- exceptionFields info, value /= 0 ] of
            (a : _) -> Just a
            [] -> Nothing
        place = address >>= \a -> M.findWithDefault Nothing a locations
    in ExceptionDetail (exceptionVariant info) place address task
         [ (field, fieldText field value) | (field, value) <- exceptionFields info, field /= "address" ]

  where

    fieldText field value =
        if field `elem` ["queue", "task", "operation", "error", "size", "index", "lower", "upper", "expected", "width", "amount", "source", "exception"]
            then show value else "0x" ++ showHex value ""

-- | The lines that explain a case that did not pass, without indentation
caseDetailLines :: Descriptor -> M.Map Integer (Maybe Location) -> CaseResult -> [String]
caseDetailLines descriptor locations r =
    let failures = map (failureLine . failureDetail descriptor locations) (resultFailures r) in
    case resultVerdict r of
        Passed -> [ "warning: the case ran no checks" | resultChecks r == Just 0 ]
        Failed -> failures
        Raised -> failures ++ [exceptionLine (exceptionDetail descriptor locations <$> resultException r)]
                  ++ (if null (resultBacktrace r) then [] else "backtrace:" : map ("  " ++) (resultBacktrace r))
        TimedOut -> failures ++ ["the case did not end before the timeout; last step " ++ maybe "0" show (resultLastStep r)]
        NotRun -> []
        Unrecorded -> failures ++ ["the record overflowed before the end of the case"]

  where

    failureLine detail =
        let step = if detailStep detail /= 0 then "  step " ++ show (detailStep detail) else "" in
        placeText (detailPlace detail) (detailAddress detail) ++ "  " ++ detailText detail ++ valuesText (detailValues detail) ++ step

    valuesText values =
        case values of
            Nothing -> ""
            Just (ShownValues expected got) -> "  expected " ++ expected ++ ", got " ++ got
            Just (ShownElapsed took limit) -> "  took " ++ took ++ ", more than " ++ limit

    exceptionLine exception =
        case exception of
            Nothing -> "the process ended in the middle of the case, without a record of an exception"
            Just detail ->
                let place = maybe "" (\a -> " at " ++ placeText (exceptionPlace detail) a) (exceptionAddress detail)
                    fields = [ field ++ " " ++ value | (field, value) <- exceptionValues detail ]
                in exceptionName detail ++ place ++ " in the task " ++ exceptionTaskName detail
                   ++ (if null fields then "" else ": " ++ intercalate ", " fields)

    placeText place address =
        case place of
            Just (Location file line) -> file ++ ":" ++ show line
            Nothing -> "0x" ++ showHex address ""

-- | The report of the console, with the location of each address and the text of each check of the descriptor
renderReport :: Descriptor -> M.Map Integer (Maybe Location) -> TestResult -> [String]
renderReport descriptor locations result =
    case testInvalid result of
        Just reason -> [testLabel result, "  invalid: " ++ reason]
        Nothing ->
            [testLabel result]
            ++ concatMap caseLines (testCases result)
            ++ maybe [] overflowLine (testOverflow result)
            ++ ["  " ++ summary (testCases result)]

  where

    width = maximum (0 : [ length (caseId (resultCase r)) | r <- testCases result ])

    pad text = text ++ replicate (width - length text) ' '

    caseLines r =
        let name = pad (caseId (resultCase r))
            first = case resultVerdict r of
                Passed -> "  ok       " ++ name ++ "    " ++ checksText (resultChecks r)
                Failed -> "  FAIL     " ++ name ++ "    " ++ checksText (resultChecks r)
                Raised -> "  EXCEPT   " ++ name
                TimedOut -> "  TIMEOUT  " ++ name
                NotRun -> "  --       " ++ name ++ "    not run"
                Unrecorded -> "  ??       " ++ name ++ "    without a record"
        in dropWhileEnd (== ' ') first : map ("           " ++) (caseDetailLines descriptor locations r)

    checksText checks =
        case checks of
            Just 1 -> "1 check"
            Just n -> show n ++ " checks"
            Nothing -> ""

    overflowLine (ended, failed) =
        [ "  the record overflowed: " ++ show ended ++ " cases ended and " ++ show failed
          ++ " checks failed in all, and the failures beyond it are not detailed" ]

    summary results =
        let count verdict = length [ r | r <- results, resultVerdict r == verdict ]
            parts = [ show (count Passed) ++ " passed" ]
                ++ [ show (count Failed) ++ " failed" | count Failed > 0 ]
                ++ [ show (count Raised) ++ " with an exception" | count Raised > 0 ]
                ++ [ show (count TimedOut) ++ " timed out" | count TimedOut > 0 ]
                ++ [ show (count NotRun) ++ " not run" | count NotRun > 0 ]
                ++ [ show (count Unrecorded) ++ " without a record" | count Unrecorded > 0 ]
        in intercalate ", " parts

-- | 0 when every case passes, 1 when one does not, 2 when the test cannot be read
exitCodeOf :: TestResult -> Int
exitCodeOf result =
    case testInvalid result of
        Just _ -> 2
        Nothing -> if all ((== Passed) . resultVerdict) (testCases result) then 0 else 1
