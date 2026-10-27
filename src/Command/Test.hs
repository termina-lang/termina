{-# LANGUAGE OverloadedStrings #-}

-- | The tests of the project: it builds and runs them, or lists them with
-- their platforms and their cases, which it reads from the modules of each
-- test without type checking or building them.
module Command.Test (
    testCmdArgsParser, testCommand, TestCmdArgs,
    TestCase(..), testCases
) where

import Command.Build (BuildCmdArgs(..), Selection(..), announce, buildDeployment, selectDeployments)
import Command.Common (loadTerminaModule, loadModules)
import Configuration.Configuration
import Configuration.Platform (Platform(..), checkPlatform)
import Runner.Analysis (TestResult(TestResult), analyse, addressesOf, renderReport, exitCodeOf)
import Runner.Descriptor (Descriptor(..), Symbols(..), readDescriptor)
import Runner.Posix (PosixRun(..), runPosix)
import Runner.Record (Header(..), decodeHeader, decodeRecords, headerSize, platformLayout)
import Runner.Symbols (locateAddresses)

import Control.Exception (try)
import Data.Bits (testBit)
import qualified Data.ByteString as BS
import Numeric (showHex)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import Command.Types
import Command.Utils
import Configuration.Manifest
import Core.AST
import Core.TestCases (NumberedCase(caseLocation), numberCases)
import qualified Core.TestCases as NC
import Modules.Modules
import Parser.AST
import Parser.Types (ParserAnn)
import Utils.Annotations

import Control.Monad
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Options.Applicative as O
import System.Exit
import System.FilePath
import Text.Parsec.Pos (sourceLine, sourceName)

-- | Data type for the "test" command arguments
data TestCmdArgs =
    TestCmdArgs
        (Maybe String) -- ^ The test to run, or every test
        (Maybe Platform) -- ^ Only this platform
        (Maybe ProjectProfile) -- ^ Profile of the tests
        Bool -- ^ List the tests instead of running them
        Bool -- ^ Write the list in JSON
    deriving Show

-- | Parser for the "test" command arguments
testCmdArgsParser :: O.Parser TestCmdArgs
testCmdArgsParser = TestCmdArgs
    <$> O.optional (O.strArgument (O.metavar "NAME" <> O.help "The test to run (default: every test)"))
    <*> O.optional (O.option (O.maybeReader checkPlatform)
          ( O.long "platform"
          <> O.metavar "PLATFORM"
          <> O.help "Only this platform"))
    <*> O.optional (O.option (O.eitherReader readProfile)
          ( O.long "profile"
          <> O.metavar "debug|release"
          <> O.help "Profile of the tests (default: the one of the application, debug in a library)"))
    <*> O.switch (O.long "list"
        <> O.help "List the tests, their platforms and their cases, without building them")
    <*> O.switch (O.long "json"
        <> O.help "Write the list in JSON")

  where

    readProfile :: String -> Either String ProjectProfile
    readProfile profileName =
        case profileName of
            "debug" -> Right Debug
            "release" -> Right Release
            other -> Left $ "Unknown profile: " ++ other ++ " (use debug or release)"

-- | A case of a test, named by its test instance
data TestCase = TestCase {
    caseInstance :: Identifier,
    caseName :: Identifier,
    caseFile :: FilePath,
    caseLine :: Int
  } deriving (Eq, Show)

-- | Command handler for the "test" command
testCommand :: TestCmdArgs -> IO ()
testCommand (TestCmdArgs mName mPlatform mProfile list json) =
    if list then listTests json else runTests mName mPlatform mProfile

-- | Builds and runs each selected test on each of its platforms, and exits with
-- the worst code of them: 0 when every case passes, 1 when one does not, 2 when
-- a test cannot be built or read
runTests :: Maybe String -> Maybe Platform -> Maybe ProjectProfile -> IO ()
runTests mName mPlatform mProfile = do
    -- | The report goes in order with the output of make
    hSetBuffering stdout LineBuffering
    manifest <- loadManifest >>= either dieWithErrors return
    let selection = maybe SelectTests SelectTest mName
        buildArgs = BuildCmdArgs False False Nothing selection mPlatform mProfile
    deployments <- either (die . errorMessage) return (selectDeployments manifest selection mPlatform mProfile)
    codes <- forM deployments $ \deployment -> do
        announce deployments ("Building " ++ deploymentLabel deployment)
        built <- try (buildDeployment buildArgs deployment) :: IO (Either ExitCode [(QualifiedName, FilePath)])
        case built of
            Left _ -> return 2
            Right _ -> runDeployment deployment
    let worst = maximum (0 : codes)
    if worst == 0 then exitSuccess else exitWith (ExitFailure worst)

-- | Runs a test that is built, on posix; a board comes with the executor of GDB
runDeployment :: Deployment -> IO Int
runDeployment deployment = do
    let config = deploymentConfig deployment
        plt = deploymentPlatform deployment
        folder = outputFolder config
        label = T.unpack (name config) ++ " (" ++ show plt ++ ")"
    if plt /= POSIXGCC
      then do
        putStrLn . warnMessage $ label ++ ": running on a board comes with the executor of GDB, the test is only built"
        return 0
      else do
        loaded <- readDescriptor (folder </> "test" <.> "json")
        case loaded of
          Left err -> report label Nothing ("its descriptor cannot be read: " ++ err)
          Right descriptor -> do
            let layout = platformLayout plt
                binary = folder </> "bin" </> T.unpack (name config)
                logFile = folder </> "test" <.> "log"
            ran <- runPosix layout binary (folder </> "record" <.> "bin") logFile
                     (recordCapacity config) 0 (descriptorTimeout descriptor)
            case ran of
              Left err -> report label (Just descriptor) err
              Right posixRun -> readRun label descriptor layout binary logFile posixRun

  where

    report label mDescriptor reason = do
        let empty = Descriptor 1 0 "" "unit" "" Nothing [] [] M.empty M.empty (Symbols "" "" "" "")
        mapM_ putStrLn (renderReport (maybe empty id mDescriptor) M.empty (TestResult label (Just reason) [] False 0))
        return 2

    readRun label descriptor layout binary logFile posixRun = do
        let bytes = posixRecord posixRun
            killed = posixExit posixRun == Nothing
        case decodeHeader layout bytes of
          Left err -> report label (Just descriptor) err
          Right header ->
            if headerMagic header /= BS.pack (map (fromIntegral . fromEnum) "TRMTEST\0")
              then report label (Just descriptor) (noRecord (posixExit posixRun) logFile)
              else if headerStamp header /= descriptorStamp descriptor
                then report label (Just descriptor) ("the stamp of the record, 0x" ++ showHex (headerStamp header) ""
                       ++ ", is not the one of test.json, 0x" ++ showHex (descriptorStamp descriptor) "" ++ ": the binary is not the one of this descriptor")
                else do
                  let area = BS.take (fromIntegral (headerWriteOffset header)) (BS.drop (headerSize layout) bytes)
                  case decodeRecords layout area of
                    Left err -> report label (Just descriptor) err
                    Right records -> do
                      let results = analyse descriptor records killed
                      (locations, warning) <- locateAddresses binary (headerAnchor header) (addressesOf results)
                      maybe (return ()) (putStrLn . warnMessage) warning
                      let result = TestResult label Nothing results (testBit (headerFlags header) 1) (headerLastStep header)
                      mapM_ putStrLn (renderReport descriptor locations result)
                      return (exitCodeOf result)

    noRecord ended logFile =
        "the binary wrote no record" ++ (case ended of
            Just (ExitFailure code) -> ", and exited with " ++ show code
            _ -> "") ++ "; its output is in " ++ logFile

-- | Lists the tests, their platforms and their cases
listTests :: Bool -> IO ()
listTests json = do
    manifest <- loadManifest >>= either dieWithErrors return
    listed <- forM (M.toList (manifestTests manifest)) $ \(testName, entry) -> do
      cases <- loadTestCases (T.unpack testName)
      return (testName, entry, cases)
    if json
      then BLC.putStrLn $ encodePretty' defConfig { confCompare = keyOrder keys } (object [
          "tests" .= [ object [
              "name" .= testName,
              "folder" .= ("tests" </> T.unpack testName),
              "level" .= level cases,
              "platforms" .= map show (testPlatforms entry),
              "cases" .= [ object [
                  "id" .= caseId c,
                  "instance" .= caseInstance c,
                  "case" .= caseName c,
                  "file" .= caseFile c,
                  "line" .= caseLine c
                ] | c <- cases ]
            ] | (testName, entry, cases) <- listed ]
        ])
      else forM_ listed $ \(testName, entry, cases) -> do
        putStrLn $ T.unpack testName ++ " (" ++ level cases ++ ") on "
          ++ joinWith ", " (map show (testPlatforms entry))
        forM_ cases $ \c -> putStrLn ("  " ++ caseId c)

  where

    keys = ["tests", "name", "folder", "level", "platforms", "cases", "id", "instance", "case", "file", "line"]

    caseId c = caseInstance c ++ "." ++ caseName c

    -- | A test without test instances is a concurrent test
    level :: [TestCase] -> String
    level [] = "concurrent"
    level _ = "unit"

    joinWith sep = foldr1 (\a b -> a ++ sep ++ b)

-- | Parse the modules of a test and read its cases
loadTestCases :: String -> IO [TestCase]
loadTestCases testName = do
    let testFolder = "tests" </> testName
        sources = ["src", testFolder </> "src"]
    testModule <- loadTerminaModule testFolder "test" sources
    parsedModules <- loadModules (importedModules testModule) sources
    let parsedProject = M.insert (qualifiedName testModule) testModule parsedModules
    orderedModules <- either
        (\cycleFiles -> die . errorMessage $ "Detected cycle between the source files of the test " ++ testName ++ ": " ++ show cycleFiles)
        return
        (sortProjectDepsOrLoop (M.map importedModules parsedProject))
    either (die . errorMessage) return $
      testCases [parsedAST . metadata $ parsedProject M.! m | m <- orderedModules]

-- | The cases of a test in the order in which the harness numbers them.
testCases :: [AnnotatedProgram ParserAnn] -> Either String [TestCase]
testCases programs =
    case numberCases instances testClasses of
        Left (ident, cls) -> Left $ "the test instance " ++ ident ++ " is of type " ++ cls ++ ", which is not a test class"
        Right cases -> Right [ TestCase (NC.caseInstance c) (NC.caseName c) (fileOf loc) (lineOf loc)
                             | c <- cases, let loc = caseLocation c ]

  where

    elements = concat programs

    instances = [ (ident, cls, ann) | GlobalDeclaration (Test ident (TSDefinedType cls []) _ _ ann) <- elements ]

    testClasses = M.fromList [ (ident, [ (cname, ann) | ClassCase _ cname _ _ ann <- members ])
                             | TypeDefinition (Class TestClass ident members _ _) _ <- elements ]

    fileOf (Position _ start _) = sourceName start
    fileOf _ = ""

    lineOf (Position _ start _) = sourceLine start
    lineOf _ = 0
