{-# LANGUAGE OverloadedStrings #-}

-- | The tests of the project. For now it only lists them, with their
-- platforms and their cases, which it reads from the modules of each test
-- without type checking or building them.
module Command.Test (
    testCmdArgsParser, testCommand, TestCmdArgs,
    TestCase(..), testCases
) where

import Command.Common (loadTerminaModule, loadModules)
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
        Bool -- ^ List the tests instead of running them
        Bool -- ^ Write the list in JSON
    deriving Show

-- | Parser for the "test" command arguments
testCmdArgsParser :: O.Parser TestCmdArgs
testCmdArgsParser = TestCmdArgs
    <$> O.switch (O.long "list"
        <> O.help "List the tests, their platforms and their cases, without building them")
    <*> O.switch (O.long "json"
        <> O.help "Write the list in JSON")

-- | A case of a test, named by its test instance
data TestCase = TestCase {
    caseInstance :: Identifier,
    caseName :: Identifier,
    caseFile :: FilePath,
    caseLine :: Int
  } deriving (Eq, Show)

-- | Command handler for the "test" command
testCommand :: TestCmdArgs -> IO ()
testCommand (TestCmdArgs list json) = do
    unless list $
      die . errorMessage $ "termina test can only list the tests for now: use --list"
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
