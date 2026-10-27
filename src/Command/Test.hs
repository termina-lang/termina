{-# LANGUAGE OverloadedStrings #-}

-- | The tests of the project. For now it only lists them, with their
-- platforms and their cases, which it reads from the modules of each test
-- without type checking or building them.
module Command.Test (
    testCmdArgsParser, testCommand, TestCmdArgs,
    TestCase(..), testCases
) where

import Command.Common (loadTerminaModule, loadModules)
import Command.Sources (loadSources)
import Command.Types
import Command.Utils
import Configuration.Manifest
import Configuration.Namespace (locationLine)
import Core.AST
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
import System.FilePath
import Text.Parsec.Pos (sourceName)

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
      dieWithError $ "termina test can only list the tests for now: use --list"
    manifest <- loadManifest >>= either dieWithFirstError return
    listed <- forM (M.toList (manifestTests manifest)) $ \(testName, entry) -> do
      cases <- loadTestCases manifest (T.unpack testName)
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
loadTestCases :: Manifest -> String -> IO [TestCase]
loadTestCases manifest testName = do
    let testFolder = "tests" </> testName
    sources <- loadSources manifest (Just testName) testFolder
    testModule <- loadTerminaModule sources testFolder "test"
    parsedModules <- loadModules sources testFolder (importedModules testModule)
    let parsedProject = M.insert (qualifiedName testModule) testModule parsedModules
    orderedModules <- either
        (\cycleFiles -> dieWithError $ "Detected cycle between the source files of the test " ++ testName ++ ": " ++ show cycleFiles)
        return
        (sortProjectDepsOrLoop (M.map importedModules parsedProject))
    either dieWithError return $
      testCases [parsedAST . metadata $ parsedProject M.! m | m <- orderedModules]

-- | The cases of a test, from its modules in the order in which they are
-- type checked: for each test instance in the order of declaration, the cases
-- of its class in the order of declaration.
testCases :: [AnnotatedProgram ParserAnn] -> Either String [TestCase]
testCases programs = concat <$> mapM casesOf instances

  where

    elements = concat programs

    instances = [ (ident, cls) | GlobalDeclaration (Test ident (TSDefinedType cls []) _ _ _) <- elements ]

    testClasses = M.fromList [ (ident, members) | TypeDefinition (Class TestClass ident members _ _) _ <- elements ]

    casesOf (ident, cls) =
        case M.lookup cls testClasses of
            Nothing -> Left $ "the test instance " ++ ident ++ " is of type " ++ cls ++ ", which is not a test class"
            Just members -> Right [ TestCase ident cname (fileOf ann) (locationLine ann) | ClassCase _ cname _ _ ann <- members ]

    fileOf (Position _ start _) = sourceName start
    fileOf _ = ""
