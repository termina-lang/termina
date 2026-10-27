{-# LANGUAGE OverloadedStrings #-}

-- | The descriptor of a unit test, test.json: its cases, its checks, the
-- tasks and the emitters by identifier, and the symbols the runner stops at.
-- Its stamp is the first 32 bits of the SHA-256 of the rest.
module Generator.TestDescriptor (
    genTestDescriptor
) where

import Command.Types (LoweredProject)
import Configuration.Configuration
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils (architectureCases, blocksIn, classMemberFunctions)
import Core.TestCases (NumberedCase(..))
import Generator.CodeGen.Application.Utils (emitterIdOrder, taskIdOrder)
import Generator.CodeGen.Common (testHarness)
import Lowering.AST
import Modules.Modules
import Semantic.Types
import Utils.Annotations

import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word (Word32)
import Text.Parsec.Pos (SourcePos, sourceColumn, sourceLine, sourceName)
import Text.Printf (printf)

-- | The text of the descriptor and its stamp
genTestDescriptor :: TerminaConfig -> LoweredProject -> TerminaProgArch SemanticAnn -> Either String (BL.ByteString, Word32)
genTestDescriptor config loweredProject progArchitecture = do
    cases <- either (\(ident, cls) -> Left $ "the test instance " ++ ident ++ " is of type " ++ cls ++ ", which is not a test class")
        Right (architectureCases progArchitecture)
    let rest = [
            "version" .= (1 :: Integer),
            "name" .= testName,
            "level" .= ("unit" :: T.Text),
            "platform" .= platform config,
            "timeout" .= runTimeout config,
            "cases" .= map caseEntry cases,
            "checks" .= checks,
            "tasks" .= numbered (taskIdOrder progArchitecture),
            "emitters" .= numbered (emitterIdOrder progArchitecture),
            "symbols" .= object [
                "header" .= ("termina__test__header" :: T.Text),
                "init" .= ("termina__test__init" :: T.Text),
                "end" .= ("termina__test__end" :: T.Text),
                "reboot" .= ("termina__exec__reboot" :: T.Text)
              ]
          ]
        stamp = stampOf (render (object rest))
    return (render (object (("stamp" .= (printf "0x%08x" stamp :: String)) : rest)), stamp)

    where

        testName = case deploymentKind config of
            TestBuild n -> n
            ApplicationBuild -> name config

        render :: Value -> BL.ByteString
        render = encodePretty' defConfig { confCompare = keyOrder keys }

        keys = ["version", "stamp", "number", "id", "name", "level", "platform", "timeout", "cases", "checks", "tasks", "emitters", "symbols",
                "instance", "case", "file", "line", "procedure", "text", "verifies",
                "header", "init", "end", "reboot"]

        caseEntry c = object [
                "number" .= caseNumber c,
                "id" .= (caseInstance c ++ "." ++ caseName c),
                "instance" .= caseInstance c,
                "case" .= caseName c,
                "file" .= fileOf (caseLocation c),
                "line" .= lineOf (caseLocation c),
                "verifies" .= ([] :: [T.Text])
            ]

        numbered :: [Identifier] -> [Value]
        numbered idents = [ object ["id" .= n, "name" .= nameOf ident] | (n, ident) <- zip [0 :: Integer ..] idents ]

        nameOf ident = if ident == testHarness then "harness" else ident

        sources = M.fromList [ (fullPath m, sourcecode m) | m <- M.elems loweredProject ]

        classes = concatMap M.elems [taskClasses progArchitecture, handlerClasses progArchitecture,
                                     resourceClasses progArchitecture, testClasses progArchitecture]

        -- | Every check, a call to a procedure of TestChecks other than step, in the order of the file and the line
        checks = [ object [
                        "file" .= sourceName start,
                        "line" .= sourceLine start,
                        "procedure" .= procedure,
                        "text" .= cutText start end
                    ]
                 | (procedure, Position _ start end) <- sortOn snd (concatMap checksOf classes) ]

        checksOf cls = [ (procedure, getLocation ann)
                       | TPFunction _ _ _ blk _ <- M.elems (classMemberFunctions cls),
                         SystemCall obj procedure _ ann <- blocksIn (blockBody blk),
                         isTestChecks obj, procedure /= "step" ]

        isTestChecks obj = case getObjectSAnns (getAnnotation obj) of
            Just (_, TAccessPort (TInterface _ "TestChecks")) -> True
            _ -> False

        -- | The text of the call in the .fin, in one line
        cutText :: SourcePos -> SourcePos -> T.Text
        cutText start end = case M.lookup (sourceName start) sources of
            Nothing -> ""
            Just src ->
                let ls = drop (sourceLine start - 1) (take (sourceLine end) (T.lines src))
                    cut = case ls of
                        [] -> []
                        [l] -> [T.take (sourceColumn end - sourceColumn start) (T.drop (sourceColumn start - 1) l)]
                        (l : more) -> T.drop (sourceColumn start - 1) l : init more ++ [T.take (sourceColumn end - 1) (last more)]
                in T.unwords (T.words (T.unwords cut))

        fileOf (Position _ start _) = sourceName start
        fileOf _ = ""

        lineOf (Position _ start _) = sourceLine start
        lineOf _ = 0

-- | The first 32 bits of the SHA-256 of a text
stampOf :: BL.ByteString -> Word32
stampOf text = BS.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0 (BS.take 4 (SHA256.hashlazy text))
