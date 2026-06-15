{-# LANGUAGE TupleSections #-}
-- | Shared helpers for the parser-stage test family.
--
-- Positive specs (@Parser.Positive.*@) drive a grammar production directly with
-- 'parseWith' / 'parses' and assert on the resulting AST. Negative specs
-- (@Parser.Negative.*@) drive the *parser stage in isolation* (lex + parse +
-- module-name building + dependency ordering, NO type checking or codegen) with
-- 'parserStageError' / 'parserStageErrorCode'.
module Parser.Common
  ( parseWith
  , parses
  , ctorName
  , parserStageError
  , parserStageErrorCode
  , moduleStageError
  ) where

import qualified Data.Map.Strict as M
import Data.Text (Text)

import Parser.Parsing (TerminaParser, terminaModuleParser)
import Parser.Errors (Error(..), ParsingErrors)
import Core.AST (TerminaModule'(..), ModuleImport'(..))
import Modules.Modules (ModuleDependency(..))
import Modules.Utils (buildModuleName)
import Command.Utils (sortProjectDepsOrLoop)
import Utils.Annotations (annotateError, Location(Internal), QualifiedName)
import Utils.Errors (ErrorMessage(errorIdent))

import Text.Parsec (runParser, eof, ParseError)

-- Positive side --------------------------------------------------------------

-- | Run a production on a fragment, requiring it to consume all input.
parseWith :: TerminaParser a -> String -> Either ParseError a
parseWith p = runParser (p <* eof) "test" ""

-- | Did the fragment parse cleanly?
parses :: TerminaParser a -> String -> Bool
parses p s = either (const False) (const True) (parseWith p s)

-- | The outermost constructor name of a value, via 'Show'. Lets a spec assert
-- the AST node a production built (e.g. @"ReturnStmt"@, @"TSArray"@) without
-- depending on the constructor arity.
ctorName :: Show a => a -> String
ctorName = takeWhile (/= ' ') . dropParen . show
  where dropParen ('(':xs) = xs
        dropParen xs = xs

-- Negative side: the parser stage in isolation -------------------------------

-- | Lex + parse + build module dependencies + order them for a set of in-memory
-- modules, returning the first parser-stage error (the rich 'ParsingErrors'
-- value, so a 'DetailSpec' can inspect its constructor and payload), or
-- 'Nothing' if the stage succeeds. No type checking is performed.
parserStageError :: [(QualifiedName, String)] -> Maybe ParsingErrors
parserStageError sources =
  case mapM parseOne sources of
    Left err -> Just err
    Right parsed ->
      case sortProjectDepsOrLoop (M.fromList parsed) of
        Left loop -> Just (annotateError Internal (EImportedFilesLoop loop))
        Right _ -> Nothing
  where
    parseOne (qname, src) =
      case runParser terminaModuleParser qname "" src of
        Left err -> Left (annotateError Internal (EParseError err))
        Right (Termina imports _) -> (qname,) <$> mapM toDep imports
    toDep (ModuleImport ident ann) =
      (`ModuleDependency` ann) <$> buildModuleName ann ident

-- | The 'errorIdent' (@"PE-NNN"@) of the first parser-stage error.
parserStageErrorCode :: [(QualifiedName, String)] -> Maybe Text
parserStageErrorCode = fmap errorIdent . parserStageError

-- | Single-module convenience: the parser-stage error for one source named
-- @test@.
moduleStageError :: String -> Maybe ParsingErrors
moduleStageError src = parserStageError [("test", src)]
