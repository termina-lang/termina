-- | The module files of a project and of its dependencies as the parser reads
-- them. The maps hold lazy values, and a file is parsed when its entry is
-- first used.
module Configuration.Parsed (
    ParseFailure(..),
    ParsedFile,
    ParsedFiles,
    parseFile,
    parseText,
    parseFolder,
    parsePackage,
    parsedTree
) where

import qualified Data.ByteString as BS
import Data.List (stripPrefix)
import qualified Data.Map.Lazy as ML
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath
import Text.Parsec (ParseError, runParser)

import Parser.AST (TerminaModule)
import Parser.Parsing (terminaModuleParser)
import Parser.Types (ParserAnn)
import Utils.Annotations (QualifiedName)

-- | Why a module file has no tree.
data ParseFailure =
    -- | The file is not valid UTF-8
    NotUtf8
    -- | The parser rejects the text of the file
    | InvalidSyntax !T.Text !ParseError
  deriving Show

-- | A module file read by the parser: its text and its tree.
type ParsedFile = Either ParseFailure (T.Text, TerminaModule ParserAnn)

-- | Module files by path.
type ParsedFiles = ML.Map FilePath ParsedFile

-- | Parse the bytes of a module, given its qualified name and its file.
parseFile :: QualifiedName -> FilePath -> BS.ByteString -> ParsedFile
parseFile qname file bytes =
    case TE.decodeUtf8' bytes of
        Left _ -> Left NotUtf8
        Right text -> parseText qname file text

-- | Parse the text of a module, given its qualified name and its file.
parseText :: QualifiedName -> FilePath -> T.Text -> ParsedFile
parseText qname file text =
    either (Left . InvalidSyntax text) (\ast -> Right (text, ast))
        (runParser terminaModuleParser qname file (T.unpack text))

-- | The modules of a source folder by their path from the project folder,
-- given the files of the source folder by their path in it.
parseFolder :: FilePath -> [(FilePath, BS.ByteString)] -> ParsedFiles
parseFolder folder files = ML.fromList
    [ (file, parseFile (dropExtension rel) file bytes)
    | (rel, bytes) <- files, takeExtension rel == ".fin"
    , let file = normalise (folder </> rel) ]

-- | The modules of the src/ folder of a package, given the files of the
-- package by their path in it and the folder where the package is.
parsePackage :: FilePath -> [(FilePath, BS.ByteString)] -> ParsedFiles
parsePackage folder files =
    parseFolder (folder </> "src") [ (rel, bytes) | (path, bytes) <- files, Just rel <- [stripPrefix "src/" path] ]

-- | The tree of a parsed module file, or nothing when it does not parse.
parsedTree :: ParsedFile -> Maybe (TerminaModule ParserAnn)
parsedTree = either (const Nothing) (Just . snd)
