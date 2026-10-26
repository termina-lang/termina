-- | The namespaces of a project: the prefixes that a library exports.
module Configuration.Namespace (
    checkExports,
    exportedModules,
    prefixText,
    hasLibraryPrefix,
    parseModule
) where

import Control.Monad
import Data.Char (isUpper, toLower, toUpper)
import Data.List (find, intercalate, isPrefixOf, nub, sort, tails, (\\))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath
import Text.Parsec (runParser)
import Text.Parsec.Error (errorPos)
import Text.Parsec.Pos (sourceLine)

import Configuration.Manifest
import Core.AST
import Parser.AST (TerminaModule)
import Parser.Parsing (terminaModuleParser)
import Parser.Types (ParserAnn)
import Semantic.Environment (declaredNames)
import Utils.Annotations

-- | The prefix as it is written in the manifest, such as "drivers.gaisler".
prefixText :: Prefix -> String
prefixText (Prefix segments) = T.unpack (T.intercalate (T.pack ".") segments)

-- | A module of the "src" of the project, by the segments of its name, with
-- its file and the result of parsing it.
data SourceModule = SourceModule {
    moduleSegments :: ![String],
    moduleFile :: !FilePath,
    moduleParsed :: !(Either Int (TerminaModule ParserAnn))
  }

-- | Check the prefixes of the exports of a library against each other and
-- against its sources, and the exported modules against the prefixes, and
-- collect every problem so that all of them are reported at once. An
-- application exports nothing and has nothing to check.
checkExports :: FilePath -> Manifest -> IO [String]
checkExports root manifest =
    case manifestNature manifest of
        Application _ -> return []
        Library [] -> return ["a library exports at least one prefix"]
        Library prefixes -> do
            sources <- sourceModules root
            return $ repeatedErrors prefixes ++ overlapErrors (nub prefixes)
                ++ concatMap (emptyErrors sources) (nub prefixes)
                ++ concatMap (exportedErrors (nub prefixes) sources) sources

  where

    repeatedErrors prefixes =
        ["the prefix " ++ prefixText p ++ " is listed twice in exports" | p <- nub (prefixes \\ nub prefixes)]

    overlapErrors prefixes =
        [ "the prefixes " ++ prefixText outer ++ " and " ++ prefixText inner ++ " of exports overlap: "
            ++ prefixText outer ++ " contains " ++ prefixText inner
        | (a : rest) <- tails prefixes, b <- rest, (outer, inner) <- [(a, b), (b, a)], contains outer inner ]

    contains (Prefix outer) (Prefix inner) = outer `isPrefixOf` inner && outer /= inner

    -- | A prefix owns the module that it names and everything under the
    -- folder of the same name.
    emptyErrors sources p@(Prefix segments) =
        let path = "src" </> joinPath (map T.unpack segments) in
        ["the prefix " ++ prefixText p ++ " of exports has no module: there is no "
            ++ path <.> "fin" ++ " and no .fin under " ++ path ++ "/"
        | not (any (\m -> map T.unpack segments `isPrefixOf` moduleSegments m) sources)]

    -- | An exported module has to parse, import only exported modules of the
    -- library, and give every name the prefix of the library.
    exportedErrors prefixes sources m =
        case owner prefixes (moduleSegments m) of
            Nothing -> []
            Just (Prefix segments) -> case moduleParsed m of
                Left line -> [at m line ++ "the exported module " ++ dotted (moduleSegments m) ++ " does not parse"]
                Right ast -> importErrors prefixes sources m ast ++ nameErrors (T.unpack (last segments)) m ast
                    ++ testClassErrors m ast

    importErrors prefixes sources m ast =
        [ at m (lineOf ann) ++ "the exported module " ++ dotted (moduleSegments m) ++ " imports "
            ++ dotted imported ++ ", which is not exported"
        | ModuleImport imported ann <- modules ast
        , imported `S.member` moduleSet sources
        , owner prefixes imported == Nothing ]

    nameErrors segment m ast =
        [ at m (lineOf loc) ++ "the name " ++ name ++ " of the exported module " ++ dotted (moduleSegments m)
            ++ " does not start with the prefix " ++ segment ++ " (" ++ segment ++ "_..., "
            ++ map toUpper segment ++ "_..., " ++ pascal segment ++ "...)"
        | (name, loc) <- M.toList (declaredNames (frags ast))
        , not (hasLibraryPrefix segment name) ]

    -- | A test class holds the cases of one test, so a library does not
    -- publish it: what tests reuse is ordinary code.
    testClassErrors m ast =
        [ at m (lineOf loc) ++ "the exported module " ++ dotted (moduleSegments m) ++ " declares the test class "
            ++ ident ++ ", which only a test or a module that is not exported may hold"
        | TypeDefinition (Class TestClass ident _ _ _) loc <- frags ast ]

    moduleSet sources = S.fromList (map moduleSegments sources)

    owner prefixes ms = find (\(Prefix segments) -> map T.unpack segments `isPrefixOf` ms) prefixes

    at m line = moduleFile m ++ ":" ++ show line ++ ": "

    lineOf (Position _ start _) = sourceLine start
    lineOf _ = 0

    dotted = intercalate "."

    pascal = concatMap capitalize . splitOn '_'

    capitalize (c : cs) = toUpper c : cs
    capitalize [] = []

    splitOn sep s = case break (== sep) s of
        (part, []) -> [part]
        (part, _ : rest) -> part : splitOn sep rest

-- | The exported modules of a library that parse, by their qualified name
-- ("pus/service1"), and none in an application.
exportedModules :: FilePath -> Manifest -> IO [QualifiedName]
exportedModules root manifest =
    case manifestNature manifest of
        Application _ -> return []
        Library prefixes -> do
            sources <- sourceModules root
            return [ joinPath (moduleSegments m) | m <- sources
                   , any (\(Prefix segments) -> map T.unpack segments `isPrefixOf` moduleSegments m) prefixes
                   , either (const False) (const True) (moduleParsed m) ]

-- | Whether a name starts with the last segment of the prefix of its library:
-- in snake case or upper case, the segment and "_" ("char_dev_send",
-- "CHAR_DEV_MAX"); in Pascal case, the segment without "_" and an upper-case
-- letter ("CharDevSend").
hasLibraryPrefix :: String -> String -> Bool
hasLibraryPrefix segment name =
    (segment ++ "_") `isPrefixOf` map toLower name
        || (map toLower (take (length joined) name) == joined
            && case drop (length joined) name of
                (c : _) -> isUpper c
                [] -> False)

  where

    joined = filter (/= '_') segment

-- | Parse the bytes of a module, given its qualified name and its file, or
-- give the line of the error (the first one when it is not UTF-8).
parseModule :: QualifiedName -> FilePath -> BS.ByteString -> Either Int (TerminaModule ParserAnn)
parseModule qname file bytes =
    case TE.decodeUtf8' bytes of
        Left _ -> Left 1
        Right text -> either (Left . sourceLine . errorPos) Right
            (runParser terminaModuleParser qname file (T.unpack text))

-- | Every module under "src", parsed. A module that does not parse keeps the
-- line of the error.
sourceModules :: FilePath -> IO [SourceModule]
sourceModules root = do
    files <- finFiles (root </> "src") ""
    forM (sort files) $ \rel -> do
        let file = "src" </> rel
            segments = splitDirectories (dropExtension rel)
            qname = joinPath segments
        SourceModule segments file . parseModule qname file <$> BS.readFile (root </> file)

  where

    finFiles dir rel = do
        exists <- doesDirectoryExist (dir </> rel)
        if not exists then return [] else do
            entries <- filter (not . ("." `isPrefixOf`)) <$> listDirectory (dir </> rel)
            concat <$> forM entries (\e -> do
                let relEntry = if null rel then e else rel </> e
                isDir <- doesDirectoryExist (dir </> relEntry)
                if isDir then finFiles dir relEntry
                    else return [relEntry | takeExtension e == ".fin"])
