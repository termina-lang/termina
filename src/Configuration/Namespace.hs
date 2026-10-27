-- | The namespaces of a project: the prefixes that a library exports.
module Configuration.Namespace (
    checkExports,
    exportedModules,
    hasLibraryPrefix,
    parseModule,
    locationLine,
    dottedName
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
import Text.Parsec.Pos (sourceLine)

import Configuration.Manifest
import Configuration.Prefix
import Core.AST
import Parser.AST (TerminaModule)
import Parser.Parsing (terminaModuleParser)
import Parser.Types (ParserAnn)
import Semantic.Environment (declaredNames)
import Utils.Annotations

-- | A module of the "src" of the project, by the segments of its name, with
-- its file and its tree, if it parses.
data SourceModule = SourceModule {
    moduleSegments :: ![String],
    moduleFile :: !FilePath,
    moduleParsed :: !(Maybe (TerminaModule ParserAnn))
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
        | (a : rest) <- tails prefixes, b <- rest, overlaps a b
        , let (outer, inner) = if ownsModule a (prefixSegments b) then (a, b) else (b, a) ]

    emptyErrors sources p =
        ["the prefix " ++ prefixText p ++ " of exports has no module: there is no "
            ++ prefixFolder p ++ ".fin and no .fin under " ++ prefixFolder p ++ "/"
        | not (any (ownsModule p . moduleSegments) sources)]

    -- | An exported module has to import only exported modules of the
    -- library, and give every name the prefix of the library. One that does
    -- not parse is left to the type check of the library, which loads it and
    -- reports the error of the parser.
    exportedErrors prefixes sources m =
        case find (`ownsModule` moduleSegments m) prefixes of
            Nothing -> []
            Just p -> case moduleParsed m of
                Nothing -> []
                Just ast -> importErrors prefixes sources m ast ++ nameErrors (last (prefixSegments p)) m ast
                    ++ testClassErrors m ast

    importErrors prefixes sources m ast =
        [ at m (locationLine ann) ++ "the exported module " ++ dottedName (moduleSegments m) ++ " imports "
            ++ dottedName imported ++ ", which is not exported"
        | ModuleImport imported ann <- modules ast
        , imported `S.member` moduleSet sources
        , not (any (`ownsModule` imported) prefixes) ]

    nameErrors segment m ast =
        [ at m (locationLine loc) ++ "the name " ++ name ++ " of the exported module " ++ dottedName (moduleSegments m)
            ++ " does not start with the prefix " ++ segment ++ " (" ++ segment ++ "_..., "
            ++ map toUpper segment ++ "_..., " ++ pascal segment ++ "...)"
        | (name, loc) <- M.toList (declaredNames (frags ast))
        , not (hasLibraryPrefix segment name) ]

    -- | A test class holds the cases of one test, so a library does not
    -- publish it: what tests reuse is ordinary code.
    testClassErrors m ast =
        [ at m (locationLine loc) ++ "the exported module " ++ dottedName (moduleSegments m) ++ " declares the test class "
            ++ ident ++ ", which only a test or a module that is not exported may hold"
        | TypeDefinition (Class TestClass ident _ _ _) loc <- frags ast ]

    moduleSet sources = S.fromList (map moduleSegments sources)

    at m line = moduleFile m ++ ":" ++ show line ++ ": "

    pascal = concatMap capitalize . splitOn '_'

    capitalize (c : cs) = toUpper c : cs
    capitalize [] = []

    splitOn sep s = case break (== sep) s of
        (part, []) -> [part]
        (part, _ : rest) -> part : splitOn sep rest

-- | The exported modules of a library, by their qualified name
-- ("pus/service1"), those that do not parse included, and none in an
-- application.
exportedModules :: FilePath -> Manifest -> IO [QualifiedName]
exportedModules root manifest =
    case manifestPrefixes manifest of
        [] -> return []
        prefixes -> do
            sources <- sourceModules root
            return [joinPath (moduleSegments m) | m <- sources, any (`ownsModule` moduleSegments m) prefixes]

-- | The line where a location starts, or 0 when it has none.
locationLine :: Location -> Int
locationLine (Position _ start _) = sourceLine start
locationLine _ = 0

-- | A module name with dots, such as "pus.service1".
dottedName :: [String] -> String
dottedName = intercalate "."

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

-- | Parse the bytes of a module, given its qualified name and its file;
-- nothing when they are not UTF-8 or do not parse.
parseModule :: QualifiedName -> FilePath -> BS.ByteString -> Maybe (TerminaModule ParserAnn)
parseModule qname file bytes =
    case TE.decodeUtf8' bytes of
        Left _ -> Nothing
        Right text -> either (const Nothing) Just (runParser terminaModuleParser qname file (T.unpack text))

-- | Every module under "src", parsed when it can be.
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
