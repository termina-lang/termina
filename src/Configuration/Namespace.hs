-- | The prefixes that a library exports, which share out the module paths
-- among the libraries, and the modules of a source folder without loading
-- them.
module Configuration.Namespace (
    SourceModule(..),
    sourceModule,
    moduleNames,
    checkExports,
    exportedModules,
    locationLine,
    dottedName
) where

import Data.List (find, intercalate, nub, tails, (\\))
import qualified Data.Map.Lazy as ML
import qualified Data.Set as S
import System.FilePath
import Text.Parsec.Pos (sourceLine)

import Configuration.Manifest
import Configuration.Parsed
import Configuration.Prefix
import Core.AST
import Parser.AST (TerminaModule)
import Parser.Types (ParserAnn)
import Utils.Annotations
import Utils.Files (filesUnder)

-- | A module of a source folder, by the segments of its name, with its file
-- and its tree, if it parses.
data SourceModule = SourceModule {
    moduleSegments :: ![String],
    moduleFile :: !FilePath,
    moduleParsed :: !(Maybe (TerminaModule ParserAnn))
  }

-- | Check the exports of a library, and give every problem found: a prefix
-- listed twice, two prefixes that overlap, a prefix without modules, and an
-- exported module that imports a module that is not exported, or declares a
-- test class or an instance. Only the exported modules are read, and an
-- application has none.
checkExports :: FilePath -> ParsedFiles -> Manifest -> IO [String]
checkExports root parsed manifest =
    case manifestNature manifest of
        Application _ -> return []
        Library [] -> return ["a library exports at least one prefix"]
        Library prefixes -> do
            names <- moduleNames root "src"
            let exported = map (sourceModule parsed "src") [ segments | segments <- names, any (`ownsModule` segments) prefixes ]
            return $ repeatedErrors prefixes ++ overlapErrors (nub prefixes)
                ++ concatMap (emptyErrors names) (nub prefixes)
                ++ concatMap (exportedErrors (nub prefixes) names) exported

  where

    repeatedErrors prefixes =
        ["the prefix " ++ prefixText p ++ " is listed twice in exports" | p <- nub (prefixes \\ nub prefixes)]

    overlapErrors prefixes =
        [ "the prefixes " ++ prefixText outer ++ " and " ++ prefixText inner ++ " of exports overlap: "
            ++ prefixText outer ++ " contains " ++ prefixText inner
        | (a : rest) <- tails prefixes, b <- rest, overlaps a b
        , let (outer, inner) = if ownsModule a (prefixSegments b) then (a, b) else (b, a) ]

    emptyErrors names p =
        ["the prefix " ++ prefixText p ++ " of exports has no module: there is no "
            ++ prefixFolder p ++ ".fin and no .fin under " ++ prefixFolder p ++ "/"
        | not (any (ownsModule p) names)]

    -- | An exported module imports only exported modules of the library. A
    -- module that does not parse gives no error here; the type check of the
    -- library reports it.
    exportedErrors prefixes names m =
        case find (`ownsModule` moduleSegments m) prefixes of
            Nothing -> []
            Just _ -> case moduleParsed m of
                Nothing -> []
                Just ast -> importErrors prefixes names m ast ++ testClassErrors m ast ++ instanceErrors m ast

    importErrors prefixes names m ast =
        [ at m (locationLine ann) ++ "the exported module " ++ dottedName (moduleSegments m) ++ " imports "
            ++ dottedName imported ++ ", which is not exported"
        | ModuleImport imported ann <- modules ast
        , imported `S.member` S.fromList names
        , not (any (`ownsModule` imported) prefixes) ]

    -- | An exported module declares no test class.
    testClassErrors m ast =
        [ at m (locationLine loc) ++ "the exported module " ++ dottedName (moduleSegments m) ++ " declares the test class "
            ++ ident ++ ", which only a test or a module that is not exported may hold"
        | TypeDefinition (Class TestClass ident _ _ _) loc <- frags ast ]

    -- | An exported module declares no instance, which a deployment only
    -- takes from its main module (AE-023).
    instanceErrors m ast =
        [ at m (locationLine loc) ++ "the exported module " ++ dottedName (moduleSegments m) ++ " declares the instance "
            ++ ident ++ ", which only the app.fin of an application or the test.fin of a test may hold"
        | GlobalDeclaration g <- frags ast, Just (ident, loc) <- [instanceOf g] ]

    instanceOf = \case
        Task ident _ _ _ loc -> Just (ident, loc)
        Resource ident _ _ _ loc -> Just (ident, loc)
        Channel ident _ _ _ loc -> Just (ident, loc)
        Emitter ident _ _ _ loc -> Just (ident, loc)
        Handler ident _ _ _ loc -> Just (ident, loc)
        Test ident _ _ _ loc -> Just (ident, loc)
        _ -> Nothing

    at m line = moduleFile m ++ ":" ++ show line ++ ": "

-- | The exported modules of a library, by their qualified name
-- ("pus/service1"), without reading them, and none in an application.
exportedModules :: FilePath -> Manifest -> IO [QualifiedName]
exportedModules root manifest =
    case manifestPrefixes manifest of
        [] -> return []
        prefixes -> do
            names <- moduleNames root "src"
            return [joinPath segments | segments <- names, any (`ownsModule` segments) prefixes]

-- | The line where a location starts, or 0 when it has none.
locationLine :: Location -> Int
locationLine (Position _ start _) = sourceLine start
locationLine _ = 0

-- | A module name with dots, such as "pus.service1".
dottedName :: [String] -> String
dottedName = intercalate "."

-- | The module of a source folder with the given segments, taken from the
-- parsed files.
sourceModule :: ParsedFiles -> FilePath -> [String] -> SourceModule
sourceModule parsed folder segments =
    SourceModule segments file (ML.lookup (normalise file) parsed >>= parsedTree)

  where

    file = folder </> joinPath segments <.> "fin"

-- | The modules under a source folder of the project, such as "src" or
-- "tests/hk/src", by the segments of their names, without reading them.
moduleNames :: FilePath -> FilePath -> IO [[String]]
moduleNames root folder =
    map (splitDirectories . dropExtension) . filter ((== ".fin") . takeExtension) <$> filesUnder (root </> folder)
