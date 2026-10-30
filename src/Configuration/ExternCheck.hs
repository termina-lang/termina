{-# LANGUAGE OverloadedStrings #-}

-- | The checks of the "extern" sections, written once for the project and for
-- its dependencies. Those that follow from the manifest look at the "extern"
-- folder and at the modules that the entries name; those of the modules look
-- at a set of modules that the caller gives: the exported modules of a
-- library, and those that a deployment loads.
module Configuration.ExternCheck (
    ExternSection(..),
    ModuleExterns(..),
    sectionErrors,
    moduleErrors,
    sourceExterns,
    projectSections,
    ownerOfFolder,
    listFiles
) where

import Control.Monad
import Data.List (find, isPrefixOf, nub, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing, mapMaybe)
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Configuration.Configuration
import Configuration.Manifest
import Configuration.Namespace (SourceModule(..), readModule, moduleNames, locationLine, dottedName)
import Configuration.Platform (checkPlatform)
import Core.AST
import Generator.CodeGen.Application.Config (genConfigPathName)
import Generator.CodeGen.Application.Option (genOptionPathName)
import Generator.CodeGen.Application.Status (genStatusPathName)
import Generator.CodeGen.Application.Result (genResultPathName)
import Utils.Annotations (Location)

-- | A section of "extern" with what its checks read.
data ExternSection = ExternSection {
    -- | Who it belongs to, as the messages name it: "the project", "the test hk"
    sectionOwner :: !String,
    -- | Its "extern" folder
    sectionFolder :: !FilePath,
    -- | The source folder of its modules
    sectionSourceFolder :: !FilePath,
    sectionConfig :: !ExternConfig,
    -- | The files of its "extern" folder, relative to it
    sectionFiles :: ![FilePath],
    -- | The modules of its source folder, by the segments of their names
    sectionModules :: ![[String]],
    -- | The modules whose generated headers may reach its C, with their
    -- files: those of every source folder of the project
    sectionSeen :: ![([String], FilePath)],
    -- | The modules that its entries name and that parse
    sectionListed :: ![ModuleExterns]
  }

-- | The extern functions that a module declares, with where each one is.
data ModuleExterns = ModuleExterns {
    externSegments :: ![String],
    externFile :: !FilePath,
    externDeclared :: ![(Identifier, Location)]
  }

-- | The extern functions of a module read by the parser, or nothing when it
-- does not parse.
sourceExterns :: SourceModule -> Maybe ModuleExterns
sourceExterns m = ModuleExterns (moduleSegments m) (moduleFile m) . externsOf <$> moduleParsed m

  where

    externsOf ast = [ (ident, ann) | ExternFunction ident _ _ _ _ ann <- frags ast ]

-- | Every problem of the sections of a manifest that follows from the manifest
-- itself: the "extern" folder, the lists and the modules that the entries
-- name. The sections go together, since an entry may name a module of the
-- source folder of another one.
sectionErrors :: [ExternSection] -> [String]
sectionErrors sections = concatMap errorsOf sections

  where

    errorsOf s = misplacedErrors s ++ hidingErrors s ++ listErrors s ++ unlistedErrors s ++ keyErrors s

    -- | The sources, the headers and the libraries of each platform have a folder each
    misplacedErrors s =
        [ "The file " ++ sectionFolder s </> file ++ problem | file <- sectionFiles s, Just problem <- [misplaced s file] ]

    -- | The headers of the folder go before the generated ones, so none can
    -- have the path of a generated one, and the C would not see it
    hidingErrors s = concat
        [ [ "The file " ++ sectionFolder s </> file ++ " has the path of a header that the transpiler generates"
          | header `elem` generatedHeaders ]
          ++ [ "The file " ++ sectionFolder s </> file ++ " has the path of the header that the transpiler generates for "
                ++ dottedName segments ++ " (" ++ file' ++ ")"
             | (segments, file') <- sectionSeen s, joinPath segments <.> "h" == header ]
        | file <- sectionFiles s
        , folder : rest <- [splitDirectories file], folder == externHeaderFolder
        , let header = joinPath rest ]

    -- | Each listed path names a file, and no file twice in the same list
    listErrors s = concat
        [ [ listing s path ++ " under " ++ T.unpack modName ++ ", and "
              ++ (if '*' `elem` path then "the pattern matches no file" else "the file does not exist")
          | (path, []) <- named ]
          ++ [ listing s file ++ " twice under " ++ T.unpack modName | file <- nub (files \\ nub files) ]
        | (modName, entry) <- M.toList (externModules (sectionConfig s))
        , paths <- listsOf entry
        , let named = [ (path, listedFiles (sectionFiles s) path) | path <- paths ]
              files = concatMap snd named ]

    -- | Every file of the folder is listed, so that no C is left uncompiled and
    -- a package carries every header and library
    unlistedErrors s =
        [ "The file " ++ sectionFolder s </> file ++ " is not listed under any module in " ++ sectionOf (sectionOwner s)
        | file <- sectionFiles s, file `notElem` listed ]

      where

        listed = concat [ listedFiles (sectionFiles s) path | entry <- M.elems (externModules (sectionConfig s)), paths <- listsOf entry, path <- paths ]

    -- | Each entry names a module of the source folder of its section that
    -- declares some extern function. A module that does not parse is
    -- reported by the caller.
    keyErrors s = concatMap (keyError s) (M.keys (externModules (sectionConfig s)))

    keyError s modName
        | segments `elem` sectionModules s =
            [ listing' ++ ", which declares no extern function (" ++ externFile m ++ ")"
            | Just m <- [find ((== segments) . externSegments) (sectionListed s)], null (externDeclared m) ]
        | any (segments `isPrefixOf`) (sectionModules s) =
            [listing' ++ ", which is a folder (" ++ sectionSourceFolder s </> joinPath segments ++ ") and not a module"]
        | other : _ <- [ o | o <- sections, sectionFolder o /= sectionFolder s, segments `elem` sectionModules o ] =
            [listing' ++ ", which is a module of " ++ sectionOwner other ++ " (" ++ moduleOf other
                ++ "). Move its entry to " ++ sectionOf (sectionOwner other)]
        | otherwise =
            [listing' ++ ", which is not a module (" ++ moduleOf s ++ " does not exist)"]

      where

        segments = map T.unpack (T.splitOn "." modName)

        listing' = "The extern section of " ++ sectionOwner s ++ " lists " ++ T.unpack modName

        moduleOf o = sectionSourceFolder o </> joinPath segments <.> "fin"

    listing s path = "The extern section of " ++ sectionOwner s ++ " lists " ++ sectionFolder s </> path

    listsOf (ExternEntry srcs hdrs libs) =
        [map (externSourceFolder </>) srcs, map (externHeaderFolder </>) hdrs]
        ++ [ map ((externLibraryFolder </> T.unpack plt) </>) pltLibs | (plt, pltLibs) <- M.toList libs ]

    -- | What is wrong with the place of a file of the folder, if anything
    misplaced s file =
        case splitDirectories file of
            (folder : _ : _) | folder == externSourceFolder -> holds folder ".c" "C sources"
            (folder : _ : _) | folder == externHeaderFolder -> holds folder ".h" "headers"
            (folder : plt : _ : _) | folder == externLibraryFolder ->
                if checkPlatform plt == Nothing
                    then Just $ " is in " ++ inFolder (folder </> plt) ++ ", and " ++ plt ++ " is not a platform"
                    else holds (folder </> plt) ".a" "libraries"
            _ -> Just $ " cannot be in " ++ inFolder "" ++ ". The C sources go in " ++ inFolder externSourceFolder
                ++ ", their headers in " ++ inFolder externHeaderFolder ++ " and the libraries of each platform in "
                ++ inFolder (externLibraryFolder </> "<platform>")

      where

        holds folder extension what
            | takeExtension file == extension = Nothing
            | otherwise = Just $ " is in " ++ inFolder folder ++ ", which only holds " ++ what ++ " (" ++ extension ++ ")"

        inFolder folder = addTrailingPathSeparator (sectionFolder s </> folder)

    -- | The headers that the transpiler generates besides those of the modules
    generatedHeaders = [genConfigPathName, genOptionPathName, genStatusPathName, genResultPathName, appFilename <.> "h", testFilename <.> "h"]

-- | Every module with an extern function has its entry in the section of its
-- source folder, given by its owner, its source folder and its entries. The
-- module that wires a deployment has no entry, so it declares no extern
-- function.
moduleErrors :: String -> FilePath -> ExternConfig -> Maybe ModuleExterns -> [ModuleExterns] -> [String]
moduleErrors owner srcFolder config mainModule externs =
    [ externFile m ++ ":" ++ show (locationLine ann) ++ ": " ++ takeFileName (externFile m)
        ++ " cannot declare the extern function " ++ ident ++ ". Declare it in a module of "
        ++ srcFolder ++ "/ and list that module in " ++ sectionOf owner
    | Just m <- [mainModule], (ident, ann) <- externDeclared m ]
    ++ [ "The module " ++ dottedName (externSegments m) ++ " declares the extern function " ++ ident
            ++ " and is not listed in " ++ sectionOf owner
       | m <- externs
       , (ident, _) : _ <- [externDeclared m]
       , not (M.member (T.pack (dottedName (externSegments m))) (externModules config)) ]

sectionOf :: String -> String
sectionOf owner = "the extern section of " ++ owner

-- | The sections of a project, read from its folders: the one of "src" and
-- the one of each test. In a test, the C of the project and the one of the
-- test go to one folder of the output, so the headers of every section are
-- checked against the modules of every source folder. Of the modules, only
-- those that the entries name are read. It also gives the named modules that
-- do not parse, by their qualified name and their file, for the caller to
-- report.
projectSections :: FilePath -> Manifest -> IO ([ExternSection], [(FilePath, FilePath)])
projectSections root manifest = do
    let folders = (externFolder, "src", manifestExtern manifest)
            : [ (folder </> externFolder, folder </> "src", testExtern entry)
              | (testName, entry) <- M.toList (manifestTests manifest), let folder = "tests" </> T.unpack testName ]
    names <- forM folders $ \(_, src, _) -> moduleNames root src
    let seen = [ (segments, src </> joinPath segments <.> "fin") | ((_, src, _), srcNames) <- zip folders names, segments <- srcNames ]
    sections <- zipWithM (\(folder, src, config) srcNames -> section folder src config srcNames seen) folders names
    return (map fst sections, concatMap snd sections)

  where

    section folder src config names seen = do
        exists <- doesDirectoryExist (root </> folder)
        files <- if exists then listFiles (root </> folder) else return []
        listed <- mapM (readModule root src)
            [ segments | key <- M.keys (externModules config), let segments = map T.unpack (T.splitOn "." key), segments `elem` names ]
        return ( ExternSection (ownerOfFolder folder) folder src config files names seen (mapMaybe sourceExterns listed)
               , [ (joinPath (moduleSegments m), moduleFile m) | m <- listed, isNothing (moduleParsed m) ] )

-- | Who the "extern" folder of the project or of a test belongs to, as the
-- messages name it.
ownerOfFolder :: FilePath -> String
ownerOfFolder folder =
    case splitDirectories folder of
        ["tests", testName, _] -> "the test " ++ testName
        _ -> "the project"

-- | The files under a folder, as paths relative to it.
listFiles :: FilePath -> IO [FilePath]
listFiles root = go ""

  where

    go rel = do
      entries <- listDirectory (root </> rel)
      concat <$> forM entries (\entry -> do
        let relEntry = if null rel then entry else rel </> entry
        isDir <- doesDirectoryExist (root </> relEntry)
        if isDir then go relEntry else return [relEntry])
