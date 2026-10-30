{-# LANGUAGE OverloadedStrings #-}

-- | The validation of a fetched package before it goes to "vendor/": what
-- the fetch needs to know what to copy, and what the build of the consumer
-- would not see. The rest is left to the build.
module Configuration.Validate (
    validatePackage,
    validateLocalPackage
) where

import Data.Char (toLower)
import Data.List (find, isPrefixOf, sortOn, groupBy, stripPrefix)
import Data.Function (on)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import System.FilePath

import Configuration.Configuration
import Configuration.ExternCheck (ExternSection(..), sectionErrors, moduleErrors, sourceExterns)
import Configuration.Fetch (FetchedFile(..))
import Configuration.Manifest
import Configuration.Namespace (SourceModule(..), parseModule)
import Configuration.Prefix
import Configuration.Vendor (isVendored, exportedExtern)

-- | Check the files of the commit of a package, and give its manifest or
-- every problem found.
validatePackage :: [FetchedFile] -> Either [String] Manifest
validatePackage = validateFiles False

-- | Check the files of a library in a local folder as those of a fetched
-- package, except that it may itself depend on other local folders.
validateLocalPackage :: [FetchedFile] -> Either [String] Manifest
validateLocalPackage = validateFiles True

validateFiles :: Bool -> [FetchedFile] -> Either [String] Manifest
validateFiles local files =
    case find ((== "termina.yaml") . fetchedPath) files of
        Nothing -> Left ["the package has no termina.yaml"]
        Just yaml -> case decodeManifest (fetchedBytes yaml) of
            Left err -> Left ["termina.yaml: " ++ err]
            Right manifest -> case manifestNature manifest of
                Application _ -> Left ["the package is an application: it has no exports, so it cannot be a dependency"]
                Library [] -> Left ["the package exports no prefix, so it cannot be a dependency"]
                Library prefixes ->
                    case pathErrors manifest ++ externErrors manifest ++ linkErrors manifest prefixes ++ caseErrors manifest of
                        [] -> Right manifest
                        errs -> Left errs

  where

    externPaths = externFiles . manifestExtern

    -- | A path dependency only exists on the machine of the author.
    pathErrors manifest =
        [ "the package declares the path dependency " ++ p ++ ", which only exists on the machine of its author"
        | not local, DepPath p <- manifestDependencies manifest ]

    -- | The checks of the C of the project, over what the consumer receives:
    -- the entries of the exported modules, the files that go to "vendor/" and
    -- the exported modules. An exported module that does not parse is left to
    -- the build of the consumer, which reports the error of the parser.
    externErrors manifest =
        sectionErrors [section] ++ moduleErrors owner "src" config Nothing (mapMaybe sourceExterns exported)

      where

        owner = "the package"

        config = exportedExtern manifest

        vendored = [ f | f <- files, isVendored manifest (fetchedPath f) ]

        exported =
            [ SourceModule segments path (parseModule (joinPath segments) path bytes)
            | FetchedFile path _ bytes <- vendored, Just rel <- [stripPrefix "src/" path], takeExtension rel == ".fin"
            , let segments = splitDirectories (dropExtension rel) ]

        keys = [ map T.unpack (T.splitOn "." key) | key <- M.keys (externModules config) ]

        section = ExternSection owner externFolder "src" config
            [ rel | FetchedFile path _ _ <- vendored, Just rel <- [stripPrefix (externFolder ++ "/") path] ]
            (map moduleSegments exported)
            [ (moduleSegments m, moduleFile m) | m <- exported ]
            (mapMaybe sourceExterns [ m | m <- exported, moduleSegments m `elem` keys ])

    -- | A symbolic link or a submodule where the build reads: in an exported
    -- subtree, on the way to one, or in place of a file that is copied.
    linkErrors manifest prefixes =
        [ path ++ ": " ++ what ++ ", which a package cannot hold where its files are copied"
        | FetchedFile path mode _ <- files
        , Just what <- [lookup mode [("120000", "a symbolic link"), ("160000", "a submodule")]]
        , any (related path) targets ]

      where

        bases = map prefixFolder prefixes

        targets = "termina.yaml" : map ("extern/" ++) (externPaths manifest) ++ map (++ ".fin") bases ++ bases

        -- A link is the file itself, a folder on the way to it, or lies under a prefix.
        related path target = listedMatches target path || (path ++ "/") `isPrefixOf` target
            || any (`ownsPath` path) prefixes

    -- | Two copied paths that differ only in case would be one file on
    -- macOS and Windows.
    caseErrors manifest =
        [ unwords (joinAnd (map fst group)) ++ " differ only in case"
        | group <- groupBy ((==) `on` snd) (sortOn (\(p, l) -> (l, p)) [(p, map toLower p) | FetchedFile p _ _ <- files, isVendored manifest p])
        , length group > 1 ]

    joinAnd [a, b] = [a, "and", b]
    joinAnd (a : rest) = (a ++ ",") : joinAnd rest
    joinAnd [] = []
