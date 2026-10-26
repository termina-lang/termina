{-# LANGUAGE OverloadedStrings #-}

-- | The validation of a fetched package before it goes to "vendor/": what
-- the fetch needs to know what to copy, and what the build of the consumer
-- would not see. The rest is left to the build.
module Configuration.Validate (
    validatePackage
) where

import Data.Char (toLower)
import Data.List (find, isPrefixOf, sortOn, groupBy)
import Data.Function (on)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml (decodeEither', prettyPrintParseException)

import Configuration.Configuration
import Configuration.Fetch (FetchedFile(..))
import Configuration.Manifest
import Configuration.Vendor (isVendored)

-- | Check the files of the commit of a package, and give its manifest or
-- every problem found.
validatePackage :: [FetchedFile] -> Either [String] Manifest
validatePackage files =
    case find ((== "termina.yaml") . fetchedPath) files of
        Nothing -> Left ["the package has no termina.yaml"]
        Just yaml -> case either (Left . prettyPrintParseException) parseManifest (decodeEither' (fetchedBytes yaml)) of
            Left err -> Left ["termina.yaml: " ++ err]
            Right manifest -> case manifestNature manifest of
                Application _ -> Left ["the package is an application: it has no exports, so it cannot be a dependency"]
                Library [] -> Left ["the package exports no prefix, so it cannot be a dependency"]
                Library prefixes ->
                    case pathErrors manifest ++ externErrors manifest ++ linkErrors manifest prefixes ++ caseErrors manifest of
                        [] -> Right manifest
                        errs -> Left errs

  where

    modes = M.fromList [(fetchedPath f, fetchedMode f) | f <- files]

    externPaths manifest =
        let extern = manifestExtern manifest
        in externSources extern ++ concat (M.elems (externLibraries extern))

    -- | A path dependency only exists on the machine of the author.
    pathErrors manifest =
        [ "the package declares the path dependency " ++ p ++ ", which only exists on the machine of its author"
        | DepPath p <- manifestDependencies manifest ]

    -- | Each file of the extern section lies inside "extern/" and is in the
    -- commit; a link in its place is reported with the others.
    externErrors manifest = concatMap check (externPaths manifest)

      where

        check p
            | "/" `isPrefixOf` p = ["extern/" ++ p ++ ": the extern section lists an absolute path"]
            | ".." `elem` splitPath p =["extern/" ++ p ++ ": the extern section lists a path outside extern/"]
            | M.member ("extern/" ++ p) modes = []
            | otherwise = ["extern/" ++ p ++ ": the extern section lists a file that the package does not have"]

    -- | A symbolic link or a submodule where the build reads: in an exported
    -- subtree, on the way to one, or in place of a file that is copied.
    linkErrors manifest prefixes =
        [ path ++ ": " ++ what ++ ", which a package cannot hold where its files are copied"
        | FetchedFile path mode _ <- files
        , Just what <- [lookup mode [("120000", "a symbolic link"), ("160000", "a submodule")]]
        , any (related path) targets ]

      where

        bases = ["src/" ++ T.unpack (T.intercalate "/" segments) | Prefix segments <- prefixes]

        targets = "termina.yaml" : map ("extern/" ++) (externPaths manifest) ++ map (++ ".fin") bases ++ bases

        related path target = path == target || (path ++ "/") `isPrefixOf` target
            || any (\base -> (base ++ "/") `isPrefixOf` path) bases

    -- | Two copied paths that differ only in case would be one file on
    -- macOS and Windows.
    caseErrors manifest =
        [ unwords (joinAnd (map fst group)) ++ " differ only in case"
        | group <- groupBy ((==) `on` snd) (sortOn (\(p, l) -> (l, p)) [(p, map toLower p) | FetchedFile p _ _ <- files, isVendored manifest p])
        , length group > 1 ]

    joinAnd [a, b] = [a, "and", b]
    joinAnd (a : rest) = (a ++ ",") : joinAnd rest
    joinAnd [] = []

    splitPath p = case break (== '/') p of
        (s, []) -> [s]
        (s, _ : rest) -> s : splitPath rest
