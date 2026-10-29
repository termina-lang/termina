{-# LANGUAGE OverloadedStrings #-}

-- | The validation of a fetched package before it goes to "vendor/": what
-- the fetch needs to know what to copy, and what the build of the consumer
-- would not see. The rest is left to the build.
module Configuration.Validate (
    validatePackage,
    validateLocalPackage
) where

import Data.Char (toLower)
import Data.List (find, isPrefixOf, sortOn, groupBy)
import Data.Function (on)
import qualified Data.Map.Strict as M

import Configuration.Configuration
import Configuration.Fetch (FetchedFile(..))
import Configuration.Manifest
import Configuration.Prefix
import Configuration.Vendor (isVendored)

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

    modes = M.fromList [(fetchedPath f, fetchedMode f) | f <- files]

    externPaths = externFiles . manifestExtern

    -- | A path dependency only exists on the machine of the author.
    pathErrors manifest =
        [ "the package declares the path dependency " ++ p ++ ", which only exists on the machine of its author"
        | not local, DepPath p <- manifestDependencies manifest ]

    -- | Each file of the extern section lies inside "extern/" and is in the
    -- commit; a link in its place is reported with the others.
    externErrors manifest = concatMap check (externPaths manifest)

      where

        check p
            | not (insideFolder p) = ["the extern section lists " ++ p ++ ", which is outside extern/"]
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

        bases = map prefixFolder prefixes

        targets = "termina.yaml" : map ("extern/" ++) (externPaths manifest) ++ map (++ ".fin") bases ++ bases

        -- A link is the file itself, a folder on the way to it, or lies under a prefix.
        related path target = path == target || (path ++ "/") `isPrefixOf` target
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
