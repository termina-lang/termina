-- | The checks of a resolved graph of packages that no package can do alone:
-- the prefixes of all of them are disjoint, and the exported modules of each
-- one import only from it and from the dependencies it declares. The
-- libraries taken from local folders count as packages.
module Configuration.Graph (
    checkGraph
) where

import Data.List (isSuffixOf, tails)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.FilePath (dropExtension, splitDirectories)

import Configuration.Fetch (FetchedFile(..))
import Configuration.Local (localFolderOf)
import Configuration.Lock (normalizeUrl)
import Configuration.Manifest
import Configuration.Namespace (locationLine, dottedName)
import Configuration.Package
import Configuration.Parsed (parsedTree)
import Configuration.Prefix
import Core.AST

-- | Every problem of the graph of the root, given its packages, those of
-- repositories and those of local folders: prefixes that overlap between two
-- packages, or between a package and the root when it is a library, and
-- imports of exported modules that leave their package and its dependencies.
checkGraph :: Manifest -> [Package] -> [String]
checkGraph root packages = overlapErrors ++ concatMap importErrors packages

  where

    fromGit = M.fromList [ (normalizeUrl url, p) | p@Package { packageOrigin = FromGit url _ _ } <- packages ]

    fromFolder = M.fromList [ (folder, p) | p@Package { packageOrigin = FromFolder folder } <- packages ]

    nameOf = T.unpack . projectName . manifestProject

    -- | Where the files of a package are shown in the messages.
    shownAs p = case packageOrigin p of
        FromGit url _ _ -> "vendor/" ++ T.unpack (normalizeUrl url)
        FromFolder folder -> folder

    -- | The prefixes with their owner, and whether the owner is the root,
    -- whose own overlaps are reported by its build.
    owned = [(nameOf root, True, p) | p <- manifestPrefixes root]
        ++ [(nameOf (packageManifest pkg), False, p) | pkg <- packages, p <- manifestPrefixes (packageManifest pkg)]

    overlapErrors =
        [ "the prefix " ++ prefixText p1 ++ " of " ++ o1 ++ " overlaps " ++ prefixText p2 ++ " of " ++ o2
        | ((o1, r1, p1) : rest) <- tails owned, (o2, r2, p2) <- rest
        , not (r1 && r2), overlaps p1 p2 ]

    importErrors pkg =
        [ shownAs pkg ++ "/" ++ path ++ ":" ++ show (locationLine ann) ++ ": the exported module "
            ++ dottedName segments ++ " imports " ++ dottedName imported ++ ", which is neither exported by "
            ++ nameOf m ++ " nor by a dependency it declares"
        | FetchedFile path _ _ <- packageFiles pkg
        , ".fin" `isSuffixOf` path, any (`ownsPath` path) (manifestPrefixes m)
        , let segments = splitDirectories (dropExtension (drop (length ("src/" :: String)) path))
        , Just ast <- [packageModule pkg path >>= parsedTree]
        , ModuleImport imported ann <- modules ast
        , not (any (`ownsModule` imported) allowed) ]

      where

        m = packageManifest pkg

        -- | Path dependencies are relative to the folder of a local package.
        base = case packageOrigin pkg of
            FromFolder folder -> folder
            FromGit {} -> "."

        allowed = manifestPrefixes m ++ concat
            [ manifestPrefixes (packageManifest dep)
            | DepGit g <- manifestDependencies m
            , Just dep <- [M.lookup (normalizeUrl (gitUrl g)) fromGit] ]
            ++ concat
            [ manifestPrefixes (packageManifest dep)
            | DepPath p <- manifestDependencies m
            , Just dep <- [M.lookup (localFolderOf base p) fromFolder] ]
