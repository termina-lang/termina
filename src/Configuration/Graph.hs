-- | The checks of a resolved graph of packages that no package can do alone:
-- the prefixes of all of them are disjoint, and the exported modules of each
-- one import only from it and from the dependencies it declares. The
-- libraries taken from local folders count as packages.
module Configuration.Graph (
    checkGraph
) where

import Data.List (isSuffixOf, tails)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.FilePath (joinPath, dropExtension, splitDirectories)

import Configuration.Fetch (FetchedFile(..))
import Configuration.Local
import Configuration.Lock (normalizeUrl)
import Configuration.Manifest
import Configuration.Namespace (parseModule, locationLine, dottedName)
import Configuration.Prefix
import Configuration.Resolve
import Core.AST

-- | A package of the graph as the checks see it: where its files are shown,
-- its manifest and files, and the folder of a local one, which its path
-- dependencies are relative to.
data Unit = Unit String Manifest [FetchedFile] (Maybe FilePath)

-- | Every problem of the graph of the root: prefixes that overlap between two
-- packages, or between a package and the root when it is a library, and
-- imports of exported modules that leave their package and its dependencies.
checkGraph :: Manifest -> Resolution -> M.Map FilePath LocalPackage -> [String]
checkGraph root resolution locals = overlapErrors ++ concatMap importErrors units

  where

    packages = resolvedPackages resolution

    manifestOf = obtainedManifest . resolvedObtained

    nameOf = T.unpack . projectName . manifestProject

    units = [ Unit ("vendor/" ++ T.unpack key) (manifestOf pkg) (obtainedFiles (resolvedObtained pkg)) Nothing
            | (key, pkg) <- M.toList packages ]
        ++ [ Unit folder (localManifest l) (localFiles l) (Just folder) | (folder, l) <- M.toList locals ]

    -- | The prefixes with their owner, and whether the owner is the root,
    -- whose own overlaps are reported by its build.
    owned = [(nameOf root, True, p) | p <- manifestPrefixes root]
        ++ [(nameOf m, False, p) | Unit _ m _ _ <- units, p <- manifestPrefixes m]

    overlapErrors =
        [ "the prefix " ++ prefixText p1 ++ " of " ++ o1 ++ " overlaps " ++ prefixText p2 ++ " of " ++ o2
        | ((o1, r1, p1) : rest) <- tails owned, (o2, r2, p2) <- rest
        , not (r1 && r2), overlaps p1 p2 ]

    importErrors (Unit shown m files folder) =
        [ shown ++ "/" ++ path ++ ":" ++ show (locationLine ann) ++ ": the exported module "
            ++ dottedName segments ++ " imports " ++ dottedName imported ++ ", which is neither exported by "
            ++ nameOf m ++ " nor by a dependency it declares"
        | FetchedFile path _ bytes <- files
        , ".fin" `isSuffixOf` path, any (`ownsPath` path) (manifestPrefixes m)
        , let segments = splitDirectories (dropExtension (drop (length ("src/" :: String)) path))
        , Just ast <- [parseModule (joinPath segments) path bytes]
        , ModuleImport imported ann <- modules ast
        , not (any (`ownsModule` imported) allowed) ]

      where

        allowed = manifestPrefixes m ++ concat
            [ manifestPrefixes (manifestOf dep)
            | DepGit g <- manifestDependencies m
            , Just dep <- [M.lookup (normalizeUrl (gitUrl g)) packages] ]
            ++ concat
            [ manifestPrefixes (localManifest l)
            | DepPath p <- manifestDependencies m
            , Just l <- [M.lookup (localFolderOf (fromMaybe "." folder) p) locals] ]
