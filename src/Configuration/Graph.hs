-- | The checks of a resolved graph of packages that no package can do alone:
-- the prefixes of all of them are disjoint, and the exported modules of each
-- one import only from it and from the dependencies it declares.
module Configuration.Graph (
    checkGraph
) where

import Data.List (isSuffixOf, tails)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.FilePath (joinPath, dropExtension, splitDirectories)

import Configuration.Fetch (FetchedFile(..))
import Configuration.Lock (normalizeUrl)
import Configuration.Manifest
import Configuration.Namespace (parseModule, locationLine, dottedName)
import Configuration.Prefix
import Configuration.Resolve
import Core.AST

-- | Every problem of the graph of the root: prefixes that overlap between two
-- packages, or between a package and the root when it is a library, and
-- imports of exported modules that leave their package and its dependencies.
checkGraph :: Manifest -> Resolution -> [String]
checkGraph root resolution = overlapErrors ++ concatMap importErrors (M.toList packages)

  where

    packages = resolvedPackages resolution

    manifestOf = obtainedManifest . resolvedObtained

    nameOf = T.unpack . projectName . manifestProject

    -- | The prefixes with their owner, and whether the owner is the root,
    -- whose own overlaps are reported by its build.
    owned = [(nameOf root, True, p) | p <- manifestPrefixes root]
        ++ [(nameOf (manifestOf pkg), False, p) | pkg <- M.elems packages, p <- manifestPrefixes (manifestOf pkg)]

    overlapErrors =
        [ "the prefix " ++ prefixText p1 ++ " of " ++ o1 ++ " overlaps " ++ prefixText p2 ++ " of " ++ o2
        | ((o1, r1, p1) : rest) <- tails owned, (o2, r2, p2) <- rest
        , not (r1 && r2), overlaps p1 p2 ]

    importErrors (key, pkg) =
        [ "vendor/" ++ T.unpack key ++ "/" ++ path ++ ":" ++ show (locationLine ann) ++ ": the exported module "
            ++ dottedName segments ++ " imports " ++ dottedName imported ++ ", which is neither exported by "
            ++ nameOf m ++ " nor by a dependency it declares"
        | FetchedFile path _ bytes <- obtainedFiles (resolvedObtained pkg)
        , ".fin" `isSuffixOf` path, any (`ownsPath` path) (manifestPrefixes m)
        , let segments = splitDirectories (dropExtension (drop (length ("src/" :: String)) path))
        , Just ast <- [parseModule (joinPath segments) path bytes]
        , ModuleImport imported ann <- modules ast
        , not (any (`ownsModule` imported) allowed) ]

      where

        m = manifestOf pkg

        allowed = manifestPrefixes m ++ concat
            [ manifestPrefixes (manifestOf dep)
            | DepGit g <- manifestDependencies m
            , Just dep <- [M.lookup (normalizeUrl (gitUrl g)) packages] ]
