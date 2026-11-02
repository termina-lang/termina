-- | A library that a project depends on, read once from where it comes:
-- the clone of a tag, its copy in vendor/, or a local folder.
module Configuration.Package (
    Origin(..),
    Package(..),
    packageFromFiles,
    packageCommit,
    packageModule
) where

import qualified Data.Map.Lazy as ML
import qualified Data.Text as T
import System.FilePath

import Configuration.Fetch (FetchedFile(..))
import Configuration.Manifest
import Configuration.Parsed

-- | Where a package comes from.
data Origin =
    -- | A tag of a repository: its URL, the tag and the commit
    FromGit !T.Text !T.Text !T.Text
    -- | A local folder, as the project reaches it
    | FromFolder !FilePath
  deriving (Eq, Show)

data Package = Package {
    packageOrigin :: !Origin,
    -- | The folder where the build sees it: vendor/<url> or a local folder
    packageFolder :: !FilePath,
    packageManifest :: !Manifest,
    -- | Its files, by their path in it
    packageFiles :: ![FetchedFile],
    -- | The modules of its src/, by their path from the project folder
    packageModules :: !ParsedFiles
  }

-- | Two packages are the same when they come from the same place with the
-- same files.
instance Eq Package where
    a == b = packageOrigin a == packageOrigin b && packageFiles a == packageFiles b

instance Show Package where
    show p = "Package " ++ show (packageOrigin p) ++ " " ++ show (packageFolder p)

-- | The package of some files, given where it comes from and the folder
-- where the build sees it: its manifest is decoded, and its modules are
-- parsed when first used.
packageFromFiles :: Origin -> FilePath -> [FetchedFile] -> Either String Package
packageFromFiles origin folder files =
    case [ bytes | FetchedFile "termina.yaml" _ bytes <- files ] of
        [] -> Left "the package has no termina.yaml"
        bytes : _ -> case decodeManifest bytes of
            Left err -> Left ("termina.yaml: " ++ err)
            Right manifest -> Right (Package origin folder manifest files
                (parsePackage folder [ (path, content) | FetchedFile path _ content <- files ]))

-- | The commit of a package of a repository, or nothing for a local folder.
packageCommit :: Package -> Maybe T.Text
packageCommit p = case packageOrigin p of
    FromGit _ _ commit -> Just commit
    FromFolder _ -> Nothing

-- | A module of a package, by its path in it.
packageModule :: Package -> FilePath -> Maybe ParsedFile
packageModule p path = ML.lookup (normalise (packageFolder p </> path)) (packageModules p)
