-- | What the tests of the package manager share: manifests written in place,
-- and git with a fixed identity.
module Configuration.Helpers (
    header,
    manifestOf,
    git
) where

import qualified Data.ByteString.Char8 as BS
import System.Process (readProcess)

import Configuration.Manifest

-- | The project section of a manifest with the given name.
header :: String -> String
header name = "project:\n  name: " ++ name ++ "\n  version: 0.1.0\n  manifest-version: 1\n"

-- | The manifest of a text that the test knows is right.
manifestOf :: String -> Manifest
manifestOf = either error id . decodeManifest . BS.pack

-- | Run git in a repository, with an identity, no signing and no conversion
-- of line endings, whatever the configuration of the machine.
git :: FilePath -> [String] -> IO String
git repo args = readProcess "git" (["-C", repo, "-c", "user.name=test", "-c", "user.email=test@test",
    "-c", "commit.gpgsign=false", "-c", "tag.gpgsign=false", "-c", "core.autocrlf=false"] ++ args) ""
