{-# LANGUAGE OverloadedStrings #-}

-- | The provenance.json of a build and of a deliverable, which share their
-- schema: the environment that built the binary, the binary and each module,
-- with where its source comes from and the SHA-256 of its object without
-- debugging information. A deliverable adds its files.
module Command.Provenance (
    provenanceFields,
    writeProvenanceFile,
    buildInfo,
    readHashes,
    terminaVersion
) where

import Command.Utils (dieWithError)
import Configuration.Configuration
import Utils.Annotations (QualifiedName)

import Data.Aeson (object, (.=))
import Data.Aeson.Types (Pair)
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Version (versionBranch)
import Paths_termina (version)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (readProcessWithExitCode)

-- | The fields of the provenance of a build, given the values of its Makefile,
-- the hashes of its bin/stripped.sha256 and its modules with their source
-- files.
provenanceFields :: TerminaConfig -> M.Map String String -> M.Map FilePath String -> [(QualifiedName, FilePath)] -> IO [Pair]
provenanceFields config info hashes modules = do
    osalVersion <- readOsalVersion (dropTrailingPathSeparator (M.findWithDefault "" "TERMINA_OSAL_DIR" info))
    return [
        "name" .= name config,
        "deployment" .= (if isTestBuild config then "test" else "app" :: String),
        "platform" .= platform config,
        "profile" .= profile config,
        "termina" .= terminaVersion,
        "osal" .= osalVersion,
        "compiler" .= M.lookup "CC_VERSION" info,
        "binary" .= object [
            "path" .= binary,
            "stripped-sha256" .= M.lookup binary hashes
          ],
        "modules" .= map moduleEntry modules
      ]

  where

    binary = "bin" </> T.unpack (name config)

    objectOf qname = "obj" </> "src" </> qname <.> "o"

    originOf source = if take 1 (splitDirectories source) == ["tests"] then "test" else "project" :: String

    moduleEntry (qname, source) = object [
        "module" .= map (\c -> if c == '/' then '.' else c) qname,
        "origin" .= originOf source,
        "source" .= source,
        "object" .= objectOf qname,
        "stripped-sha256" .= M.lookup (objectOf qname) hashes
      ]

-- | Write the provenance.json of a folder, given its fields.
writeProvenanceFile :: FilePath -> [Pair] -> IO ()
writeProvenanceFile folder fields =
    BL.writeFile (folder </> "provenance" <.> "json") $
      encodePretty' defConfig { confCompare = keyOrder keys } (object fields)

  where

    keys = ["name", "deployment", "platform", "profile", "termina", "osal", "compiler", "binary", "modules", "files",
            "path", "module", "origin", "source", "object"]

-- | The values that the build-info target of the generated Makefile
-- prints, one per line, as "NAME=value", and "VAR NAME=value" for the
-- variables of the OSAL and of the toolchain.
buildInfo :: String -> FilePath -> IO (M.Map String String)
buildInfo label folder = do
    (code, out, err) <- readProcessWithExitCode "make" ["-s", "--no-print-directory", "-C", folder, "build-info"] ""
    case code of
        ExitSuccess -> return . M.fromList . map (fmap (drop 1) . break (== '=')) . filter ('=' `elem`) . lines $ out
        ExitFailure _ -> dieWithError $ "make failed to give the values of the build of " ++ label ++ ":\n" ++ err

-- | The hashes that the stripped-hashes target writes, by path.
readHashes :: FilePath -> IO (M.Map FilePath String)
readHashes file = do
    exists <- doesFileExist file
    if not exists then return M.empty else
      M.fromList . concatMap (toPair . words) . lines <$> readFile file

  where

    toPair [hash, path] = [(normalise path, hash)]
    toPair _ = []

-- | The version of the OSAL in a folder, from its VERSION file.
readOsalVersion :: FilePath -> IO (Maybe String)
readOsalVersion osalDir = do
    let file = osalDir </> "VERSION"
    exists <- doesFileExist file
    if exists then Just . filter (not . isSpace) <$> readFile file else return Nothing

-- | The package version is PVP (X.Y.Z.0); the public one drops the fourth
-- component.
terminaVersion :: String
terminaVersion = intercalate "." (map show (take 3 (versionBranch version)))
