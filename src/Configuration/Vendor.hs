{-# LANGUAGE OverloadedStrings #-}

-- | The copies of the dependencies in "vendor/": which files of a package
-- go there, where, and the hash of their content that the lock records.
module Configuration.Vendor (
    VendoredFile,
    vendorDir,
    packageDir,
    isVendored,
    contentHash,
    writePackage,
    readPackage
) where

import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (intercalate, isPrefixOf, isSuffixOf, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath

import Configuration.Configuration
import Configuration.Lock
import Configuration.Manifest

-- | A file of a package: its path from the root of the package, with "/"
-- separators, and its bytes.
type VendoredFile = (String, BS.ByteString)

-- | The folder that holds the copies of the dependencies of a project.
vendorDir :: FilePath -> FilePath
vendorDir root = root </> "vendor"

-- | The folder of a package in "vendor/", named after its normalized URL.
packageDir :: FilePath -> T.Text -> FilePath
packageDir root url = vendorDir root </> joinPath (T.unpack <$> T.splitOn "/" (normalizeUrl url))

-- | Whether a file of a package goes to "vendor/": its "termina.yaml", the
-- modules of its exported prefixes and the files that its "extern" section
-- lists.
isVendored :: Manifest -> String -> Bool
isVendored manifest path =
    path == "termina.yaml" || any exported prefixes || path `elem` externFiles

  where

    prefixes = case manifestNature manifest of
        Library ps -> ps
        Application _ -> []

    exported (Prefix segments) =
        let base = "src/" ++ intercalate "/" (map T.unpack segments)
        in path == base ++ ".fin" || ((base ++ "/") `isPrefixOf` path && ".fin" `isSuffixOf` path)

    extern = manifestExtern manifest

    externFiles = map ("extern/" ++) (externSources extern ++ concat (M.elems (externLibraries extern)))

-- | The hash of the content of a package: the SHA-256 of the lines that
-- sha256sum writes for its files, "hex-digest  path", in the byte order of
-- the paths.
contentHash :: [VendoredFile] -> T.Text
contentHash files =
    "sha256:" <> hex (SHA256.hash (BS.concat (map line (sortOn (pathBytes . fst) files))))

  where

    pathBytes = TE.encodeUtf8 . T.pack

    line (path, bytes) = BS.concat [BC.pack (T.unpack (hex (SHA256.hash bytes))), "  ", pathBytes path, "\n"]

    hex = T.pack . concatMap byteHex . BS.unpack

    byteHex b = [digits !! fromIntegral (b `div` 16), digits !! fromIntegral (b `mod` 16)]

    digits = "0123456789abcdef"

-- | Replace the folder of a package in "vendor/" with the given files, and
-- keep the "vendor/.gitattributes" that stops git from converting them.
writePackage :: FilePath -> T.Text -> [VendoredFile] -> IO ()
writePackage root url files = do
    let dir = packageDir root url
    removePathForcibly dir
    forM_ files $ \(path, bytes) -> do
        let target = dir </> joinPath (splitOnSlash path)
        createDirectoryIfMissing True (takeDirectory target)
        BS.writeFile target bytes
    let attributes = vendorDir root </> ".gitattributes"
    exists <- doesFileExist attributes
    current <- if exists then BS.readFile attributes else return ""
    when (current /= gitAttributes) $ BS.writeFile attributes gitAttributes

  where

    gitAttributes = "* -text\n"

    splitOnSlash = map T.unpack . T.splitOn "/" . T.pack

-- | The files of a package in "vendor/", to hash them again.
readPackage :: FilePath -> T.Text -> IO [VendoredFile]
readPackage root url = do
    let dir = packageDir root url
    paths <- listFiles dir ""
    forM paths $ \path -> (,) path <$> BS.readFile (dir </> path)

  where

    listFiles dir rel = do
        entries <- listDirectory (dir </> rel)
        concat <$> forM entries (\e -> do
            let relPath = if null rel then e else rel ++ "/" ++ e
            isDir <- doesDirectoryExist (dir </> relPath)
            if isDir then listFiles dir relPath else return [relPath])
