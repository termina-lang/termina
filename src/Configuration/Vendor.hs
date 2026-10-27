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
    readPackage,
    lockedPackage,
    checkVendor
) where

import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (isSuffixOf, sortOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath

import Configuration.Configuration
import Configuration.Lock
import Configuration.Manifest
import Configuration.Prefix

-- | A file of a package: its path from the root of the package, with "/"
-- separators, and its bytes.
type VendoredFile = (String, BS.ByteString)

-- | The folder that holds the copies of the dependencies of a project.
vendorDir :: FilePath -> FilePath
vendorDir root = root </> "vendor"

-- | The folder of a package in "vendor/", named after its normalized URL.
packageDir :: FilePath -> T.Text -> FilePath
packageDir root url = vendorDir root </> normalizedFolder url

-- | Whether a file of a package goes to "vendor/": its "termina.yaml", the
-- modules of its exported prefixes and the files that its "extern" section
-- lists.
isVendored :: Manifest -> String -> Bool
isVendored manifest path =
    path == "termina.yaml"
        || (".fin" `isSuffixOf` path && any (`ownsPath` path) (manifestPrefixes manifest))
        || path `elem` map ("extern/" ++) (externFiles (manifestExtern manifest))

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
        let target = dir </> joinPath (splitDirectories path)
        createDirectoryIfMissing True (takeDirectory target)
        BS.writeFile target bytes
    let attributes = vendorDir root </> ".gitattributes"
    exists <- doesFileExist attributes
    current <- if exists then BS.readFile attributes else return ""
    when (current /= gitAttributes) $ BS.writeFile attributes gitAttributes

  where

    gitAttributes = "* -text\n"

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

-- | The files of a locked package in "vendor/", or why they are not the ones
-- the lock records: the folder is missing or its content-hash differs. The
-- copy is never repaired, so a modified "vendor/" cannot enter a build.
lockedPackage :: FilePath -> LockedPackage -> IO (Either String [VendoredFile])
lockedPackage root locked = do
    let url = lockedUrl locked
        folder = "vendor/" ++ T.unpack (normalizeUrl url)
    exists <- doesDirectoryExist (packageDir root url)
    if not exists
        then return (Left $ folder ++ " is missing. Run termina restore to bring it back.")
        else do
            files <- readPackage root url
            return $ if contentHash files == lockedContentHash locked
                then Right files
                else Left $ folder ++ " does not match termina.lock: its files were changed after termina added it.\n"
                    ++ "Run termina restore to bring it back. If you meant to change the library, change it upstream, "
                    ++ "tag it and point the dependency to the new tag."

-- | Check every package of the lock against its copy in "vendor/".
checkVendor :: FilePath -> Lock -> IO [String]
checkVendor root lock = concatMap (either pure (const [])) <$> mapM (lockedPackage root) (lockPackages lock)
