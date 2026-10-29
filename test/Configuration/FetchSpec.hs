{-# LANGUAGE OverloadedStrings #-}

-- | The fetch of a package, against repositories that each test creates on
-- disk and reaches through file://.
module Configuration.FetchSpec (spec) where

import Configuration.Fetch

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Process (readProcess)
import Test.Hspec

import Configuration.Helpers (git)

-- | A fresh repository with a module written with CRLF, a symbolic link and
-- a commit tagged v0.1.0 (and v0.2.0, annotated), and its URL.
repository :: String -> IO (FilePath, T.Text)
repository label = do
    tmp <- getTemporaryDirectory
    let root = tmp </> "termina-fetch-spec" </> label
        repo = root </> "upstream"
    removePathForcibly root
    createDirectoryIfMissing True (repo </> "src" </> "pus")
    _ <- readProcess "git" ["init", "-q", repo] ""
    BS.writeFile (repo </> "termina.yaml") "project:\n  name: pus\n"
    BS.writeFile (repo </> "src" </> "pus" </> "service1.fin") "// crlf\r\n"
    createFileLink "service1.fin" (repo </> "src" </> "pus" </> "link.fin")
    _ <- git repo ["add", "."]
    _ <- git repo ["commit", "-q", "-m", "first"]
    _ <- git repo ["tag", "v0.1.0"]
    _ <- git repo ["tag", "-a", "-m", "annotated", "v0.2.0"]
    return (root, T.pack ("file://" ++ repo))

headOf :: FilePath -> IO T.Text
headOf root = T.pack . takeWhile (/= '\n') <$> git (root </> "upstream") ["rev-parse", "HEAD"]

spec :: Spec
spec = do

  describe "tag" $ do

    it "resolves a tag, and an annotated one, to the commit" $ do
      (root, url) <- repository "resolve"
      commit <- headOf root
      resolveTag url "v0.1.0" `shouldReturn` Right commit
      resolveTag url "v0.2.0" `shouldReturn` Right commit

    it "rejects a tag that the repository does not have" $ do
      (_, url) <- repository "missing"
      resolveTag url "v9.9.9" `shouldReturn` Left (TagNotFound url "v9.9.9")

  describe "files of a commit" $ do

    it "reads the bytes and modes as git stores them, line endings and links included" $ do
      (root, url) <- repository "files"
      commit <- headOf root
      fetchPackage root url "v0.1.0" commit `shouldReturn` Right [
          FetchedFile "src/pus/link.fin" "120000" "service1.fin",
          FetchedFile "src/pus/service1.fin" "100644" "// crlf\r\n",
          FetchedFile "termina.yaml" "100644" "project:\n  name: pus\n"
        ]

    it "rejects a tag that points to another commit than the expected one" $ do
      (root, url) <- repository "moved"
      old <- headOf root
      _ <- git (root </> "upstream") ["commit", "-q", "--allow-empty", "-m", "second"]
      _ <- git (root </> "upstream") ["tag", "-f", "v0.1.0"]
      new <- headOf root
      fetchPackage root url "v0.1.0" old `shouldReturn` Left (TagMoved "v0.1.0" old new)

    it "takes a clone of the prefetch folder instead of the repository" $ do
      (root, upstream) <- repository "prefetch"
      commit <- headOf root
      let url = "https://example.invalid/termina/pus"
      _ <- readProcess "git" ["-c", "advice.detachedHead=false", "clone", "-q", "--branch", "v0.1.0", "--depth", "1", T.unpack upstream, prefetchDir root url] ""
      fmap (map fetchedPath) <$> fetchPackage root url "v0.1.0" commit
        `shouldReturn` Right ["src/pus/link.fin", "src/pus/service1.fin", "termina.yaml"]
