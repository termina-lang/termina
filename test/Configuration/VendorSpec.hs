{-# LANGUAGE OverloadedStrings #-}

-- | The copies of the dependencies in "vendor/" and the hash of their content.
module Configuration.VendorSpec (spec) where

import Configuration.Manifest
import Configuration.Vendor

import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import Data.Yaml (decodeEither')
import System.Directory
import System.FilePath
import Test.Hspec

package :: [VendoredFile]
package = [("termina.yaml", "a\n"), ("src/pus/x.fin", "b\r\n")]

manifestOf :: String -> Manifest
manifestOf yaml = either error id (either (Left . show) parseManifest (decodeEither' (BS.pack yaml)))

library :: Manifest
library = manifestOf $ "project:\n  name: pus\n  version: 0.1.0\n  manifest-version: 1\n"
    ++ "exports:\n  - pus.service1\n  - ccsds\n"
    ++ "extern:\n  sources: [crc.c]\n  libraries:\n    posix-gcc: [posix/libcrc.a]\n"

spec :: Spec
spec = do

  describe "content hash" $ do

    it "is the hash of the output of sha256sum over the files, checked in a shell" $
      contentHash package `shouldBe` "sha256:3cd06287cae2c5d55ff3d435270af59e6270f789e85dfa22942a8595f9229c7e"

    it "does not depend on the order of the files, and changes with a byte or a path" $ do
      contentHash (reverse package) `shouldBe` contentHash package
      contentHash [("termina.yaml", "a\n"), ("src/pus/x.fin", "b\n")] `shouldNotBe` contentHash package
      contentHash [("termina.yaml", "a\n"), ("src/pus/y.fin", "b\r\n")] `shouldNotBe` contentHash package

  describe "files that go to vendor" $

    it "keeps the manifest, the modules of the exported prefixes and the listed extern files" $
      filter (isVendored library) [
          "termina.yaml", "src/pus/service1.fin", "src/pus/service1/tm.fin", "src/ccsds/tc/frame.fin",
          "src/pus/service2.fin", "src/test_support/fake.fin", "src/ccsds/README.md", "extern/crc.c",
          "extern/posix/libcrc.a", "extern/notes.c", "tests/t1/test.fin", "README.md"
        ] `shouldBe` ["termina.yaml", "src/pus/service1.fin", "src/pus/service1/tm.fin", "src/ccsds/tc/frame.fin",
          "extern/crc.c", "extern/posix/libcrc.a"]

  describe "vendor folder" $

    it "writes a package under its normalized URL, byte for byte, and reads it back" $ do
      tmp <- getTemporaryDirectory
      let root = tmp </> "termina-vendor-spec"
          url = "https://github.com/Termina-Lang/PUS.git"
      removePathForcibly root
      writePackage root url [("src/old.fin", "")]
      writePackage root url package
      doesFileExist (root </> "vendor" </> "github.com" </> "termina-lang" </> "pus" </> "termina.yaml") `shouldReturn` True
      files <- readPackage root url
      sort files `shouldBe` sort package
      contentHash files `shouldBe` contentHash package
      BS.readFile (root </> "vendor" </> ".gitattributes") `shouldReturn` "* -text\n"
