{-# LANGUAGE OverloadedStrings #-}

-- | The copies of the dependencies in "vendor/" and the hash of their content.
module Configuration.VendorSpec (spec) where

import Configuration.Lock
import Configuration.Manifest
import Configuration.Vendor

import qualified Data.ByteString.Char8 as BS
import Data.List (sort)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import Test.Hspec

import Configuration.Helpers

package :: [VendoredFile]
package = [("termina.yaml", "a\n"), ("src/pus/x.fin", "b\r\n")]

library :: Manifest
library = manifestOf $ header "pus"
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

  describe "check against the lock" $

    it "accepts an intact copy, and rejects a modified one and a missing one" $ do
      tmp <- getTemporaryDirectory
      let root = tmp </> "termina-vendor-spec-check"
          locked name = LockedPackage ("https://github.com/acme/" <> name) "v1" (T.pack (replicate 40 'a')) (contentHash package)
      removePathForcibly root
      writePackage root "https://github.com/acme/pus" package
      writePackage root "https://github.com/acme/tm" package
      BS.writeFile (root </> "vendor" </> "github.com" </> "acme" </> "tm" </> "termina.yaml") "changed\n"
      checkVendor root (Lock 1 [locked "pus", locked "tm", locked "ccsds"]) `shouldReturn`
        [ "vendor/github.com/acme/tm does not match termina.lock: its files were changed after termina added it.\n"
            ++ "Run termina restore to bring it back. If you meant to change the library, change it upstream, "
            ++ "tag it and point the dependency to the new tag."
        , "vendor/github.com/acme/ccsds is missing. Run termina restore to bring it back." ]

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
