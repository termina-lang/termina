{-# LANGUAGE OverloadedStrings #-}

-- | The validation of a fetched package, over lists of files written by hand.
module Configuration.ValidateSpec (spec) where

import Configuration.Fetch
import Configuration.Validate

import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import Test.Hspec

import Configuration.Helpers (header)

manifest :: String -> FetchedFile
manifest body = FetchedFile "termina.yaml" "100644" (BS.pack (header "pus" ++ body))

library :: String
library = "exports: [pus]\nextern:\n  pus.crc:\n    sources: [crc.c]\n    libraries:\n      posix-gcc: [libcrc.a]\n"

file :: String -> FetchedFile
file path = FetchedFile path "100644" ""

link :: String -> FetchedFile
link path = FetchedFile path "120000" "../elsewhere"

package :: [FetchedFile]
package = [manifest library, file "src/pus/service1.fin", file "extern/src/crc.c", file "extern/lib/posix-gcc/libcrc.a",
    file "extern/notes.txt", file "README.md"]

errorsOf :: [FetchedFile] -> [String]
errorsOf files = either id (const []) (validatePackage files)

spec :: Spec
spec = do

  describe "manifest" $ do

    it "accepts a library and gives its manifest" $
      validatePackage package `shouldSatisfy` isRight

    it "rejects a package without termina.yaml" $
      errorsOf (tail package) `shouldBe` ["the package has no termina.yaml"]

    it "rejects an application" $
      errorsOf [manifest "app:\n  platform: posix-gcc\n  profile: release\n"]
        `shouldBe` ["the package is an application: it has no exports, so it cannot be a dependency"]

    it "rejects a path dependency" $
      errorsOf (manifest "exports: [pus]\ndependencies:\n  - path: ../hal\n" : [file "src/pus/service1.fin"])
        `shouldBe` ["the package declares the path dependency ../hal, which only exists on the machine of its author"]

  describe "extern" $ do

    it "rejects a missing file and a pattern that matches none" $
      errorsOf (manifest "exports: [pus]\nextern:\n  pus.crc:\n    sources: [crc.c, \"spi/*.c\"]\n" : file "src/pus/service1.fin" : [])
        `shouldBe` [ "extern/src/crc.c: the extern section lists a file that the package does not have"
                   , "extern/src/spi/*.c: the extern section lists a pattern that matches no file of the package" ]

    it "accepts a pattern that matches some file" $
      validatePackage [manifest "exports: [pus]\nextern:\n  pus.crc:\n    sources: [\"spi/*.c\"]\n", file "src/pus/service1.fin", file "extern/src/spi/a.c"]
        `shouldSatisfy` isRight

  describe "links and submodules" $ do

    it "rejects them where files are copied, and on the way there" $
      errorsOf (package ++ [link "src/pus/drivers", FetchedFile "src/pus/sub" "160000" "", link "extern/lib/posix-gcc"])
        `shouldBe` [ "src/pus/drivers: a symbolic link, which a package cannot hold where its files are copied"
                   , "src/pus/sub: a submodule, which a package cannot hold where its files are copied"
                   , "extern/lib/posix-gcc: a symbolic link, which a package cannot hold where its files are copied" ]

    it "ignores them where nothing is copied" $
      validatePackage (package ++ [link "docs/latest", link "src/util/crc.fin", link "extern/old.c"])
        `shouldSatisfy` isRight

  describe "case" $

    it "rejects two copied paths that differ only in case" $
      errorsOf (package ++ [file "src/pus/Service1.fin", file "README.MD"])
        `shouldBe` ["src/pus/Service1.fin and src/pus/service1.fin differ only in case"]
