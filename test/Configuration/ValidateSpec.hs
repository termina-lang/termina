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

-- | The module whose extern function the C of the package defines.
crcModule :: FetchedFile
crcModule = FetchedFile "src/pus/crc.fin" "100644" "extern function pus_crc16() -> u16;\n"

package :: [FetchedFile]
package = [manifest library, file "src/pus/service1.fin", crcModule, file "extern/src/crc.c", file "extern/lib/posix-gcc/libcrc.a",
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
      errorsOf [manifest "exports: [pus]\nextern:\n  pus.crc:\n    sources: [crc.c, \"spi/*.c\"]\n", file "src/pus/service1.fin", crcModule]
        `shouldBe` [ "The extern section of the package lists extern/src/crc.c under pus.crc, and the file does not exist"
                   , "The extern section of the package lists extern/src/spi/*.c under pus.crc, and the pattern matches no file" ]

    it "accepts a pattern that matches some file" $
      validatePackage [manifest "exports: [pus]\nextern:\n  pus.crc:\n    sources: [\"spi/*.c\"]\n", file "src/pus/service1.fin", crcModule,
                       file "extern/src/spi/a.c"]
        `shouldSatisfy` isRight

    it "rejects an exported module with an extern function and no entry" $
      errorsOf (package ++ [FetchedFile "src/pus/tm.fin" "100644" "extern function pus_tm_raw() -> u32;\n"])
        `shouldBe` ["The module pus.tm declares the extern function pus_tm_raw and is not listed in the extern section of the package"]

    it "ignores the C of a module that it does not export" $
      validatePackage (manifest (library ++ "  test_support.fake:\n    sources: [fake.c]\n")
                       : tail package ++ [FetchedFile "src/test_support/fake.fin" "100644" "extern function fake() -> u32;\n", file "extern/src/fake.c"])
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
