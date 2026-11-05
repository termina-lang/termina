-- | What termina doctor reads from a devcontainer.json and from the
-- platform.mk of a platform.
module Command.DoctorSpec (spec) where

import Command.Doctor

import Test.Hspec

spec :: Spec
spec = describe "termina doctor" $ do

  describe "devcontainerImageTag" $ do

    it "takes the tag of the image, in a file with comments" $
      devcontainerImageTag "{\n    // the image of termina\n    \"image\": \"ghcr.io/termina-lang/docker-termina:0.7\",\n}\n"
        `shouldBe` Just "0.7"

    it "gives nothing when the file names no docker-termina image" $
      devcontainerImageTag "{ \"image\": \"ubuntu:24.04\" }" `shouldBe` Nothing

  describe "minorVersion" $ do

    it "takes MAJOR.MINOR of a version, of a tag and of an old tag with a v" $
      map minorVersion ["0.7", "0.7.1", "v0.3.0"] `shouldBe` [Just "0.7", Just "0.7", Just "0.3"]

    it "gives nothing for a tag without a version" $
      map minorVersion ["latest", "0", "0.x"] `shouldBe` [Nothing, Nothing, Nothing]

  describe "toolchainLine" $ do

    let rcc = Toolchain "/opt/rcc/bin/sparc-gaisler-rtems5-gcc" "/opt/rcc/bin/sparc-gaisler-rtems5-objcopy"
            "$(TERMINA_RCC_DIR)/bin/sparc-gaisler-rtems5-"

    it "names the variable to set when the compiler is not found" $
      toolchainLine rcc Nothing False
        `shouldBe` Left "/opt/rcc/bin/sparc-gaisler-rtems5-gcc not found (install it there or set TERMINA_RCC_DIR)"

    it "gives the version of the compiler, and says when objcopy is missing" $ do
      toolchainLine rcc (Just "sparc-gaisler-rtems5-gcc 10.2.1") True `shouldBe` Right "sparc-gaisler-rtems5-gcc 10.2.1"
      toolchainLine rcc (Just "sparc-gaisler-rtems5-gcc 10.2.1") False
        `shouldBe` Right "sparc-gaisler-rtems5-gcc 10.2.1; /opt/rcc/bin/sparc-gaisler-rtems5-objcopy not found, so there are no hashes and no deliver"
