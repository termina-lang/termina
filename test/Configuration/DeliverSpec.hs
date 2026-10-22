-- | How termina deliver reads the dependency files and the values of make,
-- and rewrites their paths for the flat Makefile of the deliverable.
module Configuration.DeliverSpec (spec) where

import Command.Deliver (dependencies, inFolder, pathsIn, relocate, toolchainVariables)

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Test.Hspec

substitutions :: [(String, String)]
substitutions = [
    ("/home/u/obdh/output/app", ""),
    ("/opt/termina/osal", "osal"),
    ("/opt/rcc", "$(TERMINA_RCC_DIR)")
  ]

spec :: Spec
spec = describe "termina deliver" $ do

  describe "dependencies" $ do

    it "reads the prerequisites across continued lines" $
      dependencies "bin/main.o: /p/main.c \\\n  /opt/osal/api/termina.h \\\n  /opt/osal/api/termina/types.h\n"
        `shouldBe` ["/p/main.c", "/opt/osal/api/termina.h", "/opt/osal/api/termina/types.h"]

    it "reads nothing from the phony rules of -MP" $
      dependencies "/opt/osal/api/termina.h:\n\n/opt/osal/api/termina/types.h:\n"
        `shouldBe` []

    it "unescapes spaces, # and $ in a path" $
      dependencies "bin/a.o: /my\\ dir/a.c /x/b\\#1.h /x/c$$.h\n"
        `shouldBe` ["/my dir/a.c", "/x/b#1.h", "/x/c$.h"]

  describe "inFolder" $ do

    it "takes a path inside the folder" $
      inFolder "/opt/termina/osal" "/opt/termina/osal/api/termina.h" `shouldBe` True

    it "takes a path that climbs inside the folder" $
      inFolder "/opt/termina/osal" "/opt/termina/osal/os/rtems/src/../include/x.h" `shouldBe` True

    it "leaves out a path that climbs out of the folder" $
      inFolder "/opt/termina/osal" "/opt/termina/osal/api/../../other/x.h" `shouldBe` False

    it "leaves out a folder that only shares the beginning of the name" $
      inFolder "/opt/rcc" "/opt/rcc-1.3.2/bin/gcc" `shouldBe` False

  describe "pathsIn" $
    it "finds the path after a one-letter option, an = and a ," $
      filter ("/" `isPrefixOf`) (concatMap pathsIn ["-T/opt/termina/osal/x.ld", "-Wl,-Map=/a/b.map", "-isystem", "/opt/rcc/include"])
        `shouldBe` ["/opt/termina/osal/x.ld", "/a/b.map", "/opt/rcc/include"]

  describe "relocate" $ do

    it "makes the output folder the folder of the deliverable" $
      map (relocate substitutions) ["/home/u/obdh/output/app/include", "/home/u/obdh/output/app"]
        `shouldBe` ["include", "."]

    it "moves the OSAL to its folder in the deliverable" $
      relocate substitutions "-T/opt/termina/osal/platform/x.ld" `shouldBe` "-Tosal/platform/x.ld"

    it "writes back the variable of the toolchain" $
      relocate substitutions "-B/opt/rcc/lib" `shouldBe` "-B$(TERMINA_RCC_DIR)/lib"

    it "leaves a folder that only contains another as its middle part" $
      relocate substitutions "/home/u/opt/rcc/lib" `shouldBe` "/home/u/opt/rcc/lib"

    it "leaves a folder that only shares the beginning of the name" $
      relocate substitutions "/opt/rcc-1.3.2/bin" `shouldBe` "/opt/rcc-1.3.2/bin"

    it "rewrites a path after a comma and an =" $
      relocate substitutions "-Wl,-Map=/home/u/obdh/output/app/bin/x.map" `shouldBe` "-Wl,-Map=bin/x.map"

  describe "toolchainVariables" $ do

    it "takes the variables that CROSS refers to" $
      toolchainVariables (M.fromList [("CROSS", "$(TERMINA_RCC_DIR)/bin/sparc-gaisler-rtems5-")])
        `shouldBe` ["TERMINA_RCC_DIR"]

    it "takes none from an empty CROSS" $
      toolchainVariables (M.fromList [("CROSS", "")]) `shouldBe` []
