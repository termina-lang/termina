{-# LANGUAGE OverloadedStrings #-}
-- | How termina deliver reads the dependency files and the values of make,
-- and rewrites their paths for the flat Makefile of the deliverable.
module Configuration.DeliverSpec (spec) where

import Command.Deliver (dependencies, inFolder, pathsIn, relocatePaths, toolchainVariables, releaseBuild, genDoxyfile, genDoxygenGroups, naturalKey)
import Configuration.Configuration
import Configuration.Manifest

import qualified Data.ByteString.Char8 as BS
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml (decodeEither')
import Configuration.Platform (Platform(..))
import System.FilePath
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

  describe "relocatePaths" $ do

    it "makes the output folder the folder of the deliverable" $
      map (relocatePaths substitutions) ["/home/u/obdh/output/app/include", "/home/u/obdh/output/app"]
        `shouldBe` ["include", "."]

    it "moves the OSAL to its folder in the deliverable" $
      relocatePaths substitutions "-T/opt/termina/osal/platform/x.ld" `shouldBe` "-Tosal/platform/x.ld"

    it "writes back the variable of the toolchain" $
      relocatePaths substitutions "-B/opt/rcc/lib" `shouldBe` "-B$(TERMINA_RCC_DIR)/lib"

    it "leaves a folder that only contains another as its middle part" $
      relocatePaths substitutions "/home/u/opt/rcc/lib" `shouldBe` "/home/u/opt/rcc/lib"

    it "leaves a folder that only shares the beginning of the name" $
      relocatePaths substitutions "/opt/rcc-1.3.2/bin" `shouldBe` "/opt/rcc-1.3.2/bin"

    it "gives a file of the substitutions its path in the deliverable" $
      relocatePaths (("/home/u/obdh/assets/linker/linkcmds", "ld/linkcmds") : substitutions) "-T/home/u/obdh/assets/linker/linkcmds"
        `shouldBe` "-Tld/linkcmds"

    it "rewrites a path after a comma and an =" $
      relocatePaths substitutions "-Wl,-Map=/home/u/obdh/output/app/bin/x.map" `shouldBe` "-Wl,-Map=bin/x.map"

  describe "toolchainVariables" $ do

    it "takes the variables that CROSS refers to" $
      toolchainVariables (M.fromList [("CROSS", "$(TERMINA_RCC_DIR)/bin/sparc-gaisler-rtems5-")])
        `shouldBe` ["TERMINA_RCC_DIR"]

    it "takes none from an empty CROSS" $
      toolchainVariables (M.fromList [("CROSS", "")]) `shouldBe` []

  describe "releaseBuild" $

    it "builds an application in debug in release under .termina/deliver, out of output" $ do
      let yaml = "project:\n  name: demo\n  version: 0.1.0\n  manifest-version: 1\napp:\n  platform: posix-gcc\n  profile: debug\n"
      case either (Left . show) parseManifest (decodeEither' (BS.pack yaml)) >>= maybe (Left "no application") Right . appDeployment of
        Left err -> expectationFailure err
        Right deployment -> do
          let moved = releaseBuild deployment
          map (\c -> (profile c, outputFolder c)) [deploymentConfig moved, deploymentPlainConfig moved]
            `shouldBe` [(Release, ".termina" </> "deliver" </> "app"), (Release, ".termina" </> "deliver" </> "plain" </> "app")]
          profile (deploymentConfig deployment) `shouldBe` Debug

  describe "genDoxyfile" $ do

    let info = ProjectInfo { projectName = "icu-asw", projectVersion = "1.2.0", projectManifestVersion = 1, projectAssurance = [], projectDescription = Nothing }

    it "documents the C of the deliverable from its root, with the folders of the extern C and without the vendored files of the OSAL" $
      genDoxyfile info (defaultConfig "icu-asw" POSIXGCC) ["extern/icu"] True `shouldBe` T.unlines
        [ "PROJECT_NAME          = \"icu-asw\""
        , "PROJECT_NUMBER        = 1.2.0"
        , "OUTPUT_DIRECTORY      = doc"
        , "INPUT                 = groups.dox include src osal extern/icu"
        , "RECURSIVE             = YES"
        , "EXCLUDE               = osal/vendor"
        , "FILE_PATTERNS         = *.h *.c"
        , "OPTIMIZE_OUTPUT_FOR_C = YES"
        , "EXTRACT_STATIC        = YES"
        , "MARKDOWN_SUPPORT      = YES"
        , "WARN_IF_UNDOCUMENTED  = NO"
        , "QUIET                 = YES"
        , "GENERATE_LATEX        = NO"
        , "HAVE_DOT              = YES"
        , "DOT_IMAGE_FORMAT      = svg"
        , "INTERACTIVE_SVG       = YES"
        , "DOT_GRAPH_MAX_NODES   = 100"
        ]

    it "leaves the graphs out with --no-graphs" $
      drop 12 (T.lines (genDoxyfile info (defaultConfig "icu-asw" POSIXGCC) [] False))
        `shouldBe` ["GENERATE_LATEX        = NO", "HAVE_DOT              = NO"]

    it "names the test in the project of a deliverable of a test" $
      take 1 (T.lines (genDoxyfile info ((defaultConfig "uart" POSIXGCC) { deploymentKind = TestBuild "uart" }) [] True))
        `shouldBe` ["PROJECT_NAME          = \"icu-asw, test uart\""]

  describe "genDoxygenGroups" $ do

    let app = ProjectInfo { projectName = "icu-asw", projectVersion = "1.2.0", projectManifestVersion = 1, projectAssurance = [], projectDescription = Just "Software of the ICU." }
        pus = ProjectInfo { projectName = "termina-pus", projectVersion = "0.1.0", projectManifestVersion = 1, projectAssurance = [], projectDescription = Nothing }
        vendored = "vendor" </> "github.com" </> "acme" </> "termina-pus"
        modules =
          [ ("icu/manager", "src" </> "icu" </> "manager.fin")
          , ("icu/manager/executor", "src" </> "icu" </> "manager" </> "executor.fin")
          , ("pus/services/service12/internal", vendored </> "src" </> "pus" </> "services" </> "service12" </> "internal.fin")
          , ("pus/services/service3/internal", vendored </> "src" </> "pus" </> "services" </> "service3" </> "internal.fin") ]
        groups = T.lines (genDoxygenGroups app [(vendored, pus)] (M.fromList [(["icu", "manager"], "Manager task.")]) modules)
        defined = [ T.unwords (drop 2 (T.words l)) | l <- groups, " * @defgroup " `T.isPrefixOf` l ]

    it "defines the application, its namespaces, the libraries with theirs in natural order, and the OSAL" $
      defined `shouldBe`
        [ "termina__project Application icu-asw", "termina__ns__icu icu", "termina__ns__icu__manager icu.manager"
        , "termina__libraries Libraries", "termina__package__termina_pus termina-pus 0.1.0"
        , "termina__ns__pus pus", "termina__ns__pus__services pus.services"
        , "termina__ns__pus__services__service3 pus.services.service3", "termina__ns__pus__services__service12 pus.services.service12"
        , "termina__osal OSAL", "termina__osal__api OSAL api", "termina__osal__shared OSAL shared"
        , "termina__osal__os OSAL os", "termina__osal__platform OSAL platform" ]

    it "gives the application and a package their description or one with their version, and a namespace the one found for it" $ do
      take 4 groups `shouldBe` ["/**", " * @defgroup termina__project Application icu-asw", " *", " * @brief Software of the ICU."]
      groups `shouldContain` [" * @brief Library termina-pus, version 0.1.0."]
      groups `shouldContain` [" * @brief Manager task."]

    it "puts each namespace in its parent, the first level of a package in the package" $ do
      let after label = takeWhile (/= " */") (drop 1 (dropWhile (not . T.isInfixOf label) groups))
      after "@defgroup termina__ns__pus " `shouldContain` [" * @ingroup termina__package__termina_pus"]
      after "@defgroup termina__ns__icu__manager " `shouldContain` [" * @ingroup termina__ns__icu"]
      after "@defgroup termina__ns__icu " `shouldContain` [" * @ingroup termina__project"]

  describe "naturalKey" $

    it "sorts the numbers in names by their value" $
      sortOn naturalKey ["service12", "service3", "service1", "a10b2", "a10b10"] `shouldBe` ["a10b2", "a10b10", "service1", "service3", "service12"]
