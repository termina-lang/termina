-- | The manifest, "termina.yaml": its parsing, the configuration of each
-- deployment derived from it, and the check of the folders of the project.
module Configuration.ManifestSpec (spec) where

import Configuration.Configuration
import Configuration.Manifest
import Configuration.Platform

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml (decodeEither')
import System.Directory
import System.FilePath
import Test.Hspec

project, app :: String
project = "project:\n  name: test\n  version: 0.1.0\n  manifest-version: 1\n"
app = "app:\n  platform: rtems5-leon3sf-nexysa7\n  profile: release\n"

parsed :: String -> Either String Manifest
parsed = either (Left . show) parseManifest . decodeEither' . BS.pack

failsWith :: String -> String -> Expectation
failsWith input message =
  case parsed input of
    Left err -> err `shouldSatisfy` isInfixOf message
    Right _ -> expectationFailure "the manifest was accepted"

-- | A project in a fresh folder, made of the given files.
withProject :: String -> [FilePath] -> String -> (FilePath -> Manifest -> IO a) -> IO a
withProject label files yaml action = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "termina-manifest-spec" </> label
  removePathForcibly root
  forM_ files $ \f -> do
    createDirectoryIfMissing True (root </> takeDirectory f)
    writeFile (root </> f) ""
  case parsed yaml of
    Left err -> fail err
    Right manifest -> action root manifest

spec :: Spec
spec = describe "Manifest" $ do

  describe "parsing" $ do

    it "reads an application" $
      fmap manifestNature (parsed (project ++ app))
        `shouldBe` Right (Application (AppDeployment RTEMS5LEON3SFNEXYSA7 Release Nothing))

    it "reads a library" $
      fmap manifestNature (parsed (project ++ "exports:\n  - drivers.gaisler\n"))
        `shouldBe` Right (Library [Prefix [T.pack "drivers", T.pack "gaisler"]])

    it "rejects an output section, since the output is fixed" $
      (project ++ "output:\n  folder: build\n" ++ app) `failsWith` "unknown field \"output\""

    it "rejects a manifest without manifest-version" $
      ("project:\n  name: test\n  version: 0.1.0\n" ++ app) `failsWith` "no project.manifest-version"

    it "rejects the flat manifest of earlier versions" $
      "name: test\nplatform: posix-gcc\nsource-modules: src\noutput-folder: output\nbuilder: make\n"
        `failsWith` "no project.manifest-version"

    it "rejects an unknown manifest-version" $
      ("project:\n  name: test\n  version: 0.1.0\n  manifest-version: 2\n" ++ app)
        `failsWith` "manifest-version 2 is not supported"

    it "suggests the field closest to an unknown one" $
      (project ++ "app:\n  paltform: posix-gcc\n  profile: release\n")
        `failsWith` "did you mean \"platform\"?"

    it "rejects a project with both exports and app" $
      (project ++ app ++ "exports:\n  - drivers\n") `failsWith` "not both"

    it "rejects a project with neither exports nor app" $
      project `failsWith` "needs either exports"

    it "rejects an SSH dependency, with its https:// form and the rule to keep SSH" $ do
      let ssh = "Use https://github.com/uah/drivers instead.\n"
            ++ "If you want git to clone it over SSH, run this once on your computer:\n"
            ++ "  git config --global url.\"git@github.com:\".insteadOf \"https://github.com/\"\n"
      (project ++ app ++ "dependencies:\n  - git: git@github.com:uah/drivers\n    rev: v1\n") `failsWith` ssh
      (project ++ app ++ "dependencies:\n  - git: ssh://git@github.com/uah/drivers\n    rev: v1\n") `failsWith` ssh

    it "rejects a rev that YAML reads as a number" $
      (project ++ app ++ "dependencies:\n  - git: https://github.com/a/b\n    rev: 1.10\n")
        `failsWith` "the rev of https://github.com/a/b is read as a number. Write the tag in quotes, as in rev: \"1.10\""

    it "rejects a dependency that is not https" $
      (project ++ app ++ "dependencies:\n  - git: http://example.com/a/b\n    rev: v1\n")
        `failsWith` "http://example.com/a/b is not an https:// URL, and dependencies are fetched with https://"

    it "rejects a tick that does not divide 1000000" $
      (project ++ "app:\n  platform: posix-gcc\n  profile: release\n  microseconds-per-tick: 7\n")
        `failsWith` "must divide 1000000"

    it "rejects a test without platforms" $
      (project ++ app ++ "tests:\n  hk:\n    platforms: []\n    timeout: 10\n")
        `failsWith` "has no platforms"

    it "rejects a test name that is not snake_case" $
      (project ++ app ++ "tests:\n  HkTask:\n    platforms: [posix-gcc]\n    timeout: 10\n")
        `failsWith` "not lowercase snake_case"

    it "reads the entry of a test" $
      fmap (M.toList . manifestTests)
        (parsed (project ++ app ++ "tests:\n  hk:\n    platforms: [posix-gcc, rtems5-leon3sf-nexysa7]\n    timeout: 10\n"))
        `shouldBe` Right [(T.pack "hk", TestEntry [POSIXGCC, RTEMS5LEON3SFNEXYSA7] 10 defaultTestLogSize Nothing Nothing emptyExternConfig [])]

  describe "deployments" $ do

    let withTick = project ++ "app:\n  platform: rtems5-leon3sf-nexysa7\n  profile: release\n  microseconds-per-tick: 1000\n"
          ++ "tests:\n  hk:\n    platforms: [posix-gcc]\n    timeout: 10\n"
          ++ "  timing:\n    platforms: [posix-gcc]\n    timeout: 10\n    microseconds-per-tick: 500\n"

    it "gives a test without a tick the one of the application" $
      case parsed withTick of
        Right m -> microsecondsPerTick (testConfig m (T.pack "hk") (manifestTests m M.! T.pack "hk") POSIXGCC Debug) `shouldBe` 1000
        Left err -> expectationFailure err

    it "keeps the tick of a test that sets its own" $
      case parsed withTick of
        Right m -> microsecondsPerTick (testConfig m (T.pack "timing") (manifestTests m M.! T.pack "timing") POSIXGCC Debug) `shouldBe` 500
        Left err -> expectationFailure err

    it "gives a test the size of its record and the stack of its harness" $
      case parsed (project ++ app ++ "tests:\n  hk:\n    platforms: [posix-gcc]\n    timeout: 10\n"
                     ++ "    test-log-size: 2048\n    harness-stack-size: 8192\n") of
        Right m -> do
          let config = testConfig m (T.pack "hk") (manifestTests m M.! T.pack "hk") POSIXGCC Debug
          recordCapacity config `shouldBe` 2048
          harnessStackSize config `shouldBe` Just 8192
        Left err -> expectationFailure err

    it "gives a test that says nothing the default record and the stack of the platform" $
      case parsed withTick of
        Right m -> do
          let config = testConfig m (T.pack "hk") (manifestTests m M.! T.pack "hk") POSIXGCC Debug
          recordCapacity config `shouldBe` defaultTestLogSize
          harnessStackSize config `shouldBe` Nothing
        Left err -> expectationFailure err

    it "rejects a harness stack size that is not positive" $
      failsWith (project ++ app ++ "tests:\n  hk:\n    platforms: [posix-gcc]\n    timeout: 10\n    harness-stack-size: 0\n")
        "harness-stack-size must be positive"

    it "builds each test on each platform into its own folder" $
      case parsed withTick of
        Right m -> outputFolder (testConfig m (T.pack "hk") (manifestTests m M.! T.pack "hk") POSIXGCC Debug)
          `shouldBe` "output" </> "tests" </> "hk" </> "posix-gcc"
        Left err -> expectationFailure err

    it "prints every deployment with the #line pragmas and as plain C, each in its folder" $
      case parsed withTick of
        Right m -> map (\d -> map (\c -> (outputFolder c, lineDirectives c)) [deploymentConfig d, deploymentPlainConfig d])
            (maybe [] pure (appDeployment m) ++ [testDeployment m (T.pack "hk") (manifestTests m M.! T.pack "hk") POSIXGCC Release])
          `shouldBe`
            [ [("output" </> "app", True), ("output" </> "plain" </> "app", False)]
            , [ ("output" </> "tests" </> "hk" </> "posix-gcc", True)
              , ("output" </> "plain" </> "tests" </> "hk" </> "posix-gcc", False) ] ]
        Left err -> expectationFailure err

    it "has no application in a library" $
      fmap appConfig (parsed (project ++ "exports:\n  - drivers\n")) `shouldBe` Right Nothing

  describe "layout" $ do

    let testsYaml = project ++ app ++ "tests:\n  hk:\n    platforms: [posix-gcc]\n    timeout: 10\n"

    it "accepts a project that matches its manifest" $
      withProject "ok" ["app/app.fin", "src/obdh/tasks/hk.fin", "tests/hk/test.fin", "tests/hk/src/fakes/clock.fin"] testsYaml
        (\root m -> checkLayout root m) `shouldReturn` []

    it "reports every mismatch at once" $ do
      errors <- withProject "bad" ["app/app.fin", "app/other.fin", "src/top.fin", "src/Obdh/hk.fin", "tests/extra/test.fin"] testsYaml
        (\root m -> checkLayout root m)
      -- app/other.fin, src/top.fin, src/Obdh, tests/extra undeclared, tests/hk missing
      length errors `shouldBe` 5

    it "rejects a module of a test with the name of one of the project" $ do
      errors <- withProject "shadow" ["app/app.fin", "src/util/maths.fin", "tests/hk/test.fin", "tests/hk/src/util/maths.fin"] testsYaml
        (\root m -> checkLayout root m)
      errors `shouldBe` ["tests/hk/src/util/maths.fin: the project has a module with the same name in src/"]

    it "rejects an app/ folder in a library" $ do
      errors <- withProject "lib" ["app/app.fin"] (project ++ "exports:\n  - drivers\n")
        (\root m -> checkLayout root m)
      errors `shouldSatisfy` any (isInfixOf "cannot have an app/ folder")

    it "rejects an application without app/" $ do
      errors <- withProject "noapp" ["src/obdh/hk.fin"] (project ++ app)
        (\root m -> checkLayout root m)
      errors `shouldSatisfy` any (isInfixOf "there is no app/ folder")

  it "writes a manifest for a new application that it reads back" $
    parsed (newApplicationManifest "demo" POSIXGCC) `shouldSatisfy` isRight

  it "writes a manifest for a new library that it reads back" $
    fmap manifestNature (parsed (newLibraryManifest "my-lib" "my_lib"))
      `shouldBe` Right (Library [Prefix [T.pack "my_lib"]])

  describe "new tests" $ do

    let nexys = project ++ app

    it "runs a new test on the host and on the platform of the application" $
      fmap defaultTestPlatforms (parsed nexys) `shouldBe` Right [POSIXGCC, RTEMS5LEON3SFNEXYSA7]

    it "adds a tests section at the end when there is none" $
      addTestEntry nexys "hk" [POSIXGCC] 10
        `shouldBe` Right (nexys ++ "\ntests:\n  hk:\n    platforms: [posix-gcc]\n    timeout: 10\n")

    it "adds the entry at the end of the tests section, keeping the rest" $ do
      let original = project ++ "tests:   # unit tests\n    old:\n        platforms: [posix-gcc]\n        timeout: 5\n\n# the application\n" ++ app
      addTestEntry original "hk" [POSIXGCC] 10
        `shouldBe` Right (project ++ "tests:   # unit tests\n    old:\n        platforms: [posix-gcc]\n        timeout: 5\n"
          ++ "    hk:\n        platforms: [posix-gcc]\n        timeout: 10\n\n# the application\n" ++ app)

    it "gives a manifest that reads back with the new test" $
      (addTestEntry nexys "hk" [POSIXGCC, RTEMS5LEON3SFNEXYSA7] 10 >>= parsed) `shouldSatisfy`
        either (const False) (M.member (T.pack "hk") . manifestTests)

    it "refuses a tests section in flow style" $
      addTestEntry (project ++ app ++ "tests: {}\n") "hk" [POSIXGCC] 10 `shouldSatisfy` isLeft

  describe "new dependencies" $ do

    let nexys = project ++ app
        pus = T.pack "https://github.com/acme/pus"
        v1 = T.pack "v0.3.1"
        hk = "tests:\n  hk:\n    platforms: [posix-gcc]\n    timeout: 10\n"

    it "adds a dependencies section at the end when there is none" $
      addDependencyEntry nexys Nothing pus v1
        `shouldBe` Right (nexys ++ "\ndependencies:\n  - git: https://github.com/acme/pus\n    rev: \"v0.3.1\"\n")

    it "adds the entry at the end of the dependencies, keeping the rest" $ do
      let original = project ++ "dependencies:   # flight\n    - git: https://github.com/acme/ccsds\n      rev: v1.0\n\n# the application\n" ++ app
      addDependencyEntry original Nothing pus v1
        `shouldBe` Right (project ++ "dependencies:   # flight\n    - git: https://github.com/acme/ccsds\n      rev: v1.0\n"
          ++ "    - git: https://github.com/acme/pus\n      rev: \"v0.3.1\"\n\n# the application\n" ++ app)

    it "adds the entry to an indentless sequence" $
      addDependencyEntry (nexys ++ "dependencies:\n- git: https://github.com/acme/ccsds\n  rev: v1.0\n") Nothing pus v1
        `shouldBe` Right (nexys ++ "dependencies:\n- git: https://github.com/acme/ccsds\n  rev: v1.0\n- git: https://github.com/acme/pus\n  rev: \"v0.3.1\"\n")

    it "adds the dependencies of a test, keeping the tests after it" $
      addDependencyEntry (nexys ++ hk ++ "  tm:\n    platforms: [posix-gcc]\n    timeout: 10\n") (Just "hk") pus v1
        `shouldBe` Right (nexys ++ hk ++ "    dependencies:\n      - git: https://github.com/acme/pus\n        rev: \"v0.3.1\"\n"
          ++ "  tm:\n    platforms: [posix-gcc]\n    timeout: 10\n")

    it "gives a manifest that reads back with the new dependency" $ do
      let deps m = (manifestDependencies m, map testDependencies (M.elems (manifestTests m)))
          dep = DepGit (GitDependency pus v1)
      fmap deps (addDependencyEntry (nexys ++ hk) Nothing pus v1 >>= parsed) `shouldBe` Right ([dep], [[]])
      fmap deps (addDependencyEntry (nexys ++ hk) (Just "hk") pus v1 >>= parsed) `shouldBe` Right ([], [[dep]])

    it "refuses a dependencies section in flow style, and a test that does not exist" $ do
      addDependencyEntry (nexys ++ "dependencies: []\n") Nothing pus v1 `shouldSatisfy` isLeft
      addDependencyEntry (nexys ++ hk) (Just "tm") pus v1 `shouldBe` Left "the manifest declares no test tm"

    it "rejects the overrides of a test, which apply to the whole project" $
      (nexys ++ hk ++ "    overrides:\n      - git: https://github.com/acme/pus\n        rev: v1\n")
        `failsWith` "they go at the top level of the manifest"
