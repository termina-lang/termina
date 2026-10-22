-- | The manifest, "termina.yaml": its parsing, the configuration of each
-- deployment derived from it, and the check of the folders of the project.
module Configuration.ManifestSpec (spec) where

import Configuration.Configuration
import Configuration.Manifest
import Configuration.Platform

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import Data.Either (isRight)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml (decodeEither')
import System.Directory
import System.FilePath
import Test.Hspec

project, output, app :: String
project = "project:\n  name: test\n  version: 0.1.0\n  manifest-version: 1\n"
output = "output:\n  folder: output\n  builder: make\n"
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
      fmap manifestNature (parsed (project ++ output ++ app))
        `shouldBe` Right (Application (AppDeployment RTEMS5LEON3SFNEXYSA7 Release Nothing))

    it "reads a library" $
      fmap manifestNature (parsed (project ++ output ++ "exports:\n  - drivers.gaisler\n"))
        `shouldBe` Right (Library [Prefix [T.pack "drivers", T.pack "gaisler"]])

    it "takes the default output when the section is absent" $
      fmap manifestOutput (parsed (project ++ app)) `shouldBe` Right (OutputConfig "output" "efp" Make)

    it "takes the defaults of the fields absent from output" $
      fmap manifestOutput (parsed (project ++ "output:\n  folder: build\n" ++ app)) `shouldBe` Right (OutputConfig "build" "efp" Make)

    it "rejects a manifest without manifest-version" $
      ("project:\n  name: test\n  version: 0.1.0\n" ++ output ++ app) `failsWith` "no project.manifest-version"

    it "rejects the flat manifest of earlier versions" $
      "name: test\nplatform: posix-gcc\nsource-modules: src\noutput-folder: output\nbuilder: make\n"
        `failsWith` "no project.manifest-version"

    it "rejects an unknown manifest-version" $
      ("project:\n  name: test\n  version: 0.1.0\n  manifest-version: 2\n" ++ output ++ app)
        `failsWith` "manifest-version 2 is not supported"

    it "suggests the field closest to an unknown one" $
      (project ++ output ++ "app:\n  paltform: posix-gcc\n  profile: release\n")
        `failsWith` "did you mean \"platform\"?"

    it "rejects a project with both exports and app" $
      (project ++ output ++ app ++ "exports:\n  - drivers\n") `failsWith` "not both"

    it "rejects a project with neither exports nor app" $
      (project ++ output) `failsWith` "needs either exports"

    it "rejects a dependency that is not https" $
      (project ++ output ++ app ++ "dependencies:\n  - git: git@github.com:a/b\n    rev: v1\n")
        `failsWith` "is not an https:// URL"

    it "rejects a tick that does not divide 1000000" $
      (project ++ output ++ "app:\n  platform: posix-gcc\n  profile: release\n  microseconds-per-tick: 7\n")
        `failsWith` "must divide 1000000"

    it "rejects a test without platforms" $
      (project ++ output ++ app ++ "tests:\n  hk:\n    platforms: []\n    timeout: 10\n")
        `failsWith` "has no platforms"

    it "rejects a test name that is not snake_case" $
      (project ++ output ++ app ++ "tests:\n  HkTask:\n    platforms: [posix-gcc]\n    timeout: 10\n")
        `failsWith` "not lowercase snake_case"

    it "reads the entry of a test" $
      fmap (M.toList . manifestTests)
        (parsed (project ++ output ++ app ++ "tests:\n  hk:\n    platforms: [posix-gcc, rtems5-leon3sf-nexysa7]\n    timeout: 10\n"))
        `shouldBe` Right [(T.pack "hk", TestEntry [POSIXGCC, RTEMS5LEON3SFNEXYSA7] 10 defaultTestLogSize Nothing emptyExternConfig [] [])]

  describe "deployments" $ do

    let withTick = project ++ output ++ "app:\n  platform: rtems5-leon3sf-nexysa7\n  profile: release\n  microseconds-per-tick: 1000\n"
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

    it "builds each test on each platform into its own folder" $
      case parsed withTick of
        Right m -> outputFolder (testConfig m (T.pack "hk") (manifestTests m M.! T.pack "hk") POSIXGCC Debug)
          `shouldBe` "output" </> "tests" </> "hk" </> "posix-gcc"
        Left err -> expectationFailure err

    it "has no application in a library" $
      fmap appConfig (parsed (project ++ output ++ "exports:\n  - drivers\n")) `shouldBe` Right Nothing

  describe "layout" $ do

    let testsYaml = project ++ output ++ app ++ "tests:\n  hk:\n    platforms: [posix-gcc]\n    timeout: 10\n"

    it "accepts a project that matches its manifest" $
      withProject "ok" ["app/app.fin", "src/obdh/tasks/hk.fin", "tests/hk/test.fin", "tests/hk/src/fakes/clock.fin"] testsYaml
        (\root m -> checkLayout root m) `shouldReturn` []

    it "reports every mismatch at once" $ do
      errors <- withProject "bad" ["app/app.fin", "app/other.fin", "src/top.fin", "src/Obdh/hk.fin", "tests/extra/test.fin"] testsYaml
        (\root m -> checkLayout root m)
      -- app/other.fin, src/top.fin, src/Obdh, tests/extra undeclared, tests/hk missing
      length errors `shouldBe` 5

    it "rejects an app/ folder in a library" $ do
      errors <- withProject "lib" ["app/app.fin"] (project ++ output ++ "exports:\n  - drivers\n")
        (\root m -> checkLayout root m)
      errors `shouldSatisfy` any (isInfixOf "cannot have an app/ folder")

    it "rejects an application without app/" $ do
      errors <- withProject "noapp" ["src/obdh/hk.fin"] (project ++ output ++ app)
        (\root m -> checkLayout root m)
      errors `shouldSatisfy` any (isInfixOf "there is no app/ folder")

  it "writes a manifest for a new application that it reads back" $
    parsed (newApplicationManifest "demo" POSIXGCC) `shouldSatisfy` isRight
