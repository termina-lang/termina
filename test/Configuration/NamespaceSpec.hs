-- | The prefixes that a library exports, checked against each other and
-- against its sources.
module Configuration.NamespaceSpec (spec) where

import Configuration.Manifest
import Configuration.Namespace

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import Data.Yaml (decodeEither')
import System.Directory
import System.FilePath
import Test.Hspec

project :: String
project = "project:\n  name: test\n  version: 0.1.0\n  manifest-version: 1\n"

library :: [String] -> String
library prefixes = project ++ "exports:" ++ concatMap ("\n  - " ++) prefixes ++ (if null prefixes then " []" else "") ++ "\n"

-- | The errors of the exports of a project in a fresh folder, made of the
-- given files.
exportErrors :: String -> [FilePath] -> String -> IO [String]
exportErrors label files yaml = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "termina-namespace-spec" </> label
  removePathForcibly root
  createDirectoryIfMissing True root
  forM_ files $ \f -> do
    createDirectoryIfMissing True (root </> takeDirectory f)
    writeFile (root </> f) ""
  case either (Left . show) parseManifest (decodeEither' (BS.pack yaml)) of
    Left err -> expectationFailure err >> return []
    Right manifest -> checkExports root manifest

spec :: Spec
spec = describe "exports" $ do

  it "accepts disjoint prefixes under a shared root" $
    exportErrors "shared" ["src/drivers/gaisler/uart/apbuart.fin", "src/drivers/stm32/spi.fin"]
      (library ["drivers.gaisler", "drivers.stm32"]) `shouldReturn` []

  it "accepts a prefix that names a module" $
    exportErrors "module" ["src/pus/service1.fin"] (library ["pus.service1"]) `shouldReturn` []

  it "checks nothing in an application" $
    exportErrors "app" ["app/app.fin"] (project ++ "app:\n  platform: posix-gcc\n  profile: debug\n")
      `shouldReturn` []

  it "rejects a library that exports nothing" $
    exportErrors "none" [] (library []) `shouldReturn` ["a library exports at least one prefix"]

  it "rejects a prefix listed twice" $
    exportErrors "twice" ["src/pus/service1.fin"] (library ["pus", "pus"])
      `shouldReturn` ["the prefix pus is listed twice in exports"]

  it "rejects two prefixes when one contains the other, in either order" $ do
    exportErrors "overlap" ["src/drivers/gaisler/uart.fin"] (library ["drivers", "drivers.gaisler"])
      `shouldReturn` ["the prefixes drivers and drivers.gaisler of exports overlap: drivers contains drivers.gaisler"]
    exportErrors "overlap-rev" ["src/drivers/gaisler/uart.fin"] (library ["drivers.gaisler", "drivers"])
      `shouldReturn` ["the prefixes drivers and drivers.gaisler of exports overlap: drivers contains drivers.gaisler"]

  it "rejects a prefix with no module, whatever other files its folder holds" $
    exportErrors "empty" ["src/pus/README.md", "src/pus/service1/notes.txt", "src/ccsds/tc.fin"] (library ["pus", "ccsds"])
      `shouldReturn` ["the prefix pus of exports has no module: there is no src/pus.fin and no .fin under src/pus/"]

  it "finds a module at any depth under the prefix" $
    exportErrors "deep" ["src/pus/services/s1/tc.fin"] (library ["pus"]) `shouldReturn` []
