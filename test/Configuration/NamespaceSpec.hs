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
exportErrors label files = exportErrorsWith label [(f, "") | f <- files]

-- | The same, with the content of each file.
exportErrorsWith :: String -> [(FilePath, String)] -> String -> IO [String]
exportErrorsWith label files yaml = do
  tmp <- getTemporaryDirectory
  let root = tmp </> "termina-namespace-spec" </> label
  removePathForcibly root
  createDirectoryIfMissing True root
  forM_ files $ \(f, content) -> do
    createDirectoryIfMissing True (root </> takeDirectory f)
    writeFile (root </> f) content
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

  describe "exported modules" $ do

    let crc = "function pus_crc16(x : u16) -> u16 {\n    return x;\n}\n"

    it "accepts an exported module that imports another exported one" $
      exportErrorsWith "imports-ok" [("src/pus/service1.fin", "import pus.util.crc;\n"), ("src/pus/util/crc.fin", crc)]
        (library ["pus"]) `shouldReturn` []

    it "rejects an exported module that imports one that is not exported" $
      exportErrorsWith "imports-bad" [("src/pus/service1.fin", "import test_support.crc;\n"), ("src/test_support/crc.fin", "")]
        (library ["pus"])
        `shouldReturn` ["src/pus/service1.fin:1: the exported module pus.service1 imports test_support.crc, which is not exported"]

    it "lets a module that is not exported import anything" $
      exportErrorsWith "imports-free" [("src/pus/util/crc.fin", crc), ("src/test_support/fake.fin", "import pus.util.crc;\n")]
        (library ["pus"]) `shouldReturn` []

    it "leaves to the build an import of a module that is not in src" $
      exportErrorsWith "imports-missing" [("src/pus/service1.fin", "import hal.gpio;\n")] (library ["pus"]) `shouldReturn` []

    it "rejects an exported module that does not parse, and ignores one that is not exported" $
      exportErrorsWith "parse" [("src/pus/service1.fin", "function {\n"), ("src/test_support/fake.fin", "function {\n")]
        (library ["pus"]) `shouldReturn` ["src/pus/service1.fin:1: the exported module pus.service1 does not parse"]

    it "rejects a name of an exported module without the prefix of the library" $
      exportErrorsWith "names" [("src/drivers/gaisler/crc.fin", "function crc16(x : u16) -> u16 {\n    return x;\n}\n")]
        (library ["drivers.gaisler"])
        `shouldReturn` ["src/drivers/gaisler/crc.fin:1: the name crc16 of the exported module drivers.gaisler.crc does not start with the prefix gaisler (gaisler_..., GAISLER_..., Gaisler...)"]

    it "does not ask for the prefix in a module that is not exported" $
      exportErrorsWith "names-free" [("src/pus/util/crc.fin", crc), ("src/test_support/fake.fin", "function fake(x : u16) -> u16 {\n    return x;\n}\n")]
        (library ["pus"]) `shouldReturn` []

  describe "prefix of a name" $ do

    it "accepts the segment in snake case, upper case and Pascal case" $
      map (hasLibraryPrefix "gaisler") ["gaisler_uart_init", "GAISLER_MAX_FRAME", "GaislerAPBUARTDriver", "GaislerCharDev"]
        `shouldBe` [True, True, True, True]

    it "accepts a segment with underscores in Pascal case without them" $
      map (hasLibraryPrefix "char_dev") ["char_dev_send", "CHAR_DEV_MAX", "CharDevSend"] `shouldBe` [True, True, True]

    it "rejects a name glued to the segment, the segment alone, and a C or an I before it" $
      map (hasLibraryPrefix "gaisler") ["Gaislerdriver", "gaislerx", "Gaisler", "gaisler", "CGaislerUart", "IGaislerCharDev", "crc16"]
        `shouldBe` replicate 7 False
