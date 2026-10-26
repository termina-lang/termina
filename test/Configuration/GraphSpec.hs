{-# LANGUAGE OverloadedStrings #-}

-- | The checks of a resolved graph, over resolutions written by hand.
module Configuration.GraphSpec (spec) where

import Configuration.Fetch
import Configuration.Graph
import Configuration.Lock (normalizeUrl)
import Configuration.Manifest
import Configuration.Resolve

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml (decodeEither')
import Test.Hspec

manifestOf :: String -> Manifest
manifestOf yaml = either error id (either (Left . show) parseManifest (decodeEither' (BS.pack yaml)))

header :: String -> String
header name = "project:\n  name: " ++ name ++ "\n  version: 0.1.0\n  manifest-version: 1\n"

url :: String -> String
url name = "https://github.com/acme/" ++ name

-- | A package with its exports, the names of the packages it depends on and
-- its modules.
package :: String -> String -> [String] -> [(String, String)] -> ResolvedPackage
package name exports deps modules = ResolvedPackage (T.pack (url name)) "v1" (Obtained (T.pack (replicate 40 'a')) m files) ["app"]

  where

    m = manifestOf $ header name ++ "exports: [" ++ exports ++ "]\n"
        ++ (if null deps then "" else "dependencies:\n" ++ concat ["  - git: " ++ url d ++ "\n    rev: v1\n" | d <- deps])

    files = FetchedFile "termina.yaml" "100644" "" : [FetchedFile p "100644" (BS.pack body) | (p, body) <- modules]

resolution :: [ResolvedPackage] -> Resolution
resolution pkgs = Resolution (M.fromList [(normalizeUrl (resolvedUrl p), p) | p <- pkgs]) [] []

app :: Manifest
app = manifestOf $ header "app" ++ "app:\n  platform: posix-gcc\n  profile: release\n"

module' :: [String] -> String
module' imports = concat ["import " ++ i ++ ";\n" | i <- imports] ++ "function f() {\n    return;\n}\n"

ccsds :: ResolvedPackage
ccsds = package "ccsds" "ccsds" [] [("src/ccsds/tc.fin", module' [])]

hal :: ResolvedPackage
hal = package "hal" "hal" [] [("src/hal/gpio.fin", module' [])]

spec :: Spec
spec = do

  describe "prefixes" $ do

    it "accepts disjoint prefixes" $
      checkGraph app (resolution [ccsds, hal]) `shouldBe` []

    it "rejects a prefix of a package that contains one of another" $
      checkGraph app (resolution [package "drivers" "drivers" [] [], package "gaisler" "drivers.gaisler" [] []])
        `shouldBe` ["the prefix drivers of drivers overlaps drivers.gaisler of gaisler"]

    it "rejects a prefix of a package that the library being built also owns" $
      checkGraph (manifestOf (header "lib" ++ "exports: [ccsds.tm]\n")) (resolution [ccsds])
        `shouldBe` ["the prefix ccsds.tm of lib overlaps ccsds of ccsds"]

  describe "imports of the exported modules" $ do

    it "accepts its own modules and those of the dependencies it declares" $
      checkGraph app (resolution [ccsds, package "pus" "pus" ["ccsds"] [("src/pus/s1.fin", module' ["pus.util", "ccsds.tc"])]])
        `shouldBe` []

    it "rejects a package of the graph that it does not declare, and a module of nobody" $
      checkGraph app (resolution [ccsds, hal, package "pus" "pus" ["ccsds"] [("src/pus/s1.fin", module' ["hal.gpio", "util.crc"])]])
        `shouldBe`
          [ "vendor/github.com/acme/pus/src/pus/s1.fin:1: the exported module pus.s1 imports hal.gpio, which is neither exported by pus nor by a dependency it declares"
          , "vendor/github.com/acme/pus/src/pus/s1.fin:2: the exported module pus.s1 imports util.crc, which is neither exported by pus nor by a dependency it declares" ]

    it "leaves to the build a module that does not parse, and one that is not exported" $
      checkGraph app (resolution [package "pus" "pus" [] [("src/pus/s1.fin", "import util.crc\n"), ("src/util/x.fin", module' ["hal.gpio"])]])
        `shouldBe` []
