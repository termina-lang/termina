{-# LANGUAGE OverloadedStrings #-}

-- | The transitive resolution, over packages kept in memory instead of
-- fetched with git.
module Configuration.ResolveSpec (spec) where

import Configuration.Lock (normalizeUrl)
import Configuration.Manifest
import Configuration.Resolve

import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml (decodeEither')
import Test.Hspec

manifestOf :: String -> Manifest
manifestOf yaml = either error id (either (Left . show) parseManifest (decodeEither' (BS.pack yaml)))

header :: String -> String
header name = "project:\n  name: " ++ name ++ "\n  version: 0.1.0\n  manifest-version: 1\n"

depsOf :: String -> [(String, String)] -> String
depsOf _ [] = ""
depsOf key deps = key ++ ":\n" ++ concat ["  - git: " ++ u ++ "\n    rev: \"" ++ t ++ "\"\n" | (u, t) <- deps]

-- | A library named as the last segment of its URL, with its dependencies
-- and its overrides.
library :: String -> [(String, String)] -> [(String, String)] -> Manifest
library name deps overrides = manifestOf $ header name ++ "exports: [" ++ map snake name ++ "]\n"
    ++ depsOf "dependencies" deps ++ depsOf "overrides" overrides

  where

    snake c = if c == '-' then '_' else c

root :: [(String, String)] -> [(String, [(String, String)])] -> [(String, String)] -> Manifest
root deps tests overrides = manifestOf $ header "obdh" ++ "app:\n  platform: posix-gcc\n  profile: release\n"
    ++ depsOf "dependencies" deps ++ depsOf "overrides" overrides
    ++ (if null tests then "" else "tests:\n" ++ concat
        [ "  " ++ t ++ ":\n    platforms: [posix-gcc]\n    timeout: 10\n"
            ++ unlines (map ("    " ++) (lines (depsOf "dependencies" ds)))
        | (t, ds) <- tests ])

url :: String -> String
url name = "https://github.com/acme/" ++ name

-- | The packages that the fake fetch knows, by URL and tag.
type Registry = M.Map (T.Text, T.Text) Obtained

registry :: [(String, String, Manifest)] -> Registry
registry entries = M.fromList
    [ ((normalizeUrl (T.pack (url name)), T.pack tag), Obtained (T.pack (replicate 40 'a')) m [])
    | (name, tag, m) <- entries ]

-- | Resolve against a registry, counting the packages brought.
run :: Registry -> Manifest -> (Either [ResolveError String] Resolution, Int)
run reg m = runState (resolve fetch m) 0

  where

    fetch u t = do
        modify (+ 1)
        return $ maybe (Left "needs credentials") Right (M.lookup (normalizeUrl u, t) reg)

names :: Either [ResolveError String] Resolution -> Either [ResolveError String] [T.Text]
names = fmap (M.keys . resolvedPackages)

spec :: Spec
spec = do

  describe "graph" $ do

    it "brings the dependencies of the dependencies, with the path to each" $ do
      let reg = registry [("pus", "v1", library "pus" [(url "ccsds", "v2")] []), ("ccsds", "v2", library "ccsds" [] [])]
          (result, _) = run reg (root [(url "pus", "v1")] [] [])
      fmap (map resolvedPath . M.elems . resolvedPackages) result `shouldBe` Right [["obdh", "pus"], ["obdh"]]

    it "brings once a package that two packages require with the same tag" $ do
      let reg = registry [ ("pus", "v1", library "pus" [(url "hal", "v1")] [])
                         , ("drivers", "v1", library "drivers" [(url "hal", "v1")] [])
                         , ("hal", "v1", library "hal" [] []) ]
      snd (run reg (root [(url "pus", "v1"), (url "drivers", "v1")] [] [])) `shouldBe` 3

    it "ends a cycle between two packages at the same tag" $ do
      let reg = registry [("a", "v1", library "a" [(url "b", "v1")] []), ("b", "v1", library "b" [(url "a", "v1")] [])]
      let (result, calls) = run reg (root [(url "a", "v1")] [] [])
      (names result, calls) `shouldBe` (Right ["github.com/acme/a", "github.com/acme/b"], 2)

    it "keeps the URL written closest to the root" $ do
      let reg = registry [("pus", "v1", library "pus" [(url "hal.git", "v1")] []), ("hal", "v1", library "hal" [] [])]
          (result, _) = run reg (root [(url "pus", "v1"), ("https://github.com/Acme/HAL", "v1")] [] [])
      fmap (map resolvedUrl . M.elems . resolvedPackages) result `shouldBe` Right ["https://github.com/Acme/HAL", T.pack (url "pus")]

  describe "versions" $ do

    let reg = registry [ ("gaisler-drivers", "v0.2.0", library "gaisler-drivers" [(url "embedded-hal", "v0.3.1")] [])
                       , ("hal-fixtures", "v0.1.0", library "hal-fixtures" [(url "embedded-hal", "v0.4.0")] [])
                       , ("embedded-hal", "v0.3.1", library "embedded-hal" [] [])
                       , ("embedded-hal", "v0.4.0", library "embedded-hal" [] []) ]
        project = root [(url "gaisler-drivers", "v0.2.0")] [("tc_rx", [(url "hal-fixtures", "v0.1.0")])]

    it "rejects two tags of one package in the project, its tests included, with both paths" $ do
      let (result, _) = run reg (project [])
      either (map (describeResolveError id)) (const []) result `shouldBe`
        [ "version conflict on github.com/acme/embedded-hal\n"
            ++ "  v0.3.1  obdh -> gaisler-drivers -> embedded-hal\n"
            ++ "  v0.4.0  obdh (test tc_rx) -> hal-fixtures -> embedded-hal\n"
            ++ "  write an override in termina.yaml to force one version for the whole project" ]

    it "forces the tag of an override of the root, and records what was requested" $ do
      let (result, _) = run reg (project [(url "embedded-hal", "v0.4.0")])
      fmap appliedOverrides result `shouldBe`
        Right [AppliedOverride "github.com/acme/embedded-hal" "v0.4.0" "v0.3.1" ["obdh", "gaisler-drivers"]]
      fmap (map resolvedTag . M.elems . resolvedPackages) result `shouldBe` Right ["v0.4.0", "v0.2.0", "v0.1.0"]

    it "ignores the overrides of a dependency, and names them" $ do
      let reg' = registry [ ("pus", "v1", library "pus" [] [(url "hal", "v9")]) ]
      fmap ignoredOverrides (fst (run reg' (root [(url "pus", "v1")] [] []))) `shouldBe` Right [("pus", T.pack (url "hal"))]

  describe "errors" $

    it "reports every package that could not be brought, and goes on with the rest" $ do
      let reg = registry [("pus", "v1", library "pus" [] [])]
      fst (run reg (root [(url "private1", "v1"), (url "pus", "v1"), (url "private2", "v1")] [] [])) `shouldBe` Left
        [ ObtainFailed (T.pack (url "private1")) "v1" ["obdh"] "needs credentials"
        , ObtainFailed (T.pack (url "private2")) "v1" ["obdh"] "needs credentials" ]
