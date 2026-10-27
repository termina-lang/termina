{-# LANGUAGE OverloadedStrings #-}

-- | Where a build looks up the module of an import: the folders that the
-- folder of the importer sees, over a project with its packages in vendor/.
module Configuration.SourcesSpec (spec) where

import Command.Sources
import Configuration.Lock
import Configuration.Vendor (packageDir)

import Control.Monad (forM_)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import Test.Hspec

import Configuration.Helpers

url :: String -> T.Text
url name = T.pack ("https://github.com/acme/" ++ name)

deps :: [String] -> String
deps [] = ""
deps names = "dependencies:\n" ++ concat ["  - git: " ++ T.unpack (url n) ++ "\n    rev: \"v1\"\n" | n <- names]

-- | obdh declares pus, which declares ccsds; the test hk declares fixtures;
-- and obdh has a module of its own with the name of one of pus.
withProject :: (Sources -> Sources -> IO a) -> IO a
withProject action = do
    tmp <- getTemporaryDirectory
    let root = tmp </> "termina-sources-spec"
    removePathForcibly root
    createDirectoryIfMissing True root
    withCurrentDirectory root $ do
        let write path text = createDirectoryIfMissing True (takeDirectory path) >> writeFile path text
            package name pkgDeps = do
                write (packageDir "." (url name) </> "termina.yaml") (header name ++ "exports: [" ++ name ++ "]\n" ++ deps pkgDeps)
                write (packageDir "." (url name) </> "src" </> name </> "m.fin") ""
        write "termina.yaml" ""
        package "pus" ["ccsds"]
        package "ccsds" []
        package "fixtures" []
        forM_ ["src/obdh/tm.fin", "src/pus/m.fin", "tests/hk/src/hk/h.fin"] (`write` "")
        writeLockFile "termina.lock" (Lock 1 [LockedPackage (url n) "v1" (T.pack (replicate 40 'a')) (T.pack ("sha256:" ++ replicate 64 'b')) | n <- ["pus", "ccsds", "fixtures"]])
        let manifest = manifestOf $ header "obdh" ++ "app:\n  platform: posix-gcc\n  profile: release\n" ++ deps ["pus"]
                ++ "tests:\n  hk:\n    platforms: [posix-gcc]\n    timeout: 10\n    dependencies:\n      - git: " ++ T.unpack (url "fixtures") ++ "\n        rev: \"v1\"\n"
        app <- either (error . unlines) id <$> buildSources manifest Nothing "app"
        test <- either (error . unlines) id <$> buildSources manifest (Just "hk") ("tests" </> "hk")
        action app test

vendored :: String -> FilePath
vendored name = normalise (packageDir "." (url name) </> "src")

-- | Where an import of a folder lands, as text that a test can compare.
lookupIn :: Sources -> FilePath -> String -> IO String
lookupIn sources importer qname = do
    found <- lookupModule sources importer qname
    return $ case found of
        Found folder -> "found in " ++ folder
        Ambiguous a b -> collisionMessage sources qname a b
        Hidden folder -> hiddenMessage folder (maybe Project folderOwner (lookup importer [(folderPath f, f) | f <- sources])) (dotted qname)
        Missing -> "missing"

  where

    dotted = map (\c -> if c == '/' then '.' else c)

spec :: Spec
spec = describe "source folders of a build" $ do

  it "finds a module of a declared dependency, and one of the dependencies of a package" $
    withProject $ \app _ -> do
      lookupIn app "src" "obdh/tm" `shouldReturn` "found in src"
      lookupIn app (vendored "pus") "ccsds/m" `shouldReturn` ("found in " ++ vendored "ccsds")

  it "refuses a package that only reaches the build through another" $
    withProject $ \app _ ->
      lookupIn app "src" "ccsds/m" `shouldReturn`
        "ccsds reaches this build through pus, but termina.yaml does not declare it, so this project cannot import ccsds.m. Add ccsds to the dependencies to use it."

  it "lets the test see its dependencies and its modules, and keeps them from the code of src/" $
    withProject $ \_ test -> do
      lookupIn test ("tests" </> "hk") "fixtures/m" `shouldReturn` ("found in " ++ vendored "fixtures")
      lookupIn test ("tests" </> "hk" </> "src") "hk/h" `shouldReturn` ("found in " ++ "tests" </> "hk" </> "src")
      lookupIn test "src" "fixtures/m" `shouldReturn` "fixtures is a dependency of the test hk only, so the code of src/ cannot import fixtures.m."
      lookupIn test "src" "hk/h" `shouldReturn` "hk.h is a module of the test hk, so the code of src/ cannot import it."

  it "refuses a module of the project with the name of one of a dependency" $
    withProject $ \app _ ->
      lookupIn app "src" "pus/m" `shouldReturn`
        ("src/pus/m.fin has the same name as the module pus.m of the dependency pus, in " ++ vendored "pus"
          ++ "/. Two modules of a build cannot have the same name.")
