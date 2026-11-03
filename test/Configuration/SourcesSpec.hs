{-# LANGUAGE OverloadedStrings #-}

-- | Where a build looks up the module of an import: the folders that the
-- folder of the importer sees, over a project with its packages in vendor/.
module Configuration.SourcesSpec (spec) where

import Command.Common (loadTerminaModule)
import Command.Sources
import Command.Utils (CheckFailure(..))
import Configuration.Lock
import Configuration.Package (Package(..))
import Configuration.Parsed (ParseFailure(..), ParsedFiles)
import Configuration.Vendor (packageDir)

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
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

-- | The sources of the build of the application and of the test hk of the
-- project of withLoaded.
withProject :: (Sources -> Sources -> IO a) -> IO a
withProject action =
    withLoaded $ \project -> action (buildSources project Nothing "app") (buildSources project (Just "hk") ("tests" </> "hk"))

-- | obdh declares pus, which declares ccsds; the test hk declares fixtures;
-- and obdh has a module of its own with the name of one of pus.
withLoaded :: (LoadedProject -> IO a) -> IO a
withLoaded action = do
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
        write ("libs" </> "drivers" </> "termina.yaml") (header "drivers" ++ "exports: [drivers]\n" ++ deps ["ccsds"])
        forM_ ["libs/drivers/src/drivers/uart.fin", "libs/drivers/src/support/fake.fin"] (`write` "")
        package "pus" ["ccsds"]
        package "ccsds" []
        package "fixtures" []
        forM_ ["src/obdh/tm.fin", "src/pus/m.fin", "tests/hk/src/hk/h.fin"] (`write` "")
        -- | Modules whose loading fails
        write "src/bad/missing.fin" "import obdh.nothing;\n"
        write "src/bad/hidden.fin" "import ccsds.m;\n"
        write "src/bad/root.fin" "import nothing;\n"
        write "src/bad/private.fin" "import support.fake;\n"
        write "src/bad/twice.fin" "import pus.m;\n"
        BS.writeFile "src/bad/latin1.fin" (BS.pack [0xff])
        writeLockFile "termina.lock" (Lock 1 [LockedPackage (url n) "v1" (T.pack (replicate 40 'a')) (T.pack ("sha256:" ++ replicate 64 'b')) | n <- ["pus", "ccsds", "fixtures"]])
        let manifest = manifestOf $ header "obdh" ++ "app:\n  platform: posix-gcc\n  profile: release\n" ++ deps ["pus"]
                ++ "  - path: libs/drivers\n"
                ++ "tests:\n  hk:\n    platforms: [posix-gcc]\n    timeout: 10\n    dependencies:\n      - git: " ++ T.unpack (url "fixtures") ++ "\n        rev: \"v1\"\n"
        readProject manifest >>= action

vendored :: String -> FilePath
vendored name = normalise (packageDir "." (url name) </> "src")

-- | Where an import of a folder lands, as text that a test can compare.
lookupIn :: Sources -> FilePath -> String -> String
lookupIn sources importer qname =
    case lookupModule sources importer qname of
        Found folder -> "found in " ++ folder
        Ambiguous a b -> collisionMessage sources qname a b
        Hidden folder -> hiddenMessage folder (maybe Project folderOwner (lookup importer [(folderPath f, f) | f <- sources])) (dotted qname)
        NotExported folder -> notExportedMessage folder (dotted qname)
        Missing -> "missing"

  where

    dotted = map (\c -> if c == '/' then '.' else c)

spec :: Spec
spec = describe "source folders of a build" $ do

  it "finds a module of a declared dependency, and one of the dependencies of a package" $
    withProject $ \app _ -> do
      lookupIn app "src" "obdh/tm" `shouldBe` "found in src"
      lookupIn app (vendored "pus") "ccsds/m" `shouldBe` ("found in " ++ vendored "ccsds")

  it "refuses a package that only reaches the build through another" $
    withProject $ \app _ ->
      lookupIn app "src" "ccsds/m" `shouldBe`
        "ccsds reaches this build through drivers, but termina.yaml does not declare it, so this project cannot import ccsds.m. Add ccsds to the dependencies to use it."

  it "lets the test see its dependencies and its modules, and keeps them from the code of src/" $
    withProject $ \_ test -> do
      lookupIn test ("tests" </> "hk") "fixtures/m" `shouldBe` ("found in " ++ vendored "fixtures")
      lookupIn test ("tests" </> "hk" </> "src") "hk/h" `shouldBe` ("found in " ++ "tests" </> "hk" </> "src")
      lookupIn test "src" "fixtures/m" `shouldBe` "fixtures is a dependency of the test hk only, so the code of src/ cannot import fixtures.m."
      lookupIn test "src" "hk/h" `shouldBe` "hk.h is a module of the test hk, so the code of src/ cannot import it."

  it "takes the exported modules of a library in a local folder, which sees its own dependencies" $
    withProject $ \app _ -> do
      let drivers = "libs" </> "drivers" </> "src"
      lookupIn app "src" "drivers/uart" `shouldBe` ("found in " ++ drivers)
      lookupIn app "src" "support/fake" `shouldBe`
        ("support.fake is in " ++ drivers ++ ", outside the exports of drivers, so it cannot be imported from it.")
      lookupIn app drivers "ccsds/m" `shouldBe` ("found in " ++ vendored "ccsds")

  it "refuses a module of the project with the name of one of a dependency" $
    withProject $ \app _ ->
      lookupIn app "src" "pus/m" `shouldBe`
        ("src/pus/m.fin has the same name as the module pus.m of the dependency pus, in " ++ vendored "pus"
          ++ "/. Two modules of a build cannot have the same name.")

  it "takes the text given for a module of the project, of a test and of a local folder" $
    withLoaded $ \project -> do
      let texts = M.fromList [ (file, "not termina") | file <- ["src/obdh/tm.fin", "tests/hk/src/hk/h.fin", "libs/drivers/src/drivers/uart.fin"] ]
          changed = withTexts texts project
          locals = ML.unions (map packageModules (M.elems (loadedLocals changed)))
      textOf "src/obdh/tm.fin" (loadedProjectModules changed) `shouldBe` Just "not termina"
      textOf "tests/hk/src/hk/h.fin" (loadedProjectModules changed) `shouldBe` Just "not termina"
      textOf "libs/drivers/src/drivers/uart.fin" locals `shouldBe` Just "not termina"
      textOf "libs/drivers/src/support/fake.fin" locals `shouldBe` Just ""

  it "PE-002: a module imports one at the root of a source folder" $
    loadCode "bad/root" `shouldReturn` Just "PE-002"

  it "PE-003: a module imports one that no folder of the build has" $
    loadCode "bad/missing" `shouldReturn` Just "PE-003"

  it "PE-005: a module imports one of a folder that it does not see" $
    loadCode "bad/hidden" `shouldReturn` Just "PE-005"

  it "PE-006: a module imports one of a local folder outside its exports" $
    loadCode "bad/private" `shouldReturn` Just "PE-006"

  it "PE-007: a module imports a name that two folders of the build have" $
    loadCode "bad/twice" `shouldReturn` Just "PE-007"

  it "PE-008: a module file is not valid UTF-8" $
    loadCode "bad/latin1" `shouldReturn` Just "PE-008"

  it "loads a module whose imports the build sees" $
    loadCode "obdh/tm" `shouldReturn` Nothing

  it "leaves out the text of a file that the project does not have" $
    withLoaded $ \project ->
      ML.member "src/obdh/new.fin" (loadedProjectModules (withTexts (M.singleton "src/obdh/new.fin" "") project)) `shouldBe` False

-- | The code of the error that the loading of a module of src/ gives in the
-- build of the application.
loadCode :: FilePath -> IO (Maybe T.Text)
loadCode qname =
    withLoaded $ \project ->
        either (Just . failureCode) (const Nothing) <$> loadTerminaModule (buildSources project Nothing "app") "src" qname

-- | The text of a module file, parsed or not.
textOf :: FilePath -> ParsedFiles -> Maybe T.Text
textOf file files = either failedText (Just . fst) =<< ML.lookup file files

  where

    failedText (InvalidSyntax text _) = Just text
    failedText NotUtf8 = Nothing
