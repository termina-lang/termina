-- | The check of a library in its own build: its exported modules type
-- checked together with the exported modules of the packages that its
-- dependencies reach.
module Configuration.LibraryCheckSpec (spec) where

import Command.Build (checkExportedModules)
import Command.Common (libraryPlatforms, loadExportedModules, typeLibrary)
import Command.Sources (buildSources, dependencyModulesOf, readProject)
import Command.Utils (CheckFailure(..))
import Utils.Annotations (Location(..), QualifiedName)

import qualified Data.Text as T
import System.Directory
import System.FilePath
import Test.Hspec

import Configuration.Helpers

-- | Run the action in the folder of the library acme, with the library ccsds
-- next to it, which acme takes from its folder. The modules of acme go before
-- those of ccsds by name, so the order of the type checker is not the one of
-- the names.
withLibraries :: String -> [(FilePath, String)] -> (FilePath -> IO a) -> IO a
withLibraries label files action = do
    tmp <- getTemporaryDirectory
    let root = tmp </> "termina-library-check-spec" </> label
    removePathForcibly root
    mapM_ (\(path, text) -> createDirectoryIfMissing True (takeDirectory (root </> path)) >> writeFile (root </> path) text)
        (("acme/termina.yaml", header "acme" ++ "exports: [acme]\ndependencies:\n  - path: ../ccsds\n") : files)
    withCurrentDirectory (root </> "acme") (action ".")

-- | The code of the first error of the check of acme and the module where it
-- is, if any.
firstError :: FilePath -> IO (Maybe (T.Text, QualifiedName))
firstError root = do
    project <- readProject root manifest >>= either (fail . unlines) return
    loaded <- loadExportedModules project
    let deps = dependencyModulesOf (buildSources project Nothing "src")
    return $ codeAndModule <$> case loaded of
        Left failure -> Just failure
        Right (modules, order) -> snd (typeLibrary manifest (head (libraryPlatforms manifest)) deps modules order)

  where

    manifest = manifestOf (header "acme" ++ "exports: [acme]\ndependencies:\n  - path: ../ccsds\n")

    codeAndModule failure = (failureCode failure, case failureLocation failure of
        Position m _ _ -> m
        _ -> "")

ccsds :: String -> [(FilePath, String)]
ccsds extra = [("ccsds/termina.yaml", header "ccsds" ++ "exports: [ccsds]\n" ++ extra)]

spec :: Spec
spec = describe "the check of a library" $ do

  it "finds a global of the library declared as well by a module of a dependency that it does not import" $
    withLibraries "global" (ccsds "" ++
      [ ("ccsds/src/ccsds/check.fin", "function crc16(x : u16) -> u16 {\n    return x;\n}\n")
      , ("acme/src/acme/frame.fin", "function crc16(x : u16) -> u16 {\n    return x;\n}\n") ]) $ \root ->
        firstError root `shouldReturn` Just (T.pack "SE-133", "acme/frame")

  it "finds a local of the library named as a global of a dependency" $
    withLibraries "local" (ccsds "" ++
      [ ("ccsds/src/ccsds/check.fin", "function crc16(x : u16) -> u16 {\n    return x;\n}\n")
      , ("acme/src/acme/frame.fin", "function size() -> u16 {\n    let crc16 : u16 = 0;\n    return crc16;\n}\n") ]) $ \root ->
        firstError root `shouldReturn` Just (T.pack "SE-081", "acme/frame")

  it "SE-239: reports in the library a global named as a local of a dependency" $
    withLibraries "dependency-local" (ccsds "" ++
      [ ("ccsds/src/ccsds/check.fin", "function size() -> u16 {\n    let crc16 : u16 = 0;\n    return crc16;\n}\n")
      , ("acme/src/acme/frame.fin", "function crc16(x : u16) -> u16 {\n    return x;\n}\n") ]) $ \root ->
        firstError root `shouldReturn` Just (T.pack "SE-239", "acme/frame")

  it "passes a library whose names differ from those of its dependencies" $
    withLibraries "distinct" (ccsds "" ++
      [ ("ccsds/src/ccsds/check.fin", "function crc16(x : u16) -> u16 {\n    return x;\n}\n")
      , ("acme/src/acme/frame.fin", "function size() -> u16 {\n    return 2;\n}\n") ]) $ \root ->
        firstError root `shouldReturn` Nothing

  it "leaves the extern functions of a dependency to the manifest of the dependency" $
    withLibraries "extern" (ccsds "extern:\n  ccsds.crc:\n    sources: [crc.c]\n" ++
      [ ("ccsds/src/ccsds/crc.fin", "extern function crc_hw(x : u16) -> u16;\n")
      , ("ccsds/extern/src/crc.c", "")
      , ("acme/src/acme/frame.fin", "function size() -> u16 {\n    return 2;\n}\n") ]) $ \root -> do
        project <- readProject root (manifestOf (header "acme" ++ "exports: [acme]\ndependencies:\n  - path: ../ccsds\n"))
            >>= either (fail . unlines) return
        checkExportedModules False project `shouldReturn` ()
