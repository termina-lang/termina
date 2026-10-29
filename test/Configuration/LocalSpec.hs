-- | The libraries of local folders: read from disk, checked as a fetched
-- package, with their own path dependencies.
module Configuration.LocalSpec (spec) where

import Configuration.Local
import Configuration.Manifest

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.Directory
import System.FilePath
import Test.Hspec

import Configuration.Helpers

-- | Run the action in a fresh folder that holds the project and, next to it,
-- the folders of its libraries.
inFolders :: String -> [(FilePath, String)] -> IO a -> IO a
inFolders label files action = do
    tmp <- getTemporaryDirectory
    let root = tmp </> "termina-local-spec" </> label
    removePathForcibly root
    createDirectoryIfMissing True (root </> "project")
    mapM_ (\(path, text) -> createDirectoryIfMissing True (takeDirectory (root </> path)) >> writeFile (root </> path) text) files
    withCurrentDirectory (root </> "project") action

library :: String -> String -> String
library name extra = header name ++ "exports: [" ++ name ++ "]\n" ++ extra

spec :: Spec
spec = describe "local libraries" $ do

  it "names a folder from the one of the manifest, without going down and up" $
    map (uncurry localFolderOf) [(".", "../drivers"), ("../drivers", "../hal"), ("../drivers", "hal"), (".", "./libs/x/../y")]
      `shouldBe` ["../drivers", "../hal", "../drivers/hal", "libs/y"]

  it "reads a library and the libraries of its own path dependencies" $
    inFolders "nested"
      [ ("drivers/termina.yaml", library "drivers" "dependencies:\n  - path: ../hal\n"), ("drivers/src/drivers/uart.fin", "")
      , ("hal/termina.yaml", library "hal" ""), ("hal/src/hal/gpio.fin", "") ] $ do
        found <- readLocalPackages [(".", "../drivers")]
        fmap (M.map (T.unpack . projectName . manifestProject . localManifest)) found
          `shouldBe` Right (M.fromList [("../drivers", "drivers"), ("../hal", "hal")])

  it "refuses a folder that does not exist, and one that is not a library" $
    inFolders "wrong" [("tool/termina.yaml", header "tool" ++ "app:\n  platform: posix-gcc\n  profile: release\n")] $ do
      fmap M.keys <$> readLocalPackages [(".", "../missing")] `shouldReturn`
        Left ["the dependency ../missing does not exist. In a container, the folder has to be mounted too."]
      fmap M.keys <$> readLocalPackages [(".", "../tool")] `shouldReturn`
        Left ["../tool: the package is an application: it has no exports, so it cannot be a dependency"]
