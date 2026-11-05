-- | The local folders that stop a build in release before termina.lock is
-- reconciled.
module Command.LocalFoldersSpec (spec) where

import Command.Build (Selection(..), localFoldersInRelease, selectDeployments)
import Configuration.Configuration (ProjectProfile(..))
import Configuration.Manifest
import Test.Hspec

import Configuration.Helpers

manifest :: String -> Manifest
manifest profileName = manifestOf $ header "app"
    ++ "app:\n  platform: posix-gcc\n  profile: " ++ profileName ++ "\n"
    ++ "dependencies:\n  - path: ../drivers/./\n"
    ++ "tests:\n  t:\n    platforms: [posix-gcc]\n    timeout: 5\n    dependencies:\n      - path: ../mocks\n"

foldersOf :: Manifest -> Selection -> Maybe ProjectProfile -> [FilePath]
foldersOf m selection mProfile =
    either error (localFoldersInRelease m) (selectDeployments m selection Nothing mProfile)

spec :: Spec
spec = describe "local folders in release" $ do

  it "gives the folders of the root to the application in release, named as the build names them" $
    foldersOf (manifest "release") SelectApp Nothing `shouldBe` ["../drivers"]

  it "gives none to the application in debug" $
    foldersOf (manifest "debug") SelectApp Nothing `shouldBe` []

  it "gives a test the folders of the root and its own, only in release" $ do
    foldersOf (manifest "debug") (SelectTest "t") (Just Release) `shouldBe` ["../drivers", "../mocks"]
    foldersOf (manifest "release") (SelectTest "t") (Just Debug) `shouldBe` []
