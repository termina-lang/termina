-- | The line that ends a build of several deployments when some of them failed.
module Command.FailedSummarySpec (spec) where

import Command.Build (failedSummary)
import Test.Hspec

spec :: Spec
spec = describe "summary of the deployments that failed" $ do

  it "gives how many failed among how many, and their labels in order" $
    failedSummary 3 ["the test broken on posix-gcc", "the test zeta on posix-gcc"]
      `shouldBe` "2 of 3 deployments failed: the test broken on posix-gcc, the test zeta on posix-gcc"

  it "names a single deployment that failed" $
    failedSummary 5 ["the application"]
      `shouldBe` "1 of 5 deployments failed: the application"
