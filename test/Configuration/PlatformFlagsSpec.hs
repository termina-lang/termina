-- | The platform flags of the manifest, whose keys have to name a platform.
module Configuration.PlatformFlagsSpec (spec) where

import Configuration.Platform
import qualified Configuration.Platform.RTEMS5LEON3SFNEXYSA7 as Nexys

import qualified Data.ByteString.Char8 as BS
import Data.Either (isLeft)
import Data.Yaml (decodeEither')
import Test.Hspec

flags :: String -> Either String PlatformFlags
flags = either (Left . show) Right . decodeEither' . BS.pack

spec :: Spec
spec = describe "Manifest: platform flags" $ do

  it "reads the flags of a platform" $
    fmap (Nexys.enableIrq2 . rtems5_leon3sf_nexysa7)
      (flags "rtems5-leon3sf-nexysa7:\n  enable-irq-2: true\n")
      `shouldBe` Right True

  it "rejects a key that names no platform" $
    flags "rtems5-leon3-nexysa7:\n  enable-irq-2: true\n" `shouldSatisfy` isLeft
