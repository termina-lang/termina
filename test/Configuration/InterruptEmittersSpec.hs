-- | The interrupt emitters a platform puts in the initial program. Each one is
-- registered under the name the application uses and has a vector in the
-- interrupt map of the platform. The LEON3 of the Nexys used to offer an
-- irq_0, which its controller does not have, registered under the name irq_1,
-- so connecting it installed the handler on line 1 without any error.
module Configuration.InterruptEmittersSpec (spec) where

import Configuration.Configuration
import Configuration.Platform
import ControlFlow.Architecture.Types
import Generator.Environment

import Data.Aeson (Result(..), Value(..), fromJSON, toJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as M
import Test.Hspec

-- | The flags of every platform with every interrupt enabled.
allFlagsEnabled :: PlatformFlags
allFlagsEnabled =
    case fromJSON (enable (toJSON defaultPlatformFlags)) of
        Success f -> f
        Error err -> error err
    where
        enable (Object o) = Object (KM.map enable o)
        enable (Bool _) = Bool True
        enable v = v

platforms :: [(String, Platform)]
platforms =
    [ ("posix-gcc", POSIXGCC)
    , ("rtems5-leon3sf-nexysa7", RTEMS5LEON3SFNEXYSA7)
    , ("freertos10-stm32l432-nucleol432kc", FreeRTOS10STM32L432NUCLEOL432KC)
    , ("rtems6-zynq7000-pynqz2", RTEMS6ZYNQ7000PYNQZ2)
    ]

-- | The interrupt emitters of the initial program, as pairs of the name the
-- program stores them under and the name the emitter carries.
interruptEmitters :: Platform -> [(String, String)]
interruptEmitters plt =
    [ (key, ident)
    | (key, TPInterruptEmitter ident _) <- M.toList (emitters initialProgram) ]
    where
        config = (defaultConfig "test" plt) { platformFlags = allFlagsEnabled }
        initialProgram = getPlatformInitialProgram config plt

spec :: Spec
spec = describe "Platforms: interrupt emitters" $
  mapM_ (\(pltName, plt) -> do
    let irqs = interruptEmitters plt
        irqMap = getPlatformInterruptMap plt
    it (pltName ++ " registers each interrupt under its own name") $
      filter (uncurry (/=)) irqs `shouldBe` []
    it (pltName ++ " has a vector for each interrupt") $
      filter ((`M.notMember` irqMap) . snd) irqs `shouldBe` []) platforms
