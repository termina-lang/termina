-- | The interrupt emitters a platform puts in the initial program, and the
-- system elements dropped when the application does not connect them. Every
-- interrupt of the interrupt map of the platform is declared under its own
-- name. The LEON3 of the Nexys used to offer an irq_0, which its controller
-- does not have, registered under the name irq_1, so connecting it installed
-- the handler on line 1 without any error.
module Configuration.InterruptEmittersSpec (spec) where

import Configuration.Configuration
import Configuration.Platform
import ControlFlow.Architecture (pruneUnconnectedSystemElements)
import ControlFlow.Architecture.Types
import Generator.Environment
import Semantic.Types (SemanticAnn)

import qualified Data.Map.Strict as M
import Test.Hspec

platforms :: [(String, Platform)]
platforms =
    [ ("posix-gcc", POSIXGCC)
    , ("rtems5-leon3sf-nexysa7", RTEMS5LEON3SFNEXYSA7)
    , ("freertos10-stm32l432-nucleol432kc", FreeRTOS10STM32L432NUCLEOL432KC)
    , ("rtems6-zynq7000-pynqz2", RTEMS6ZYNQ7000PYNQZ2)
    ]

initialProgram :: Platform -> TerminaProgArch SemanticAnn
initialProgram plt = getPlatformInitialProgram (defaultConfig "test" plt) plt

-- | The interrupt emitters of the initial program, as pairs of the name the
-- program stores them under and the name the emitter carries.
interruptEmitters :: TerminaProgArch a -> [(String, String)]
interruptEmitters tp =
    [ (key, ident)
    | (key, TPInterruptEmitter ident _) <- M.toList (emitters tp) ]

spec :: Spec
spec = describe "Platforms: interrupt emitters" $ do
  mapM_ (\(pltName, plt) -> do
    let irqs = interruptEmitters (initialProgram plt)
    it (pltName ++ " registers each interrupt under its own name") $
      filter (uncurry (/=)) irqs `shouldBe` []
    it (pltName ++ " declares every interrupt of its map") $
      map fst irqs `shouldBe` M.keys (getPlatformInterruptMap plt)) platforms

  it "drops the system elements that nothing connects" $ do
    let pruned = pruneUnconnectedSystemElements (initialProgram RTEMS5LEON3SFNEXYSA7)
    M.keys (emitters pruned) `shouldBe` []
    M.keys (resources pruned) `shouldBe` []
