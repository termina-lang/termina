-- | The data objects of a binary, read from its symbol table.
module Runner.ElfSpec (spec) where

import qualified Data.Map.Strict as M
import Runner.Elf (readObjects)
import System.FilePath ((</>))
import Test.Hspec

fixtures :: FilePath
fixtures = "test" </> "Runner" </> "fixtures"

spec :: Spec
spec = describe "Runner.Elf" $ do

    it "gives the defined objects with their values, without functions nor undefined symbols" $ do
        objects <- readObjects (fixtures </> "objects.o")
        objects `shouldBe` Right (M.fromList
            [("counter", 4), ("termina__cov_cond__m", 0), ("termina__cov_stmt__m", 0)])

    it "refuses a Mach-O binary" $ do
        objects <- readObjects (fixtures </> "macho.bin")
        either (const True) (const False) objects `shouldBe` True

    it "gives an error for a file that is not ELF" $ do
        objects <- readObjects (fixtures </> "objects.c")
        either (const True) (const False) objects `shouldBe` True
