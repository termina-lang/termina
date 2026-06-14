module Pipeline.Golden (goldenC) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>), takeDirectory)
import Test.Hspec (Expectation, expectationFailure, shouldBe)

-- | Directory holding the golden C files, relative to the package root (which
-- is the working directory @stack test@ runs in).
goldenDir :: FilePath
goldenDir = "test" </> "Pipeline" </> "golden"

-- | Assert that rendered C matches the golden file
-- @test\/Pipeline\/golden\/\<name\>.c@.
--
-- This is the file-based alternative to the embedded-string golden the IT
-- specs use. It exists because the C pretty printer wraps lines with
-- column-dependent alignment (@fillSep@), so a one-line change churns many
-- expected strings and hand-editing them is error prone. With goldens on disk,
-- a deliberate change is reviewed as a diff of @.c@ files and regenerated in
-- bulk instead of retyped.
--
-- Workflow:
--
--   * @GOLDEN_UPDATE=1 stack test@ (re)writes every golden from the actual
--     output instead of asserting. Use it after an intended codegen change,
--     then review the @.c@ diff.
--   * A missing golden is written on first run and the test fails with a
--     notice, so a brand-new fixture never silently passes.
--   * Otherwise the actual output is compared against the committed golden.
goldenC :: String -> Text -> Expectation
goldenC name actual = do
  update <- lookupEnv "GOLDEN_UPDATE"
  exists <- doesFileExist path
  case update of
    Just _ -> writeGolden actual
    Nothing
      | not exists -> do
          writeGolden actual
          expectationFailure $
            "Golden file did not exist, created it from the current output: "
            ++ path ++ "\nReview it and re-run the suite to check against it."
      | otherwise -> do
          expected <- TIO.readFile path
          actual `shouldBe` expected

  where

    path :: FilePath
    path = goldenDir </> name <.> "c"

    writeGolden :: Text -> IO ()
    writeGolden content = do
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile path content
