module ExitPaths.Negative.CodeSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)
import Data.List (sort, isPrefixOf, isSuffixOf)
import Control.Monad (forM_)
import System.Directory (listDirectory)
import System.FilePath ((</>), dropExtension)

-- | Directory holding the negative basic-block (exit-path) fixtures, one
-- @EE-NNN.fin@ per error code. The exit-path check runs after type checking,
-- so the whole pipeline stops at it with code @EE-NNN@.
fixturesDir :: FilePath
fixturesDir = "test" </> "ExitPaths" </> "fixtures"

-- | Human-readable title per error code (mirrors @errorTitle@ in
-- @ControlFlow.BasicBlocks.Checks.ExitPaths.Errors@).
titles :: [(String, String)]
titles =
  [ ("EE-001", "invalid return statement")
  , ("EE-002", "invalid continue statement")
  , ("EE-003", "missing return statement")
  , ("EE-004", "missing exit point on an action")
  , ("EE-005", "invalid continue statement on an action")
  , ("EE-006", "invalid send statement on an action")
  , ("EE-007", "missing continue statement on an action if block")
  , ("EE-008", "missing continue statement on an action match block")
  , ("EE-009", "if block shall not exit")
  , ("EE-010", "match block shall not exit")
  , ("EE-011", "missing else exit on an action if block")
  , ("EE-012", "invalid reboot statement")
  , ("EE-013", "invalid reboot statement on an action")
  ]

spec :: Spec
spec = do
  files <- runIO $ do
    entries <- listDirectory fixturesDir
    return $ sort [ f | f <- entries, "EE-" `isPrefixOf` f, ".fin" `isSuffixOf` f ]
  describe "Basic blocks: exit-path error-code coverage (one fixture per code)" $
    forM_ files $ \f -> do
      let code = dropExtension f
          name = maybe code (\t -> code ++ ": " ++ t) (lookup code titles)
      it name $ do
        src <- readFile (fixturesDir </> f)
        compileErrorCode src `shouldBe` Just (pack code)
