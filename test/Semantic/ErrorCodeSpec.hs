module Semantic.ErrorCodeSpec (spec) where

import Pipeline.Common (compileErrorCode)

import Test.Hspec
import Data.Text (pack)
import Data.List (sort, isPrefixOf, isSuffixOf, lookup)
import Control.Monad (forM_)
import System.Directory (listDirectory)
import System.FilePath ((</>), dropExtension)

-- | Directory holding the negative semantic fixtures, relative to the package
-- root. Each file is named after the error code it must trigger, e.g.
-- @SE-016.fin@ must make the whole pipeline stop at the type checker with
-- error code @SE-016@.
fixturesDir :: FilePath
fixturesDir = "test" </> "Semantic" </> "fixtures"

-- | Fixtures whose intended error is currently shadowed by an earlier-firing
-- check, so the program stops with a different code than its file name. They
-- are reported as pending (not failures) until the fixture is restructured to
-- isolate the intended error. The second component is the code actually
-- produced today.
shadowed :: [(String, String)]
shadowed =
  [ ("SE-087", "SE-069")  -- enum variant initializer shadowed by monadic variant initializer
  , ("SE-091", "SE-118")  -- enum variant param mismatch shadowed by assignment mismatch
  , ("SE-128", "SE-014")  -- proc call in member function shadowed by resource-class-action check
  , ("SE-132", "SE-014")  -- used global name shadowed by resource-class-action check
  , ("SE-133", "SE-014")  -- used function name shadowed by resource-class-action check
  , ("SE-145", "SE-208")  -- class loop shadowed by task-class-method check
  , ("SE-160", "SE-157")  -- system-init action return type shadowed by unknown-global check
  ]

spec :: Spec
spec = do
  files <- runIO $ do
    entries <- listDirectory fixturesDir
    return $ sort [ f | f <- entries, "SE-" `isPrefixOf` f, ".fin" `isSuffixOf` f ]
  describe "Semantic: exhaustive SE error-code coverage" $
    forM_ files $ \f -> do
      let code = dropExtension f
      case lookup code shadowed of
        Just actual ->
          it (code ++ " triggers " ++ code) $
            pendingWith ("currently shadowed: yields " ++ actual)
        Nothing ->
          it (code ++ " triggers " ++ code) $ do
            src <- readFile (fixturesDir </> f)
            compileErrorCode src `shouldBe` Just (pack code)
