{-# LANGUAGE LambdaCase #-}
-- | Exit-path negative tests, *detail* flavour. Exit-path errors carry no
-- payload beyond their source position, so this spec does two things the code
-- spec cannot: it cross-checks the actual error /constructor/ (independently of
-- the @errorIdent@ string the code spec trusts) and asserts the error is
-- reported at a real source 'Position' rather than 'Internal'/'Builtin'.
module ExitPaths.Negative.DetailSpec (spec) where

import ExitPaths.Common (exitPathsError)
import ControlFlow.BasicBlocks.Checks.ExitPaths.Errors (ExitCheckError(..))
import Utils.Annotations (AnnotatedError(..), Location(Position))

import System.FilePath ((</>))
import Test.Hspec

fixturesDir :: FilePath
fixturesDir = "test" </> "ExitPaths" </> "fixtures"

-- | (fixture file, predicate on the raised error constructor).
cases :: [(FilePath, ExitCheckError -> Bool)]
cases =
  [ ("EE-001.fin", \case EEInvalidReturn -> True; _ -> False)
  , ("EE-005.fin", \case EEActionInvalidContinue -> True; _ -> False)
  , ("EE-007.fin", \case EEActionIfBlockShallExit -> True; _ -> False)
  , ("EE-011.fin", \case EEActionIfBlockMissingElseExit -> True; _ -> False)
  , ("EE-013.fin", \case EEActionInvalidReboot -> True; _ -> False)
  ]

spec :: Spec
spec = describe "Exit paths: error detail (constructor + location)" $
  mapM_ each cases
  where
    each (file, isCon) = do
      err <- runIO (exitPathsError <$> readFile (fixturesDir </> file))
      it (file ++ " raises the expected constructor at a source position") $
        err `shouldSatisfy` \case
          Just (AnnotatedError e (Position{})) -> isCon e
          _ -> False
