-- | Meta-test: audits the test suite itself. For each error family it computes
--
--     codes the compiler can emit   (the @errorIdent@ clauses of its Errors.hs)
--   − codes that have a test         (fixtures + the literals in its CodeSpec)
--   − codes on an explicit allowlist (reachable-but-untested / unreachable)
--
-- and fails if anything is left over. A new compiler error code then breaks the
-- build until it gets a test or a justified allowlist entry. The ground truth is
-- @errorIdent@, the very function that prints the @XX-NNN@ a user sees.
module Meta.CodeCoverageSpec (spec) where

import Data.Char (isDigit, isAlphaNum)
import Data.List (tails, isInfixOf, stripPrefix, nub, sort, (\\))
import Control.Monad (forM_, filterM)
import System.Directory (listDirectory, doesDirectoryExist)
import Test.Hspec

-- | An error family: where its codes are defined and where they are tested.
data Family = Family
  { fName        :: String      -- ^ human label
  , fPrefix      :: String      -- ^ code prefix ("SE", "EE", ...)
  , fErrorsFile  :: FilePath    -- ^ the Errors.hs that defines errorIdent
  , fSpecFiles   :: [FilePath]  -- ^ spec sources whose code literals count as tested
  , fFixtureDirs :: [FilePath]  -- ^ dirs whose entry names are tested codes
  , fAllowlist   :: [String]    -- ^ codes deliberately left uncovered (with reason in comments)
  }

families :: [Family]
families =
  [ Family "Parser (PE)" "PE"
      "src/Parser/Errors.hs"
      ["test/Parser/Negative/CodeSpec.hs"] []
      -- PE-002 (module at project root) and PE-003 (imported file not found)
      -- come from the on-disk module loader, unreachable from in-memory sources.
      ["PE-002", "PE-003"]
  , Family "Semantic (SE)" "SE"
      "src/Semantic/Errors.hs"
      -- Most SE codes are tested via fixtures; the float-exclusive ones (e.g.
      -- SE-214) and the detail assertions name their code in the it-label.
      ["test/Semantic/Negative/FloatSpec.hs", "test/Semantic/Negative/DetailSpec.hs"]
      ["test/Semantic/fixtures", "test/Semantic/fixtures-multi"]
      []
  , Family "VarUsage (VE)" "VE"
      "src/ControlFlow/VarUsage/Errors.hs"
      ["test/VarUsage/Negative/CodeSpec.hs"] []
      []
  , Family "Architecture (AE)" "AE"
      "src/ControlFlow/Architecture/Errors.hs"
      ["test/Architecture/Negative/CodeSpec.hs"] []
      []
  , Family "ExitPaths (EE)" "EE"
      "src/ControlFlow/BasicBlocks/Checks/ExitPaths/Errors.hs"
      [] ["test/ExitPaths/fixtures"]
      []
  , Family "ConstFolding (CPE)" "CPE"
      "src/ControlFlow/ConstFolding/Errors.hs"
      [ "test/ConstFolding/Negative/ConstEvalSpec.hs"
      , "test/ConstFolding/Negative/AtomicSpec.hs"
      , "test/ConstFolding/Negative/CastSpec.hs"
      , "test/ConstFolding/Negative/SizeSpec.hs" ] []
      []
  ]

-- | Every @PREFIX-<digits>@ token in a string, at a word boundary (so "PE" does
-- not match inside "CPE").
codesWithPrefix :: String -> String -> [String]
codesWithPrefix prefix text =
  nub [ prefix ++ "-" ++ ds
      | (pre, t) <- zip (' ' : text) (tails text)
      , not (isAlphaNum pre)
      , Just afterDash <- [stripPrefix (prefix ++ "-") t]
      , let ds = takeWhile isDigit afterDash
      , not (null ds) ]

-- | Codes the compiler can emit: the @PREFIX-NNN@ literals on @errorIdent@ lines.
emittableCodes :: Family -> IO [String]
emittableCodes fam = do
  content <- readFile (fErrorsFile fam)
  let identLines = unlines [ l | l <- lines content, "errorIdent" `isInfixOf` l ]
  return $ codesWithPrefix (fPrefix fam) identLines

-- | Codes that have a test: fixture entry names plus literals in the spec files.
testedCodes :: Family -> IO [String]
testedCodes fam = do
  dirs <- filterM doesDirectoryExist (fFixtureDirs fam)
  fixtureNames <- concat <$> mapM listDirectory dirs
  specContents <- mapM readFile (fSpecFiles fam)
  let blob = unwords fixtureNames ++ " " ++ unwords specContents
  return $ codesWithPrefix (fPrefix fam) blob

spec :: Spec
spec = describe "Meta: error-code coverage per family" $
  forM_ families $ \fam -> do
    emittable <- runIO (emittableCodes fam)
    tested    <- runIO (testedCodes fam)
    let missing = sort (nub emittable \\ (tested ++ fAllowlist fam))
    it (fName fam ++ ": every emittable code is tested or allowlisted") $
      missing `shouldBe` []
