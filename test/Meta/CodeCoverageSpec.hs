-- | Meta-test: audits the test suite itself. For each error family it computes
--
--     codes the compiler can emit   (the code literals of its Errors.hs)
--   − codes that have a test         (fixtures + the literals in its CodeSpec)
--   − codes on an explicit allowlist (reachable-but-untested / unreachable)
--
-- and fails if anything is left over. A new compiler error code then breaks the
-- build until it gets a test or a justified allowlist entry. The ground truth is
-- the @"XX-NNN"@ literal the family writes to print it, so a code written with
-- the prefix of another family leaves a hole in its own list.
module Meta.CodeCoverageSpec (spec) where

import Data.Char (isDigit, isAlphaNum)
import Data.List (tails, stripPrefix, nub, sort, (\\))
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
  , Family "VarUsage (VUE)" "VUE"
      "src/ControlFlow/VarUsage/Errors.hs"
      ["test/VarUsage/Negative/CodeSpec.hs"] []
      []
  , Family "VarScope (VSE)" "VSE"
      "src/ControlFlow/VarScope/Errors.hs"
      ["test/VarScope/Negative/CodeSpec.hs"] []
      []
  , Family "BoxUsage (BE)" "BE"
      "src/ControlFlow/BoxUsage/Errors.hs"
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
  , Family "ConstFolding (CFE)" "CFE"
      "src/ControlFlow/ConstFolding/Errors.hs"
      [ "test/ConstFolding/Negative/ConstEvalSpec.hs"
      , "test/ConstFolding/Negative/AtomicSpec.hs"
      , "test/ConstFolding/Negative/CastSpec.hs"
      , "test/ConstFolding/Negative/SizeSpec.hs" ] []
      []
  , Family "ValueAnalysis (VAE)" "VAE"
      "src/ControlFlow/ValueAnalysis/Errors.hs"
      ["test/ValueAnalysis/Negative/CodeSpec.hs"] []
      []
    -- The five families of the scheduling analysis have no tests of their own
    -- yet, so their codes are allowlisted whole. What the meta-test buys here is
    -- that a code written with the prefix of another family leaves its own family
    -- short and shows up as a hole in this list.
  , Family "MAST generator (MSTE)" "MSTE"
      "src/EFP/Schedulability/MAST/Errors.hs" [] []
      ["MSTE-001", "MSTE-002", "MSTE-003"]
  , Family "Transactional paths (TPE)" "TPE"
      "src/EFP/Schedulability/TransPath/Errors.hs" [] []
      ["TPE-001", "TPE-002"]
  , Family "Worst-case paths (WCEPE)" "WCEPE"
      "src/EFP/Schedulability/WCEPath/Errors.hs" [] []
      [ "WCEPE-001", "WCEPE-002", "WCEPE-003", "WCEPE-004", "WCEPE-005"
      , "WCEPE-006", "WCEPE-007", "WCEPE-008", "WCEPE-009", "WCEPE-010" ]
  , Family "Worst-case times (WTE)" "WTE"
      "src/EFP/Schedulability/WCET/Errors.hs" [] []
      [ "WTE-001", "WTE-002", "WTE-003", "WTE-004", "WTE-005", "WTE-006"
      , "WTE-007", "WTE-008", "WTE-009", "WTE-010", "WTE-011" ]
  , Family "Real-time model (RTE)" "RTE"
      "src/EFP/Schedulability/RT/Errors.hs" [] []
      [ "RTE-" ++ pad n | n <- [1 .. 44 :: Int] ]
  ]

  where

    pad :: Int -> String
    pad n = let s = show n in replicate (3 - length s) '0' ++ s

-- | Every @PREFIX-<digits>@ token in a string, at a word boundary (so "PE" does
-- not match inside "CPE").
codesWithPrefix :: String -> String -> [String]
codesWithPrefix = codesPrecededBy (not . isAlphaNum)

-- | The same, restricted to the ones written as a string literal.
quotedCodesWithPrefix :: String -> String -> [String]
quotedCodesWithPrefix = codesPrecededBy (== '"')

codesPrecededBy :: (Char -> Bool) -> String -> String -> [String]
codesPrecededBy precedes prefix text =
  nub [ prefix ++ "-" ++ ds
      | (pre, t) <- zip (' ' : text) (tails text)
      , precedes pre
      , Just afterDash <- [stripPrefix (prefix ++ "-") t]
      , let ds = takeWhile isDigit afterDash
      , not (null ds) ]

-- | Codes the compiler can emit: the @PREFIX-NNN@ string literals of its
-- Errors.hs, which is where a code is written to be printed. The quote is what
-- tells them from the codes named in the comments of the error data type, which
-- claim a code rather than emit it.
emittableCodes :: Family -> IO [String]
emittableCodes fam = do
  content <- readFile (fErrorsFile fam)
  return $ quotedCodesWithPrefix (fPrefix fam) content

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
