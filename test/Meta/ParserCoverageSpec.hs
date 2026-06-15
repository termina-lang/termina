-- | Meta-test: the positive counterpart of 'Meta.CodeCoverageSpec'. For each
-- grammar-level AST type it enumerates the constructors the parser can produce
-- (read from the data declaration) and fails if any is not asserted by name in a
-- positive parser spec. A new grammar production then breaks the build until a
-- positive test pins the AST node it builds.
--
-- "Asserted by name" means the constructor identifier appears in a
-- @test/Parser/Positive@ spec, either in a LambdaCase pattern (@BinOp@,
-- @Dereference@) or as the expected 'ctorName' string (@"TSUInt64"@, @"Task"@).
module Meta.ParserCoverageSpec (spec) where

import Data.Char (isSpace, isUpper, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, nub, sort, (\\), foldl')
import qualified Data.Set as S
import Control.Monad (forM_)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec

-- | Grammar-level AST sum types whose constructors are productions the positive
-- specs assert, paired with the source file that declares them.
grammarTypes :: [(String, FilePath)]
grammarTypes =
  [ ("TypeSpecifier'",  "src/Core/AST.hs")
  , ("Const'",          "src/Core/AST.hs")
  , ("Object",          "src/Parser/AST.hs")
  , ("Expression",      "src/Parser/AST.hs")
  , ("Statement",       "src/Parser/AST.hs")
  , ("Global'",         "src/Core/AST.hs")
  , ("AnnASTElement'",  "src/Core/AST.hs")
  , ("ClassMember'",    "src/Core/AST.hs")
  , ("TypeDef'",        "src/Core/AST.hs")
  ]

-- | Constructors the parser never builds (produced only by later passes), so a
-- positive *parser* test cannot assert them. Each entry is justified.
allowlist :: [String]
allowlist =
  -- TSConstSubtype (the @const T@ subtype) is introduced by the semantic phase,
  -- not the parser: there is no @TSConstSubtype@ in Parsing.hs.
  [ "TSConstSubtype" ]

-- | The constructors of a data declaration, read from source.
constructorsOf :: String -> String -> [String]
constructorsOf typeName content =
  case dropWhile (not . isDecl) (lines content) of
    [] -> []
    (d : rest) ->
      let body  = d : takeWhile (not . endsBlock) rest
          clean = unwords (map stripComment body)
          afterEq = drop 1 (dropWhile (/= '=') clean)
          alts  = splitOn '|' afterEq
      in nub [ c | alt <- alts, c <- take 1 (words alt), isCtor c ]
  where
    isDecl l =
      let s = dropWhile isSpace l
      in ("data " ++ typeName) `isPrefixOf` s
         && case drop (5 + length typeName) s of
              []      -> True
              (ch : _) -> not (isAlphaNum ch) && ch /= '\''
    endsBlock l = let s = dropWhile isSpace l
                  in "deriving" `isPrefixOf` s || "instance " `isPrefixOf` s
    stripComment = go
      where go ('-':'-':_) = []
            go (x:xs)      = x : go xs
            go []          = []
    isCtor (c:_) = isUpper c
    isCtor []    = False

splitOn :: Char -> String -> [String]
splitOn sep = foldr step [[]]
  where step c acc@(cur:rest) | c == sep  = [] : acc
                              | otherwise = (c:cur) : rest
        step _ []                         = [[]]

-- | All identifier words mentioned across the positive parser specs.
testedIdentifiers :: IO (S.Set String)
testedIdentifiers = do
  let dir = "test" </> "Parser" </> "Positive"
  files <- filter (".hs" `isInfixOf`) <$> listDirectory dir
  blobs <- mapM (readFile . (dir </>)) files
  return $ S.fromList (concatMap identifiers blobs)
  where
    identifiers = foldr collect [] . tokenize
    collect w ws = if not (null w) && isUpper (head w) then w : ws else ws
    tokenize = words . map (\c -> if isAlphaNum c || c == '\'' then c else ' ')

spec :: Spec
spec = describe "Meta: parser produces no untested AST constructor" $ do
  tested <- runIO testedIdentifiers
  forM_ grammarTypes $ \(tyName, file) -> do
    ctors <- runIO (constructorsOf tyName <$> readFile file)
    let missing = sort [ c | c <- ctors, not (c `S.member` tested), c `notElem` allowlist ]
    it (tyName ++ ": every constructor is asserted by a positive test") $
      missing `shouldBe` []
