-- | Unit tests for the generated-identifier length check
-- ('checkIdentifierLengths'). Every currently supported platform declares
-- @maxIdentifierLength = Nothing@ (its GCC-family toolchain imposes no
-- significant-length limit), so the check is a no-op end to end and no
-- integration fixture can exercise the rejecting path. These tests drive the
-- mechanism directly with a synthetic limit so that the day a platform declares
-- a finite limit, the enforcement is known to work, including its reach into
-- function bodies (parameters and block-scope locals).
module Codegen.Positive.IdentifierLengthSpec (spec) where

import Test.Hspec
import Data.Either (isLeft, isRight)

import Generator.LanguageC.AST
import Generator.LanguageC.Embedded (void)
import Generator.CodeGen.Common (checkIdentifierLengths, generatedIdentifiers, internalAnn, CGeneratorError)

ann :: CAnns
ann = internalAnn CGenericAnn

longLocal :: Ident
longLocal = "this_local_variable_name_is_deliberately_long"  -- 45 chars

-- | A function whose body declares a parameter and two block-scope locals, one
-- of them deliberately long. Exercises the descent into the function body.
sampleFunction :: CFileItem
sampleFunction = CFunctionDef Nothing
    (CFunction void "do_work" [CDecl (CTypeSpec void) (Just "param_x") Nothing]
        (CSCompound
            [ CBlockDecl (CDecl (CTypeSpec void) (Just "short_local") Nothing) ann
            , CBlockDecl (CDecl (CTypeSpec void) (Just longLocal) Nothing) ann
            , CBlockStmt (CSReturn Nothing ann)
            ] ann))
    ann

-- | A translation unit with file-scope declarations plus the function above.
sampleFile :: CFile
sampleFile = CHeaderFile "test"
    [ CPPDirective (CPPDefine "SHORT_GUARD" Nothing) (internalAnn (CPPDirectiveAnn False))
    , CExtDecl (CEDTypeDef "my_type_t" void) (internalAnn (CDeclarationAnn False))
    , sampleFunction
    ]

run :: Maybe Integer -> Either CGeneratorError ()
run limit = checkIdentifierLengths limit sampleFile

spec :: Spec
spec = do
  describe "generatedIdentifiers" $ do
    it "collects file-scope declarations" $ do
      let ids = generatedIdentifiers sampleFile
      ids `shouldContain` ["SHORT_GUARD"]
      ids `shouldContain` ["my_type_t"]
      ids `shouldContain` ["do_work"]
    it "descends into function parameters and block-scope locals" $ do
      let ids = generatedIdentifiers sampleFile
      ids `shouldContain` ["param_x"]
      ids `shouldContain` ["short_local"]
      ids `shouldContain` [longLocal]

  describe "checkIdentifierLengths" $ do
    it "is a no-op when the platform declares no limit (Nothing)" $
      run Nothing `shouldSatisfy` isRight
    it "accepts identifiers within a generous limit" $
      run (Just 100) `shouldSatisfy` isRight
    it "rejects a file-scope identifier that exceeds the limit" $
      -- only the long local (45) exceeds 40; proves body reach too
      run (Just 40) `shouldSatisfy` isLeft
    it "rejects an over-long block-scope local" $
      run (Just (toInteger (length longLocal) - 1)) `shouldSatisfy` isLeft
