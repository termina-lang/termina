module PPrinter.TypeDef.UnionSpec (spec) where

import Test.Hspec
import PPrinter
import AST
import Parsing
import Data.Text


{- | Union type with three fields.
In Termina's context sytax:
union id0 {
    field0 : u8;
    field1 : u16;
    field2 : [u16; 10 : u32];
};
-}
unionWithThreeFields :: AnnASTElement Annotation
unionWithThreeFields = TypeDefinition
  (Union "id0" [
    FieldDefinition "field0" UInt8,
    FieldDefinition "field1" UInt16,
    FieldDefinition "field2" (Vector UInt32 (K 10))
  ] [] undefined)

renderSingleASTElement :: AnnASTElement a -> Text
renderSingleASTElement = render . ppHeaderASTElement ppEmptyDoc ppEmptyDoc

spec :: Spec
spec = do
  describe "Pretty printing unions" $ do
    it "Prints a union with just one field" $ do
      renderSingleASTElement unionWithThreeFields `shouldBe`
        pack (
            "\n" ++
            "typedef union {\n" ++
            "    uint8_t field0;\n" ++
            "    uint16_t field1;\n" ++
            "    uint32_t field2[10];\n" ++
            "} id0;\n" ++
            "\n" ++
            "uint8_t __id0__eq(id0 * __lhs, id0 * __rhs);\n")