module UT.PPrinter.TypeDefinition.InterfaceSpec (spec) where

import Test.Hspec
import PPrinter
import AST.Seman
import Data.Text
import Semantic.Monad
import qualified Data.Map as M

import Prettyprinter
import Control.Monad.Reader
import Generator.LanguageC.Printer
import Generator.TypeDefinition
import Generator.Common

interfaceWithOneProcedure :: AnnASTElement SemanticAnns
interfaceWithOneProcedure = TypeDefinition (Interface "iface0" [
    InterfaceProcedure "procedure0" [
      Parameter "param0" UInt8,
      Parameter "param1" UInt16,
      Parameter "param2" UInt32,
      Parameter "param3" UInt64,
      Parameter "param4" Int8,
      Parameter "param5" Int16,
      Parameter "param6" Int32,
      Parameter "param7" Int64
    ] undefined
  ] []) undefined

renderTypeDefinitionDecl :: OptionTypes -> AnnASTElement SemanticAnns -> Text
renderTypeDefinitionDecl opts decl = 
  case runReaderT (genTypeDefinitionDecl decl) opts of
    Left err -> pack $ show err
    Right cDecls -> render $ vsep $ runReader (mapM pprint cDecls) (CPrinterConfig False False)

spec :: Spec
spec = do
  describe "Pretty printing classes" $ do
    it "Prints an interface with one procedure" $ do
      renderTypeDefinitionDecl M.empty interfaceWithOneProcedure `shouldBe`
        pack (
          "\ntypedef struct {\n" ++
          "    void * __that;\n" ++
          "    void (* procedure0)(void * __this, uint8_t param0, uint16_t param1,\n" ++
          "                        uint32_t param2, uint64_t param3, int8_t param4,\n" ++
          "                        int16_t param5, int32_t param6, int64_t param7);\n" ++
          "} iface0;")