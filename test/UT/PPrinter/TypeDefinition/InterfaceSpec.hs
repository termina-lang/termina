module UT.PPrinter.TypeDefinition.InterfaceSpec (spec) where

import Test.Hspec
import Semantic.AST
import Data.Text
import Semantic.Types
import qualified Data.Map as M

import Prettyprinter
import Control.Monad.Reader
import Generator.LanguageC.Printer
import Generator.CodeGen.TypeDefinition
import Generator.CodeGen.Common
import ControlFlow.BasicBlocks
import Control.Monad.Except


interfaceWithOneProcedure :: AnnASTElement SemanticAnn
interfaceWithOneProcedure = TypeDefinition (Interface "iface0" [
    InterfaceProcedure "procedure0" [
      Parameter "param0" TUInt8,
      Parameter "param1" TUInt16,
      Parameter "param2" TUInt32,
      Parameter "param3" TUInt64,
      Parameter "param4" TInt8,
      Parameter "param5" TInt16,
      Parameter "param6" TInt32,
      Parameter "param7" TInt64
    ] undefined
  ] []) undefined

renderTypeDefinitionDecl :: OptionTypes -> AnnASTElement SemanticAnn -> Text
renderTypeDefinitionDecl opts decl = 
  case runExcept . genBBAnnASTElement $ decl of
    Left err -> pack $ show err
    Right bbDecl ->
      case runReaderT (genTypeDefinitionDecl bbDecl) opts of
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
          "    void (* procedure0)(void * const, uint8_t, uint16_t, uint32_t, uint64_t,\n" ++
          "                        int8_t, int16_t, int32_t, int64_t);\n" ++
          "} iface0;")