-- | Positive parser tests for module imports and whole-module parsing.
module Parser.Positive.ModulesSpec (spec) where

import Parser.Common (parseWith, parses, ctorName)
import Parser.Parsing (moduleImportParser, terminaModuleParser)

import Test.Hspec

spec :: Spec
spec = describe "Parser: modules and imports" $ do

  it "parses a single-segment-deep import" $
    either show ctorName (parseWith moduleImportParser "import foo.bar;")
      `shouldBe` "ModuleImport"
  it "parses a deeply nested import path" $
    parses moduleImportParser "import drivers.char_dev.uart.apbuart;" `shouldBe` True

  it "parses a whole module with imports and a declaration" $
    either show ctorName
      (parseWith terminaModuleParser "import foo.bar;\n\nfunction f() { return; }")
      `shouldBe` "Termina"
