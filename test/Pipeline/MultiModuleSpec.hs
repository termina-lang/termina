module Pipeline.MultiModuleSpec (spec) where

import Pipeline.Common
import Pipeline.Golden

import Test.Hspec

-- | A type provider module. Its qualified name is @lib/types@, so it is
-- imported as @lib.types@.
libTypes :: String
libTypes =
    "struct Point {\n" ++
    "    x : u32;\n" ++
    "    y : u32;\n" ++
    "};"

-- | The main module imports @lib.types@ and uses its 'Point' struct by bare
-- name, exercising cross-module visibility and per-module rendering.
mainModule :: String
mainModule =
    "import lib.types;\n" ++
    "\n" ++
    "function origin_x() -> u32 {\n" ++
    "    var p : Point = {x = 0 : u32, y = 0 : u32};\n" ++
    "    return p.x;\n" ++
    "}"

project :: [(String, String)]
project = [("lib/types", libTypes), ("test", mainModule)]

spec :: Spec
spec = do
  describe "Full pipeline across multiple modules" $ do
    it "Resolves imports and renders the importing module" $
      goldenC "multimodule_main" (buildAndRenderModule "test" project)
    it "Renders the imported type-provider module" $
      goldenC "multimodule_lib_types" (buildAndRenderModule "lib/types" project)
