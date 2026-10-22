-- | The "extern" section of the manifest, and the part of the generated
-- Makefile that links the C of the project.
module Configuration.ExternSpec (spec) where

import Configuration.Configuration
import Configuration.Manifest
import Configuration.Platform (Platform(TestPlatform))
import Command.Extern (ExternBuild(..))
import Generator.CodeGen.Application.Makefile (genMakefile)
import Generator.Makefile.Printer (runMakefilePrinter)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml (decodeEither')
import Test.Hspec

manifest :: String -> BS.ByteString
manifest extra = BS.pack $
  "project:\n  name: test\n  version: 0.1.0\n  manifest-version: 1\n" ++
  "app:\n  platform: rtems5-leon3sf-nexysa7\n  profile: release\n" ++
  extra

parsedExtern :: BS.ByteString -> Maybe ExternConfig
parsedExtern = either (const Nothing) (either (const Nothing) (Just . manifestExtern) . parseManifest) . decodeEither'

spec :: Spec
spec = describe "Manifest: extern section" $ do

  it "reads the sources and the libraries of each platform" $
    parsedExtern (manifest
      ("extern:\n" ++
       "  sources:\n" ++
       "    - crc.c\n" ++
       "  libraries:\n" ++
       "    rtems5-leon3sf-nexysa7:\n" ++
       "      - leon3sf/libmcs.a\n"))
      `shouldBe` Just (ExternConfig ["crc.c"] (M.fromList [(T.pack "rtems5-leon3sf-nexysa7", ["leon3sf/libmcs.a"])]))

  it "has no C when the section is absent" $
    parsedExtern (manifest "") `shouldBe` Just (ExternConfig [] M.empty)

  it "links the C into a relocatable object listed among the objects" $ do
    let config = defaultConfig "test" TestPlatform
        makefile = runMakefilePrinter (genMakefile config M.empty (Just (ExternBuild ["crc.c"] ["leon3sf/libmcs.a"])))
    mapM_ (\line -> T.unpack makefile `shouldContain` line)
      [ "EXTERN_SRCS+=$R/extern/crc.c\n"
      , "EXTERN_LIBS+=$R/extern/leon3sf/libmcs.a\n"
      , "OBJS+=$(EXTERN_OBJ)\n"
      , "$(EXTERN_OBJ): $(EXTERN_OBJS) $(EXTERN_LIBS) $R/Makefile\n"
      , "@$(call termina_extern_link,$@,$(EXTERN_OBJS),$(EXTERN_LIBS),$(EXTERN_SYMBOLS))\n"
      ]
