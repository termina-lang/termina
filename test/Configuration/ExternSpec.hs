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
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml (decodeEither')
import Test.Hspec

manifest :: String -> BS.ByteString
manifest extra = BS.pack $
  "project:\n  name: test\n  version: 0.1.0\n  manifest-version: 1\n" ++
  "app:\n  platform: rtems5-leon3sf-nexysa7\n  profile: release\n" ++
  extra

parsedExtern :: BS.ByteString -> Either String ExternConfig
parsedExtern = either (Left . show) (fmap manifestExtern . parseManifest) . decodeEither'

failsWith :: String -> String -> Expectation
failsWith section message =
  case parsedExtern (manifest section) of
    Left err -> err `shouldSatisfy` isInfixOf message
    Right _ -> expectationFailure "the extern section was accepted"

spec :: Spec
spec = describe "Manifest: extern section" $ do

  it "reads the sources, the headers and the libraries of each module" $
    parsedExtern (manifest
      ("extern:\n" ++
       "  drivers.uart:\n" ++
       "    sources: [uart.c, table.c]\n" ++
       "    headers: [table.h]\n" ++
       "  math.mcs:\n" ++
       "    sources: [mcs.c]\n" ++
       "    libraries:\n" ++
       "      rtems5-leon3sf-nexysa7: [libmcs.a]\n"))
      `shouldBe` Right (ExternConfig (M.fromList
        [ (T.pack "drivers.uart", ExternEntry ["uart.c", "table.c"] ["table.h"] M.empty)
        , (T.pack "math.mcs", ExternEntry ["mcs.c"] [] (M.fromList [(T.pack "rtems5-leon3sf-nexysa7", ["libmcs.a"])])) ]))

  it "lists each file once, under the folder of its kind" $
    externFiles (ExternConfig (M.fromList
      [ (T.pack "drivers.uart", ExternEntry ["uart.c", "table.c"] ["table.h"] M.empty)
      , (T.pack "drivers.spi", ExternEntry ["spi.c", "table.c"] ["table.h"] (M.fromList [(T.pack "posix-gcc", ["libspi.a"])])) ]))
      `shouldBe` ["src/spi.c", "src/table.c", "include/table.h", "lib/posix-gcc/libspi.a", "src/uart.c"]

  it "explains the section of before 0.7.0" $
    "extern:\n  sources:\n    - crc.c\n" `failsWith` "under the module that declares their extern functions"

  it "rejects a key that is not the name of a module" $
    "extern:\n  Drivers.uart:\n    sources: [uart.c]\n" `failsWith` "which is not the name of a module"

  it "rejects an entry with neither sources nor libraries" $
    "extern:\n  drivers.uart:\n    headers: [regs.h]\n" `failsWith` "has neither sources nor libraries"

  it "rejects a file of the wrong kind in a list" $
    "extern:\n  drivers.uart:\n    sources: [uart.c]\n    headers: [regs.c]\n" `failsWith` "which is not a .h file"

  it "takes patterns in the file name of sources and headers only" $ do
    "extern:\n  drivers.uart:\n    sources: [\"*/uart.c\"]\n" `failsWith` "whose * is in a folder"
    "extern:\n  math.mcs:\n    libraries:\n      posix-gcc: [\"*.a\"]\n" `failsWith` "the libraries take no patterns"

  it "matches a pattern against the files of one folder" $
    map (listedMatches "src/spi/*.c") ["src/spi/a.c", "src/spi/b.c", "src/spi/sub/c.c", "src/a.c", "src/spi/a.h"]
      `shouldBe` [True, True, False, False, False]

  it "keeps the files of the section inside the extern folder" $
    map insideFolder ["crc.c", "leon3sf/libmcs.a", "../crc.c", "lib/../../crc.c", "/etc/crc.c"]
      `shouldBe` [True, True, False, False, False]

  it "has no C when the section is absent" $
    parsedExtern (manifest "") `shouldBe` Right emptyExternConfig

  it "links the C into a relocatable object listed among the objects" $ do
    let config = defaultConfig "test" TestPlatform
        makefile = runMakefilePrinter (genMakefile config M.empty (Just (ExternBuild ["src/crc.c"] ["lib/rtems5-leon3sf-nexysa7/libmcs.a"])) False)
    mapM_ (\line -> T.unpack makefile `shouldContain` line)
      [ "EXTERN_SRCS+=$R/extern/src/crc.c\n"
      , "EXTERN_LIBS+=$R/extern/lib/rtems5-leon3sf-nexysa7/libmcs.a\n"
      , "OBJS+=$(EXTERN_OBJ)\n"
      , "$(EXTERN_OBJ): $(EXTERN_OBJS) $(EXTERN_LIBS) $R/Makefile $(TERMINA_FLAGS_FILE)\n"
      , "@$(call termina_extern_link,$@,$(EXTERN_OBJS),$(EXTERN_LIBS),$(EXTERN_SYMBOLS))\n"
      ]

  it "compiles the C with its own headers before the generated ones" $ do
    let config = defaultConfig "test" TestPlatform
        makefile = runMakefilePrinter (genMakefile config M.empty (Just (ExternBuild ["src/crc.c"] [])) False)
    mapM_ (\line -> T.unpack makefile `shouldContain` line)
      [ "$(EXTERN_OBJS): TERMINA_EXTERN_CPPFLAGS:=-iquote $R/extern/include\n"
      , "$(CC) $(CFLAGS) $(TERMINA_EXTERN_CPPFLAGS) $(CPPFLAGS) -MMD -MP -c -o $@ $<"
      ]
