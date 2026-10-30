{-# LANGUAGE OverloadedStrings #-}

-- | The checks of the extern sections, over files and modules written by hand.
module Configuration.ExternCheckSpec (spec) where

import Configuration.Configuration
import Configuration.ExternCheck
import Configuration.Namespace (SourceModule(..), parseModule)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import System.FilePath
import Test.Hspec

-- | The extern functions of a module of a source folder, from its file and
-- its text.
externs :: FilePath -> FilePath -> String -> ModuleExterns
externs folder file text =
    let qname = dropExtension file
    in head (mapMaybe sourceExterns [SourceModule (splitDirectories qname) (folder </> file)
                                         (parseModule qname (folder </> file) (BS.pack text))])

uart, util, clock :: ModuleExterns
uart = externs "src" "drivers/uart.fin" "extern function uart_write(c : u8);\n"
util = externs "src" "drivers/util.fin" "function util_one() -> u32 {\n    return 1;\n}\n"
clock = externs "tests/hk/src" "fake/clock.fin" "extern function now() -> u32;\n"

entry :: [FilePath] -> [FilePath] -> ExternEntry
entry srcs hdrs = ExternEntry srcs hdrs M.empty

seenOf :: [ModuleExterns] -> [([String], FilePath)]
seenOf ms = [ (externSegments m, externFile m) | m <- ms ]

-- | The section of the project, with the modules uart and util; the entries
-- name the modules that are listed.
project :: [(T.Text, ExternEntry)] -> [FilePath] -> ExternSection
project entries files = ExternSection "the project" "extern" "src" (ExternConfig (M.fromList entries))
    files (map externSegments [uart, util]) (seenOf [uart, util, clock]) [uart, util]

-- | The section of the test hk, with the module fake.clock.
test :: [(T.Text, ExternEntry)] -> [FilePath] -> ExternSection
test entries files = ExternSection "the test hk" "tests/hk/extern" "tests/hk/src" (ExternConfig (M.fromList entries))
    files [externSegments clock] (seenOf [uart, util, clock]) [clock]

good :: ExternSection
good = project [("drivers.uart", entry ["uart.c"] ["regs.h"])] ["src/uart.c", "include/regs.h"]

spec :: Spec
spec = do

  describe "a section without problems" $

    it "gives no error" $
      sectionErrors [good, test [("fake.clock", entry ["clock.c"] [])] ["src/clock.c"]] `shouldBe` []

  describe "the folder" $ do

    it "rejects a file out of its folder, and lists each one" $
      sectionErrors [good { sectionFiles = sectionFiles good ++ ["notes.txt", "src/regs.h", "lib/leon3/libx.a"] }]
        `shouldBe` [ "The file extern/notes.txt cannot be in extern/. The C sources go in extern/src/, their headers in "
                       ++ "extern/include/ and the libraries of each platform in extern/lib/<platform>/"
                   , "The file extern/src/regs.h is in extern/src/, which only holds C sources (.c)"
                   , "The file extern/lib/leon3/libx.a is in extern/lib/leon3/, and leon3 is not a platform"
                   , "The file extern/notes.txt is not listed under any module in the extern section of the project"
                   , "The file extern/src/regs.h is not listed under any module in the extern section of the project"
                   , "The file extern/lib/leon3/libx.a is not listed under any module in the extern section of the project" ]

    it "rejects a header with the path of a generated one" $
      sectionErrors [project [("drivers.uart", entry ["uart.c"] ["drivers/uart.h", "config.h"])]
                      ["src/uart.c", "include/drivers/uart.h", "include/config.h"]]
        `shouldBe` [ "The file extern/include/drivers/uart.h has the path of the header that the transpiler generates for "
                       ++ "drivers.uart (src/drivers/uart.fin)"
                   , "The file extern/include/config.h has the path of a header that the transpiler generates" ]

    it "rejects a header of the project with the path of the one of a module of a test" $
      sectionErrors [project [("drivers.uart", entry ["uart.c"] ["fake/clock.h"])] ["src/uart.c", "include/fake/clock.h"]]
        `shouldBe` [ "The file extern/include/fake/clock.h has the path of the header that the transpiler generates for "
                       ++ "fake.clock (tests/hk/src/fake/clock.fin)" ]

  describe "the lists" $

    it "rejects a missing file, a pattern that matches none and a file named twice" $
      sectionErrors [project [("drivers.uart", entry ["uart.c", "spi/*.c", "uart.c"] [])] ["src/uart.c"]]
        `shouldBe` [ "The extern section of the project lists extern/src/spi/*.c under drivers.uart, and the pattern matches no file"
                   , "The extern section of the project lists extern/src/uart.c twice under drivers.uart" ]

  describe "the entries" $ do

    it "rejects an entry that is not a module, a folder or a module without extern functions" $
      sectionErrors [project [ ("drivers.uart", entry ["uart.c"] []), ("drivers.spi", entry ["uart.c"] [])
                             , ("drivers", entry ["uart.c"] []), ("drivers.util", entry ["uart.c"] []) ] ["src/uart.c"]]
        `shouldBe` [ "The extern section of the project lists drivers, which is a folder (src/drivers) and not a module"
                   , "The extern section of the project lists drivers.spi, which is not a module (src/drivers/spi.fin does not exist)"
                   , "The extern section of the project lists drivers.util, which declares no extern function (src/drivers/util.fin)" ]

    it "names the section of a module of another folder" $
      sectionErrors [good { sectionConfig = ExternConfig (M.insert "fake.clock" (entry ["uart.c"] []) (externModules (sectionConfig good))) },
                     test [("fake.clock", entry ["clock.c"] [])] ["src/clock.c"]]
        `shouldBe` [ "The extern section of the project lists fake.clock, which is a module of the test hk "
                       ++ "(tests/hk/src/fake/clock.fin). Move its entry to the extern section of the test hk" ]

  describe "the modules" $ do

    it "rejects a module with an extern function and no entry" $
      moduleErrors "the test hk" "tests/hk/src" (ExternConfig M.empty) Nothing [clock, util]
        `shouldBe` [ "The module fake.clock declares the extern function now and is not listed in the extern section of the test hk" ]

    it "accepts a module with its entry" $
      moduleErrors "the project" "src" (sectionConfig good) Nothing [uart, util] `shouldBe` []

    it "rejects an extern function in the module that wires the deployment" $
      moduleErrors "the project" "src" (sectionConfig good) (Just (externs "app" "app.fin" "\nextern function seven() -> u32;\n")) []
        `shouldBe` [ "app/app.fin:2: app.fin cannot declare the extern function seven. Declare it in a module of src/ "
                       ++ "and list that module in the extern section of the project" ]
