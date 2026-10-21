-- | The parts of the generated Makefile that come from the configuration of
-- the project and not from its modules.
module Configuration.MakefileSpec (spec) where

import Configuration.Configuration
import Configuration.Platform (Platform(TestPlatform))
import Generator.CodeGen.Application.Makefile (genMakefile)
import Generator.Makefile.Printer (runMakefilePrinter)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

makefileFor :: TerminaConfig -> String
makefileFor config = T.unpack . runMakefilePrinter $ genMakefile config M.empty Nothing

spec :: Spec
spec = describe "Makefile: project configuration" $ do

  it "lets the environment choose the OSAL directory" $
    makefileFor (defaultConfig "test" TestPlatform) `shouldContain` "TERMINA_OSAL_DIR ?= /opt/termina/osal\n"

  it "passes no buffer size when the project keeps the defaults" $
    makefileFor (defaultConfig "test" TestPlatform) `shouldNotContain` "BUFFER_SIZE"

  it "passes the buffer sizes of sys_print and sys_read as flags" $ do
    let config = (defaultConfig "test" TestPlatform) {
            sysPrintOutputBufferSize = 512,
            sysReadInputBufferSize = 64
          }
    mapM_ (\line -> makefileFor config `shouldContain` line)
      [ "CFLAGS+=-DTERMINA__SYS_PRINT__OUTPUT_BUFFER_SIZE=512\n"
      , "CFLAGS+=-DTERMINA__SYS_READ__INPUT_BUFFER_SIZE=64\n"
      ]
