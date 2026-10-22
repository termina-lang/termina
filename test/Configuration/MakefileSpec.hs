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

testIn :: ProjectProfile -> TerminaConfig
testIn prf = (defaultConfig "test" TestPlatform) { profile = prf, deploymentKind = TestBuild (T.pack "hk") }

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

  it "adds the debugging information to a test in release" $
    makefileFor (testIn Release) `shouldContain` "CFLAGS+=-g\n"

  it "leaves the flags of the profile to a test in debug and to the application" $ do
    makefileFor (testIn Debug) `shouldNotContain` "CFLAGS+=-g\n"
    makefileFor (defaultConfig "test" TestPlatform) `shouldNotContain` "CFLAGS+=-g\n"

  it "deletes the target of a recipe that fails" $
    makefileFor (defaultConfig "test" TestPlatform) `shouldContain` ".DELETE_ON_ERROR:"
