-- | The addresses of the GDB and XSDB servers of the boards.
module Runner.ServersSpec (spec) where

import Configuration.Platform (Platform(..))
import Runner.Servers

import Data.Either (isLeft)
import Test.Hspec

spec :: Spec
spec = do
  describe "readServer" $ do
    it "reads a host" $
      readServer "GDB server" "coruscant" `shouldBe` Right (Server "coruscant" Nothing)
    it "reads a host and a port" $
      readServer "GDB server" "coruscant:2223" `shouldBe` Right (Server "coruscant" (Just 2223))
    it "reads a port alone, with the default host" $
      readServer "XSDB server" ":3200" `shouldBe` Right (Server "" (Just 3200))
    it "refuses a port that is not a number" $
      readServer "GDB server" "coruscant:gdb" `shouldSatisfy` isLeft

  describe "readGdbPort" $ do
    it "reads a platform and a port" $
      readGdbPort "rtems6-noel32imafd-artya7:2223" `shouldBe` Right (RTEMS6NOEL32IMAFDARTYA7, 2223)
    it "refuses a platform that does not exist" $
      readGdbPort "nexys:2223" `shouldSatisfy` isLeft
    it "refuses a platform without a port" $
      readGdbPort "rtems6-zynq7000-pynqz2" `shouldSatisfy` isLeft

  describe "checkGdbPorts" $ do
    it "takes one port per platform" $
      checkGdbPorts [(RTEMS5LEON3SFNEXYSA7, 2223), (RTEMS6ZYNQ7000PYNQZ2, 3003)]
        `shouldBe` Right [(RTEMS5LEON3SFNEXYSA7, 2223), (RTEMS6ZYNQ7000PYNQZ2, 3003)]
    it "refuses two ports for a platform" $
      checkGdbPorts [(RTEMS5LEON3SFNEXYSA7, 2223), (RTEMS5LEON3SFNEXYSA7, 2224)] `shouldSatisfy` isLeft

  describe "gdbAddress" $ do
    let ports = [(RTEMS5LEON3SFNEXYSA7, 2223)]
    it "takes the port of --gdb-port for its platform" $
      gdbAddress RTEMS5LEON3SFNEXYSA7 ports ("coruscant", Just 4000) `shouldBe` "coruscant:2223"
    it "takes the port of --gdb for the other platforms" $
      gdbAddress RTEMS6ZYNQ7000PYNQZ2 ports ("coruscant", Just 4000) `shouldBe` "coruscant:4000"
    it "takes the port of the platform otherwise" $
      gdbAddress RTEMS6ZYNQ7000PYNQZ2 ports ("localhost", Nothing) `shouldBe` "localhost:3000"

  describe "xsdbAddress" $ do
    it "gives the port given to the PYNQ" $
      xsdbAddress RTEMS6ZYNQ7000PYNQZ2 ("localhost", Just 3200) `shouldBe` Just "localhost:3200"
    it "gives no address to a platform without XSDB" $
      xsdbAddress RTEMS5LEON3SFNEXYSA7 ("localhost", Just 3200) `shouldBe` Nothing
