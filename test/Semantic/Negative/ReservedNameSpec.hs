-- | The names of C that a Termina identifier may not take (SE-219). The code
-- spec covers one of the four reasons with a fixture; here the four are
-- contrasted against the names that look alike and are correct, which is where
-- the rule earns its keep: a local object called @total@ or @memory@ must go
-- through, and a global one called @memory@ must not.
module Semantic.Negative.ReservedNameSpec (spec) where

import Semantic.Common (typeCheckErrorOn)

import Test.Hspec
import Data.Text (pack)
import Configuration.Platform (Platform(POSIXGCC, TestPlatform))

-- | @index@ is not in the standard library of C, it comes from <strings.h>,
-- which <string.h> pulls in on the platforms whose flags ask for it. So the
-- only thing that knows about it is the generated list of the platform.
localNamedIndex :: String
localNamedIndex =
  "function scan() -> u8 {\n" ++
  "    var index : u8 = 0;\n" ++
  "    return index;\n" ++
  "}\n"

localNamedMalloc :: String
localNamedMalloc =
  "function allocate() -> u32 {\n" ++
  "    var malloc : u32 = 0;\n" ++
  "    return malloc;\n" ++
  "}\n"

localNamedUnion :: String
localNamedUnion =
  "function tag() -> u32 {\n" ++
  "    var union : u32 = 0;\n" ++
  "    return union;\n" ++
  "}\n"

-- | An object identifier may carry one leading underscore, which is how an
-- unused parameter is written. Followed by an uppercase letter it lands on the
-- names C keeps for itself at every scope.
paramNamedUnderscoreUpper :: String
paramNamedUnderscoreUpper =
  "function ignore(_Unused : u32) -> u32 {\n" ++
  "    return 0 : u32;\n" ++
  "}\n"

paramNamedUnderscoreLower :: String
paramNamedUnderscoreLower =
  "function ignore(_unused : u32) -> u32 {\n" ++
  "    return 0 : u32;\n" ++
  "}\n"

globalNamedMemcpy :: String
globalNamedMemcpy =
  "function memcpy() -> u32 {\n" ++
  "    return 0 : u32;\n" ++
  "}\n"

-- | Names that only look like the ones of the library.
globalNamedMemory :: String
globalNamedMemory =
  "function memory() -> u32 {\n" ++
  "    return 0 : u32;\n" ++
  "}\n"

localNamedMemory :: String
localNamedMemory =
  "function scan() -> u32 {\n" ++
  "    var memory : u32 = 0;\n" ++
  "    var total : u32 = memory;\n" ++
  "    return total;\n" ++
  "}\n"

-- | The types of the prelude are in the global environment and collide there;
-- the ones that take type arguments are recognised by their shape, so nothing
-- held their names until SE-219 did.
structNamedOption, structNamedOptions, globalNamedPool :: String
structNamedOption = "struct Option {\n    a : u32;\n};\n"
structNamedOptions = "struct Options {\n    a : u32;\n};\n"
globalNamedPool =
  "function Pool() -> u32 {\n" ++
  "    return 0;\n" ++
  "}\n"

spec :: Spec
spec = do
  describe "SE-219: names that C keeps for itself" $ do
    it "rejects a local object named after a keyword of C" $
      typeCheckErrorOn TestPlatform localNamedUnion `shouldBe` Just (pack "SE-219")
    it "rejects a local object named after a function of the standard library" $
      typeCheckErrorOn TestPlatform localNamedMalloc `shouldBe` Just (pack "SE-219")
    it "rejects a parameter whose underscore is followed by an uppercase letter" $
      typeCheckErrorOn TestPlatform paramNamedUnderscoreUpper `shouldBe` Just (pack "SE-219")
    it "rejects a function named after one of the standard library" $
      typeCheckErrorOn TestPlatform globalNamedMemcpy `shouldBe` Just (pack "SE-219")

  describe "SE-219: the types of Termina that take type arguments" $ do
    it "rejects a struct named Option" $
      typeCheckErrorOn TestPlatform structNamedOption `shouldBe` Just (pack "SE-219")
    it "rejects a function named Pool" $
      typeCheckErrorOn TestPlatform globalNamedPool `shouldBe` Just (pack "SE-219")
    it "accepts a name that only begins like one of them" $
      typeCheckErrorOn TestPlatform structNamedOptions `shouldBe` Nothing


  describe "SE-219: names that look alike and are correct" $ do
    it "accepts a parameter whose underscore is followed by a lowercase letter" $
      typeCheckErrorOn TestPlatform paramNamedUnderscoreLower `shouldBe` Nothing
    it "accepts a local object whose name only begins like one of the library" $
      typeCheckErrorOn TestPlatform localNamedMemory `shouldBe` Nothing
    it "accepts a function whose name only begins like one of the library" $
      typeCheckErrorOn TestPlatform globalNamedMemory `shouldBe` Nothing

  describe "SE-219: what the platform declares" $ do
    it "rejects a local object named index on a platform that declares it" $
      typeCheckErrorOn POSIXGCC localNamedIndex `shouldBe` Just (pack "SE-219")
    it "accepts the same name on a platform that declares nothing" $
      typeCheckErrorOn TestPlatform localNamedIndex `shouldBe` Nothing
