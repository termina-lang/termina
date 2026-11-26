-- | The names of the generated C that C or the platform keeps for itself, which
-- the build rejects before writing the files (CE codes).
module Codegen.Negative.CodeSpec (spec) where

import Test.Hspec

import Configuration.Platform (Platform(..))
import Generator.Names.Errors (Error(..))
import Pipeline.Common (generatedNameErrorOn)
import Semantic.Reserved (ReservedBy(..))

-- | A class uuid with a procedure generate_random, which the C names
-- uuid__generate_random. Neither part is reserved, but the whole name is a
-- symbol of the RTEMS of the Nexys A7.
uuidGenerator :: String
uuidGenerator = unlines
  [ "interface IGenerator {"
  , "    procedure generate_random(&mut self, n : &mut u32);"
  , "};"
  , "resource class uuid provides IGenerator {"
  , "    count : u32;"
  , "    procedure generate_random(&mut self, n : &mut u32) {"
  , "        *n = self->count;"
  , "        return;"
  , "    }"
  , "};" ]

spec :: Spec
spec = describe "Codegen: names of the generated C" $ do

  it "CE-001: a procedure whose C name the platform links" $
    generatedNameErrorOn RTEMS5LEON3SFNEXYSA7 [("test", uuidGenerator)] `shouldSatisfy` \case
      Right (Just (EGeneratedNameReserved "uuid__generate_random" (CLinked RTEMS5LEON3SFNEXYSA7))) -> True
      _ -> False

  it "accepts the same procedure on a platform that does not link that name" $
    generatedNameErrorOn POSIXGCC [("test", uuidGenerator)] `shouldSatisfy` \case
      Right Nothing -> True
      _ -> False
