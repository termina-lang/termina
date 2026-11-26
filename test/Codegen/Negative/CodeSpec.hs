-- | Names of the generated C reserved by C or the platform (CE codes).
module Codegen.Negative.CodeSpec (spec) where

import Test.Hspec

import Configuration.Platform (Platform(..))
import Generator.Names.Errors (Error(..))
import Pipeline.Common (generatedNameErrorOn)
import Semantic.Reserved (ReservedBy(..))

-- | Class whose procedure becomes uuid__generate_random, a symbol of RTEMS 5.
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
