-- | Full-pipeline golden for a match with a single case and no default: the
-- case needs no check of the variant, and when it binds no variable the
-- matched object is discarded with a cast to void, so that C does not see it
-- unused.
module Pipeline.Positive.SoleMatchCaseSpec (spec) where

import Pipeline.Common
import Golden

import Test.Hspec

-- | The error of a status, bound by a case and matched against the only
-- variant of its enum, which carries no data.
soleMatchCaseSrc :: String
soleMatchCaseSrc =
    "enum Error0 {\n" ++
    "    Variant0\n" ++
    "};\n" ++
    "function fun0(s : Status<Error0>) -> u32 {\n" ++
    "    var r : u32;\n" ++
    "    match s {\n" ++
    "        case Success => {\n" ++
    "            r = 1;\n" ++
    "        }\n" ++
    "        case Failure(error) => {\n" ++
    "            match error {\n" ++
    "                case Variant0 => {\n" ++
    "                    r = 2;\n" ++
    "                }\n" ++
    "            }\n" ++
    "        }\n" ++
    "    }\n" ++
    "    return r;\n" ++
    "}\n"

spec :: Spec
spec = describe "Full pipeline: match with a single case" $
  it "discards the matched object when the case binds nothing" $
    goldenC "sole_match_case" (buildAndRenderModule "test" [("test", soleMatchCaseSrc)])
