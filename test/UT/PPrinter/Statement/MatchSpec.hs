module UT.PPrinter.Statement.MatchSpec (spec) where

import Test.Hspec
import PPrinter
import SemanAST
import Data.Text hiding (empty)
import Data.Map
import Semantic.Monad
import PPrinter.Statement
import UT.PPrinter.Expression.Common

optionDynUInt32SemAnn :: SemanticAnns
optionDynUInt32SemAnn = optionDynSemAnn UInt32

vectorAnn, dynVectorAnn :: SemanticAnns
vectorAnn = vectorSemAnn UInt32 (I UInt32 10)
dynVectorAnn = dynVectorSemAnn UInt32 (I UInt32 10)

param0, param1 :: Object SemanticAnns
param0 = Variable "param0" dynUInt32SemAnn
param1 = Variable "param1" dynVectorAnn

uint32Const0 :: Expression SemanticAnns
uint32Const0 = Constant (I UInt32 0) uint32SemAnn

uint8Const0x8 :: Expression SemanticAnns
uint8Const0x8 = Constant (I UInt8 8) uint8SemAnn

optionVar :: Expression SemanticAnns
optionVar = AccessObject (Variable "option_var" optionDynUInt32SemAnn)

vector0IndexConstant :: Expression SemanticAnns
vector0IndexConstant = AccessObject (VectorIndexExpression (Undyn param1 vectorAnn) uint8Const0x8 uint32SemAnn)

foo1 :: Object SemanticAnns
foo1 = Variable "foo1" uint32SemAnn

param0ToFoo1, constToFoo1 :: Statement SemanticAnns
param0ToFoo1 = AssignmentStmt foo1 (AccessObject (Undyn param0 uint32SemAnn)) undefined
constToFoo1 = AssignmentStmt foo1 uint32Const0 undefined

matchCaseSome0 :: MatchCase SemanticAnns
matchCaseSome0 = MatchCase "Some" ["param0"] [param0ToFoo1] undefined

param1ToFoo1 :: Statement SemanticAnns
param1ToFoo1 = AssignmentStmt foo1 vector0IndexConstant undefined

matchCaseSome1 :: MatchCase SemanticAnns
matchCaseSome1 = MatchCase "Some" ["param1"] [param1ToFoo1] undefined

matchCaseNone :: MatchCase SemanticAnns
matchCaseNone = MatchCase "None" [] [constToFoo1] undefined

-- | A match statement with two cases. In Termina syntax:
-- match option_var {
--   Some(param0) => {
--     foo1 = param0;
--   }
--   None => {
--     foo1 = 0;
--   }
-- }
matchOption0 :: Statement SemanticAnns 
matchOption0 = MatchStmt optionVar [matchCaseSome0, matchCaseNone] undefined

matchOption1 :: Statement SemanticAnns
matchOption1 = MatchStmt optionVar [matchCaseNone, matchCaseSome1] undefined

getInteger :: Expression SemanticAnns
getInteger = FunctionExpression "get_integer" [] (SemAnn undefined (ETy (AppType [] (Option (DynamicSubtype UInt32)))))

matchOption2 :: Statement SemanticAnns
matchOption2 = MatchStmt getInteger [matchCaseSome0, matchCaseNone] undefined

renderStatement :: Statement SemanticAnns -> Text
renderStatement = render . ppStatement empty

spec :: Spec
spec = do
  describe "Pretty printing match statements" $ do
    it "Prints a match option statement" $ do
      renderStatement matchOption0 `shouldBe`
        pack (
          "if (option_var.__variant == Some) {\n" ++
          "\n" ++
          "    foo1 = *((uint32_t *)option_var.__Some.__0.data);\n" ++
          "\n" ++
          "} else {\n" ++
          "\n" ++
          "    foo1 = (uint32_t)0;\n" ++
          "\n" ++
          "}")
    it "Prints a match option vector statement" $ do
      renderStatement matchOption1 `shouldBe`
        pack (
          "if (option_var.__variant == None) {\n" ++
          "\n" ++
          "    foo1 = (uint32_t)0;\n" ++
          "\n" ++
          "} else {\n" ++
          "\n" ++
          "    foo1 = ((uint32_t *)option_var.__Some.__0.data)[(uint8_t)8];\n" ++
          "\n" ++
          "}")
    it "Prints a match option statement with a complex expression" $ do
      renderStatement matchOption2 `shouldBe`
        pack (
          "{\n" ++
          "    __termina_option_dyn_t __match = get_integer();\n" ++
          "\n" ++
          "    if (__match.__variant == Some) {\n" ++
          "\n" ++
          "        foo1 = *((uint32_t *)__match.__Some.__0.data);\n" ++
          "\n" ++
          "    } else {\n" ++
          "\n" ++
          "        foo1 = (uint32_t)0;\n" ++
          "\n" ++
          "    }\n" ++
          "}")
