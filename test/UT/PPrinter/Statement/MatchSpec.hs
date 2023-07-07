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

param0 :: Expression SemanticAnns
param0 = Variable "param0" uint32SemAnn

uint32Const0 :: Expression SemanticAnns
uint32Const0 = Constant (I UInt32 0) uint32SemAnn

optionVar :: Expression SemanticAnns
optionVar = Variable "option_var" optionDynUInt32SemAnn

param0ToFoo1, constToFoo1 :: Statement SemanticAnns
param0ToFoo1 = AssignmentStmt "foo1" param0 undefined
constToFoo1 = AssignmentStmt "foo1" uint32Const0 undefined

matchCaseSome :: MatchCase SemanticAnns
matchCaseSome = MatchCase "Some" ["param0"] [param0ToFoo1] undefined

matchCaseNone :: MatchCase SemanticAnns
matchCaseNone = MatchCase "None" [] [constToFoo1] undefined

matchOption :: Statement SemanticAnns 
matchOption = MatchStmt optionVar [matchCaseSome, matchCaseNone] undefined

renderStatement :: Statement SemanticAnns -> Text
renderStatement = render . ppStatement empty

spec :: Spec
spec = do
  describe "Pretty printing match statements" $ do
    it "Prints a match option statement" $ do
      renderStatement matchOption `shouldBe`
        pack (
          "if (option_var.__variant == Some) {\n" ++
          "\n" ++
          "    foo1 = option_var.__Some.__0;\n" ++
          "\n" ++
          "} else {\n" ++
          "\n" ++
          "    foo1 = (uint32_t)0;\n" ++
          "\n" ++
          "}")
