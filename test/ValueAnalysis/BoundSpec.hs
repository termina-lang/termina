-- | The bound a branch puts on a variable, driven from the two functions that
-- work it out.
--
-- Bounding is the only operation of the pass that makes it claim /more/ about
-- a variable, so a shifted end or an inverted side reports a condition of
-- correct code. 'boundOf' is where a strict comparison turns into an end that
-- is included, and 'narrow' is where a bound that would leave a variable with
-- no value at all is refused.
module ValueAnalysis.BoundSpec (spec) where

import ControlFlow.ValueAnalysis (Bound(..), Value(..), Values(..), boundOf, narrow)
import Core.AST (Const'(..), IntRepr(..), Op(..), TInteger(..))

import qualified Data.Set as S
import Control.Monad (forM_)
import Test.Hspec

number :: Integer -> Value
number value = Value (I (TInteger value DecRepr) Nothing)

discrete :: [Integer] -> Values
discrete = Discrete . S.fromList . map number

boolean :: Bool -> Values
boolean value = Discrete (S.singleton (Value (B value)))

-- | What a comparison against the value 10 says about the variable, on the
-- side of the branch that takes it and on the side that does not.
boundCases :: [(String, Bool, Op, Maybe Bound)]
boundCases =
  [ ("x < 10 where it holds", True, RelationalLT, Just (AtMost 9))
  , ("x < 10 where it does not", False, RelationalLT, Just (AtLeast 10))
  , ("x <= 10 where it holds", True, RelationalLTE, Just (AtMost 10))
  , ("x <= 10 where it does not", False, RelationalLTE, Just (AtLeast 11))
  , ("x > 10 where it holds", True, RelationalGT, Just (AtLeast 11))
  , ("x > 10 where it does not", False, RelationalGT, Just (AtMost 10))
  , ("x >= 10 where it holds", True, RelationalGTE, Just (AtLeast 10))
  , ("x >= 10 where it does not", False, RelationalGTE, Just (AtMost 9))
  , ("an equality bounds nothing", True, RelationalEqual, Nothing)
  , ("an inequality bounds nothing", False, RelationalNotEqual, Nothing)
  , ("an operator that is not a comparison", True, Addition, Nothing)
  ]

-- | What a bound leaves of the values a variable may hold. Nothing means the
-- variable is left exactly as it was, either because the bound rules out
-- nothing or because it would rule out everything.
narrowCases :: [(String, Bound, Values, Maybe Values)]
narrowCases =
  [ ("an upper end cuts the values above it"
    , AtMost 3, discrete [0, 1, 2, 3, 4, 5], Just (discrete [0, 1, 2, 3]))
  , ("a lower end cuts the values below it"
    , AtLeast 3, discrete [0, 1, 2, 3, 4, 5], Just (discrete [3, 4, 5]))
  , ("a bound that rules out no value changes nothing"
    , AtMost 10, discrete [0, 1], Nothing)
  , ("a bound that rules out every value changes nothing"
    , AtMost (-1), discrete [0, 1], Nothing)
  , ("a bound on values that are not integers changes nothing"
    , AtMost 3, boolean True, Nothing)
  , ("a lower end raises the low end of a range"
    , AtLeast 5, Interval 0 9, Just (Interval 5 9))
  , ("an upper end lowers the high end of a range"
    , AtMost 5, Interval 0 9, Just (Interval 0 5))
  , ("a lower end at the low end of a range changes nothing"
    , AtLeast 0, Interval 0 9, Nothing)
  , ("an upper end at the high end of a range changes nothing"
    , AtMost 9, Interval 0 9, Nothing)
  , ("a lower end above a whole range changes nothing"
    , AtLeast 10, Interval 0 9, Nothing)
  , ("an upper end below a whole range changes nothing"
    , AtMost (-1), Interval 0 9, Nothing)
  , ("a bound that pins a range to one value is kept"
    , AtLeast 9, Interval 0 9, Just (Interval 9 9))
  ]

spec :: Spec
spec = do
  describe "ValueAnalysis: the bound a comparison puts on a variable" $
    forM_ boundCases $ \(what, holds, op, expected) ->
      it what $ boundOf holds op 10 `shouldBe` expected

  describe "ValueAnalysis: what a bound leaves of the values of a variable" $
    forM_ narrowCases $ \(what, bound, values, expected) ->
      it what $ narrow bound values `shouldBe` expected
