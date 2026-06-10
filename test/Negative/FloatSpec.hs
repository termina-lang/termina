module Negative.FloatSpec (spec) where

import Test.Hspec
import Semantic.AST
import Semantic.Errors
import Semantic.Types (SemanticAnn)
import Negative.Common

-- | Modulo (%) is restricted to integer types; floats must be rejected.
testModulo :: String
testModulo = "function test0() {\n" ++
       "    var foo : f32 = 0.0 : f32;\n" ++
       "    foo = foo % 2.0 : f32;\n" ++
       "    return;\n" ++
       "}\n"

-- | Bitwise operators are restricted to integer types.
testBitwiseAnd :: String
testBitwiseAnd = "function test0() {\n" ++
       "    var foo : f32 = 0.0 : f32;\n" ++
       "    foo = foo & 2.0 : f32;\n" ++
       "    return;\n" ++
       "}\n"

-- | Shift operators are restricted to integer types.
testShift :: String
testShift = "function test0() {\n" ++
       "    var foo : f32 = 0.0 : f32;\n" ++
       "    foo = foo << 1 : usize;\n" ++
       "    return;\n" ++
       "}\n"

-- | Equality (== / !=) is not allowed on floating-point types (MISRA-C).
testEquality :: String
testEquality = "function test0() {\n" ++
       "    var foo : f32 = 0.0 : f32;\n" ++
       "    var bar : f32 = 1.0 : f32;\n" ++
       "    var eq : bool = foo == bar;\n" ++
       "    return;\n" ++
       "}\n"

-- | A floating-point literal cannot be annotated with a non-float type.
testFloatLiteralIntType :: String
testFloatLiteralIntType = "function test0() {\n" ++
       "    var foo : u32 = 3.14 : u32;\n" ++
       "    return;\n" ++
       "}\n"

-- | char cannot be cast to a floating-point type.
testCharAsFloat :: String
testCharAsFloat = "function test0() {\n" ++
       "    var c : char = 'a';\n" ++
       "    var f : f32 = 0.0 : f32;\n" ++
       "    f = c as f32;\n" ++
       "    return;\n" ++
       "}\n"

-- | No implicit conversion: an integer literal cannot initialise a float.
testIntLitToFloat :: String
testIntLitToFloat = "function test0() {\n" ++
       "    var f : f32 = 5;\n" ++
       "    return;\n" ++
       "}\n"

-- | No implicit conversion: a float literal cannot initialise an integer.
testFloatLitToInt :: String
testFloatLitToInt = "function test0() {\n" ++
       "    var i : i32 = 5.0;\n" ++
       "    return;\n" ++
       "}\n"

spec :: Spec
spec = do
  describe "Floating-point semantic errors" $ do
    it "SE-214: rejects modulo (%) on floating-point operands" $
      runNegativeTestTypeCheck testModulo
        `shouldSatisfy` isEBinOpExpectedTypeNotInt Modulo TFloat32
    it "SE-214: rejects bitwise and (&) on floating-point operands" $
      runNegativeTestTypeCheck testBitwiseAnd
        `shouldSatisfy` isEBinOpExpectedTypeNotInt BitwiseAnd TFloat32
    it "SE-214: rejects left shift (<<) on a floating-point operand" $
      runNegativeTestTypeCheck testShift
        `shouldSatisfy` isEBinOpExpectedTypeNotInt BitwiseLeftShift TFloat32
    it "SE-051: rejects equality (==) on floating-point operands" $
      runNegativeTestTypeCheck testEquality
        `shouldSatisfy` isEBinOpLeftTypeNotEq RelationalEqual TFloat32
    it "SE-107: rejects a floating-point literal annotated with an integer type" $
      runNegativeTestTypeCheck testFloatLiteralIntType
        `shouldSatisfy` isEInvalidNumericConstantType TUInt32
    it "rejects casting char to a floating-point type" $
      runNegativeTestTypeCheck testCharAsFloat
        `shouldSatisfy` isENotCasteable TChar TFloat32
    it "rejects initialising a float with an integer literal" $
      runNegativeTestTypeCheck testIntLitToFloat
        `shouldSatisfy` isEUnexpectedNumericConstant TFloat32
    it "rejects initialising an integer with a float literal" $
      runNegativeTestTypeCheck testFloatLitToInt
        `shouldSatisfy` isEUnexpectedNumericConstant TInt32

  where
    isEBinOpExpectedTypeNotInt :: Op -> TerminaType SemanticAnn -> Maybe Error -> Bool
    isEBinOpExpectedTypeNotInt op ty = \case
      Just (EBinOpExpectedTypeNotInt op' ty') -> op == op' && ty == ty'
      _ -> False
    isEBinOpLeftTypeNotEq :: Op -> TerminaType SemanticAnn -> Maybe Error -> Bool
    isEBinOpLeftTypeNotEq op ty = \case
      Just (EBinOpLeftTypeNotEq op' ty') -> op == op' && ty == ty'
      _ -> False
    isEInvalidNumericConstantType :: TerminaType SemanticAnn -> Maybe Error -> Bool
    isEInvalidNumericConstantType ty = \case
      Just (EInvalidNumericConstantType ty') -> ty == ty'
      _ -> False
    isENotCasteable :: TerminaType SemanticAnn -> TerminaType SemanticAnn -> Maybe Error -> Bool
    isENotCasteable s t = \case
      Just (ENotCasteable s' t') -> s == s' && t == t'
      _ -> False
    isEUnexpectedNumericConstant :: TerminaType SemanticAnn -> Maybe Error -> Bool
    isEUnexpectedNumericConstant ty = \case
      Just (EUnexpectedNumericConstant ty') -> ty == ty'
      _ -> False
