{-# LANGUAGE LambdaCase #-}
-- | Positive parser tests for the expression grammar ('expressionParser').
module Parser.Positive.ExpressionsSpec (spec) where

import Parser.Common (parseWith, parses, ctorName)
import Parser.Parsing (expressionParser)
import Parser.AST
import Parser.Types (ParserAnn)
import Core.AST

import Test.Hspec

expr :: String -> Either String (Expression ParserAnn)
expr s = either (Left . show) Right (parseWith expressionParser s)

-- | Outermost constructor of the parsed expression.
head' :: String -> Either String String
head' = fmap ctorName . expr

spec :: Spec
spec = describe "Parser: expressions" $ do

  describe "binary operators (full table)" $
    mapM_ (\(sym, op) -> it ("parses " ++ sym) $
              expr ("a " ++ sym ++ " b") `shouldSatisfy` \case
                Right (BinOp o _ _ _) -> o == op; _ -> False)
      [ ("*", Multiplication), ("/", Division), ("%", Modulo)
      , ("+", Addition), ("-", Subtraction)
      , ("<<", BitwiseLeftShift), (">>", BitwiseRightShift)
      , ("<", RelationalLT), ("<=", RelationalLTE)
      , (">", RelationalGT), (">=", RelationalGTE)
      , ("==", RelationalEqual), ("!=", RelationalNotEqual)
      , ("&", BitwiseAnd), ("|", BitwiseOr), ("^", BitwiseXor)
      , ("&&", LogicalAnd), ("||", LogicalOr)
      ]

  describe "operator precedence and associativity" $ do
    it "binds * tighter than + (a + b * c)" $
      expr "a + b * c" `shouldSatisfy` \case
        Right (BinOp Addition _ (BinOp Multiplication _ _ _) _) -> True; _ -> False
    it "binds * tighter than + (a * b + c)" $
      expr "a * b + c" `shouldSatisfy` \case
        Right (BinOp Addition (BinOp Multiplication _ _ _) _ _) -> True; _ -> False
    it "is left-associative (a - b - c)" $
      expr "a - b - c" `shouldSatisfy` \case
        Right (BinOp Subtraction (BinOp Subtraction _ _ _) _ _) -> True; _ -> False
    it "groups with parentheses ((a + b) * c)" $
      expr "(a + b) * c" `shouldSatisfy` \case
        Right (BinOp Multiplication (BinOp Addition _ _ _) _ _) -> True; _ -> False

  describe "casts" $
    it "parses a cast" $
      expr "x as u16" `shouldSatisfy` \case Right (Casting _ _ _) -> True; _ -> False

  describe "'is' monadic-variant tests" $
    mapM_ (\(src, lbl) -> it ("parses " ++ show src) $
              expr src `shouldSatisfy` \case
                Right (IsMonadicVariantExpression _ l _) -> show l == lbl; _ -> False)
      [ ("x is None", "NoneLabel"), ("x is Some", "SomeLabel")
      , ("x is Ok", "OkLabel"), ("x is Error", "ErrorLabel")
      , ("x is Success", "SuccessLabel"), ("x is Failure", "FailureLabel")
      ]

  describe "references and dereference" $ do
    it "parses an immutable reference" $
      expr "&x" `shouldSatisfy` \case Right (ReferenceExpression Immutable _ _) -> True; _ -> False
    it "parses a mutable reference" $
      expr "&mut x" `shouldSatisfy` \case Right (ReferenceExpression Mutable _ _) -> True; _ -> False
    it "parses a dereference" $
      expr "*p" `shouldSatisfy` \case Right (AccessObject (Dereference _ _)) -> True; _ -> False
    it "parses a reference to an array slice" $
      parses expressionParser "&x[0 .. 2]" `shouldBe` True

  describe "objects (asserting the inner Object node)" $ do
    it "parses a bare variable" $
      expr "x" `shouldSatisfy` \case Right (AccessObject (Variable "x" _)) -> True; _ -> False
    it "parses a member access" $
      expr "a.b" `shouldSatisfy` \case Right (AccessObject (MemberAccess _ "b" _)) -> True; _ -> False
    it "parses a dereferencing member access" $
      expr "a->b" `shouldSatisfy` \case Right (AccessObject (DereferenceMemberAccess _ "b" _)) -> True; _ -> False
    it "parses an array index" $
      expr "a[i]" `shouldSatisfy` \case Right (AccessObject (ArrayIndexExpression _ _ _)) -> True; _ -> False
    it "parses a nested member access" $ head' "a.b.c" `shouldBe` Right "AccessObject"
    it "parses a reference to an array slice" $
      expr "&x[0 .. 2]" `shouldSatisfy` \case
        Right (ReferenceExpression Immutable (ArraySlice _ _ _ _) _) -> True; _ -> False

  describe "calls" $ do
    it "parses a function call" $
      expr "f(x)" `shouldSatisfy` \case Right (FunctionCall "f" _ _) -> True; _ -> False
    it "parses a function call with several arguments" $
      expr "f(x, y, z)" `shouldSatisfy` \case Right (FunctionCall "f" [_,_,_] _) -> True; _ -> False
    it "parses a function call with no arguments" $
      expr "f()" `shouldSatisfy` \case Right (FunctionCall "f" [] _) -> True; _ -> False
    it "parses a nested function call" $
      expr "f(g(x))" `shouldSatisfy` \case Right (FunctionCall "f" [_] _) -> True; _ -> False
    it "parses a method call through '.'" $
      expr "o.m(x)" `shouldSatisfy` \case Right (MemberFunctionCall _ "m" _ _) -> True; _ -> False
    it "parses a method call through '->'" $
      expr "self->m(x)" `shouldSatisfy` \case Right (DerefMemberFunctionCall _ "m" _ _) -> True; _ -> False

  describe "enum variant test" $
    it "parses an 'is' enum variant test" $
      expr "x is Colour::Red" `shouldSatisfy` \case
        Right (IsEnumVariantExpression _ "Colour" "Red" _) -> True; _ -> False

  describe "initializers" $ do
    it "parses a struct initializer" $
      expr "{a = 1, b = 2}" `shouldSatisfy` \case Right (StructInitializer{}) -> True; _ -> False
    it "parses a field address assignment in a struct initializer" $
      parses expressionParser "{registers @ 0x80000100}" `shouldBe` True
    it "parses an array list initializer" $
      expr "{1, 2, 3}" `shouldSatisfy` \case Right (ArrayExprListInitializer{}) -> True; _ -> False
    it "parses an array fill initializer" $
      expr "[0; 4]" `shouldSatisfy` \case Right (ArrayInitializer{}) -> True; _ -> False
    it "parses a string initializer" $
      expr "\"hello\"" `shouldSatisfy` \case Right (StringInitializer{}) -> True; _ -> False
    it "parses a Some initializer" $
      expr "Some(x)" `shouldSatisfy` \case Right (MonadicVariantInitializer{}) -> True; _ -> False
    it "parses an enum variant initializer" $
      expr "Colour::Red" `shouldSatisfy` \case Right (EnumVariantInitializer{}) -> True; _ -> False
