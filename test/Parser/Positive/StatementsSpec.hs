{-# LANGUAGE LambdaCase #-}
-- | Positive parser tests for statements.
module Parser.Positive.StatementsSpec (spec) where

import Parser.Common (parseWith, ctorName)
import Parser.Parsing
  ( returnStmtParser, continueStmtParser, rebootStmtParser
  , ifElseIfStmtParser, matchStmtParser, forLoopStmtParser
  , mutableObjDeclarationParser, immutableObjDeclarationParser
  , assignmentStmtPaser, singleExprStmtParser )
import Parser.AST

import Test.Hspec

spec :: Spec
spec = describe "Parser: statements" $ do

  describe "declarations" $ do
    it "parses a mutable (var) declaration" $
      parseWith mutableObjDeclarationParser "var x : u32 = 0 : u32;" `shouldSatisfy` \case
        Right (Declaration _ Mutable _ _ _) -> True; _ -> False
    it "parses an immutable (let) declaration" $
      parseWith immutableObjDeclarationParser "let x : u32 = 0 : u32;" `shouldSatisfy` \case
        Right (Declaration _ Immutable _ _ _) -> True; _ -> False

  describe "control flow" $ do
    it "parses a return statement" $
      ctorOf returnStmtParser "return x;" `shouldBe` "ReturnStmt"
    it "parses a continue statement" $
      ctorOf continueStmtParser "continue self->act(x);" `shouldBe` "ContinueStmt"
    it "parses a reboot statement" $
      ctorOf rebootStmtParser "reboot;" `shouldBe` "RebootStmt"
    it "parses an if/else statement" $
      ctorOf ifElseIfStmtParser "if (c) { return x; } else { return y; }" `shouldBe` "IfElseStmt"
    it "parses an if/else-if/else chain" $
      ctorOf ifElseIfStmtParser
        "if (a) { return x; } else if (b) { return y; } else { return z; }" `shouldBe` "IfElseStmt"
    it "parses an if with no else" $
      ctorOf ifElseIfStmtParser "if (c) { return x; }" `shouldBe` "IfElseStmt"
    it "parses a match statement" $
      ctorOf matchStmtParser "match x { case Some(v) => { } case None => { } }" `shouldBe` "MatchStmt"
    it "parses a match with a default case" $
      ctorOf matchStmtParser "match x { case None => { } case _ => { } }" `shouldBe` "MatchStmt"
    it "parses a for loop" $
      ctorOf forLoopStmtParser "for i : usize in 0 : usize .. 10 : usize { }" `shouldBe` "ForLoopStmt"
    it "parses a for loop with a while guard" $
      ctorOf forLoopStmtParser "for i : usize in 0 : usize .. 10 : usize while (i < n) { }" `shouldBe` "ForLoopStmt"

  describe "other statements" $ do
    it "parses an assignment" $
      ctorOf assignmentStmtPaser "x = 1 : u32;" `shouldBe` "AssignmentStmt"
    it "parses a single-expression statement" $
      ctorOf singleExprStmtParser "f(x);" `shouldBe` "SingleExpStmt"

  where
    ctorOf p s = either show ctorName (parseWith p s)
