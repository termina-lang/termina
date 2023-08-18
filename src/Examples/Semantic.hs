-- | Small module with some Semantic Examples.

module Examples.Semantic where

import AST

import Parsing
import Semantic.TypeChecking
import Semantic.Monad hiding (getExpType)

import           Text.Parsec.Pos
import Semantic.Utils (getElemSemanticInfo, getExpType)

-- identityFunction :: Annotation -> Annotation -> TypeSpecifier -> AnnASTElement a
-- identityFunction annF annR type_ident =
--   Function "ident" [Parameter param  type_ident] (Just type_ident) (BlockRet [] (ReturnStmt (Just (Variable param annR)) annR)) [] annF
--   where
--     param = "x"

-- callingIdent :: Annotation -> Expression Annotation
-- callingIdent ann = FunctionExpression "ident" [ Constant (B True) ann ] ann

-- typingInvocation :: (Either SemanticErrors (Expression SemanticAnns) , ExpressionState)
-- typingInvocation = runTypeChecking state
--   expressionType (callingIdent (Position calling_pos))
--   where
--     init_position = initialPos "TestingIdentity"
--     function_decl = incSourceLine init_position 1
--     return_fun = incSourceLine function_decl 1
--     calling_pos = incSourceLine return_fun 42
--     --
--     state = insertGlobalFun (Position function_decl) "ident" [Parameter "x" Bool] Bool

-- getTypeOfCall :: Expression SemanticAnns -> Maybe TypeSpecifier
-- getTypeOfCall e@( FunctionExpression {}) = getExpType e
-- getTypeOfCall _ = Nothing
