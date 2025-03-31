{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Function where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Semantic.Types
import Control.Monad
import Control.Monad.Except
import Generator.CodeGen.Common
import Generator.CodeGen.Statement
import Generator.LanguageC.Embedded
import Utils.Annotations


genFunctionDecl :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genFunctionDecl (Function identifier parameters rts _ _ ann) = do
    cRetType <- maybe (return (CTVoid noqual)) (genType noqual) rts
    cParamDecls <- mapM genParameterDeclaration parameters
    return [CExtDecl (CEDFunction cRetType identifier cParamDecls) (buildDeclarationAnn ann True)]
genFunctionDecl item = throwError $ InternalError $ "Not a function: " ++ show item

genFunction :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genFunction (Function identifier parameters rts (Block stmts _) _ ann) = do
    cRetType <- maybe (return (CTVoid noqual)) (genType noqual) rts
    cParamDecls <- mapM (\(Parameter pid pty) -> do
        cPty <- genType noqual pty
        return $ pid @: cPty) parameters
    cBody <- foldM (\acc x -> do
        cStmt <- genBlocks x
        return $ acc ++ cStmt) [] stmts
    return [ pre_cr $ function identifier cParamDecls @-> cRetType $
                    ((trail_cr . block $ cBody) |>> location ann) |>> location ann]
genFunction item = throwError $ InternalError $ "Not a function: " ++ show item
