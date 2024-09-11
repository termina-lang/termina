{-# LANGUAGE FlexibleContexts #-}

module Generator.CCCodeGen.Function where

import AST.Seman
import Generator.LanguageC.CompCertC
import Semantic.Monad
import Control.Monad.Except
import Generator.CCCodeGen.Common
import Generator.CCCodeGen.Statement


genFunctionDecl :: AnnASTElement SemanticAnn -> CHeaderGenerator [CExternalDeclaration]
genFunctionDecl (Function identifier parameters rts _ _ ann) = do
    cRetType <- maybe (return CTVoid) (genType noqual) rts
    cParamDecls <- mapM genParameterDeclaration parameters
    return [CEDFunction cRetType identifier cParamDecls (buildDeclarationAnn ann True)]
genFunctionDecl item = throwError $ InternalError $ "Not a function: " ++ show item

genFunction :: AnnASTElement SemanticAnn -> CSourceGenerator [CFileItem]
genFunction (Function identifier parameters rts (BlockRet body ret) _ ann) = do
    cRetType <- maybe (return CTVoid) (genType noqual) rts
    cParamDecls <- mapM genParameterDeclaration parameters
    cReturn <- genReturnStatement ret
    cBody <- foldM (\acc x -> do
        cStmt <- genBlockItem x
        return $ acc ++ cStmt) [] body
    return [ CFunctionDef Nothing (CFunction cRetType identifier cParamDecls
        (CSCompound (cBody ++ cReturn) (buildCompoundAnn ann False True))
        (buildDeclarationAnn ann True))]
genFunction item = throwError $ InternalError $ "Not a function: " ++ show item
