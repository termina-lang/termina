{-# LANGUAGE FlexibleContexts #-}

module Generator.Function where

import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Control.Monad.Except
import Generator.Common
import Generator.Statement
import Utils.AST.Core


genFunctionDecl :: AnnASTElement SemanticAnns -> CHeaderGenerator [CExternalDeclaration]
genFunctionDecl (Function identifier constParameters parameters rts _ _ ann) = do
    let cAnn = buildGenericAnn ann
    retTypeDecl <- maybe (return [CTypeSpec CVoidType]) genDeclSpecifiers rts
    cConstParams <- mapM (genParameterDeclaration ann . unConstParam) constParameters
    cParams <- mapM (genParameterDeclaration ann) parameters
    return [ CDeclExt $ CDeclaration retTypeDecl
        [(Just (CDeclarator (Just identifier) [CFunDeclr (cConstParams ++ cParams) [] cAnn] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)]
genFunctionDecl item = throwError $ InternalError $ "Not a function: " ++ show item

genFunction :: AnnASTElement SemanticAnns -> CSourceGenerator [CExternalDeclaration]
genFunction (Function identifier constParameters parameters rts (BlockRet body ret) _ ann) = do
    retTypeDecl <- maybe (return [CTypeSpec CVoidType]) genDeclSpecifiers rts
    cConstParams <- mapM (genParameterDeclaration ann . unConstParam) constParameters
    cParams <- mapM (genParameterDeclaration ann) parameters
    cReturn <- genReturnStatement ret
    let cAnn = buildGenericAnn ann
    cBody <- foldM (\acc x -> do
        cStmt <- genBlockItem x
        return $ acc ++ cStmt) [] body
    return [ CFDefExt $ CFunDef retTypeDecl
        (CDeclarator (Just identifier) [CFunDeclr (cConstParams ++ cParams) [] cAnn] [] cAnn)
        (CCompound (cBody ++ cReturn) (buildCompoundAnn ann False True))
        (buildDeclarationAnn ann True)]
genFunction item = throwError $ InternalError $ "Not a function: " ++ show item
