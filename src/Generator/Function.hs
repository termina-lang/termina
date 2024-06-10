{-# LANGUAGE FlexibleContexts #-}

module Generator.Function where

import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Control.Monad.Except
import Generator.Common
import Generator.Statement


genFunctionDecl :: AnnASTElement SemanticAnns -> CHeaderGenerator [CExternalDeclaration]
genFunctionDecl (Function identifier parameters rts _ _ ann) = do
    let cAnn = buildGenericAnn ann
    retTypeDecl <- maybe (return [CTypeSpec CVoidType]) genDeclSpecifiers rts
    cParams <- mapM (genParameterDeclaration ann) parameters
    return [ CDeclExt $ CDeclaration retTypeDecl
        [(Just (CDeclarator (Just identifier) [CFunDeclr cParams [] cAnn] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)]
genFunctionDecl item = throwError $ InternalError $ "Not a function: " ++ show item

genFunction :: AnnASTElement SemanticAnns -> CSourceGenerator [CExternalDeclaration]
genFunction (Function identifier parameters rts (BlockRet body ret) _ ann) = do
    retTypeDecl <- maybe (return [CTypeSpec CVoidType]) genDeclSpecifiers rts
    cParams <- mapM (genParameterDeclaration ann) parameters
    cReturn <- genReturnStatement ret
    let cAnn = buildGenericAnn ann
    cBody <- foldM (\acc x -> do
        cStmt <- genBlockItem x
        return $ acc ++ cStmt) [] body
    return [ CFDefExt $ CFunDef retTypeDecl
        (CDeclarator (Just identifier) [CFunDeclr cParams [] cAnn] [] cAnn)
        (CCompound (cBody ++ cReturn) (buildCompoundAnn ann False True))
        (buildDeclarationAnn ann True)]
genFunction item = throwError $ InternalError $ "Not a function: " ++ show item
