{-# LANGUAGE FlexibleContexts #-}

module Generator.Global where

import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Control.Monad.Except
import Generator.Common

genGlobalDecl :: AnnASTElement SemanticAnns -> CHeaderGenerator [CFileItem]
genGlobalDecl (GlobalDeclaration (Resource identifier ts _ _ ann)) = do
    let cAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return [ CExtDecl $ CDeclExt $ CDeclaration (CStorageSpec CExtern : decl)
        [(Just (CDeclarator (Just identifier) [] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)] 
genGlobalDecl (GlobalDeclaration (Channel identifier ts _ _ ann)) = do
    let cAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return [ CExtDecl $ CDeclExt $ CDeclaration (CStorageSpec CExtern : decl)
        [(Just (CDeclarator (Just identifier) [] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)] 
genGlobalDecl (GlobalDeclaration (Task identifier ts _ _ ann)) = do
    let cAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return [ CExtDecl $ CDeclExt $ CDeclaration (CStorageSpec CExtern : decl)
        [(Just (CDeclarator (Just identifier) [] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)] 
genGlobalDecl (GlobalDeclaration (Handler identifier ts _ _ ann)) = do
    let cAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return [ CExtDecl $ CDeclExt $ CDeclaration (CStorageSpec CExtern : decl)
        [(Just (CDeclarator (Just identifier) [] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)] 
genGlobalDecl (GlobalDeclaration (Emitter identifier ts _ _ ann)) = do
    let cAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return [ CExtDecl $ CDeclExt $ CDeclaration (CStorageSpec CExtern : decl)
        [(Just (CDeclarator (Just identifier) [] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)] 
genGlobalDecl (GlobalDeclaration (Const identifier ts contExpr _ ann)) = do
    case contExpr of
        KC (I ti _) _ -> return [CPPDirective $ CPPDefine identifier (Just [printLiteral (I ti (Just ts))]) (buildCPPDirectiveAnn ann True)]
        KC cst _ -> return [CPPDirective $ CPPDefine identifier (Just [printLiteral cst]) (buildCPPDirectiveAnn ann True)]
        KV v _ -> return [CPPDirective $ CPPDefine identifier (Just [show v]) (buildCPPDirectiveAnn ann True)]
genGlobalDecl decl = throwError $ InternalError $ "unsupported global declaration: " ++ show decl


genGlobal :: AnnASTElement SemanticAnns -> CSourceGenerator [CExternalDeclaration]
genGlobal (GlobalDeclaration (Resource identifier ts _ _ ann)) = do
    let cAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return [ CDeclExt $ CDeclaration decl
        [(Just (CDeclarator (Just identifier) [] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)] 
genGlobal (GlobalDeclaration (Channel identifier ts _ _ ann)) = do
    let cAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return [ CDeclExt $ CDeclaration decl
        [(Just (CDeclarator (Just identifier) [] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)] 
genGlobal (GlobalDeclaration (Task identifier ts _ _ ann)) = do
    let cAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return [ CDeclExt $ CDeclaration decl
        [(Just (CDeclarator (Just identifier) [] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)] 
genGlobal (GlobalDeclaration (Handler identifier ts _ _ ann)) = do
    let cAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return [ CDeclExt $ CDeclaration decl
        [(Just (CDeclarator (Just identifier) [] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)] 
genGlobal (GlobalDeclaration (Emitter identifier ts _ _ ann)) = do
    let cAnn = buildGenericAnn ann
    decl <- genDeclSpecifiers ts
    return [ CDeclExt $ CDeclaration decl
        [(Just (CDeclarator (Just identifier) [] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)] 
genGlobal (GlobalDeclaration (Const {})) = return []
genGlobal decl = throwError $ InternalError $ "unsupported global declaration: " ++ show decl
