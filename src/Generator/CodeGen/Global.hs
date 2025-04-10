{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Global where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Semantic.Types
import Control.Monad.Except
import Generator.CodeGen.Common
import Generator.LanguageC.Embedded
import Generator.CodeGen.Expression
import Utils.Annotations

genGlobalDecl :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genGlobalDecl (GlobalDeclaration (Resource identifier ts _ _ ann)) = do
    cTs <- genType noqual ts
    return [CExtDecl (CEDVariable (Just CExtern) (CDecl (CTypeSpec cTs)
       (Just identifier) Nothing)) (buildDeclarationAnn ann True)] 
genGlobalDecl (GlobalDeclaration (Channel identifier ts _ _ ann)) = do
    cTs <- genType noqual ts
    return [CExtDecl (CEDVariable (Just CExtern) (CDecl (CTypeSpec cTs)
        (Just identifier) Nothing)) (buildDeclarationAnn ann True)] 
genGlobalDecl (GlobalDeclaration (Task identifier ts _ _ ann)) = do
    cTs <- genType noqual ts
    return [CExtDecl (CEDVariable (Just CExtern) (CDecl (CTypeSpec cTs)
        (Just identifier) Nothing)) (buildDeclarationAnn ann True)] 
genGlobalDecl (GlobalDeclaration (Handler identifier ts _ _ ann)) = do
    cTs <- genType noqual ts
    return [CExtDecl (CEDVariable (Just CExtern) (CDecl (CTypeSpec cTs)
        (Just identifier) Nothing)) (buildDeclarationAnn ann True)] 
genGlobalDecl (GlobalDeclaration (Emitter identifier ts _ _ ann)) = do
    cTs <- genType noqual ts
    return [CExtDecl (CEDVariable (Just CExtern) (CDecl (CTypeSpec cTs)
        (Just identifier) Nothing)) (buildDeclarationAnn ann True)] 
genGlobalDecl (GlobalDeclaration (Const identifier ts _expr _ ann)) = do
    cTs <- genType constqual ts
    return [
            pre_cr $ extern (var identifier cTs) |>> getLocation ann
        ]
genGlobalDecl decl = throwError $ InternalError $ "unsupported global declaration: " ++ show decl


genGlobal :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genGlobal (GlobalDeclaration (Resource identifier ts _ _ ann)) = do
    cTs <- genType noqual ts
    return [CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec cTs)
        (Just identifier) Nothing)) (buildDeclarationAnn ann True)] 
genGlobal (GlobalDeclaration (Channel identifier ts _ _ ann)) = do
    cTs <- genType noqual ts
    return [CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec cTs)
        (Just identifier) Nothing)) (buildDeclarationAnn ann True)] 
genGlobal (GlobalDeclaration (Task identifier ts _ _ ann)) = do
    cTs <- genType noqual ts
    return [CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec cTs)
        (Just identifier) Nothing)) (buildDeclarationAnn ann True)] 
genGlobal (GlobalDeclaration (Handler identifier ts _ _ ann)) = do
    cTs <- genType noqual ts
    return [CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec cTs)
        (Just identifier) Nothing)) (buildDeclarationAnn ann True)] 
genGlobal (GlobalDeclaration (Emitter identifier ts _ _ ann)) = do
    cTs <- genType noqual ts
    return [CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec cTs)
        (Just identifier) Nothing)) (buildDeclarationAnn ann True)] 
genGlobal (GlobalDeclaration (Const identifier ts expr _ ann)) = do
    cTs <- genType constqual ts
    cExpr <- genExpression expr
    return [
            pre_cr . global $ identifier @: cTs @:= cExpr |>> getLocation ann
        ]
genGlobal decl = throwError $ InternalError $ "unsupported global declaration: " ++ show decl
