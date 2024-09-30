{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Global where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Semantic.Types
import Control.Monad.Except
import Generator.CodeGen.Common

genGlobalDecl :: AnnASTElement SemanticAnn -> CHeaderGenerator [CFileItem]
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
genGlobalDecl (GlobalDeclaration (Const identifier ts expr _ ann)) = do
    case expr of
        Constant (I ti _) _ -> return [CPPDirective (CPPDefine identifier (Just [printLiteral (I ti (Just ts))])) (buildCPPDirectiveAnn ann True)]
        Constant cst _ -> return [CPPDirective (CPPDefine identifier (Just [printLiteral cst])) (buildCPPDirectiveAnn ann True)]
        (AccessObject (Variable v _)) -> return [CPPDirective (CPPDefine identifier (Just [show v])) (buildCPPDirectiveAnn ann True)]
        _ -> throwError $ InternalError $ "unsupported constant expression: " ++ show expr
genGlobalDecl decl = throwError $ InternalError $ "unsupported global declaration: " ++ show decl


genGlobal :: AnnASTElement SemanticAnn -> CSourceGenerator [CFileItem]
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
genGlobal (GlobalDeclaration (Const {})) = return []
genGlobal decl = throwError $ InternalError $ "unsupported global declaration: " ++ show decl
