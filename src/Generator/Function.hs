{-# LANGUAGE FlexibleContexts #-}

module Generator.Function where

import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Generator.Common
import Generator.Statement
import Data.Map (fromList, union)


genFunctionDecl :: AnnASTElement SemanticAnns -> CHeaderGenerator [CExternalDeclaration]
genFunctionDecl (Function identifier parameters rts _ _ ann) = do
    let cAnn = buildGenericAnn ann
    retTypeDecl <- maybe (return [CTypeSpec CVoidType]) genReturnTypeDeclSpecifiers rts
    cParam <- mapM (genParameterDeclaration ann) parameters
    return [ CDeclExt $ CDeclaration retTypeDecl
        [(Just (CDeclarator (Just identifier) [CFunDeclr cParam [] cAnn] [] cAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann True)]
genFunctionDecl item = throwError $ InternalError $ "Not a function: " ++ show item

genFunction :: AnnASTElement SemanticAnns -> CSourceGenerator [CExternalDeclaration]
genFunction (Function identifier parameters rts (BlockRet body ret) _ ann) = do
    retTypeDecl <- maybe (return [CTypeSpec CVoidType]) genReturnTypeDeclSpecifiers rts
    cParam <- mapM (genParameterDeclaration ann) parameters
    cReturn <- genReturnStatement ret
    let cAnn = buildGenericAnn ann
        newKeyVals = fromList $ [(pid, CMember (CVar pid cAnn) "array" False cAnn) | (Parameter pid (Vector {})) <- parameters]
    cBody <- Control.Monad.Reader.local (union newKeyVals) $ foldM (\acc x -> do
        cStmt <- genBlockItem x
        return $ acc ++ cStmt) [] body
    return [ CFDefExt $ CFunDef retTypeDecl
        (CDeclarator (Just identifier) [CFunDeclr cParam [] cAnn] [] cAnn)
        (CCompound (cBody ++ cReturn) (buildCompoundAnn ann False True))
        (buildDeclarationAnn ann True)]
genFunction item = throwError $ InternalError $ "Not a function: " ++ show item
