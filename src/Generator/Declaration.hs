{-# LANGUAGE FlexibleContexts #-}

module Generator.Declaration where

import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Control.Monad.Except
import Generator.Common
import Generator.Expression
import Data.Map (fromList, union)
import qualified Control.Monad.Reader
import Parser.Parsing

filterStructModifiers :: [Modifier] -> [Modifier]
filterStructModifiers = filter (\case
      Modifier "packed" Nothing -> True
      Modifier "aligned" _ -> True
      _ -> False)

genStructField :: SemanticAnns -> FieldDefinition -> CHeaderGenerator CDeclaration
genStructField ann (FieldDefinition identifier ts) = do
    let exprCAnn = buildGenericAnn ann
        arrayDecl = genArraySizeDeclarator ts ann
    decl <- genDeclSpecifiers ts
    return $ CDeclaration decl [(Just (CDeclarator (Just identifier) arrayDecl [] exprCAnn), Nothing, Nothing)] 
        (buildDeclarationAnn ann False)

genAttribute :: Modifier -> CHeaderGenerator CAttribute
genAttribute (Modifier name Nothing) = do
    return $ CAttr name []
genAttribute (Modifier name (Just expr)) = do
    cExpr <- genConstExpression expr
    return $ CAttr name [cExpr]

    where

        genConstExpression :: (MonadError CGeneratorError m) => ConstExpression -> m CExpression
        genConstExpression (KC c) = do
            let cAnn = CAnnotations Internal CGenericAnn
            case c of
                (I _ i) -> return $ CConst (CIntConst (CInteger i DecRepr)) cAnn
                (B True) -> return $ CConst (CIntConst (CInteger 1 DecRepr)) cAnn
                (B False) -> return $ CConst (CIntConst (CInteger 0 DecRepr)) cAnn
                (C char) -> return $ CConst (CCharConst (CChar char)) cAnn
        genConstExpression cex = throwError $ InternalError $ "Unsupported constant expression: " ++ show cex


-- | TypeDef pretty printer.
genTypeDeclaration :: AnnASTElement SemanticAnns -> CHeaderGenerator [CExternalDeclaration]
genTypeDeclaration (TypeDefinition (Struct identifier fls modifiers) ann) = do
    let cAnn = buildDeclarationAnn ann True
    structModifiers <- mapM genAttribute (filterStructModifiers modifiers)
    cFields <- mapM (genStructField ann) fls
    return [
            CDeclExt $ CDeclaration [CStorageSpec CTypedef, CTypeSpec $ CSUType (
                CStruct CStructTag Nothing (Just cFields) structModifiers)] [
                    (Just (CDeclarator (Just identifier) [] [] (buildGenericAnn ann)), Nothing, Nothing)
                ] cAnn
        ]
genTypeDeclaration ts = throwError $ InternalError $ "Unsupported type declaration: " ++ show ts


{--
        vsep $ (typedefC <+> structC <+> braces' (
        indentTab . align $ vsep $
        map ppStructField fls
        ) <+> ppTypeAttributes structModifiers <> pretty identifier <> semi)--}