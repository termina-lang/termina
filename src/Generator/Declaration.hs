{-# LANGUAGE FlexibleContexts #-}

module Generator.Declaration where

import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Control.Monad.Except
import Generator.Common
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Reader
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

genParameter :: SemanticAnns -> Parameter -> CHeaderGenerator CDeclaration
genParameter ann (Parameter identifier ts) = do
    let exprCAnn = buildGenericAnn ann
        arrayDecl = genArraySizeDeclarator ts ann
    decl <- genDeclSpecifiers ts
    return $ CDeclaration decl [(Just (CDeclarator (Just identifier) arrayDecl [] exprCAnn), Nothing, Nothing)]
        (buildDeclarationAnn ann False)

genOptionSomeParameterStruct :: SemanticAnns -> TypeSpecifier ->  CHeaderGenerator CExternalDeclaration
genOptionSomeParameterStruct ann ts = do
    decl <- genDeclSpecifiers ts
    let cAnn = buildDeclarationAnn ann True
        exprCAnn = buildGenericAnn ann
        arrayDecl = genArraySizeDeclarator ts ann
        field = CDeclaration decl [
                (Just (CDeclarator (Just optionSomeField) arrayDecl [] exprCAnn), Nothing, Nothing)]
                (buildDeclarationAnn ann False)
    identifier <- genOptionParameterStructName ts
    return $
            CDeclExt $ CDeclaration [CStorageSpec CTypedef, CTypeSpec $ CSUType (
                CStruct CStructTag Nothing (Just [field]) [])] [
                    (Just (CDeclarator (Just identifier) [] [] (buildGenericAnn ann)), Nothing, Nothing)
                ] cAnn


genOptionStruct :: SemanticAnns -> TypeSpecifier -> CHeaderGenerator [CExternalDeclaration]
genOptionStruct ann (Option ts) = do
    paramsStructName <- genOptionParameterStructName ts
    enumStructName <- genEnumStructName "option"
    paramsDecl <- genDeclSpecifiers (DefinedType paramsStructName)
    optionDecl <- genDeclSpecifiers (DefinedType enumStructName)
    let cAnn = buildDeclarationAnn ann True
        exprCAnn = buildGenericAnn ann
        some = CDeclaration paramsDecl [
                (Just (CDeclarator (Just optionSomeVariant) [] [] exprCAnn), Nothing, Nothing)]
                (buildDeclarationAnn ann False)
        variant = CDeclaration optionDecl [
                (Just (CDeclarator (Just enumVariantsField) [] [] exprCAnn), Nothing, Nothing)]
                (buildDeclarationAnn ann False)
    identifier <- genOptionStructName ts
    paramStruct <- genOptionSomeParameterStruct ann ts
    return [
            paramStruct,
            CDeclExt $ CDeclaration [CStorageSpec CTypedef, CTypeSpec $ CSUType (
                CStruct CStructTag Nothing (Just [some, variant]) [])] [
                    (Just (CDeclarator (Just identifier) [] [] (buildGenericAnn ann)), Nothing, Nothing)
                ] cAnn
        ]
genOptionStruct ts _ = throwError $ InternalError $ "Type not an option: " ++ show ts

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

genEnumVariantParameterStruct :: SemanticAnns -> Identifier -> EnumVariant -> CHeaderGenerator CExternalDeclaration
genEnumVariantParameterStruct ann identifier (EnumVariant variant params) = do
    let cAnn = buildDeclarationAnn ann True
    pParams <- zipWithM genEnumVariantParameter params [0..]
    paramsStructName <- genEnumParameterStructName identifier variant
    return $ CDeclExt (CDeclaration [CStorageSpec CTypedef, CTypeSpec $ CSUType (
                CStruct CStructTag Nothing (Just pParams) [])] [
                    (Just (CDeclarator (Just paramsStructName) [] [] (buildGenericAnn ann)), Nothing, Nothing)
                ] cAnn)
    where
        genEnumVariantParameter :: TypeSpecifier -> Integer -> CHeaderGenerator CDeclaration
        genEnumVariantParameter ts index = do
            let exprCAnn = buildGenericAnn ann
                arrayDecl = genArraySizeDeclarator ts ann
            decl <- genDeclSpecifiers ts
            return $ CDeclaration decl [(Just (CDeclarator (Just (namefy $ show index)) arrayDecl [] exprCAnn), Nothing, Nothing)]
                (buildDeclarationAnn ann False)

-- | TypeDef pretty printer.
genTypeDeclaration :: AnnASTElement SemanticAnns -> CHeaderGenerator [CExternalDeclaration]
genTypeDeclaration (TypeDefinition (Struct identifier fls modifiers) ann) = do
    let cAnn = buildDeclarationAnn ann True
    structModifiers <- mapM genAttribute (filterStructModifiers modifiers)
    cFields <- mapM (genStructField ann) fls
    opts <- ask
    optsDeclExt <- concat <$> maybe (return [])
        (mapM (genOptionStruct ann) . S.toList) (M.lookup (DefinedType identifier) opts)
    return $ CDeclExt (CDeclaration [CStorageSpec CTypedef, CTypeSpec $ CSUType (
                CStruct CStructTag Nothing (Just cFields) structModifiers)] [
                    (Just (CDeclarator (Just identifier) [] [] (buildGenericAnn ann)), Nothing, Nothing)
                ] cAnn) : optsDeclExt
genTypeDeclaration (TypeDefinition (Enum identifier variants _) ann) = do
    let cAnn = buildDeclarationAnn ann True
        variantsWithParams = filter (not . null . assocData) variants
    enumName <- genEnumStructName identifier
    pEnumVariants <- mapM (\(EnumVariant variant _) -> genEnumVariantName identifier variant) variants
    pEnumParameterStructs <- mapM (genEnumVariantParameterStruct ann identifier) variantsWithParams
    enumStruct <- genEnumStruct enumName variantsWithParams
    return $ CDeclExt (CDeclaration [CStorageSpec CTypedef,
                CTypeSpec $ CEnumType $ CEnum Nothing (
                    Just ([(v, Nothing) | v <- pEnumVariants])) []]
                    [(Just (CDeclarator (Just enumName) [] [] (buildGenericAnn ann)), Nothing, Nothing)
                    ] cAnn) 
                : pEnumParameterStructs ++ [enumStruct]

        where

            genParameterUnionField :: EnumVariant -> CHeaderGenerator CDeclaration
            genParameterUnionField (EnumVariant variant _) = do
                let exprCAnn = buildGenericAnn ann
                paramsStructName <- genEnumParameterStructName identifier variant
                decl <- genDeclSpecifiers (DefinedType paramsStructName)
                return $ CDeclaration decl [(Just (CDeclarator (Just variant) [] [] exprCAnn), Nothing, Nothing)]
                    (buildDeclarationAnn ann False)
            
            genParameterUnion :: [EnumVariant] -> CHeaderGenerator CDeclaration
            genParameterUnion variantsWithParams = do
                let cAnn = buildDeclarationAnn ann False
                pFields <- mapM genParameterUnionField variantsWithParams
                return $ CDeclaration [CTypeSpec $ CSUType (
                            CStruct CUnionTag Nothing (Just pFields) [])] [] cAnn
            
            genEnumStruct :: Identifier -> [EnumVariant] ->CHeaderGenerator CExternalDeclaration
            genEnumStruct enumName variantsWithParams = do
                decl <- genDeclSpecifiers (DefinedType enumName)
                let cAnn = buildDeclarationAnn ann True
                    exprCAnn = buildGenericAnn ann
                    enumField = CDeclaration decl [(Just (CDeclarator (Just enumVariantsField) [] [] exprCAnn), Nothing, Nothing)]
                        (buildDeclarationAnn ann False)
                case variantsWithParams of
                    [] -> return $ CDeclExt (CDeclaration [CStorageSpec CTypedef, CTypeSpec $ CSUType (
                                CStruct CStructTag Nothing (Just [enumField]) [])] [
                                    (Just (CDeclarator (Just identifier) [] [] exprCAnn), Nothing, Nothing)
                                ] cAnn)
                    [var] -> do
                        unionField <- genParameterUnionField var
                        return $ CDeclExt (CDeclaration [CStorageSpec CTypedef, CTypeSpec $ CSUType (
                                CStruct CStructTag Nothing (Just [enumField, unionField]) [])] [
                                    (Just (CDeclarator (Just identifier) [] [] exprCAnn), Nothing, Nothing)
                                ] cAnn)
                    _ -> do
                        unionField <- genParameterUnion variantsWithParams
                        return $ CDeclExt (CDeclaration [CStorageSpec CTypedef, CTypeSpec $ CSUType (
                                CStruct CStructTag Nothing (Just [enumField, unionField]) [])] [
                                    (Just (CDeclarator (Just identifier) [] [] exprCAnn), Nothing, Nothing)
                                ] cAnn)
genTypeDeclaration (TypeDefinition (Interface identifier members _) ann) = do
    let cAnn = buildDeclarationAnn ann True
        exprCAnn = buildGenericAnn ann
        cThatField = CDeclaration [CTypeSpec CVoidType] 
                [(Just (CDeclarator (Just thatField) [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)]
                (buildDeclarationAnn ann False)
    procedureFields <- mapM genInterfaceProcedureField members
    return [CDeclExt (CDeclaration [CStorageSpec CTypedef, CTypeSpec $ CSUType (
                                CStruct CStructTag Nothing (Just (cThatField : procedureFields)) [])] [
                                    (Just (CDeclarator (Just identifier) [] [] exprCAnn), Nothing, Nothing)
                                ] cAnn)]

    where
            
        genInterfaceProcedureField :: InterfaceMember SemanticAnns -> CHeaderGenerator CDeclaration
        genInterfaceProcedureField (InterfaceProcedure procedure params ann') = do
            pParams <- mapM (genParameter ann') params
            let cAnn = buildDeclarationAnn ann' False
                cThisParam = CDeclaration [CTypeSpec CVoidType] 
                    [(Just (CDeclarator (Just thisParam) [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)]
                    (buildDeclarationAnn ann False)
            return $ CDeclaration [CTypeSpec CVoidType] 
                [(Just (CDeclarator (Just procedure) 
                    [CPtrDeclr [] cAnn, CFunDeclr (cThisParam : pParams) [] (buildGenericAnn ann')] [] 
                    (buildGenericAnn ann')), Nothing, Nothing)]
                cAnn


{--

    (Interface identifier members _) ->
      vsep [
              typedefC <+> structC <+> braces' (
                indentTab . align $
                vsep $
                  -- | Print the that field
                  voidC <+> pretty "*" <+> ppInterfaceThatField <> semi :
                  -- | Print the procedure fields
                  map ppInterfaceProcedureField members)
                    <+> pretty identifier <> semi,
              emptyDoc
      ]
--}

genTypeDeclaration ts = throwError $ InternalError $ "Unsupported type declaration: " ++ show ts