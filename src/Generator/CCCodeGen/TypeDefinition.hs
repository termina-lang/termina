{-# LANGUAGE FlexibleContexts #-}

module Generator.CCCodeGen.TypeDefinition where

import AST.Seman
import Generator.LanguageC.CompCertC
import Semantic.Monad
import Control.Monad.Except
import Generator.CCCodeGen.Common
import Generator.CCCodeGen.Statement
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Reader
import Utils.Annotations

filterStructModifiers :: [Modifier] -> [Modifier]
filterStructModifiers = filter (\case
      Modifier "packed" Nothing -> True
      Modifier "aligned" _ -> True
      _ -> False)

genFieldDeclaration :: FieldDefinition -> CHeaderGenerator CDeclaration
genFieldDeclaration (FieldDefinition identifier ts@(Location {})) = do
    cTs <- genType noqual ts
    return $ CDecl (CTypeSpec cTs) (Just identifier) Nothing
genFieldDeclaration (FieldDefinition identifier (AccessPort ts@(Allocator {}))) = do
    cTs <- genType noqual ts
    return $ CDecl (CTypeSpec cTs) (Just identifier) Nothing
genFieldDeclaration (FieldDefinition identifier (AccessPort ts@(AtomicAccess {}))) = do
    cTs <- genType noqual ts
    return $ CDecl (CTypeSpec cTs) (Just identifier) Nothing
genFieldDeclaration (FieldDefinition identifier (AccessPort ts@(AtomicArrayAccess {}))) = do
    cTs <- genType noqual ts
    return $ CDecl (CTypeSpec cTs) (Just identifier) Nothing
genFieldDeclaration (FieldDefinition identifier ts) = do
    cTs <- genType noqual ts
    return $ CDecl (CTypeSpec cTs) (Just identifier) Nothing

genOptionSomeParameterStruct :: SemanticAnn -> TypeSpecifier ->  CHeaderGenerator CExternalDeclaration
genOptionSomeParameterStruct ann ts = do
    cTs <- genType noqual ts
    let cAnn = buildDeclarationAnn ann True
        field = CDecl (CTypeSpec cTs) (Just optionSomeField) Nothing
    identifier <- genOptionParameterStructName ts
    return $
            CEDStructUnion (Just identifier) (CStruct CStructTag Nothing [field] []) cAnn

genOptionStruct :: SemanticAnn -> TypeSpecifier -> CHeaderGenerator [CExternalDeclaration]
genOptionStruct ann (Option ts) = do
    paramsStructName <- genOptionParameterStructName ts
    enumStructName <- genEnumStructName "option"
    paramsStructType <- genType noqual (DefinedType paramsStructName)
    enumStructType <- genType noqual (DefinedType enumStructName)
    let cAnn = buildDeclarationAnn ann True
        some = CDecl (CTypeSpec paramsStructType) (Just optionSomeVariant) Nothing
        variant = CDecl (CTypeSpec enumStructType) (Just enumVariantsField) Nothing
    identifier <- genOptionStructName ts
    paramStruct <- genOptionSomeParameterStruct ann ts
    return [
            paramStruct,
            CEDStructUnion (Just identifier) (CStruct CStructTag Nothing [some, variant] []) cAnn
        ]
genOptionStruct ts _ = throwError $ InternalError $ "Type not an option: " ++ show ts

genAttribute :: Modifier -> CHeaderGenerator CAttribute
genAttribute (Modifier name Nothing) = do
    return $ CAttr name []
genAttribute (Modifier name (Just expr)) = do
    cExpr <- genConst expr
    return $ CAttr name [cExpr]

    where

        genConst :: (MonadError CGeneratorError m) => Const -> m CExpression
        genConst c = do
            let cAnn = Located CGenericAnn Internal
            case c of
                (I i _) -> 
                    let cInteger = genInteger i in
                    return $ CExprConstant (CIntConst cInteger) (CTInt IntSize32 Unsigned noqual) cAnn
                (B True) -> return $ CExprConstant (CIntConst (CInteger 1 CDecRepr)) (CTBool noqual) cAnn
                (B False) -> return $ CExprConstant (CIntConst (CInteger 0 CDecRepr)) (CTBool noqual) cAnn
                (C char) -> return $ CExprConstant (CCharConst (CChar char)) (CTChar noqual) cAnn

genEnumVariantParameterStruct :: SemanticAnn -> Identifier -> EnumVariant -> CHeaderGenerator CExternalDeclaration
genEnumVariantParameterStruct ann identifier (EnumVariant variant params) = do
    let cAnn = buildDeclarationAnn ann True
    pParams <- zipWithM genEnumVariantParameter params [0..]
    paramsStructName <- genEnumParameterStructName identifier variant
    return $ CEDStructUnion (Just paramsStructName) (CStruct CStructTag Nothing pParams []) cAnn
    where
        genEnumVariantParameter :: TypeSpecifier -> Integer -> CHeaderGenerator CDeclaration
        genEnumVariantParameter ts index = do
            cParamType <- genType noqual ts
            return $ CDecl (CTypeSpec cParamType) (Just (namefy $ show index)) Nothing

classifyClassMembers :: (MonadError CGeneratorError m) => TypeDef SemanticAnn -> m ([ClassMember SemanticAnn], [ClassMember SemanticAnn])
classifyClassMembers (Class clsKind _identifier members _provides _modifiers) =
    return $ foldr (\m (fields, funcs) -> case m of
        field@(ClassField fieldDef _) -> case clsKind of
            TaskClass -> (field : fields, funcs)
            _ -> case fieldDef of
                (FieldDefinition _ (SinkPort {})) -> (fields, funcs)
                (FieldDefinition _ (InPort {})) -> (fields, funcs)
                _ -> (field : fields, funcs)
        func -> (fields, func : funcs)) ([], []) members
classifyClassMembers e = throwError $ InternalError $ "Not a class definition: " ++ show e

genThisParam :: (MonadError CGeneratorError m) => m CDeclaration
genThisParam = return $ CDecl (CTypeSpec (CTPointer CTVoid constqual)) (Just thisParam) Nothing

genSelfParam :: (MonadError CGeneratorError m) => AnnASTElement SemanticAnn -> m CDeclaration
genSelfParam (TypeDefinition (Class _clsKind identifier _members _provides _modifiers) _) =
    return $ CDecl (CTypeSpec (CTTypeDef identifier noqual)) (Just selfParam) Nothing
genSelfParam e = throwError $ InternalError $ "Not a class definition: " ++ show e

genConstSelfParam :: (MonadError CGeneratorError m) => AnnASTElement SemanticAnn -> m CDeclaration
genConstSelfParam (TypeDefinition (Class _clsKind identifier _members _provides _modifiers) _) =
    return $ CDecl (CTypeSpec (CTPointer (CTTypeDef identifier constqual) constqual)) (Just selfParam) Nothing
genConstSelfParam e = throwError $ InternalError $ "Not a class definition: " ++ show e


-- | TypeDef pretty printer.
genTypeDefinitionDecl :: AnnASTElement SemanticAnn -> CHeaderGenerator [CFileItem]
genTypeDefinitionDecl (TypeDefinition (Struct identifier fls modifiers) ann) = do
    let cAnn = buildDeclarationAnn ann True
    structModifiers <- mapM genAttribute (filterStructModifiers modifiers)
    cFields <- mapM genFieldDeclaration fls
    opts <- ask
    optsDeclExt <- concat <$> maybe (return [])
        (mapM (genOptionStruct ann) . S.toList) (M.lookup (DefinedType identifier) opts)
    return $ CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing cFields structModifiers) cAnn) : (CExtDecl <$> optsDeclExt)
genTypeDefinitionDecl (TypeDefinition (Enum identifier variants _) ann) = do
    let cAnn = buildDeclarationAnn ann True
        variantsWithParams = filter (not . null . assocData) variants
    enumName <- genEnumStructName identifier
    pEnumVariants <- mapM (\(EnumVariant variant _) -> genEnumVariantName identifier variant) variants
    pEnumParameterStructs <- mapM (genEnumVariantParameterStruct ann identifier) variantsWithParams
    enumStruct <- genEnumStruct enumName variantsWithParams
    return $ CExtDecl (CEDEnum (Just enumName) (CEnum Nothing [(v, Nothing) | v <- pEnumVariants] []) cAnn)
                : (CExtDecl <$> pEnumParameterStructs) ++ [CExtDecl enumStruct]

        where

            genParameterUnionField :: EnumVariant -> CHeaderGenerator CDeclaration
            genParameterUnionField (EnumVariant variant _) = do
                paramsStructName <- genEnumParameterStructName identifier variant
                paramsStructType <- genType noqual (DefinedType paramsStructName)
                return $ CDecl (CTypeSpec paramsStructType) (Just variant) Nothing

            genParameterUnion :: [EnumVariant] -> CHeaderGenerator CDeclaration
            genParameterUnion variantsWithParams = do
                pFields <- mapM genParameterUnionField variantsWithParams
                return $ CDecl (CTSStructUnion (CStruct CUnionTag Nothing pFields [])) Nothing Nothing

            genEnumStruct :: Identifier -> [EnumVariant] -> CHeaderGenerator CExternalDeclaration
            genEnumStruct enumName variantsWithParams = do
                enumType <- genType noqual (DefinedType enumName)
                let cAnn = buildDeclarationAnn ann True
                    enumField = CDecl (CTypeSpec enumType) (Just enumVariantsField) Nothing
                case variantsWithParams of
                    [] -> return $ CEDStructUnion (Just identifier) (CStruct CStructTag Nothing [enumField] []) cAnn
                    [var] -> do
                        unionField <- genParameterUnionField var
                        return $ CEDStructUnion (Just identifier) (CStruct CStructTag Nothing [enumField, unionField] []) cAnn
                    _ -> do
                        unionField <- genParameterUnion variantsWithParams
                        return $ CEDStructUnion (Just identifier) (CStruct CStructTag Nothing [enumField, unionField] []) cAnn
genTypeDefinitionDecl (TypeDefinition (Interface identifier members _) ann) = do
    let cAnn = buildDeclarationAnn ann True
        cThatField = CDecl (CTypeSpec (CTPointer CTVoid noqual)) (Just thatField) Nothing
    procedureFields <- mapM genInterfaceProcedureField members
    return [CExtDecl $ CEDStructUnion (Just identifier) (CStruct CStructTag Nothing (cThatField : procedureFields) []) cAnn]

    where

        genInterfaceProcedureField :: InterfaceMember SemanticAnn -> CHeaderGenerator CDeclaration
        genInterfaceProcedureField (InterfaceProcedure procedure params _) = do
            cParamTypes <- mapM (genType noqual . paramTypeSpecifier) params
            let cThisParamType = CTPointer CTVoid noqual
                cFuncPointerType = CTPointer (CTFunction CTVoid (cThisParamType : cParamTypes)) noqual
            return $ CDecl (CTypeSpec cFuncPointerType) (Just procedure) Nothing

genTypeDefinitionDecl (TypeDefinition cls@(Class clsKind identifier _members _provides modifiers) ann) = do
    (fields, functions) <- classifyClassMembers cls
    structModifiers <- mapM genAttribute (filterStructModifiers modifiers)
    fields' <- case clsKind of
        TaskClass -> return fields
        _ -> return $ filter (\case {
            ClassField (FieldDefinition _ (SinkPort {})) _ -> False;
            ClassField (FieldDefinition _ (InPort {})) _ -> False;
            _ -> True}) fields
    cFields <- mapM genClassField fields'
    cFunctions <- mapM genClassFunctionDeclaration functions
    let cAnn = buildDeclarationAnn ann True
        structFields = case clsKind of
            TaskClass ->
                let cIDField = CDecl (CTypeSpec $ CTTypeDef taskID noqual) (Just taskClassIDField) Nothing in
                cIDField : cFields
            ResourceClass ->
                let cIDField = CDecl (CTypeSpec $ CTTypeDef resourceID noqual) (Just resourceClassIDField) Nothing in
                cIDField : cFields
            _ -> cFields
    return $ CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing structFields structModifiers) cAnn)
                : (CExtDecl <$> cFunctions)

    where

        genClassField :: ClassMember SemanticAnn -> CHeaderGenerator CDeclaration
        genClassField (ClassField fld _) = genFieldDeclaration fld
        genClassField member = throwError $ InternalError $ "invalid class member. Not a field: " ++ show member

        genClassFunctionDeclaration :: ClassMember SemanticAnn -> CHeaderGenerator CExternalDeclaration
        genClassFunctionDeclaration (ClassViewer viewer params rts _ _) = do
            retType <- maybe (return CTVoid) (genType noqual) rts
            cParamDecls <- mapM genParameterDeclaration params
            clsFuncName <- genClassFunctionName identifier viewer
            return $ CEDFunction retType clsFuncName cParamDecls (buildDeclarationAnn ann True)
        genClassFunctionDeclaration (ClassProcedure procedure params _ _) = do
            cParamDecls <- mapM genParameterDeclaration params
            cThisParam <- genThisParam
            clsFuncName <- genClassFunctionName identifier procedure
            return $ CEDFunction CTVoid clsFuncName (cThisParam : cParamDecls) (buildDeclarationAnn ann True)
        genClassFunctionDeclaration (ClassMethod method rts _ _) = do
            retType <- maybe (return CTVoid) (genType noqual) rts
            clsFuncName <- genClassFunctionName identifier method
            return $ CEDFunction retType clsFuncName [] (buildDeclarationAnn ann True)
        genClassFunctionDeclaration (ClassAction action param rts _ _) = do
            retType <- genType noqual rts
            cParamDecl <- genParameterDeclaration param
            clsFuncName <- genClassFunctionName identifier action
            return $ CEDFunction retType clsFuncName [cParamDecl] (buildDeclarationAnn ann True)
        genClassFunctionDeclaration member = throwError $ InternalError $ "invalid class member. Not a function: " ++ show member
genTypeDefinitionDecl ts = throwError $ InternalError $ "Unsupported type definition: " ++ show ts

genClassDefinition :: AnnASTElement SemanticAnn -> CSourceGenerator [CFileItem]
genClassDefinition clsdef@(TypeDefinition cls@(Class _clsKind identifier _members _provides _) _) = do
    (_fields, functions) <- classifyClassMembers cls
    mapM genClassFunctionDefinition functions

    where

        genClassFunctionDefinition :: ClassMember SemanticAnn -> CSourceGenerator CFileItem
        genClassFunctionDefinition (ClassViewer viewer parameters rts (BlockRet body ret) ann) = do
            clsFuncName <- genClassFunctionName identifier viewer
            cRetType <- maybe (return CTVoid) (genType noqual) rts
            cParamDecls <- mapM genParameterDeclaration parameters
            cParamTypes <- mapM (genType noqual . paramTypeSpecifier) parameters
            cReturn <- genReturnStatement ret
            let cFunctionType = CTFunction cRetType (CTPointer CTVoid constqual : cParamTypes)
            cBody <- foldM (\acc x -> do
                cStmt <- genBlockItem x
                return $ acc ++ cStmt) [] body
            return $ CFunctionDef (CFunction cFunctionType clsFuncName cParamDecls
                (CSCompound (cBody ++ cReturn) (buildCompoundAnn ann False True))
                (buildDeclarationAnn ann True))
        genClassFunctionDefinition (ClassProcedure procedure parameters body ann) = do
            clsFuncName <- genClassFunctionName identifier procedure
            cThisParam <- genThisParam
            cParamDecls <- mapM genParameterDeclaration parameters
            cParamTypes <- mapM (genType noqual . paramTypeSpecifier) parameters
            cReturn <- genReturnStatement (ReturnStmt Nothing ann)
            let cFunctionType = CTFunction CTVoid (CTPointer CTVoid constqual : cParamTypes)
            selfCastStmt <- genSelfCastStmt
            cBody <- foldM (\acc x -> do
                cStmt <- genBlockItem x
                return $ acc ++ cStmt) [] body
            return $ CFunctionDef (CFunction cFunctionType clsFuncName (cThisParam : cParamDecls)
                (CSCompound ([selfCastStmt, genProcedureOnEntry] ++ cBody ++ (genProcedureOnExit : cReturn)) (buildCompoundAnn ann False True))
                (buildDeclarationAnn ann True))

            where

                selfVariable :: Identifier
                selfVariable = "self"

                genSelfCastStmt :: CSourceGenerator CCompoundBlockItem
                genSelfCastStmt = do
                    let cAnn = buildGenericAnn ann
                    selfCType <- flip CTPointer noqual <$> genType noqual (DefinedType identifier)
                    let cExpr = CExprCast (CExprValOf (CVar thisParam (CTPointer CTVoid noqual)) (CTPointer CTVoid noqual) cAnn) selfCType cAnn
                    return $ CBlockDecl (CDecl (CTypeSpec selfCType) (Just selfVariable) (Just cExpr)) (buildDeclarationAnn ann True)

                genProcedureOnEntry, genProcedureOnExit :: CCompoundBlockItem
                genProcedureOnEntry =
                    let cAnn = buildGenericAnn ann
                        selfResourceExpr = CExprAddrOf (CField (CExprValOf (CVar selfVariable (CTTypeDef identifier noqual)) (CTTypeDef identifier noqual) cAnn) resourceClassIDField cResourceIDType) (CTPointer cResourceIDType noqual) cAnn
                    in
                    CBlockStmt $ CSDo (CExprCall (CExprValOf (CVar resourceLock cResourceLockFuncType) cResourceLockFuncType cAnn) [selfResourceExpr] CTVoid cAnn) (buildStatementAnn ann True)
                genProcedureOnExit =
                    let cAnn = buildGenericAnn ann
                        selfResourceExpr = CExprAddrOf (CField (CExprValOf (CVar selfVariable (CTTypeDef identifier noqual)) (CTTypeDef identifier noqual) cAnn) resourceClassIDField cResourceIDType) (CTPointer cResourceIDType noqual) cAnn
                    in
                    CBlockStmt $ CSDo (CExprCall (CExprValOf (CVar resourceUnlock cResourceLockFuncType) cResourceUnlockFuncType cAnn) [selfResourceExpr] CTVoid cAnn) (buildStatementAnn ann True)

        genClassFunctionDefinition (ClassMethod method rts (BlockRet body ret) ann) = do
            clsFuncName <- genClassFunctionName identifier method
            cRetType <- maybe (return CTVoid) (genType noqual) rts
            cSelfParam <- genSelfParam clsdef
            cReturn <- genReturnStatement ret
            let cFunctionType = CTFunction cRetType [CTPointer CTVoid constqual]
            cBody <- foldM (\acc x -> do
                cStmt <- genBlockItem x
                return $ acc ++ cStmt) [] body
            return $ CFunctionDef (CFunction cFunctionType clsFuncName [cSelfParam]
                (CSCompound (cBody ++ cReturn) (buildCompoundAnn ann False True))
                (buildDeclarationAnn ann True))
        genClassFunctionDefinition (ClassAction action param rts (BlockRet body ret) ann) = do
            clsFuncName <- genClassFunctionName identifier action
            cRetType <- genType noqual rts
            cSelfParam <- genSelfParam clsdef
            cParam <- genParameterDeclaration param
            cParamType <- genType noqual . paramTypeSpecifier $ param
            cReturn <- genReturnStatement ret
            let cFunctionType = CTFunction cRetType [cParamType]
            cBody <- foldM (\acc x -> do
                cStmt <- genBlockItem x
                return $ acc ++ cStmt) [] body
            return $ CFunctionDef (CFunction cFunctionType clsFuncName [cSelfParam, cParam]
                (CSCompound (cBody ++ cReturn) (buildCompoundAnn ann False True))
                (buildDeclarationAnn ann True))
        genClassFunctionDefinition member = throwError $ InternalError $ "invalid class member. Not a function: " ++ show member

genClassDefinition e = throwError $ InternalError $ "AST element is not a class: " ++ show e