{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.TypeDefinition where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Semantic.Types
import Control.Monad.Except
import Generator.CodeGen.Common
import Generator.CodeGen.Statement
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
genFieldDeclaration (FieldDefinition identifier ts@(TLocation {})) = do
    cTs <- genType noqual ts
    return $ CDecl (CTypeSpec cTs) (Just identifier) Nothing
genFieldDeclaration (FieldDefinition identifier (TAccessPort ts@(TAllocator {}))) = do
    cTs <- genType noqual ts
    return $ CDecl (CTypeSpec cTs) (Just identifier) Nothing
genFieldDeclaration (FieldDefinition identifier (TAccessPort ts@(TAtomicAccess {}))) = do
    cTs <- genType noqual ts
    return $ CDecl (CTypeSpec cTs) (Just identifier) Nothing
genFieldDeclaration (FieldDefinition identifier (TAccessPort ts@(TAtomicArrayAccess {}))) = do
    cTs <- genType noqual ts
    return $ CDecl (CTypeSpec cTs) (Just identifier) Nothing
genFieldDeclaration (FieldDefinition identifier ts) = do
    cTs <- genType noqual ts
    return $ CDecl (CTypeSpec cTs) (Just identifier) Nothing

genOptionSomeParameterStruct :: SemanticAnn -> TerminaType ->  CHeaderGenerator CFileItem
genOptionSomeParameterStruct ann ts = do
    cTs <- genType noqual ts
    let cAnn = buildDeclarationAnn ann True
        field = CDecl (CTypeSpec cTs) (Just optionSomeField) Nothing
    identifier <- genOptionParameterStructName ts
    return $
            CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing [field] [])) cAnn

genOptionStruct :: SemanticAnn -> TerminaType -> CHeaderGenerator [CFileItem]
genOptionStruct ann (TOption ts) = do
    paramsStructName <- genOptionParameterStructName ts
    enumStructName <- genEnumStructName "option"
    paramsStructType <- genType noqual (TDefinedType paramsStructName)
    enumStructType <- genType noqual (TDefinedType enumStructName)
    let cAnn = buildDeclarationAnn ann True
        some = CDecl (CTypeSpec paramsStructType) (Just optionSomeVariant) Nothing
        this_variant = CDecl (CTypeSpec enumStructType) (Just variant) Nothing
    identifier <- genOptionStructName ts
    paramStruct <- genOptionSomeParameterStruct ann ts
    return [
            paramStruct,
            CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing [some, this_variant] [])) cAnn
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

genEnumVariantParameterStruct :: SemanticAnn -> Identifier -> EnumVariant -> CHeaderGenerator CFileItem
genEnumVariantParameterStruct ann identifier (EnumVariant this_variant params) = do
    let cAnn = buildDeclarationAnn ann True
    pParams <- zipWithM genEnumVariantParameter params [0..]
    paramsStructName <- genEnumParameterStructName identifier this_variant
    return $ CExtDecl (CEDStructUnion (Just paramsStructName) (CStruct CStructTag Nothing pParams [])) cAnn
    where
        genEnumVariantParameter :: TerminaType -> Integer -> CHeaderGenerator CDeclaration
        genEnumVariantParameter ts index = do
            cParamType <- genType noqual ts
            return $ CDecl (CTypeSpec cParamType) (Just (namefy $ show index)) Nothing

classifyClassMembers :: (MonadError CGeneratorError m) => TypeDef SemanticAnn -> m ([ClassMember SemanticAnn], [ClassMember SemanticAnn])
classifyClassMembers (Class clsKind _identifier members _provides _modifiers) =
    return $ foldr (\m (fields, funcs) -> case m of
        field@(ClassField fieldDef _) -> case clsKind of
            TaskClass -> (field : fields, funcs)
            _ -> case fieldDef of
                (FieldDefinition _ (TSinkPort {})) -> (fields, funcs)
                (FieldDefinition _ (TInPort {})) -> (fields, funcs)
                _ -> (field : fields, funcs)
        func -> (fields, func : funcs)) ([], []) members
classifyClassMembers e = throwError $ InternalError $ "Not a class definition: " ++ show e

genThisParam :: (MonadError CGeneratorError m) => m CDeclaration
genThisParam = return $ CDecl (CTypeSpec (CTPointer (CTVoid noqual) constqual)) (Just thisParam) Nothing

genSelfParam :: (MonadError CGeneratorError m) => AnnASTElement SemanticAnn -> m CDeclaration
genSelfParam (TypeDefinition (Class _clsKind identifier _members _provides _modifiers) _) =
    return $ CDecl (CTypeSpec (CTPointer (CTTypeDef identifier noqual) constqual)) (Just selfParam) Nothing
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
        (mapM (genOptionStruct ann) . S.toList) (M.lookup (TDefinedType identifier) opts)
    return $ CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing cFields structModifiers)) cAnn : optsDeclExt
genTypeDefinitionDecl (TypeDefinition (Enum identifier variants _) ann) = do
    let cAnn = buildDeclarationAnn ann True
        variantsWithParams = filter (not . null . assocData) variants
    enumName <- genEnumStructName identifier
    pEnumVariants <- mapM (\(EnumVariant this_variant _) -> genEnumVariantName identifier this_variant) variants
    pEnumParameterStructs <- mapM (genEnumVariantParameterStruct ann identifier) variantsWithParams
    enumStruct <- genEnumStruct enumName variantsWithParams
    return $ CExtDecl (CEDEnum (Just enumName) (CEnum Nothing [(v, Nothing) | v <- pEnumVariants] [])) cAnn
                : pEnumParameterStructs ++ [enumStruct]

        where

            genParameterUnionField :: EnumVariant -> CHeaderGenerator CDeclaration
            genParameterUnionField (EnumVariant this_variant _) = do
                paramsStructName <- genEnumParameterStructName identifier this_variant
                paramsStructType <- genType noqual (TDefinedType paramsStructName)
                return $ CDecl (CTypeSpec paramsStructType) (Just this_variant) Nothing

            genParameterUnion :: [EnumVariant] -> CHeaderGenerator CDeclaration
            genParameterUnion variantsWithParams = do
                pFields <- mapM genParameterUnionField variantsWithParams
                return $ CDecl (CTSStructUnion (CStruct CUnionTag Nothing pFields [])) Nothing Nothing

            genEnumStruct :: Identifier -> [EnumVariant] -> CHeaderGenerator CFileItem
            genEnumStruct enumName variantsWithParams = do
                enumType <- genType noqual (TDefinedType enumName)
                let cAnn = buildDeclarationAnn ann True
                    enumField = CDecl (CTypeSpec enumType) (Just variant) Nothing
                case variantsWithParams of
                    [] -> return $ CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing [enumField] [])) cAnn
                    [var] -> do
                        unionField <- genParameterUnionField var
                        return $ CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing [enumField, unionField] [])) cAnn
                    _ -> do
                        unionField <- genParameterUnion variantsWithParams
                        return $ CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing [enumField, unionField] [])) cAnn
genTypeDefinitionDecl (TypeDefinition (Interface identifier members _) ann) = do
    let cAnn = buildDeclarationAnn ann True
        cThatField = CDecl (CTypeSpec (CTPointer (CTVoid noqual) noqual)) (Just thatField) Nothing
    procedureFields <- mapM genInterfaceProcedureField members
    return [CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing (cThatField : procedureFields) [])) cAnn]

    where

        genInterfaceProcedureField :: InterfaceMember SemanticAnn -> CHeaderGenerator CDeclaration
        genInterfaceProcedureField (InterfaceProcedure procedure params _) = do
            cParamTypes <- mapM (genType noqual . paramTerminaType) params
            let cThisParamType = CTPointer (CTVoid noqual) constqual
                cFuncPointerType = CTPointer (CTFunction (CTVoid noqual) (cThisParamType : cParamTypes)) noqual
            return $ CDecl (CTypeSpec cFuncPointerType) (Just procedure) Nothing

genTypeDefinitionDecl clsdef@(TypeDefinition cls@(Class clsKind identifier _members _provides modifiers) ann) = do
    (fields, functions) <- classifyClassMembers cls
    structModifiers <- mapM genAttribute (filterStructModifiers modifiers)
    fields' <- case clsKind of
        TaskClass -> return fields
        _ -> return $ filter (\case {
            ClassField (FieldDefinition _ (TSinkPort {})) _ -> False;
            ClassField (FieldDefinition _ (TInPort {})) _ -> False;
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
    return $ CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing structFields structModifiers)) cAnn
                : cFunctions

    where

        genClassField :: ClassMember SemanticAnn -> CHeaderGenerator CDeclaration
        genClassField (ClassField fld _) = genFieldDeclaration fld
        genClassField member = throwError $ InternalError $ "invalid class member. Not a field: " ++ show member

        genClassFunctionDeclaration :: ClassMember SemanticAnn -> CHeaderGenerator CFileItem
        genClassFunctionDeclaration (ClassViewer viewer params rts _ _) = do
            retType <- maybe (return (CTVoid noqual)) (genType noqual) rts
            cParamDecls <- mapM genParameterDeclaration params
            cSelfParam <- genConstSelfParam clsdef
            clsFuncName <- genClassFunctionName identifier viewer
            return $ CExtDecl (CEDFunction retType clsFuncName (cSelfParam : cParamDecls)) (buildDeclarationAnn ann True)
        genClassFunctionDeclaration (ClassProcedure procedure params _ _) = do
            cParamDecls <- mapM genParameterDeclaration params
            cThisParam <- genThisParam
            clsFuncName <- genClassFunctionName identifier procedure
            return $ CExtDecl (CEDFunction (CTVoid noqual) clsFuncName (cThisParam : cParamDecls)) (buildDeclarationAnn ann True)
        genClassFunctionDeclaration (ClassMethod method rts _ _) = do
            retType <- maybe (return (CTVoid noqual)) (genType noqual) rts
            clsFuncName <- genClassFunctionName identifier method
            cSelfParam <- genSelfParam clsdef
            return $ CExtDecl (CEDFunction retType clsFuncName [cSelfParam]) (buildDeclarationAnn ann True)
        genClassFunctionDeclaration (ClassAction action param rts _ _) = do
            retType <- genType noqual rts
            cParamDecl <- genParameterDeclaration param
            cSelfParam <- genSelfParam clsdef
            clsFuncName <- genClassFunctionName identifier action
            return $ CExtDecl (CEDFunction retType clsFuncName [cSelfParam, cParamDecl]) (buildDeclarationAnn ann True)
        genClassFunctionDeclaration member = throwError $ InternalError $ "invalid class member. Not a function: " ++ show member
genTypeDefinitionDecl ts = throwError $ InternalError $ "Unsupported type definition: " ++ show ts

genClassDefinition :: AnnASTElement SemanticAnn -> CSourceGenerator [CFileItem]
genClassDefinition clsdef@(TypeDefinition cls@(Class _clsKind identifier _members _provides _) _) = do
    (_fields, functions) <- classifyClassMembers cls
    mapM genClassFunctionDefinition functions

    where

        genClassFunctionDefinition :: ClassMember SemanticAnn -> CSourceGenerator CFileItem
        genClassFunctionDefinition (ClassViewer viewer parameters rts (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier viewer
            cRetType <- maybe (return (CTVoid noqual)) (genType noqual) rts
            cParamDecls <- mapM genParameterDeclaration parameters
            cSelfParam <- genConstSelfParam clsdef
            cBody <- foldM (\acc x -> do
                cStmt <- genBlocks x
                return $ acc ++ cStmt) [] stmts
            return $ CFunctionDef Nothing (CFunction cRetType clsFuncName (cSelfParam : cParamDecls)
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True)
        genClassFunctionDefinition (ClassProcedure procedure parameters (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier procedure
            cThisParam <- genThisParam
            cParamDecls <- mapM genParameterDeclaration parameters
            selfCastStmt <- genSelfCastStmt
            cBody <- genProcedureStmts [selfCastStmt, genProcedureOnEntry] stmts
            return $ CFunctionDef Nothing (CFunction (CTVoid noqual) clsFuncName (cThisParam : cParamDecls)
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True)

            where

                selfVariable :: Identifier
                selfVariable = "self"

                genSelfCastStmt :: CSourceGenerator CCompoundBlockItem
                genSelfCastStmt = do
                    let cAnn = buildGenericAnn ann
                    selfCType <- flip CTPointer noqual <$> genType noqual (TDefinedType identifier)
                    let cExpr = CExprCast (CExprValOf (CVar thisParam (CTPointer (CTVoid noqual) noqual)) (CTPointer (CTVoid noqual) noqual) cAnn) selfCType cAnn
                    return $ CBlockDecl (CDecl (CTypeSpec selfCType) (Just selfVariable) (Just cExpr)) (buildDeclarationAnn ann True)

                genProcedureOnEntry, genProcedureOnExit :: CCompoundBlockItem
                genProcedureOnEntry =
                    let cAnn = buildGenericAnn ann
                        selfResourceExpr = CExprAddrOf (CField (CVar selfVariable (CTPointer (CTTypeDef identifier noqual) noqual)) resourceClassIDField cResourceIDType) (CTPointer cResourceIDType noqual) cAnn
                    in
                    CBlockStmt $ CSDo (CExprCall (CExprValOf (CVar resourceLock cResourceLockFuncType) cResourceLockFuncType cAnn) [selfResourceExpr] (CTVoid noqual) cAnn) (buildStatementAnn ann True)
                genProcedureOnExit =
                    let cAnn = buildGenericAnn ann
                        selfResourceExpr = CExprAddrOf (CField (CVar selfVariable (CTPointer (CTTypeDef identifier noqual) noqual)) resourceClassIDField cResourceIDType) (CTPointer cResourceIDType noqual) cAnn
                    in
                    CBlockStmt $ CSDo (CExprCall (CExprValOf (CVar resourceUnlock cResourceLockFuncType) cResourceUnlockFuncType cAnn) [selfResourceExpr] (CTVoid noqual) cAnn) (buildStatementAnn ann True)
                
                genProcedureStmts :: [CCompoundBlockItem] -> [BasicBlock SemanticAnn] -> CSourceGenerator [CCompoundBlockItem]
                genProcedureStmts acc [ret@(ReturnBlock {})] = do
                    cReturn <- genBlocks ret
                    return $ acc ++ (genProcedureOnExit : cReturn)
                genProcedureStmts _ [_] = throwError $ InternalError $ "Last block is not a statement: " ++ show procedure
                genProcedureStmts _ [] = throwError $ InternalError $ "Empty procedure: " ++ show procedure
                genProcedureStmts acc (x : xs) = do
                    cStmt <- genBlocks x
                    genProcedureStmts (acc ++ cStmt) xs

        genClassFunctionDefinition (ClassMethod method rts (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier method
            cRetType <- maybe (return (CTVoid noqual)) (genType noqual) rts
            cSelfParam <- genSelfParam clsdef
            cBody <- foldM (\acc x -> do
                cStmt <- genBlocks x
                return $ acc ++ cStmt) [] stmts
            return $ CFunctionDef Nothing (CFunction cRetType clsFuncName [cSelfParam]
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True)
        genClassFunctionDefinition (ClassAction action param rts (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier action
            cRetType <- genType noqual rts
            cSelfParam <- genSelfParam clsdef
            cParam <- genParameterDeclaration param
            cBody <- foldM (\acc x -> do
                cStmt <- genBlocks x
                return $ acc ++ cStmt) [] stmts
            return $ CFunctionDef Nothing (CFunction cRetType clsFuncName [cSelfParam, cParam]
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True)
        genClassFunctionDefinition member = throwError $ InternalError $ "invalid class member. Not a function: " ++ show member

genClassDefinition e = throwError $ InternalError $ "AST element is not a class: " ++ show e