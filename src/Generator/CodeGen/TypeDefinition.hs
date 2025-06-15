{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.TypeDefinition where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Semantic.Types
import Control.Monad.Except
import Generator.CodeGen.Common
import Generator.CodeGen.Statement
import qualified Data.Map as M
import Utils.Annotations
import Generator.LanguageC.Embedded
import Control.Monad (zipWithM, foldM)
import Generator.CodeGen.Application.Utils
import Generator.CodeGen.Types
import Generator.CodeGen.Expression
import Generator.Monadic
import qualified Data.Set as S
import Control.Monad.State
import Core.Utils

filterStructModifiers :: [Modifier a] -> [Modifier a]
filterStructModifiers = filter (\case
      Modifier "packed" Nothing -> True
      Modifier "aligned" _ -> True
      _ -> False)

genFieldDeclaration :: FieldDefinition SemanticAnn -> CGenerator [CDeclaration]
genFieldDeclaration (FieldDefinition identifier ts@(TFixedLocation {}) _) = do
    cTs <- genType noqual ts
    return [field identifier cTs]
genFieldDeclaration (FieldDefinition identifier (TAccessPort ts@(TAllocator {})) _) = do
    cTs <- genType noqual ts
    return [field identifier cTs]
genFieldDeclaration (FieldDefinition identifier (TAccessPort ts@(TAtomicAccess {})) _) = do
    cTs <- genType noqual ts
    return [field identifier cTs]
genFieldDeclaration (FieldDefinition identifier (TAccessPort ts@(TAtomicArrayAccess {})) _) = do
    cTs <- genType noqual ts
    return [field identifier cTs]
genFieldDeclaration (FieldDefinition identifier (TAccessPort (TInterface RegularInterface _)) (SemanticAnn (FTy (AccessPortField members)) _)) = do
    let cThatField = field thatField (ptr void)
    memberFields <- mapM genInterfaceProcedureField (M.elems members)
    return [
            CDecl
                (CTSStructUnion
                    (CStruct CStructTag Nothing (cThatField : memberFields) []))
                (Just identifier) Nothing
        ]

    where

        genInterfaceProcedureField :: InterfaceMember SemanticAnn -> CGenerator CDeclaration
        genInterfaceProcedureField (InterfaceProcedure _ak procedure params _modifiers _) = do
            cParamTypes <- mapM (genType noqual . paramType) params
            let cEventParamType = _const . ptr $ _const __termina_event_t
                cThisParamType = _const . ptr $ void
                cFuncPointerType = CTPointer (CTFunction (CTVoid noqual) (cEventParamType : cThisParamType : cParamTypes)) noqual
            return $ CDecl (CTypeSpec cFuncPointerType) (Just procedure) Nothing
genFieldDeclaration (FieldDefinition identifier (TAccessPort (TInterface SystemInterface _)) (SemanticAnn (FTy (AccessPortField members)) _)) = do
    memberFields <- mapM genInterfaceProcedureField (M.elems members)
    return [
            CDecl
                (CTSStructUnion
                    (CStruct CStructTag Nothing memberFields []))
                (Just identifier) Nothing
        ]

    where

        genInterfaceProcedureField :: InterfaceMember SemanticAnn -> CGenerator CDeclaration
        genInterfaceProcedureField (InterfaceProcedure _ak procedure params _modifiers _) = do
            let cEventParamType = _const . ptr $ _const __termina_event_t
            cParamTypes <- mapM (genType noqual . paramType) params
            let cFuncPointerType = CTPointer (CTFunction (CTVoid noqual) (cEventParamType : cParamTypes)) noqual
            return $ CDecl (CTypeSpec cFuncPointerType) (Just procedure) Nothing

genFieldDeclaration (FieldDefinition _ (TAccessPort (TInterface _ _)) ann) = error $ "Invalid access port annotation" ++ show ann
genFieldDeclaration (FieldDefinition identifier ts _) = do
    cTs <- genType noqual ts
    return [field identifier cTs]

genOptionSomeParameterStruct :: TerminaType SemanticAnn ->  CGenerator CFileItem
genOptionSomeParameterStruct ts = do
    cTs <- genType noqual ts
    let fld = field (namefy "0") cTs
    identifier <- genOptionParameterStructName ts
    return $ pre_cr $ struct identifier [fld] []

genStatusFailureParameterStruct :: TerminaType SemanticAnn ->  CGenerator CFileItem
genStatusFailureParameterStruct ts = do
    cTs <- genType noqual ts
    let fld = field (namefy "0") cTs
    identifier <- genStatusParameterStructName ts
    return $ pre_cr $ struct identifier [fld] []

genStatusStruct :: TerminaType SemanticAnn -> CGenerator [CFileItem]
genStatusStruct ts = do
    paramsStructName <- genStatusParameterStructName ts
    enumStructName <- genEnumStructName "status"
    paramsStructType <- genType noqual (TStruct paramsStructName)
    enumStructType <- genType noqual (TStruct enumStructName)
    let failure = field statusFailureVariant paramsStructType
        this_variant = field variant enumStructType
    identifier <- genStatusStructName ts
    paramStruct <- genStatusFailureParameterStruct ts
    return [
            paramStruct,
            pre_cr $ struct identifier [failure, this_variant] []
        ]

genResultOkParameterStruct :: TerminaType SemanticAnn -> TerminaType SemanticAnn ->  CGenerator CFileItem
genResultOkParameterStruct okTy errorTy = do
    cTs <- genType noqual okTy
    let fld = field (namefy "0") cTs
    identifier <- genResultParameterStructName okTy errorTy resultOkVariant
    return $ pre_cr $ struct identifier [fld] []

genResultErrorParameterStruct :: TerminaType SemanticAnn -> TerminaType SemanticAnn ->  CGenerator CFileItem
genResultErrorParameterStruct okTy errorTy = do
    cTs <- genType noqual errorTy
    let fld = field (namefy "0") cTs
    identifier <- genResultParameterStructName okTy errorTy resultErrorVariant
    return $ pre_cr $ struct identifier [fld] []

genResultStruct :: TerminaType SemanticAnn -> TerminaType SemanticAnn -> CGenerator [CFileItem]
genResultStruct okTy errorTy = do
    okParamStructName <- genResultParameterStructName okTy errorTy resultOkVariant
    okParamsStructType <- genType noqual (TStruct okParamStructName)
    errorParamStructName <- genResultParameterStructName okTy errorTy resultErrorVariant
    errorParamsStructType <- genType noqual (TStruct errorParamStructName)
    enumStructName <- genEnumStructName "result"
    enumStructType <- genType noqual (TStruct enumStructName)
    let ok = field resultOkVariant okParamsStructType
        err = field resultErrorVariant errorParamsStructType
        this_variant = field variant enumStructType
    identifier <- genResultStructName okTy errorTy
    okParamStruct <- genResultOkParameterStruct okTy errorTy
    errorParamStruct <- genResultErrorParameterStruct okTy errorTy
    return [
            okParamStruct,
            errorParamStruct,
            pre_cr $ struct identifier [ok, err, this_variant] []
        ]

genResultStructFromTypeDef :: TerminaType SemanticAnn -> (TerminaType SemanticAnn, TerminaType SemanticAnn) -> CGenerator [CFileItem]
genResultStructFromTypeDef (TStruct identifier) (okTy@(TStruct okIdentifier), errorTy@(TStruct errorIdentifier)) = do
    genTypes <- gets (generatedTypes . monadicTypes)
    if (identifier == okIdentifier) && (identifier == errorIdentifier) then do
        genResultStruct okTy errorTy
    else if (identifier == okIdentifier) && M.member errorTy genTypes then do
        let errorTyModule = genTypes M.! errorTy
        modify $ \st -> st {
            extraImports = S.insert errorTyModule (extraImports st)
        } 
        genResultStruct okTy errorTy 
    else if (identifier == errorIdentifier) && M.member okTy genTypes then do
        let okTyModule = genTypes M.! okTy
        modify $ \st -> st {
            extraImports = S.insert okTyModule (extraImports st)
        } 
        genResultStruct okTy errorTy 
    else return []
genResultStructFromTypeDef (TStruct identifier) (TStruct okIdentifier, errorTy) =
    if identifier == okIdentifier then
        genResultStruct (TStruct okIdentifier) errorTy
    else
        throwError $ InternalError $ "Invalid result struct: " ++ show identifier ++ " " ++ show okIdentifier ++ " " ++ show errorTy
genResultStructFromTypeDef (TStruct identifier) (okTy, TStruct errorIdentifier) =
    if identifier == errorIdentifier then
        genResultStruct okTy (TStruct errorIdentifier)
    else
        throwError $ InternalError $ "Invalid result struct: " ++ show identifier ++ " " ++ show okTy ++ " " ++ show errorIdentifier
genResultStructFromTypeDef (TEnum identifier) (okTy@(TEnum okIdentifier), errorTy@(TEnum errorIdentifier)) = do
    genTypes <- gets (generatedTypes . monadicTypes)
    if (identifier == okIdentifier) && (identifier == errorIdentifier) then do
        genResultStruct okTy errorTy
    else if (identifier == okIdentifier) && M.member errorTy genTypes then do
        let errorTyModule = genTypes M.! errorTy
        modify $ \st -> st {
            extraImports = S.insert errorTyModule (extraImports st)
        } 
        genResultStruct okTy errorTy 
    else if (identifier == errorIdentifier) && M.member okTy genTypes then do
        let okTyModule = genTypes M.! okTy
        modify $ \st -> st {
            extraImports = S.insert okTyModule (extraImports st)
        } 
        genResultStruct okTy errorTy 
    else return []
genResultStructFromTypeDef (TEnum identifier) (TEnum okIdentifier, errorTy) =
    if identifier == okIdentifier then
        genResultStruct (TEnum okIdentifier) errorTy
    else
        throwError $ InternalError $ "Invalid result enum: " ++ show identifier ++ " " ++ show okIdentifier ++ " " ++ show errorTy
genResultStructFromTypeDef (TEnum identifier) (okTy, TEnum errorIdentifier) =
    if identifier == errorIdentifier then
        genResultStruct okTy (TEnum errorIdentifier)
    else
        throwError $ InternalError $ "Invalid result enum: " ++ show identifier ++ " " ++ show okTy ++ " " ++ show errorIdentifier
genResultStructFromTypeDef _ _ = throwError $ InternalError "Invalid result enum: not a struct"

genOptionStruct :: TerminaType SemanticAnn -> CGenerator [CFileItem]
genOptionStruct ts = do
    paramsStructName <- genOptionParameterStructName ts
    enumStructName <- genEnumStructName "option"
    paramsStructType <- genType noqual (TStruct paramsStructName)
    enumStructType <- genType noqual (TStruct enumStructName)
    let some = field optionSomeVariant paramsStructType
        this_variant = field variant enumStructType
    identifier <- genOptionStructName ts
    paramStruct <- genOptionSomeParameterStruct ts
    return [
            paramStruct,
            pre_cr $ struct identifier [some, this_variant] []
        ]

genAttribute :: Modifier a -> CGenerator CAttribute
genAttribute (Modifier name Nothing) = do
    return $ CAttr name []
genAttribute (Modifier name (Just expr)) = do
    cExpr <- genConst expr
    return $ CAttr name [cExpr]

    where

        genConst :: (MonadError CGeneratorError m) => Const a -> m CExpression
        genConst c = do
            let cAnn = LocatedElement CGenericAnn Internal
            case c of
                (I i _) ->
                    let cInteger = genInteger i in
                    return $ CExprConstant (CIntConst cInteger) (CTInt IntSize32 Unsigned noqual) cAnn
                (B True) -> return $ CExprConstant (CIntConst (CInteger 1 CDecRepr)) (CTBool noqual) cAnn
                (B False) -> return $ CExprConstant (CIntConst (CInteger 0 CDecRepr)) (CTBool noqual) cAnn
                (C ch) -> return $ CExprConstant (CCharConst (CChar ch)) (CTChar noqual) cAnn
                Null -> throwError $ InternalError "Null constant should not be translated to C"

genEnumVariantParameterStruct :: SemanticAnn -> Identifier -> EnumVariant SemanticAnn -> CGenerator CFileItem
genEnumVariantParameterStruct ann identifier (EnumVariant this_variant params) = do
    let cAnn = buildDeclarationAnn ann True
    pParams <- zipWithM genEnumVariantParameter params [0..]
    paramsStructName <- genEnumParameterStructName identifier this_variant
    return $ CExtDecl (CEDStructUnion (Just paramsStructName) (CStruct CStructTag Nothing pParams [])) cAnn
    where
        genEnumVariantParameter :: TerminaType SemanticAnn -> Integer -> CGenerator CDeclaration
        genEnumVariantParameter ts index = do
            cParamType <- genType noqual ts
            return $ CDecl (CTypeSpec cParamType) (Just (namefy $ show index)) Nothing

classifyClassMembers :: (MonadError CGeneratorError m) => TypeDef SemanticAnn -> m ([ClassMember SemanticAnn], [ClassMember SemanticAnn])
classifyClassMembers (Class clsKind _identifier members _provides _modifiers) =
    return $ foldr (\m (fields, funcs) -> case m of
        fld@(ClassField fieldDef) -> case clsKind of
            TaskClass -> (fld : fields, funcs)
            _ -> case fieldDef of
                (FieldDefinition _ (TSinkPort {}) _) -> (fields, funcs)
                (FieldDefinition _ (TInPort {}) _) -> (fields, funcs)
                _ -> (fld : fields, funcs)
        func -> (fields, func : funcs)) ([], []) members
classifyClassMembers e = throwError $ InternalError $ "Not a class definition: " ++ show e

genThisParam :: (MonadError CGeneratorError m) => m CDeclaration
genThisParam = return $ field thisParam (_const . ptr $ void)

genConstThisParam :: (MonadError CGeneratorError m) => m CDeclaration
genConstThisParam = return $ field thisParam (_const . ptr $ _const void)

genThatParam :: (MonadError CGeneratorError m) => m CDeclaration
genThatParam = return $ field thatField (_const . ptr $ void)

getEventParam :: (MonadError CGeneratorError m) => m CDeclaration
getEventParam = return $ field eventParam (_const . ptr $ _const __termina_event_t)

genSelfParam :: (MonadError CGeneratorError m) => AnnASTElement SemanticAnn -> m CDeclaration
genSelfParam (TypeDefinition (Class _clsKind identifier _members _provides _modifiers) _) =
    return $ CDecl (CTypeSpec (CTPointer (CTTypeDef identifier noqual) constqual)) (Just selfParam) Nothing
genSelfParam e = throwError $ InternalError $ "Not a class definition: " ++ show e

genConstSelfParam :: (MonadError CGeneratorError m) => AnnASTElement SemanticAnn -> m CDeclaration
genConstSelfParam (TypeDefinition (Class _clsKind identifier _members _provides _modifiers) _) =
    return $ CDecl (CTypeSpec (CTPointer (CTTypeDef identifier constqual) constqual)) (Just selfParam) Nothing
genConstSelfParam e = throwError $ InternalError $ "Not a class definition: " ++ show e

genSelfCastStmt :: SemanticAnn -> Identifier -> CGenerator CCompoundBlockItem
genSelfCastStmt ann identifier = do
    selfCType <- genType noqual (TStruct identifier)
    let cExpr = cast (ptr selfCType) (thisParam @: ptr void)
    return $ pre_cr (var selfParam (ptr selfCType) @:= cExpr) |>> getLocation ann

genConstSelfCastStmt :: SemanticAnn -> Identifier -> CGenerator CCompoundBlockItem
genConstSelfCastStmt ann identifier = do
    selfCType <- genType constqual (TStruct identifier)
    let cExpr = cast (ptr selfCType) (thisParam @: ptr (_const void))
    return $ pre_cr (var selfParam (ptr selfCType) @:= cExpr) |>> getLocation ann

-- | __termina_lock_t __lock = __termina_resource__lock(
--        &__ev->owner, &self->__lock_type);
genResourceLockStmt :: SemanticAnn -> Identifier -> CGenerator CCompoundBlockItem
genResourceLockStmt ann identifier = do
    selfCType <- genType noqual (TStruct identifier)
    let lock = var lockVar __termina_lock_t @:= __termina_resource__lock @@ [
            addrOf (eventParam @: ptr __termina_event_t @. "owner" @: __termina_id_t),
            addrOf ("self" @: ptr selfCType @. resourceLockTypeField @: __termina_resource_lock_type_t)
            ]
    return $ pre_cr lock |>> getLocation ann

genResourceUnlockStmt :: SemanticAnn -> Identifier -> CGenerator CCompoundBlockItem
genResourceUnlockStmt ann identifier = do
    selfCType <- genType noqual (TStruct identifier)
    let unlock = __termina_resource__unlock @@ [
            addrOf (eventParam @: ptr __termina_event_t @. "owner" @: __termina_id_t),
            addrOf ("self" @: ptr selfCType @. resourceLockTypeField @: __termina_resource_lock_type_t),
            lockVar @: __termina_lock_t
            ]
    return $ pre_cr unlock |>> getLocation ann

-- | TypeDef pretty printer.
genTypeDefinitionDecl :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genTypeDefinitionDecl (TypeDefinition (Struct identifier fls modifiers) ann) = do
    let cAnn = buildDeclarationAnn ann True
    structModifiers <- mapM genAttribute (filterStructModifiers modifiers)
    cFields <- concat <$> traverse genFieldDeclaration fls
    opts <- gets (optionTypes . monadicTypes)
    optsDeclExt <- if S.member (TStruct identifier) opts then
        genOptionStruct (TStruct identifier)
        else return []
    stats <- gets (statusTypes . monadicTypes)
    statusDeclExt <- if S.member (TStruct identifier) stats then
        genStatusStruct (TStruct identifier)
        else return []
    results <- gets (resultTypes . monadicTypes)
    resultsDeclExt <- concat <$> case M.lookup (TStruct identifier) results of
        Nothing -> return []
        Just resultSet -> mapM (genResultStructFromTypeDef (TStruct identifier)) (S.toList resultSet)
    currModule <- gets currentModule
    modify $ \st -> st {
            monadicTypes = (monadicTypes st) {
                generatedTypes = M.insert (TStruct identifier) currModule (generatedTypes . monadicTypes $ st)
            }
        }
    return $ CExtDecl (CEDStructUnion (Just identifier) (CStruct CStructTag Nothing cFields structModifiers)) cAnn : (optsDeclExt ++ statusDeclExt ++ resultsDeclExt)
genTypeDefinitionDecl (TypeDefinition (Enum identifier variants _) ann) = do
    let cAnn = buildDeclarationAnn ann True
        variantsWithParams = filter (not . null . assocData) variants
    enumName <- genEnumStructName identifier
    pEnumVariants <- mapM (\(EnumVariant this_variant _) -> genEnumVariantName identifier this_variant) variants
    pEnumParameterStructs <- mapM (genEnumVariantParameterStruct ann identifier) variantsWithParams
    enumStruct <- genEnumStruct enumName variantsWithParams
    opts <- gets (optionTypes . monadicTypes)
    optsDeclExt <- if S.member (TEnum identifier) opts then
        genOptionStruct (TStruct identifier)
        else return []
    stats <- gets (statusTypes . monadicTypes)
    statusDeclExt <- if S.member (TEnum identifier) stats then
        genStatusStruct (TStruct identifier)
        else return []
    return $ CExtDecl (CEDEnum (Just enumName) (CEnum Nothing [(v, Nothing) | v <- pEnumVariants] [])) cAnn
                : pEnumParameterStructs ++ (enumStruct : optsDeclExt ++ statusDeclExt)

        where

            genParameterUnionField :: EnumVariant SemanticAnn -> CGenerator CDeclaration
            genParameterUnionField (EnumVariant this_variant _) = do
                paramsStructName <- genEnumParameterStructName identifier this_variant
                paramsStructType <- genType noqual (TStruct paramsStructName)
                return $ CDecl (CTypeSpec paramsStructType) (Just this_variant) Nothing

            genParameterUnion :: [EnumVariant SemanticAnn] -> CGenerator CDeclaration
            genParameterUnion variantsWithParams = do
                pFields <- mapM genParameterUnionField variantsWithParams
                return $ CDecl (CTSStructUnion (CStruct CUnionTag Nothing pFields [])) Nothing Nothing

            genEnumStruct :: Identifier -> [EnumVariant SemanticAnn] -> CGenerator CFileItem
            genEnumStruct enumName variantsWithParams = do
                enumType <- genType noqual (TStruct enumName)
                let enumField = field variant enumType
                case variantsWithParams of
                    [] -> return $ pre_cr $ struct identifier [enumField] [] |>> getLocation ann
                    [v] -> do
                        unionField <- genParameterUnionField v
                        return $ pre_cr $ struct identifier [enumField, unionField] [] |>> getLocation ann
                    _ -> do
                        unionField <- genParameterUnion variantsWithParams
                        return $ pre_cr $ struct identifier [enumField, unionField] [] |>> getLocation ann
genTypeDefinitionDecl (TypeDefinition (Interface RegularInterface identifier _extends procs _) ann) = do
    let cThatField = field thatField (ptr void)
    procedureFields <- mapM genInterfaceProcedureField procs
    return [pre_cr $ struct identifier (cThatField : procedureFields) [] |>> getLocation ann]

    where

        genInterfaceProcedureField :: InterfaceMember SemanticAnn -> CGenerator CDeclaration
        genInterfaceProcedureField (InterfaceProcedure _ak procedure params _modifiers _) = do
            cParamTypes <- mapM (genType noqual . paramType) params
            let cThisParamType = _const . ptr $ void
                cFuncPointerType = CTPointer (CTFunction (CTVoid noqual) (cThisParamType : cParamTypes)) noqual
            return $ CDecl (CTypeSpec cFuncPointerType) (Just procedure) Nothing

genTypeDefinitionDecl clsdef@(TypeDefinition cls@(Class clsKind identifier _members _provides modifiers) ann) = do
    -- | Classify class members into fields and functions.
    (fields, functions) <- classifyClassMembers cls
    structModifiers <- mapM genAttribute (filterStructModifiers modifiers)
    fields' <- case clsKind of
        TaskClass -> return fields
        _ -> return $ filter (\case {
            ClassField (FieldDefinition _ (TSinkPort {}) _) -> False;
            ClassField (FieldDefinition _ (TInPort {}) _) -> False;
            _ -> True}) fields
    cFields <- concat <$> traverse genClassField fields'
    cFunctions <- traverse genClassFunctionDeclaration functions
    let structFields = case clsKind of
            TaskClass ->
                let cMsgQueueIDField = field taskMsgQueueIDField (typeDef terminaID) 
                    cTaskIDField = field taskIDField (typeDef terminaID) in
                [cTaskIDField, cMsgQueueIDField] ++ cFields
            HandlerClass ->
                let cIDField = field handlerIDField (typeDef terminaID) in
                cIDField : cFields
            ResourceClass ->
                let cIDField = field resourceLockTypeField __termina_resource_lock_type_t in
                cIDField : cFields
            _ -> cFields
    case clsKind of
        TaskClass -> do
            cTaskFunction <- genTaskFunctionDeclaration
            return $ [pre_cr (struct identifier structFields structModifiers |>> getLocation ann), cTaskFunction]
                ++ cFunctions
        _ ->
            return $ pre_cr (struct identifier structFields structModifiers |>> getLocation ann)
                : cFunctions

    where

        genTaskFunctionDeclaration :: CGenerator CFileItem
        genTaskFunctionDeclaration = do
            return $ CExtDecl (CEDFunction void (namefy identifier <::> "termina_task") [
                    CDecl (CTypeSpec (_const . ptr $ void)) (Just "arg") Nothing
                ]) (buildDeclarationAnn ann True)

        genClassField :: ClassMember SemanticAnn -> CGenerator [CDeclaration]
        genClassField (ClassField fld) = genFieldDeclaration fld
        genClassField member = throwError $ InternalError $ "invalid class member. Not a field: " ++ show member

        genClassFunctionDeclaration :: ClassMember SemanticAnn -> CGenerator CFileItem
        genClassFunctionDeclaration (ClassViewer viewer params rts _ _) = do
            retType <- maybe (return void) (genType noqual) rts
            cParamDecls <- mapM genParameterDeclaration params
            cEventParam <- getEventParam
            cSelfParam <- genConstSelfParam clsdef
            clsFuncName <- genClassFunctionName identifier viewer
            return $ CExtDecl (CEDFunction retType clsFuncName (cEventParam : cSelfParam : cParamDecls)) (buildDeclarationAnn ann True)
        genClassFunctionDeclaration (ClassProcedure ak procedure params _ _) = do
            cParamDecls <- mapM genParameterDeclaration params
            cEventParam <- getEventParam
            cThisParam <- case ak of
                Immutable -> genConstThisParam
                _ -> genThisParam
            clsFuncName <- genClassFunctionName identifier procedure
            return $ CExtDecl (CEDFunction void clsFuncName (cEventParam : cThisParam : cParamDecls)) (buildDeclarationAnn ann True)
        genClassFunctionDeclaration (ClassMethod ak method rts _ _) = do
            retType <- maybe (return (CTVoid noqual)) (genType noqual) rts
            clsFuncName <- genClassFunctionName identifier method
            cEventParam <- getEventParam
            cSelfParam <- case ak of
                Immutable -> genConstSelfParam clsdef
                _ -> genSelfParam clsdef
            return $ CExtDecl (CEDFunction retType clsFuncName [cEventParam, cSelfParam]) (buildDeclarationAnn ann True)
        genClassFunctionDeclaration (ClassAction ak action param rts _ _) = do
            retType <- genType noqual rts
            cEventParam <- getEventParam
            cThisParam <- case ak of
                Immutable -> genConstThisParam
                _ -> genThisParam
            clsFuncName <- genClassFunctionName identifier action
            case param of
                Just p -> do
                    cParamDecl <- genParameterDeclaration p
                    return $ CExtDecl (CEDFunction retType clsFuncName [cEventParam, cThisParam, cParamDecl]) (buildDeclarationAnn ann True)
                Nothing ->
                    return $ CExtDecl (CEDFunction retType clsFuncName [cEventParam, cThisParam]) (buildDeclarationAnn ann True)
        genClassFunctionDeclaration member = throwError $ InternalError $ "invalid class member. Not a function: " ++ show member
genTypeDefinitionDecl ts = throwError $ InternalError $ "Unsupported type definition: " ++ show ts

genTaskClassCode :: AnnASTElement SemanticAnn -> CGenerator CFileItem
genTaskClassCode (TypeDefinition (Class TaskClass classId members _provides _) _) = do
    cBody <- genBody
    cTaskFunctionName <- taskFunctionName classId
    return $ pre_cr $ function cTaskFunctionName [
            "arg" @: ptr void
        ] @-> void $ trail_cr . block $
            cBody ++ [pre_cr (_return Nothing)]

    where

        actions :: [(Identifier, TerminaType SemanticAnn, Identifier)]
        actions = foldl (\acc member ->
            case member of
                ClassField (FieldDefinition identifier (TSinkPort dts action) _) -> (identifier, dts, action) : acc
                ClassField (FieldDefinition identifier (TInPort dts action) _) -> (identifier, dts, action) : acc
                _ -> acc
            ) [] members

        getMsgDataVariable :: Bool -> Identifier -> TerminaType SemanticAnn -> CGenerator CCompoundBlockItem
        getMsgDataVariable before action dts = do
            cDataType <- genType noqual dts
            if before then
                return $ pre_cr $ var (action <::> "msg_data") cDataType
            else
                return $ no_cr $ var (action <::> "msg_data") cDataType

        getMsgDataVariables :: [(Identifier, TerminaType SemanticAnn, Identifier)] -> CGenerator [CCompoundBlockItem]
        getMsgDataVariables [] = return []
        getMsgDataVariables ((_identifier, dts, action) : xs) = do
            decl <- getMsgDataVariable True action dts
            rest <- mapM (uncurry (getMsgDataVariable False) . (\(_, dts', action') -> (action', dts'))) xs
            return $ decl : rest

        genCase :: (Identifier, TerminaType SemanticAnn, Identifier) -> CGenerator [CCompoundBlockItem]
        genCase (port, TUnit, action) = do
            this_variant <- genVariantForPort classId port
            classFunctionName <- genClassFunctionName classId action
            classStructType <- genType noqual (TStruct classId)

            let classFunctionType = CTFunction __status_int32_t
                    [_const . ptr $ classStructType]
            return
                [
                    -- case variant:
                    pre_cr $ _case (this_variant @: enumFieldType) $
                    -- status = classFunctionName(self, action_msg_data);
                    indent . pre_cr $ "result" @: __status_int32_t @=
                        (classFunctionName @: classFunctionType) @@ [
                            addrOf ("event" @: __termina_event_t),
                            "self" @: classStructType],
                    -- if (result.__variant != Status__Success)
                    indent . pre_cr $ _if (
                            (("result" @: __status_int32_t) @. variant) @: enumFieldType @!= "Success" @: enumFieldType)
                        $ trail_cr $ block [
                            -- ExceptSource source;
                            pre_cr $ var "source" (typeDef "ExceptSource"),
                            -- source.__variant = ExceptSource__Handler;
                            no_cr $ "source" @: typeDef "ExceptSource" @. variant @: enumFieldType @= "ExceptSource__Handler" @: enumFieldType,
                            -- source.Task.__0 = port_connection->handler.handler_id;
                            no_cr $ "source" @: typeDef "ExceptSource" @. "Task" @: enumFieldType @. namefy "0" @: __termina_id_t @= ("self" @: ptr classStructType) @. taskIDField @: __termina_id_t,

                            -- __termina_except__action_failure(source, , status.Failure.__0);
                            pre_cr $ __termina_except__action_failure @@ [
                                "source" @: typeDef "ExceptSource",
                                this_variant @: size_t,
                                ("result" @: __status_int32_t) @. statusFailureVariant @: enumFieldType @. namefy "0" @: int32_t
                            ]
                        ],
                    indent . pre_cr $ _break
                ]
        genCase (port, dts, action) = do
            this_variant <- genVariantForPort classId port
            classFunctionName <- genClassFunctionName classId action
            classStructType <- genType noqual (TStruct classId)
            cDataType <- genType noqual dts

            let classFunctionType = CTFunction __status_int32_t
                    [_const . ptr $ classStructType, cDataType]
            return
                [
                    -- case variant:
                    pre_cr $ _case (this_variant @: enumFieldType) $
                    indent . pre_cr $ __termina_msg_queue__recv @@
                            [
                                ("self" @: ptr classStructType) @. port @: __termina_id_t,
                                cast void_ptr (addrOf ((action <::> "msg_data") @: cDataType)),
                                addrOf ("status" @: int32_t)
                            ],
                    -- if (status.__variant != Status__Success)
                    indent . pre_cr $ _if (
                           "status" @: int32_t @!= dec 0 @: int32_t)
                        $ block [
                            -- __termina_except__msg_queue_recv_error(port, status);
                            no_cr $ __termina_except__msg_queue_recv_error @@ [
                                ("self" @: ptr classStructType) @. port @: __termina_id_t,
                                "status" @: int32_t
                            ]
                        ],
                    -- status = classFunctionName(self, action_msg_data);
                    indent . pre_cr $ "result" @: __status_int32_t @=
                        (classFunctionName @: classFunctionType) @@ [
                            addrOf ("event" @: __termina_event_t),
                            "self" @: classStructType, (action <::> "msg_data") @: cDataType],
                    -- if (result.__variant != Status__Success)
                    indent . pre_cr $ _if (
                            (("result" @: __status_int32_t) @. variant) @: enumFieldType @!= "Success" @: enumFieldType)
                        $ trail_cr $ block [
                            -- ExceptSource source;
                            pre_cr $ var "source" (typeDef "ExceptSource"),
                            -- source.__variant = ExceptSource__Handler;
                            no_cr $ "source" @: typeDef "ExceptSource" @. variant @: enumFieldType @= "ExceptSource__Handler" @: enumFieldType,
                            -- source.Task.__0 = port_connection->handler.handler_id;
                            no_cr $ "source" @: typeDef "ExceptSource" @. "Task" @: enumFieldType @. namefy "0" @: __termina_id_t @= ("self" @: ptr classStructType) @. taskIDField @: __termina_id_t,

                            -- __termina_except__action_failure(source, , status.Failure.__0);
                            pre_cr $ __termina_except__action_failure @@ [
                                "source" @: typeDef "ExceptSource",
                                this_variant @: size_t,
                                ("result" @: __status_int32_t) @. statusFailureVariant @: enumFieldType @. namefy "0" @: int32_t
                            ]
                        ],
                    indent . pre_cr $ _break
                ]

        genLoop :: CGenerator CStatement
        genLoop = do
            classStructType <- genType noqual (TStruct classId)
            cases <- concat <$> traverse genCase actions
            return $ trail_cr . block $ [
                    -- | status = __termina_msg_queue__recv(
                    -- |                self->__task.msgq_id, &next_msg);
                    pre_cr $ __termina_msg_queue__recv @@
                            [
                                ("self" @: ptr classStructType) @. taskMsgQueueIDField @: __termina_id_t,
                                addrOf ("event" @: __termina_event_t),
                                addrOf ("status" @: int32_t)
                            ],
                    -- if (status != Status__Success)
                    pre_cr $ _if
                            ("status" @: int32_t @!= dec 0 @: int32_t)
                        $ block [
                            -- break;
                            no_cr _break
                        ],
                    pre_cr $ _switch ("event" @: __termina_event_t @. "port_id" @: __termina_id_t) $
                        trail_cr . block $ (cases ++
                            [
                                -- default:
                                pre_cr $ _default $
                                    -- __termina_exec__reboot(1);
                                    indent . pre_cr $ __termina_exec__reboot @@ [],
                                    indent . pre_cr $ _break
                            ])
                ]

        genBody :: CGenerator [CCompoundBlockItem]
        genBody = do
            msgDataVars <- getMsgDataVariables [a | a@(_, dts, _) <- actions, not (sameTy dts TUnit)]
            loop <- genLoop
            return $ [
                    -- ClassIdentifier * self = (ClassIdentifier *)&arg;
                    pre_cr $ var "self" (ptr classId) @:= cast (ptr classId) ("arg" @: ptr void),
                    -- int32_t status = 0;
                    pre_cr $ var "status" int32_t @:= dec 0 @: int32_t,
                    -- __termina_event_t ev;
                    pre_cr $ var "event" __termina_event_t,
                    -- __status_int32_t result;
                    pre_cr $ var "result" __status_int32_t,
                    -- result.__variant = Success;
                    no_cr $ ("result" @: __status_int32_t) @. variant @: enumFieldType @= "Success" @: enumFieldType
                ] ++ msgDataVars ++
                [
                    pre_cr $ _for Nothing Nothing Nothing loop
                ]
genTaskClassCode _ = throwError $ InternalError "Not a task class definition"

genClassDefinition :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genClassDefinition clsdef@(TypeDefinition cls@(Class clsKind identifier _members _provides _) _) = do
    (_fields, functions) <- classifyClassMembers cls
    cFunctionDefs <- traverse genClassFunctionDefinition functions
    case clsKind of
        TaskClass -> do
            cTaskClassCode <- genTaskClassCode clsdef
            return $ cFunctionDefs ++ [cTaskClassCode]
        _ -> return cFunctionDefs

    where

        genClassFunctionDefinition :: ClassMember SemanticAnn -> CGenerator CFileItem
        genClassFunctionDefinition (ClassViewer viewer parameters rts (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier viewer
            cRetType <- maybe (return void) (genType noqual) rts
            cParamDecls <- mapM genParameterDeclaration parameters
            cEventParam <- getEventParam
            cSelfParam <- genConstSelfParam clsdef
            cBody <- foldM (\acc x -> do
                cStmt <- genBlocks x
                return $ acc ++ cStmt) [] stmts
            return $ CFunctionDef Nothing (CFunction cRetType clsFuncName (cEventParam : cSelfParam : cParamDecls)
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True)
        genClassFunctionDefinition (ClassProcedure ak procedure parameters (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier procedure
            cEventParam <- getEventParam
            cThisParam <- case ak of
                Immutable -> genConstThisParam
                _ -> genThisParam
            cParamDecls <- mapM genParameterDeclaration parameters
            selfCastStmt <- case ak of
                Immutable -> genConstSelfCastStmt ann identifier
                _ -> genSelfCastStmt ann identifier
            resourceLockStmt <- genResourceLockStmt ann identifier
            cBody <- foldM (\acc x -> do
                case x of
                    ReturnBlock _ ann' -> do
                        cStmt <- genBlocks x
                        resourceUnlockStmt <- genResourceUnlockStmt ann' identifier
                        return $ acc ++ (resourceUnlockStmt : cStmt)
                    _ -> do
                        cStmt <- genBlocks x
                        return $ acc ++ cStmt) [selfCastStmt, resourceLockStmt] stmts
            return $ CFunctionDef Nothing (CFunction (CTVoid noqual) clsFuncName (cEventParam : cThisParam : cParamDecls)
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True)

        genClassFunctionDefinition (ClassMethod ak method rts (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier method
            cRetType <- maybe (return void) (genType noqual) rts
            cEventParam <- getEventParam
            cSelfParam <- case ak of
                Immutable -> genConstSelfParam clsdef
                _ -> genSelfParam clsdef
            cBody <- foldM (\acc x -> do
                cStmt <- genBlocks x
                return $ acc ++ cStmt) [] stmts
            return $ CFunctionDef Nothing (CFunction cRetType clsFuncName [cEventParam, cSelfParam]
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True)
        genClassFunctionDefinition (ClassAction ak action param rts (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier action
            cRetType <- genType noqual rts
            cEventParam <- getEventParam
            cThisParam <-  case ak of
                Immutable -> genConstThisParam
                _ -> genThisParam
            selfCastStmt <- case ak of
                Immutable -> genConstSelfCastStmt ann identifier
                _ -> genSelfCastStmt ann identifier
            cBody <- foldM (\acc x -> do
                cStmt <- genBlocks x
                return $ acc ++ cStmt) [selfCastStmt] stmts
            case param of
                Just p -> do
                    cParam <- genParameterDeclaration p
                    return $ CFunctionDef Nothing (CFunction cRetType clsFuncName [cEventParam, cThisParam, cParam]
                        (CSCompound cBody (buildCompoundAnn ann False True)))
                        (buildDeclarationAnn ann True)
                Nothing ->
                    return $ CFunctionDef Nothing (CFunction cRetType clsFuncName [cEventParam, cThisParam]
                        (CSCompound cBody (buildCompoundAnn ann False True)))
                        (buildDeclarationAnn ann True)
        genClassFunctionDefinition member = throwError $ InternalError $ "invalid class member. Not a function: " ++ show member
genClassDefinition e = throwError $ InternalError $ "AST element is not a class: " ++ show e
