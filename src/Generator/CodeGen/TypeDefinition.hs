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
import Generator.LanguageC.Embedded
import Control.Monad (zipWithM, foldM)
import Generator.CodeGen.Application.Utils
import Generator.CodeGen.Types
import Generator.CodeGen.Expression

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
        genInterfaceProcedureField (InterfaceProcedure procedure params _modifiers _) = do
            cParamTypes <- mapM (genType noqual . paramType) params
            let cThisParamType = _const . ptr $ void
                cFuncPointerType = CTPointer (CTFunction (CTVoid noqual) (cThisParamType : cParamTypes)) noqual
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
        genInterfaceProcedureField (InterfaceProcedure procedure params _modifiers _) = do
            cParamTypes <- mapM (genType noqual . paramType) params
            let cFuncPointerType = CTPointer (CTFunction (CTVoid noqual) cParamTypes) noqual
            return $ CDecl (CTypeSpec cFuncPointerType) (Just procedure) Nothing

genFieldDeclaration (FieldDefinition _ (TAccessPort (TInterface _ _)) ann) = error $ "Invalid access port annotation" ++ show ann
genFieldDeclaration (FieldDefinition identifier ts _) = do
    cTs <- genType noqual ts
    return [field identifier cTs]

genOptionSomeParameterStruct :: SemanticAnn -> TerminaType SemanticAnn ->  CGenerator CFileItem
genOptionSomeParameterStruct ann ts = do
    cTs <- genType noqual ts
    let fld = field optionSomeField cTs
    identifier <- genOptionParameterStructName ts
    return $ pre_cr $ struct identifier [fld] [] |>> getLocation ann

genOptionStruct :: SemanticAnn -> TerminaType SemanticAnn -> CGenerator [CFileItem]
genOptionStruct ann (TOption ts) = do
    paramsStructName <- genOptionParameterStructName ts
    enumStructName <- genEnumStructName "option"
    paramsStructType <- genType noqual (TStruct paramsStructName)
    enumStructType <- genType noqual (TStruct enumStructName)
    let some = field optionSomeVariant paramsStructType
        this_variant = field variant enumStructType
    identifier <- genOptionStructName ts
    paramStruct <- genOptionSomeParameterStruct ann ts
    return [
            paramStruct,
            pre_cr $ struct identifier [some, this_variant] [] |>> getLocation ann
        ]
genOptionStruct ts _ = throwError $ InternalError $ "Type not an option: " ++ show ts

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
    let cExpr = cast (ptr selfCType) (thisParam @: ptr void) |>> getLocation ann
    return $ pre_cr $ var selfParam (ptr selfCType) @:= cExpr |>> getLocation ann

-- | TypeDef pretty printer.
genTypeDefinitionDecl :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genTypeDefinitionDecl (TypeDefinition (Struct identifier fls modifiers) ann) = do
    let cAnn = buildDeclarationAnn ann True
    structModifiers <- mapM genAttribute (filterStructModifiers modifiers)
    cFields <- concat <$> mapM genFieldDeclaration fls
    opts <- asks optionTypes
    optsDeclExt <- concat <$> maybe (return [])
        (mapM (genOptionStruct ann) . S.toList) (M.lookup (TStruct identifier) opts)
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
        genInterfaceProcedureField (InterfaceProcedure procedure params _modifiers _) = do
            cParamTypes <- mapM (genType noqual . paramType) params
            let cThisParamType = _const . ptr $ void
                cFuncPointerType = CTPointer (CTFunction (CTVoid noqual) (cThisParamType : cParamTypes)) noqual
            return $ CDecl (CTypeSpec cFuncPointerType) (Just procedure) Nothing

genTypeDefinitionDecl clsdef@(TypeDefinition cls@(Class clsKind identifier _members _provides modifiers) ann) = do
    (fields, functions) <- classifyClassMembers cls
    structModifiers <- mapM genAttribute (filterStructModifiers modifiers)
    fields' <- case clsKind of
        TaskClass -> return fields
        _ -> return $ filter (\case {
            ClassField (FieldDefinition _ (TSinkPort {}) _) -> False;
            ClassField (FieldDefinition _ (TInPort {}) _) -> False;
            _ -> True}) fields
    cFields <- concat <$> mapM genClassField fields'
    cFunctions <- concat <$> mapM genClassFunctionDeclaration functions
    let structFields = case clsKind of
            TaskClass ->
                let cIDField = field taskMsgQueueIDField (typeDef terminaID) in
                cIDField : cFields
            ResourceClass ->
                let cIDField = field mutexIDField (typeDef terminaID) in
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

        genClassFunctionDeclaration :: ClassMember SemanticAnn -> CGenerator [CFileItem]
        genClassFunctionDeclaration (ClassViewer viewer params rts _ _) = do
            retType <- maybe (return void) (genType noqual) rts
            cParamDecls <- mapM genParameterDeclaration params
            cSelfParam <- genConstSelfParam clsdef
            clsFuncName <- genClassFunctionName identifier viewer
            return [CExtDecl (CEDFunction retType clsFuncName (cSelfParam : cParamDecls)) (buildDeclarationAnn ann True)]
        genClassFunctionDeclaration (ClassProcedure procedure params _ _) = do
            cParamDecls <- mapM genParameterDeclaration params
            cThisParam <- genThisParam
            clsFuncName <- genClassFunctionName identifier procedure
            mutexLockFunction <- procedureMutexLock clsFuncName
            taskLockFunction <- procedureTaskLock clsFuncName
            eventLockFunction <- procedureEventLock clsFuncName
            return [
                    CExtDecl (CEDFunction void clsFuncName (cThisParam : cParamDecls)) (buildDeclarationAnn ann True),
                    CExtDecl (CEDFunction void mutexLockFunction (cThisParam : cParamDecls)) (buildDeclarationAnn ann False),
                    CExtDecl (CEDFunction void taskLockFunction (cThisParam : cParamDecls)) (buildDeclarationAnn ann False),
                    CExtDecl (CEDFunction void eventLockFunction (cThisParam : cParamDecls)) (buildDeclarationAnn ann False)
                ]
        genClassFunctionDeclaration (ClassMethod method rts _ _) = do
            retType <- maybe (return (CTVoid noqual)) (genType noqual) rts
            clsFuncName <- genClassFunctionName identifier method
            cSelfParam <- genSelfParam clsdef
            return [CExtDecl (CEDFunction retType clsFuncName [cSelfParam]) (buildDeclarationAnn ann True)]
        genClassFunctionDeclaration (ClassAction action param rts _ _) = do
            retType <- genType noqual rts
            cParamDecl <- genParameterDeclaration param
            cThisParam <- genThisParam
            clsFuncName <- genClassFunctionName identifier action
            return [CExtDecl (CEDFunction retType clsFuncName [cThisParam, cParamDecl]) (buildDeclarationAnn ann True)]
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
        genCase (port, dts, action) = do
            this_variant <- genVariantForPort classId port
            classFunctionName <- genClassFunctionName classId action
            classStructType <- genType noqual (TStruct classId)
            cDataType <- genType noqual dts
            let classFunctionType = CTFunction _Result
                    [_const . ptr $ classStructType, cDataType]
            return
                [
                    -- case variant:
                    pre_cr $ _case (this_variant @: enumFieldType) $
                    -- __termina_msg_queue__recv(self->port, &action_msg_data, &status);
                    indent . pre_cr $ __termina_msg_queue__recv @@
                            [
                                ("self" @: ptr classStructType) @. port @: __termina_id_t,
                                cast void_ptr (addrOf ((action <::> "msg_data") @: cDataType)),
                                addrOf ("status" @: _Status)
                            ],
                    -- if (status.__variant != Status__Success)
                    indent . pre_cr $ _if (
                            "status" @: _Status @. variant @: enumFieldType @!= "Status__Success" @: enumFieldType)
                        $ block [
                            -- __termina_exec__shutdown();
                            no_cr $ __termina_exec__shutdown @@ []
                        ],
                    -- result = classFunctionName(self, action_msg_data);
                    indent . pre_cr $ "result" @: CTTypeDef "Result" noqual @=
                        (classFunctionName @: classFunctionType) @@ ["self" @: classStructType, (action <::> "msg_data") @: cDataType],
                    -- if (result.__variant != Result__Ok)
                    indent . pre_cr $ _if (
                            (("result" @: typeDef "Result") @. variant) @: enumFieldType @!= "Result__Ok" @: enumFieldType)
                        $ block [
                            -- rtems_shutdown_executive(1);
                            no_cr $ __termina_exec__shutdown @@ []
                        ],
                    indent . pre_cr $ _break
                ]

        genLoop :: CGenerator CStatement
        genLoop = do
            classStructType <- genType noqual (TStruct classId)
            cases <- concat <$> mapM genCase actions
            return $ trail_cr . block $ [
                    -- | status = __termina_msg_queue__recv(
                    -- |                self->__task.msgq_id, &next_msg);
                    pre_cr $ __termina_msg_queue__recv @@
                            [
                                ("self" @: ptr classStructType) @. taskMsgQueueIDField @: __termina_id_t,
                                addrOf ("next_msg" @: uint32_t),
                                addrOf ("status" @: _Status)
                            ],
                    -- if (RTEMS_SUCCESSFUL != status)
                    pre_cr $ _if
                            ("status" @: _Status @. variant @: enumFieldType @!= "Status__Success" @: enumFieldType)
                        $ block [
                            -- break;
                            no_cr _break
                        ],
                    pre_cr $ _switch ("next_msg" @: uint32_t) $
                        trail_cr . block $ (cases ++
                            [
                                -- default:
                                pre_cr $ _default $
                                    -- rtems_shutdown_executive(1);
                                    indent . pre_cr $ __termina_exec__shutdown @@ [],
                                    -- break;
                                    indent . pre_cr $ _break
                            ])
                ]

        genBody :: CGenerator [CCompoundBlockItem]
        genBody = do
            msgDataVars <- getMsgDataVariables actions
            loop <- genLoop
            return $ [
                    -- ClassIdentifier * self = (ClassIdentifier *)&arg;
                    pre_cr $ var "self" (ptr classId) @:= cast (ptr classId) ("arg" @: ptr void),
                    -- rtems_status_code status = RTEMS_SUCCESSFUL;
                    pre_cr $ var "status" _Status,
                    no_cr $ "status" @: _Status @. variant @: enumFieldType @= "Status__Success" @: enumFieldType,
                    -- uint32_t next_msg = 0U;
                    pre_cr $ var "next_msg" uint32_t @:= dec 0 @: uint32_t,
                    -- Result result;
                    pre_cr $ var "result" (typeDef "Result"),
                    -- result.__variant = Result__Ok;
                    no_cr $ ("result" @: typeDef "Result") @. variant @: enumFieldType @= "Result__Ok" @: enumFieldType
                ] ++ msgDataVars ++
                [
                    pre_cr $ _for Nothing Nothing Nothing loop,
                    pre_cr $ __termina_exec__shutdown @@ []
                ]
genTaskClassCode _ = throwError $ InternalError "Not a task class definition"

genClassDefinition :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genClassDefinition clsdef@(TypeDefinition cls@(Class clsKind identifier _members _provides _) _) = do
    (_fields, functions) <- classifyClassMembers cls
    cFunctionDefs <- concat <$> mapM genClassFunctionDefinition functions
    case clsKind of
        TaskClass -> do
            cTaskClassCode <- genTaskClassCode clsdef
            return $ cFunctionDefs ++ [cTaskClassCode]
        _ -> return cFunctionDefs

    where

        genClassFunctionDefinition :: ClassMember SemanticAnn -> CGenerator [CFileItem]
        genClassFunctionDefinition (ClassViewer viewer parameters rts (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier viewer
            cRetType <- maybe (return void) (genType noqual) rts
            cParamDecls <- mapM genParameterDeclaration parameters
            cSelfParam <- genConstSelfParam clsdef
            cBody <- foldM (\acc x -> do
                cStmt <- genBlocks x
                return $ acc ++ cStmt) [] stmts
            return [CFunctionDef Nothing (CFunction cRetType clsFuncName (cSelfParam : cParamDecls)
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True)]
        genClassFunctionDefinition (ClassProcedure procedure parameters (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier procedure
            cThisParam <- genThisParam
            cParamDecls <- mapM genParameterDeclaration parameters
            selfCastStmt <- genSelfCastStmt ann identifier
            cBody <- foldM (\acc x -> do
                cStmt <- genBlocks x
                return $ acc ++ cStmt) [selfCastStmt] stmts
            mutexLockProcedure <- genProcedureMutexLock
            taskLockProcedure <- genProcedureTaskLock
            eventLockProcedure <- genProcedureEventLock
            return [CFunctionDef Nothing (CFunction (CTVoid noqual) clsFuncName (cThisParam : cParamDecls)
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True), mutexLockProcedure, taskLockProcedure, eventLockProcedure]

            where

                genProcedureMutexLock :: CGenerator CFileItem
                genProcedureMutexLock = do
                    selfCType <- genType noqual (TStruct identifier)
                    selfCastStmt <- genSelfCastStmt ann identifier
                    cThisParam <- genThisParam
                    clsFuncName <- genClassFunctionName identifier procedure
                    cParamDecls <- mapM genParameterDeclaration parameters
                    cParamTypes <- mapM (genType noqual . paramType) parameters
                    cParamExpressions <- mapM (\(Parameter pid pty) -> do
                        cParamType <- genType noqual pty
                        return $ pid @: cParamType) parameters
                    let procedureFunction = clsFuncName @: CTFunction void
                            (ptr void : cParamTypes)
                    let cBody = [
                                selfCastStmt,
                                pre_cr $ var "status" _Status,
                                no_cr $ "status" @: _Status @. variant @: enumFieldType @= "Status__Success" @: enumFieldType,
                                pre_cr $ __termina_mutex__lock @@
                                    [
                                        (selfParam @: ptr selfCType) @. mutexIDField @: __termina_id_t,
                                        addrOf ("status" @: _Status)
                                    ],
                                no_cr $ procedureFunction @@ ("self" @: ptr void : cParamExpressions),
                                no_cr $ __termina_mutex__unlock @@
                                    [
                                        (selfParam @: ptr selfCType) @. mutexIDField @: __termina_id_t,
                                        addrOf ("status" @: _Status)
                                    ]
                            ]
                    return $ CFunctionDef Nothing (CFunction (CTVoid noqual) (clsFuncName <::> "mutex_lock") (cThisParam : cParamDecls)
                        (CSCompound cBody (buildCompoundAnn ann False True)))
                        (buildDeclarationAnn ann True)
                
                genProcedureTaskLock :: CGenerator CFileItem
                genProcedureTaskLock = do
                    cThisParam <- genThisParam
                    clsFuncName <- genClassFunctionName identifier procedure
                    cParamDecls <- mapM genParameterDeclaration parameters
                    cParamTypes <- mapM (genType noqual . paramType) parameters
                    cParamExpressions <- mapM (\(Parameter pid pty) -> do
                        cParamType <- genType noqual pty
                        return $ pid @: cParamType) parameters
                    let procedureFunction = clsFuncName @: CTFunction void
                            (ptr void : cParamTypes)
                    let cBody = [
                                pre_cr $ var "lock" __termina_task_lock_t,
                                pre_cr $ "lock" @: __termina_task_lock_t @=  __termina_task__lock @@ [],
                                no_cr $ procedureFunction @@ (thisParam @: ptr void : cParamExpressions),
                                no_cr $ __termina_task__unlock @@
                                    [
                                        "lock" @: __termina_task_lock_t
                                    ]
                            ]
                    return $ CFunctionDef Nothing (CFunction (CTVoid noqual) (clsFuncName <::> "task_lock") (cThisParam : cParamDecls)
                        (CSCompound cBody (buildCompoundAnn ann False True)))
                        (buildDeclarationAnn ann True)
                
                genProcedureEventLock :: CGenerator CFileItem
                genProcedureEventLock = do
                    cThisParam <- genThisParam
                    clsFuncName <- genClassFunctionName identifier procedure
                    cParamDecls <- mapM genParameterDeclaration parameters
                    cParamTypes <- mapM (genType noqual . paramType) parameters
                    cParamExpressions <- mapM (\(Parameter pid pty) -> do
                        cParamType <- genType noqual pty
                        return $ pid @: cParamType) parameters
                    let procedureFunction = clsFuncName @: CTFunction void
                            (ptr void : cParamTypes)
                    let cBody = [
                                pre_cr $ var "lock" __termina_event_lock_t,
                                pre_cr $ "lock" @: __termina_event_lock_t @=  __termina_event__lock @@ [],
                                no_cr $ procedureFunction @@ (thisParam @: ptr void : cParamExpressions),
                                no_cr $ __termina_event__unlock @@
                                    [
                                        "lock" @: __termina_event_lock_t
                                    ]
                            ]
                    return $ CFunctionDef Nothing (CFunction (CTVoid noqual) (clsFuncName <::> "event_lock") (cThisParam : cParamDecls)
                        (CSCompound cBody (buildCompoundAnn ann False True)))
                        (buildDeclarationAnn ann True)

        genClassFunctionDefinition (ClassMethod method rts (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier method
            cRetType <- maybe (return void) (genType noqual) rts
            cSelfParam <- genSelfParam clsdef
            cBody <- foldM (\acc x -> do
                cStmt <- genBlocks x
                return $ acc ++ cStmt) [] stmts
            return [CFunctionDef Nothing (CFunction cRetType clsFuncName [cSelfParam]
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True)]
        genClassFunctionDefinition (ClassAction action param rts (Block stmts _) ann) = do
            clsFuncName <- genClassFunctionName identifier action
            cRetType <- genType noqual rts
            cThisParam <- genThisParam
            selfCastStmt <- genSelfCastStmt ann identifier
            cParam <- genParameterDeclaration param
            cBody <- foldM (\acc x -> do
                cStmt <- genBlocks x
                return $ acc ++ cStmt) [selfCastStmt] stmts
            return [CFunctionDef Nothing (CFunction cRetType clsFuncName [cThisParam, cParam]
                (CSCompound cBody (buildCompoundAnn ann False True)))
                (buildDeclarationAnn ann True)]
        genClassFunctionDefinition member = throwError $ InternalError $ "invalid class member. Not a function: " ++ show member
genClassDefinition e = throwError $ InternalError $ "AST element is not a class: " ++ show e
