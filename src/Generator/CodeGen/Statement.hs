module Generator.CodeGen.Statement where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Semantic.Types
import Control.Monad.Except
import Generator.CodeGen.Common
import Generator.CodeGen.Expression
import Utils.Annotations
import Data.Map (fromList, union)
import Control.Monad.Reader
import Semantic.Monad (getMatchCaseTypes)

genEnumInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -> Integer ->
    -- | Enum
    CObject ->
    -- |  The initialization expression
    Expression SemanticAnn ->
    CGenerator [CStatement]
genEnumInitialization before level cObj expr = do
    case expr of
        -- \| This function can only be called with a field values assignments expressions
        (EnumVariantInitializer ts this_variant params ann) -> do
            let exprCAnn = buildGenericAnn ann
            cParams <- zipWithM (\e index -> do
                cType <- getExprType e >>= genType noqual
                let cFieldObj = CField cObj this_variant cType
                genFieldInitialization False level cFieldObj (namefy (show (index :: Integer))) e) params [0..]
            let variantsFieldsObj = CField cObj variant enumFieldType
            let variantExpr = CExprValOf (CVar (ts <::> this_variant) enumFieldType) enumFieldType exprCAnn
            if before then
                return $ pre_cr (variantsFieldsObj @= variantExpr |>> location ann) |>> location ann : concat cParams
            else
                return $ no_cr (variantsFieldsObj @= variantExpr |>> location ann) |>> location ann : concat cParams
        _ -> error "Incorrect expression"

genOptionInitialization ::
    Bool
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CGenerator [CStatement]
genOptionInitialization before level cObj expr =
    case expr of
        (OptionVariantInitializer (Some e) ann) -> do
            let cSomeVariantFieldObj = cObj @. optionSomeVariant @: enumFieldType
            fieldInitalization <- genFieldInitialization False level cSomeVariantFieldObj optionSomeField e
            let variantsFieldsObj = cObj @. variant @: enumFieldType
            let someVariantExpr = optionSomeVariant @: enumFieldType |>> location ann
            if before then
                return $ pre_cr (variantsFieldsObj @= someVariantExpr |>> location ann) |>> location ann : fieldInitalization
            else
                return $ no_cr (variantsFieldsObj @= someVariantExpr |>> location ann) |>> location ann : fieldInitalization
        (OptionVariantInitializer None ann) -> do
            let variantsFieldsObj = cObj @. variant @: enumFieldType 
            let noneVariantExpr = optionNoneVariant @: enumFieldType |>> location ann
            if before then
                return [pre_cr (variantsFieldsObj @= noneVariantExpr |>> location ann) |>> location ann]
            else
                return [no_cr (variantsFieldsObj @= noneVariantExpr |>> location ann) |>> location ann]
        _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr

genArrayInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -- | Current array nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CGenerator [CStatement]
genArrayInitialization before level cObj expr = do
    case expr of
        (ArrayInitializer expr' size ann) -> do
            cSize <- genArraySizeExpr size ann
            let iterator = namefy $ "i" ++ show level
                cIteratorExpr = iterator @: size_t |>> location ann
                initDecl = var iterator size_t @:= dec 0 @: size_t
                condExpr = cIteratorExpr @< cSize |>> location ann
                incrExpr = iterator @: size_t @= (cIteratorExpr @+ dec 1 @: size_t) @: size_t |>> location ann
                cObjType = getCObjType cObj
            cObjArrayItemType <- getCArrayItemType cObjType
            arrayInit <- genArrayInitialization False (level + 1) (cObj @$$ cIteratorExpr @: cObjArrayItemType) expr'
            if before then 
                return [pre_cr $ _for_let initDecl condExpr incrExpr (block (CBlockStmt <$> arrayInit))]
            else 
                return [no_cr $ _for_let initDecl condExpr incrExpr (block (CBlockStmt <$> arrayInit))]
        (ArrayExprListInitializer exprs _ann) ->
            genArrayItemsInitialization before level 0 exprs
        (StructInitializer {}) -> genStructInitialization False level cObj expr
        (OptionVariantInitializer {}) -> genOptionInitialization False level cObj expr
        (EnumVariantInitializer {}) -> genEnumInitialization False level cObj expr
        _ -> do
            cExpr <- genExpression expr
            exprType <- getExprType expr
            let ann = getAnnotation expr
            genArrayInitializationFromExpression level cObj cExpr exprType ann
    where

        genArrayItemsInitialization :: Bool -> Integer -> Integer -> [Expression SemanticAnn] -> CGenerator [CStatement]
        genArrayItemsInitialization _before _level _idx [] = return []
        genArrayItemsInitialization before' level' idx (x:xs) = do
            rest <- genArrayItemsInitialization False level' (idx + 1) xs
            let cObjType = getCObjType cObj
            cObjArrayItemType <- getCArrayItemType cObjType
            current <- genArrayInitialization before' level' (cObj @$$ (CInteger idx CDecRepr @: size_t) @: cObjArrayItemType) x
            return $ current ++ rest

        genArrayInitializationFromExpression :: Integer ->
            CObject ->
            CExpression ->
            TerminaType ->
            SemanticAnn ->
            CGenerator [CStatement]
        genArrayInitializationFromExpression lvl lhsCObj rhsCExpr ts ann = do
            case ts of
                -- | If the initializer is an array, we must iterate
                (TArray ts' (K s)) -> do
                    let iterator = namefy $ "i" ++ show lvl
                        cIteratorExpr = iterator @: size_t |>> location ann
                        exprCAnn = buildGenericAnn ann
                        initExpr = var iterator size_t @:= dec 0 @: size_t
                        cSize = genInteger s @: size_t
                        condExpr = cIteratorExpr @< cSize |>> location ann
                        incrExpr = iterator @: size_t @= (cIteratorExpr @+ dec 1 @: size_t) @: size_t |>> location ann
                    cTs' <- genType noqual ts'
                    rhsCObject <- unboxObject rhsCExpr
                    arrayInit <- genArrayInitializationFromExpression (lvl + 1) (CIndexOf lhsCObj cIteratorExpr cTs') (CExprValOf (CIndexOf rhsCObject cIteratorExpr cTs') cTs' exprCAnn) ts' ann
                    if before && lvl == 0then
                        return [pre_cr $ _for_let initExpr condExpr incrExpr (block (CBlockStmt <$> arrayInit))]
                    else
                        return [no_cr $ _for_let initExpr condExpr incrExpr (block (CBlockStmt <$> arrayInit))]
                _ ->  
                    if before && lvl == 0 then
                        return [pre_cr (lhsCObj @= rhsCExpr) |>> location ann]
                    else
                        return [no_cr (lhsCObj @= rhsCExpr) |>> location ann]


genFieldInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -- | Current array nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Identifier
    -> Expression SemanticAnn
    -> CGenerator [CStatement]
genFieldInitialization before level cObj field expr = do
    cExprType <- getExprType expr >>= genType noqual
    let cFieldObj = cObj @. field @: cExprType
    case expr of
        StructInitializer {} ->
            genStructInitialization before level cFieldObj expr
        OptionVariantInitializer {} ->
            genOptionInitialization before level cFieldObj expr
        ArrayInitializer {} ->
            genArrayInitialization before level cFieldObj expr
        ArrayExprListInitializer {} ->
            genArrayInitialization before level cFieldObj expr
        EnumVariantInitializer {} ->
            genEnumInitialization before level cFieldObj expr
        _ -> do
            exprType <- getExprType expr
            let ann = getAnnotation expr
            case exprType of
                TArray _ _ -> genArrayInitialization before level cFieldObj expr
                _ -> do
                    cExpr <- genExpression expr
                    if before then
                        return [pre_cr (cFieldObj @= cExpr) |>> location ann]
                    else
                        return [no_cr (cFieldObj @= cExpr) |>> location ann]

genStructInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -- | Current array nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CGenerator [CStatement]
genStructInitialization before level cObj expr = do
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (StructInitializer vas ann) -> genFieldAssignments before vas

        where

            genProcedureAssignment :: Identifier -> TerminaType -> ProcedureSeman -> CGenerator CStatement
            genProcedureAssignment field (TGlobal ResourceClass resource) (ProcedureSeman procid ptys) = do
                let cPortFieldType = struct resource
                let portFieldObj = CField cObj field cPortFieldType
                clsFunctionName <- genClassFunctionName resource procid
                clsFunctionType <- genFunctionType TUnit ptys
                let clsFunctionExpr = clsFunctionName @: clsFunctionType
                if before then
                    return $ pre_cr (portFieldObj @= clsFunctionExpr) |>> location ann
                else
                    return $ no_cr (portFieldObj @= clsFunctionExpr) |>> location ann
            genProcedureAssignment f i p = error $ "Invalid procedure assignment: " ++ show (f, i, p)
                -- throwError $ InternalError "Unsupported procedure assignment"

            genFieldAssignments :: Bool -> [FieldAssignment SemanticAnn] -> CGenerator [CStatement]
            genFieldAssignments _ [] = return []
            genFieldAssignments before' (FieldValueAssignment field expr' _: xs) = do
                fieldInit <- genFieldInitialization before' level cObj field expr'
                rest <- genFieldAssignments False xs
                return $ fieldInit ++ rest
            genFieldAssignments before' (FieldAddressAssignment field addr (LocatedElement (ETy (SimpleType ts)) _):xs) = do
                let cAddress = genInteger addr
                cTs <- genType noqual ts
                let fieldObj = CField cObj field cTs
                rest <- genFieldAssignments False xs
                if before' then
                    return $ pre_cr (fieldObj @= cast cTs (cAddress @: size_t)) : rest
                else
                    return $ no_cr (fieldObj @= cast cTs (cAddress @: size_t)) : rest
            genFieldAssignments before' (FieldPortConnection OutboundPortConnection field channel (LocatedElement (ETy (PortConnection (OutPConnTy _))) _) : xs) = do
                let cMsgQueue = CTTypeDef msgQueue noqual
                let cPtrMsgQueue = CTPointer cMsgQueue noqual
                let channelExpr = addrOf (channel @: cMsgQueue)
                rest <- genFieldAssignments False xs
                if before' then
                    return $ pre_cr (cObj @. field @: cPtrMsgQueue @= channelExpr) : rest
                else
                    return $ no_cr (cObj @. field @: cPtrMsgQueue @= channelExpr) : rest
            genFieldAssignments before' (FieldPortConnection AccessPortConnection field resource (LocatedElement (ETy (PortConnection (APConnTy rts procedures))) _) : xs) = do
                let cResourceType = CTTypeDef resource noqual
                    resourceExpr = addrOf (resource @: cResourceType)
                rest <- genFieldAssignments False xs
                cProcedures <- mapM (genProcedureAssignment field rts) procedures
                if before' then
                    return $ pre_cr ((cObj @. thatField @: void_ptr) @= resourceExpr) : (cProcedures ++ rest)
                else
                    return $ no_cr ((cObj @. thatField @: void_ptr) @= resourceExpr) : (cProcedures ++ rest)
            genFieldAssignments before' (FieldPortConnection AccessPortConnection field res (LocatedElement (ETy (PortConnection (APAtomicArrayConnTy ts size))) _) : xs) = do
                rest <- genFieldAssignments False xs
                cTs <- genType noqual (TArray ts size)
                let portFieldObj = cObj @. field @: cTs
                if before' then
                    return $ pre_cr (portFieldObj @= (res @: cTs)) : rest
                else
                    return $ no_cr (portFieldObj @= (res @: cTs)) : rest
            genFieldAssignments before' (FieldPortConnection AccessPortConnection field res (LocatedElement (ETy (PortConnection (APAtomicConnTy ts))) _) : xs) = do
                rest <- genFieldAssignments False xs
                cTs <- genType noqual ts
                let portFieldObj = cObj @. field @: ptr cTs
                    cResourceType = CTTypeDef res noqual
                if before' then
                    return $ pre_cr (portFieldObj @= addrOf (res @: cResourceType)) : rest
                else
                    return $ no_cr (portFieldObj @= addrOf (res @: cResourceType)) : rest
            genFieldAssignments before' (_ : xs) = genFieldAssignments before' xs

    _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr

genBlocks :: BasicBlock SemanticAnn -> CGenerator [CCompoundBlockItem]
genBlocks (RegularBlock stmts) = concat <$> mapM genStatement stmts
genBlocks (ProcedureCall obj ident args ann) = do
    (cFuncType, _) <- case ann of
        LocatedElement (ETy (AppType pts ts)) _ -> do
            cFuncType <- genFunctionType ts pts
            cRetType <- genType noqual ts
            return (cFuncType, cRetType)
        _ -> throwError $ InternalError $ "Invalid function annotation: " ++ show ann
    -- Generate the C code for the object
    cObj <- genObject obj
    -- Generate the C code for the parameters
    cArgs <- mapM genExpression args
    -- | Obtain the type of the object
    typeObj <- getObjType obj
    case typeObj of
        TAccessPort (TInterface iface) ->
            let thatFieldCType = ptr (struct iface) in
            return 
                [pre_cr ((cObj @. ident @: cFuncType) @@
                      ((cObj @. thatField @: thatFieldCType) : cArgs) |>> location ann) |>> location ann]
        _ -> throwError $ InternalError $ "Invalid object type: " ++ show typeObj
genBlocks (AllocBox obj arg ann) = do
    -- Generate the C code for the object
    cObj <- genObject obj
    let cObjExpr = cObj @: getCObjType cObj |>> location ann
    -- Generate the C code for the parameters
    cArg <- genExpression arg
    methodCallExpr <- genPoolMethodCallExpr "alloc" cObjExpr cArg (buildGenericAnn ann)
    return [pre_cr methodCallExpr |>> location ann]
genBlocks (FreeBox obj arg ann) = do
    -- Generate the C code for the object
    cObj <- genObject obj
    let cObjExpr = cObj @: getCObjType cObj |>> location ann
    -- Generate the C code for the parameters
    cArg <- genExpression arg
    methodCallExpr <- genPoolMethodCallExpr "free" cObjExpr cArg (buildGenericAnn ann)
    return [pre_cr methodCallExpr |>> location ann]
genBlocks (AtomicLoad obj arg ann) = do
    -- Generate the C code for the object
    cObj <- genObject obj
    let cObjExpr = cObj @: getCObjType cObj |>> location ann
    -- Generate the C code for the parameters
    cArg <- genExpression arg
    case arg of
        ReferenceExpression _ refObj _ -> do
            cRefObj <- genObject refObj
            mCall <- genAtomicMethodCall "load" cObjExpr [cArg] (buildGenericAnn ann)
            return [pre_cr (cRefObj @= mCall) |>> location ann]
        _ -> throwError $ InternalError $ "invalid params for atomic load: " ++ show arg
genBlocks (AtomicStore obj arg ann) = do
    -- Generate the C code for the object
    cObj <- genObject obj
    let cObjExpr = cObj @: getCObjType cObj |>> location ann
    -- Generate the C code for the parameters
    cArg <- genExpression arg
    methodCallExpr <- genAtomicMethodCall "store" cObjExpr [cArg] (buildGenericAnn ann)
    return [pre_cr methodCallExpr |>> location ann]
genBlocks (AtomicArrayLoad obj idx arg ann) = do
    let cAnn = buildGenericAnn ann
    -- Generate the C code for the object
    cObj <- genObject obj
    cIdx <- genExpression idx
    cArg <- genExpression arg
    case arg of
        ReferenceExpression _ refObj _ -> do
            cIndexedObj <- genIndexOf cObj cIdx
            cRefIndexedObj <- genAddrOf cIndexedObj noqual cAnn
            cRefObj <- genObject refObj
            mCall <- genAtomicMethodCall "load" cRefIndexedObj [cArg] cAnn
            return [pre_cr (cRefObj @= mCall) |>> location ann]
        _ -> throwError $ InternalError $ "invalid params for atomic load_index: " ++ show arg
genBlocks (AtomicArrayStore obj idx arg ann) = do
    let cAnn = buildGenericAnn ann
    -- Generate the C code for the object
    cObj <- genObject obj
    cIdx <- genExpression idx
    cArg <- genExpression arg
    cIndexedObj <- genIndexOf cObj cIdx
    cRefIndexedObj <- genAddrOf cIndexedObj noqual cAnn
    mCall <- genAtomicMethodCall "store" cRefIndexedObj [cArg] cAnn
    return [pre_cr mCall |>> location ann]
genBlocks (SendMessage obj arg ann) = do
    let cAnn = buildGenericAnn ann
    cObj <- genObject obj
    cArg <- genExpression arg
    -- Generate the C code for the object
    case arg of
        -- | If the argument is an access object, we can use it directly
        (AccessObject {}) -> do
            mCall <- genMsgQueueSendCall cObj cArg cAnn
            return [pre_cr mCall |>> location ann]
        -- | If it is not an object, must store it in a temporary variable
        _ -> do
            let cArgType = getCExprType cArg
                cTmp = "msg" @: cArgType
            mCall <- genMsgQueueSendCall cObj cTmp cAnn
            return $ CBlockStmt <$> [
                    block [
                        no_cr $ var "msg" cArgType @:= cArg |>> location ann,
                        no_cr mCall
                    ]
                ]
genBlocks (IfElseBlock expr ifBlk elifsBlks elseBlk ann) = do
    cExpr <- genExpression expr
    cIfBlk <- concat <$> mapM genBlocks (blockBody ifBlk)
    cElseBlk <-
        (case elseBlk of
            Nothing -> return Nothing
            Just elseBlk' ->
                mapM genBlocks (blockBody elseBlk') >>= (return . Just) . flip CSCompound (buildCompoundAnn ann False True) . concat)
    cAlts <- genAlternatives cElseBlk elifsBlks

    return $ CBlockStmt <$>
        [CSIfThenElse cExpr (CSCompound cIfBlk (buildCompoundAnn ann False True)) cAlts (buildStatementAnn ann True)]

    where
        genAlternatives :: Maybe CStatement -> [ElseIf SemanticAnn] -> CGenerator (Maybe CStatement)
        genAlternatives prev [] = return prev
        genAlternatives prev (ElseIf expr' blk ann' : xs) = do
            prev' <- genAlternatives prev xs
            cExpr' <- genExpression expr'
            cBlk <- concat <$> mapM genBlocks (blockBody blk)
            return $ Just (CSIfThenElse cExpr' (CSCompound cBlk (buildCompoundAnn ann' False True)) prev' (buildStatementAnn ann' False))
genBlocks (ForLoopBlock iterator iteratorTS initValue endValue breakCond body ann) = do
    let exprCAnn = buildGenericAnn ann
    initExpr <- genExpression initValue
    endExpr <- genExpression endValue
    cIteratorType <- genType noqual iteratorTS
    let cIteratorObj = CVar iterator cIteratorType
        cIteratorExpr = CExprValOf cIteratorObj cIteratorType exprCAnn
    condExpr <-
        case breakCond of
            Nothing -> return $ CExprBinaryOp COpLt cIteratorExpr endExpr (CTBool noqual) exprCAnn
            Just break' -> do
                    cBreak <- genExpression break'
                    return $ CExprSeqAnd
                        (CExprBinaryOp COpLt cIteratorExpr endExpr (CTBool noqual) exprCAnn) cBreak (CTBool noqual) exprCAnn
    cBody <- concat <$> mapM genBlocks (blockBody body)
    return $ CBlockStmt <$>
        [CSFor
            -- | Initialization expression
            (Right $ CDecl (CTypeSpec cIteratorType) (Just iterator) (Just initExpr))
            -- | Condition expression 
            (Just condExpr)
            -- | Increment expression
            (Just $ CExprAssign cIteratorObj
                (CExprBinaryOp COpAdd
                    cIteratorExpr
                    (CExprConstant (CIntConst (CInteger 1 CDecRepr)) cIteratorType exprCAnn)
                    cIteratorType exprCAnn)
                cIteratorType exprCAnn)
            -- | Body
            (CSCompound cBody (buildCompoundAnn ann False True))
            (buildStatementAnn ann True)]
genBlocks match@(MatchBlock expr matchCases ann) = do
    let exprCAnn = buildGenericAnn ann
    exprType <- getExprType expr
    (casePrefix, structName, genParamsStructName) <-
        case exprType of
            -- | If the expression is an enumeration, the case identifier must 
            -- be prefixed with the enumeration identifier.
            (TEnum enumId) -> do
                enumStructName <- genEnumStructName enumId
                return ((<::>) enumId, enumStructName, genEnumParameterStructName enumId)
            (TOption ts) -> do
                sname <- genOptionStructName ts
                pname <- genOptionParameterStructName ts
                return (id, sname, const (return pname))
            _ -> throwError $ InternalError $ "Unsupported match expression type: " ++ show expr
    case expr of
        (AccessObject {}) -> do
            cObj <- genExpression expr >>= unboxObject
            case matchCases of
                -- | If there is only one case, we do not need to check the variant
                [m@(MatchCase identifier _ _ _)] -> do
                    paramsStructName <- genParamsStructName identifier
                    cTs <- CTypeSpec <$> genType noqual (TStruct paramsStructName)
                    genMatchCase cTs cObj m
                -- | The first one must add a preceding blank line
                m@(MatchCase identifier _ _ ann') : xs -> do
                    paramsStructName <- genParamsStructName identifier
                    cTs <- CTypeSpec <$> genType noqual (TStruct paramsStructName)
                    rest <- genMatchCases cObj casePrefix genParamsStructName genMatchCase xs
                    cBlk <- flip CSCompound (buildCompoundAnn ann' False True) <$> genMatchCase cTs cObj m
                    -- | TODO: The size of the enum field has been hardcoded, it should be
                    -- platform dependent
                    let cEnumVariantsFieldExpr = CExprValOf (CField cObj variant (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) exprCAnn
                        cCasePrefixIdentExpr = CExprValOf (CVar (casePrefix identifier) (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) exprCAnn
                    return [CBlockStmt $ CSIfThenElse
                        (CExprBinaryOp COpEq cEnumVariantsFieldExpr cCasePrefixIdentExpr (CTBool noqual) (buildGenericAnn ann'))
                        cBlk rest (buildStatementAnn ann' True)]
                _ -> throwError $ InternalError $ "Match statement without cases: " ++ show match
        _ -> do
            cExpr <- genExpression expr
            cType <- genType noqual (TStruct structName)
            let decl = CDecl (CTypeSpec cType) (Just (namefy "match")) (Just cExpr)
                cObj' = CVar (namefy "match") cType
            case matchCases of
                [m@(MatchCase identifier _ _ _)] -> do
                    paramsStructName <- genParamsStructName identifier
                    paramsStructTypeSpec <- CTypeSpec <$> genType noqual (TStruct paramsStructName)
                    cBlk <- genAnonymousMatchCase paramsStructTypeSpec cObj' m
                    return [CBlockStmt $ CSCompound (CBlockDecl decl (buildDeclarationAnn ann True) : cBlk) (buildCompoundAnn ann True True)]
                m@(MatchCase identifier _ _ ann') : xs -> do
                    paramsStructName <- genParamsStructName identifier
                    paramsStructTypeSpec <- CTypeSpec <$> genType noqual (TStruct paramsStructName)
                    rest <- genMatchCases cObj' casePrefix genParamsStructName genAnonymousMatchCase xs
                    cBlk <- flip CSCompound (buildCompoundAnn ann' False True) <$> genAnonymousMatchCase paramsStructTypeSpec cObj' m
                    let cEnumVariantsFieldExpr = CExprValOf (CField cObj' variant (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) (buildGenericAnn ann')
                        cCasePrefixIdentExpr = CExprValOf (CVar (casePrefix identifier) (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) (buildGenericAnn ann')
                    return [CBlockStmt $ CSCompound (CBlockDecl decl  (buildDeclarationAnn ann True) : [CBlockStmt $ CSIfThenElse
                        (CExprBinaryOp COpEq cEnumVariantsFieldExpr cCasePrefixIdentExpr (CTBool noqual) (buildGenericAnn ann'))
                        cBlk rest (buildStatementAnn ann' True)]) (buildCompoundAnn ann True True)]
                _ -> throwError $ InternalError $ "Match statement without cases: " ++ show match

    where

        genMatchCases ::
            -- | The expression to match
            CObject
            -- | A function to prefix the case identifier
            -> (Identifier -> Identifier)
            -- | A function to get the parameter struct name 
            -> (Identifier -> CGenerator Identifier)
            -- | A function to generate a match case (inside the monad)
            -> (CTerminaType
                -> CObject
                -> MatchCase SemanticAnn
                -> CGenerator [CCompoundBlockItem])
            -- | The list of remaining match cases
            -> [MatchCase SemanticAnn]
            -> CGenerator (Maybe CStatement)
        -- | This should never happen
        genMatchCases _ _ _ _ [] = return Nothing
        -- | The last one does not need to check the variant
        genMatchCases cObj _ genParamsStructName genCase [m@(MatchCase identifier _ _ ann')] = do
            paramsStructName <- genParamsStructName identifier
            cTs <- CTypeSpec <$> genType noqual (TStruct paramsStructName)
            cBlk <- genCase cTs cObj m
            return $ Just (CSCompound cBlk (buildCompoundAnn ann' False True))
        genMatchCases cObj casePrefix genParamsStructName genCase (m@(MatchCase identifier _ _ ann') : xs) = do
            let cAnn = buildGenericAnn ann'
                cEnumVariantsFieldExpr = CExprValOf (CField cObj variant (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) cAnn
                cCasePrefixIdentExpr = CExprValOf (CVar (casePrefix identifier) (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) cAnn
                cExpr' = CExprBinaryOp COpEq cEnumVariantsFieldExpr cCasePrefixIdentExpr (CTBool noqual) cAnn
            paramsStructName <- genParamsStructName identifier
            cTs <- CTypeSpec <$> genType noqual (TStruct paramsStructName)
            cBlk <- flip CSCompound (buildCompoundAnn ann' False True) <$> genCase cTs cObj m
            rest <- genMatchCases cObj casePrefix genParamsStructName genCase xs
            return $ Just (CSIfThenElse cExpr' cBlk rest (buildStatementAnn ann' False))

        genAnonymousMatchCase ::
            CTerminaType
            -> CObject
            -> MatchCase SemanticAnn -> CGenerator [CCompoundBlockItem]
        genAnonymousMatchCase _ _ (MatchCase _ [] blk' _) = do
            concat <$> mapM genBlocks (blockBody blk')
        genAnonymousMatchCase (CTypeSpec cParamsStructType) cObj (MatchCase this_variant params blk' ann') = do
            let cObj' = CField cObj this_variant cParamsStructType
            cParamTypes <- case getMatchCaseTypes (element ann') of
                Just ts -> traverse (genType noqual) ts
                Nothing -> throwError $ InternalError "Match case without types"
            let newKeyVals = fromList $ zipWith3
                    (\sym index cParamType -> (sym, CField cObj' (namefy (show (index :: Integer))) cParamType)) params [0..] cParamTypes
            local (\e -> e{substitutions = newKeyVals `union` substitutions e}) $ concat <$> mapM genBlocks (blockBody blk')
        genAnonymousMatchCase _ _ _ = throwError $ InternalError "Invalid match case"

        genMatchCase ::
            CTerminaType
            -> CObject
            -> MatchCase SemanticAnn
            -> CGenerator [CCompoundBlockItem]
        genMatchCase _ _ (MatchCase _ [] blk' _) = do
            concat <$> mapM genBlocks (blockBody blk')
        genMatchCase cTs@(CTypeSpec cParamsStructType) cExpr (MatchCase this_variant params blk' ann') = do
            let cAnn = buildGenericAnn ann'
                cObj' = CVar (namefy this_variant) cParamsStructType
            cParamTypes <- case getMatchCaseTypes (element ann') of
                Just ts -> traverse (genType noqual) ts
                Nothing -> throwError $ InternalError "Match case without types"
            let newKeyVals = fromList $ zipWith3
                    (\sym index cParamType -> (sym, CField cObj' (namefy (show (index :: Integer))) cParamType)) params [0..] cParamTypes
                decl = CDecl cTs (Just (namefy this_variant))
                    (Just $ CExprValOf (CField cExpr this_variant cParamsStructType) cParamsStructType cAnn)
            cBlk <- local (\e -> e{substitutions = newKeyVals `union` substitutions e}) $ concat <$> mapM genBlocks (blockBody blk')
            return $ CBlockDecl decl (buildDeclarationAnn ann True) : cBlk
        genMatchCase _ _ _ = throwError $ InternalError "Invalid match case"
genBlocks (ReturnBlock (Just expr) ann) = do
    let cAnn = buildStatementAnn ann True
    cExpr <- genExpression expr
    return [CBlockStmt $ CSReturn (Just cExpr) cAnn]
genBlocks (ReturnBlock Nothing ann) = do
    let cAnn = buildStatementAnn ann True
    return [CBlockStmt $ CSReturn Nothing cAnn]
genBlocks (ContinueBlock expr ann) = do
    let cAnn = buildStatementAnn ann True
    cExpr <- genExpression expr
    return [CBlockStmt $ CSReturn (Just cExpr) cAnn]

genStatement :: Statement SemanticAnn -> CGenerator [CCompoundBlockItem]
genStatement (AssignmentStmt obj expr  _) = do
    typeObj <- getObjType obj
    cType <- genType noqual typeObj
    cObj <- genObject obj
    case typeObj of
        TArray _ _ -> fmap CBlockStmt <$> genArrayInitialization True 0 cObj expr
        (TFixedLocation _) -> do
            let ann = getAnnotation obj
            return $ CBlockStmt <$> [CSDo (CExprAddrOf cObj cType (buildGenericAnn ann)) (buildStatementAnn ann True)]
        _ -> case expr of
            (StructInitializer {}) -> fmap CBlockStmt <$> genStructInitialization True 0 cObj expr
            (OptionVariantInitializer {}) -> fmap CBlockStmt <$> genOptionInitialization True 0 cObj expr
            (EnumVariantInitializer {}) -> fmap CBlockStmt <$> genEnumInitialization True 0 cObj expr
            _ -> do
                cExpr <- genExpression expr
                let ann = getAnnotation expr
                return $ CBlockStmt <$>
                    [CSDo (CExprAssign cObj cExpr cType (buildGenericAnn ann)) (buildStatementAnn ann True)]
genStatement (Declaration identifier _ ts expr ann) = do
  cType <- genType noqual ts
  let cObj = CVar identifier cType
  case ts of
    TArray _ _ -> do
        let declStmt = buildDeclarationAnn ann True
        arrayInitialization <- fmap CBlockStmt <$> genArrayInitialization False 0 cObj expr
        return $
            CBlockDecl (CDecl (CTypeSpec cType) (Just identifier) Nothing) declStmt
            : arrayInitialization
    _ -> case expr of
        (StructInitializer {}) -> do
            let declStmt = buildDeclarationAnn ann True
            structInitialization <- fmap CBlockStmt <$> genStructInitialization False 0 cObj expr
            return $
                CBlockDecl (CDecl (CTypeSpec cType) (Just identifier) Nothing) declStmt
                : structInitialization
        (OptionVariantInitializer {}) -> do
            let declStmt = buildDeclarationAnn ann True
            optionInitialization <- fmap CBlockStmt <$> genOptionInitialization False 0 cObj expr
            return $
                CBlockDecl (CDecl (CTypeSpec cType) (Just identifier) Nothing) declStmt
                : optionInitialization
        (EnumVariantInitializer {}) -> do
            let declStmt = buildDeclarationAnn ann True
            enumInitialization <- fmap CBlockStmt <$> genEnumInitialization False 0 cObj expr
            return $
                CBlockDecl (CDecl (CTypeSpec cType) (Just identifier) Nothing) declStmt
                : enumInitialization
        _ -> do
            let declStmt = buildDeclarationAnn ann True
            cExpr <- genExpression expr
            return $ flip CBlockDecl declStmt <$> [CDecl (CTypeSpec cType) (Just identifier) (Just cExpr)]
genStatement (SingleExpStmt expr ann) = do
    cExpr <- genExpression expr
    return [CBlockStmt $ CSDo cExpr (buildStatementAnn ann True)]
