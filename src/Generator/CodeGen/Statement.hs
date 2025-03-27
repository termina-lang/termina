module Generator.CodeGen.Statement where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Semantic.Types
import Control.Monad
import Control.Monad.Except
import Generator.CodeGen.Common
import Generator.CodeGen.Expression
import Utils.Annotations
import Generator.CodeGen.Types
import Control.Monad.Reader
import qualified Data.Map as M

genEnumInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -> Integer ->
    -- | Enum
    CObject ->
    -- |  The initialization expression
    Expression SemanticAnn ->
    CGenerator [CCompoundBlockItem]
genEnumInitialization before level cObj expr = do
    case expr of
        -- \| This function can only be called with a field values assignments expressions
        (EnumVariantInitializer ts this_variant params ann) -> do
            let exprCAnn = buildGenericAnn ann
            cParams <- zipWithM (\e index -> do
                cType <- getExprType e >>= genType noqual
                let cFieldObj = cObj @. this_variant @: cType
                genFieldInitialization False level cFieldObj (namefy (show (index :: Integer))) e) params [0..]
            let variantsFieldsObj = cObj @. variant @: enumFieldType
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
    -> CGenerator [CCompoundBlockItem]
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
    -> CGenerator [CCompoundBlockItem]
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
                return [pre_cr $ _for_let initDecl condExpr incrExpr (block arrayInit)]
            else 
                return [no_cr $ _for_let initDecl condExpr incrExpr (block arrayInit)]
        (ArrayExprListInitializer exprs _ann) ->
            genArrayItemsInitialization before level 0 exprs
        (StructInitializer {}) -> genStructInitialization before level cObj expr
        (OptionVariantInitializer {}) -> genOptionInitialization before level cObj expr
        (EnumVariantInitializer {}) -> genEnumInitialization before level cObj expr
        _ -> do
            cExpr <- genExpression expr
            exprType <- getExprType expr
            let ann = getAnnotation expr
            genArrayInitializationFromExpression level cObj cExpr exprType ann
    where

        genArrayItemsInitialization :: Bool -> Integer -> Integer -> [Expression SemanticAnn] -> CGenerator [CCompoundBlockItem]
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
            CGenerator [CCompoundBlockItem]
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
                    if before && lvl == 0 then
                        return [pre_cr $ _for_let initExpr condExpr incrExpr (block arrayInit)]
                    else
                        return [no_cr $ _for_let initExpr condExpr incrExpr (block arrayInit)]
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
    -> CGenerator [CCompoundBlockItem]
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
    -> CGenerator [CCompoundBlockItem]
genStructInitialization before level cObj expr = do
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (StructInitializer vas ann) -> genFieldAssignments before vas

        where

            genProcedureAssignment :: Identifier -> TerminaType -> ProcedureSeman -> CGenerator CCompoundBlockItem
            genProcedureAssignment field (TGlobal ResourceClass resource) (ProcedureSeman procid ptys) = do
                cPortFieldType <- genType noqual (TStruct resource)
                let portFieldObj = cObj @. field @: cPortFieldType
                clsFunctionName <- genClassFunctionName resource procid
                clsFunctionType <- genFunctionType TUnit ptys
                let clsFunctionExpr = clsFunctionName @: clsFunctionType
                if before then
                    return $ pre_cr (portFieldObj @. procid @: clsFunctionType @= clsFunctionExpr) |>> location ann
                else
                    return $ no_cr (portFieldObj @. procid @: clsFunctionType @= clsFunctionExpr) |>> location ann
            genProcedureAssignment f i p = error $ "Invalid procedure assignment: " ++ show (f, i, p)
                -- throwError $ InternalError "Unsupported procedure assignment"

            genFieldAssignments :: Bool -> [FieldAssignment SemanticAnn] -> CGenerator [CCompoundBlockItem]
            genFieldAssignments _ [] = return []
            genFieldAssignments before' (FieldValueAssignment fld expr' _: xs) = do
                fieldInit <- genFieldInitialization before' level cObj fld expr'
                rest <- genFieldAssignments False xs
                return $ fieldInit ++ rest
            genFieldAssignments before' (FieldAddressAssignment fld addr (LocatedElement (ETy (SimpleType ts)) _):xs) = do
                let cAddress = genInteger addr
                cTs <- genType noqual ts
                let fieldObj = cObj @. fld @: cTs
                rest <- genFieldAssignments False xs
                if before' then
                    return $ pre_cr (fieldObj @= cast cTs (cAddress @: size_t)) : rest
                else
                    return $ no_cr (fieldObj @= cast cTs (cAddress @: size_t)) : rest
            genFieldAssignments before' (FieldPortConnection OutboundPortConnection fld channel (LocatedElement (ETy (PortConnection (OutPConnTy _))) _) : xs) = do
                let cMsgQueue = typeDef msgQueue 
                let cPtrMsgQueue = ptr cMsgQueue
                let channelExpr = addrOf (channel @: cMsgQueue)
                rest <- genFieldAssignments False xs
                if before' then
                    return $ pre_cr (cObj @. fld @: cPtrMsgQueue @= channelExpr) : rest
                else
                    return $ no_cr (cObj @. fld @: cPtrMsgQueue @= channelExpr) : rest
            genFieldAssignments before' (FieldPortConnection AccessPortConnection field resource (LocatedElement (ETy (PortConnection (APConnTy rts procedures))) _) : xs) = do
                let cResourceType = typeDef resource
                    resourceExpr = addrOf (resource @: cResourceType)
                rest <- genFieldAssignments False xs
                cProcedures <- mapM (genProcedureAssignment field rts) procedures
                if before' then
                    return $ pre_cr (((cObj @. field @: cResourceType) @. thatField @: void_ptr) @= resourceExpr) : (cProcedures ++ rest)
                else
                    return $ no_cr (((cObj @. field @: cResourceType) @. thatField @: void_ptr) @= resourceExpr) : (cProcedures ++ rest)
            genFieldAssignments before' (FieldPortConnection AccessPortConnection fld resource (LocatedElement (ETy (PortConnection (APPoolConnTy {}))) _) : xs) = do
                let resourceExpr = addrOf (resource @: __termina_pool_t)
                rest <- genFieldAssignments False xs
                if before' then
                    return $ pre_cr ((cObj @. fld @: ptr __termina_pool_t) @= resourceExpr) : rest
                else
                    return $ no_cr ((cObj @. fld @: ptr __termina_pool_t) @= resourceExpr) : rest
            genFieldAssignments before' (FieldPortConnection AccessPortConnection fld res (LocatedElement (ETy (PortConnection (APAtomicArrayConnTy ts size))) _) : xs) = do
                rest <- genFieldAssignments False xs
                cTs <- genType noqual (TArray ts size)
                let portFieldObj = cObj @. fld @: cTs
                if before' then
                    return $ pre_cr (portFieldObj @= (res @: cTs)) : rest
                else
                    return $ no_cr (portFieldObj @= (res @: cTs)) : rest
            genFieldAssignments before' (FieldPortConnection AccessPortConnection fld res (LocatedElement (ETy (PortConnection (APAtomicConnTy ts))) _) : xs) = do
                rest <- genFieldAssignments False xs
                cTs <- genType noqual ts
                let portFieldObj = cObj @. fld @: ptr cTs
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
        TAccessPort (TInterface RegularInterface iface) -> do
            structType <- genType noqual (TStruct iface)
            return 
                [pre_cr ((cObj @. ident @: cFuncType) @@
                      ((cObj @. thatField @: ptr structType) : cArgs) |>> location ann) |>> location ann]
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
            return [
                    no_cr $ block [
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
            Just elseBlk' -> do
                blks <- concat <$> mapM genBlocks (blockBody elseBlk')
                return . Just $ (trail_cr . block $ blks) |>> location ann)
    mAlts <- genAlternatives cElseBlk elifsBlks
    case mAlts of
        Nothing -> return [pre_cr $ _if cExpr (trail_cr . block $ cIfBlk) |>> location ann]
        Just cAlts -> return [pre_cr $ _if_else cExpr (trail_cr . block $ cIfBlk) cAlts |>> location ann]

    where

        genAlternatives :: Maybe CStatement -> [ElseIf SemanticAnn] -> CGenerator (Maybe CStatement)
        genAlternatives prev [] = return prev
        genAlternatives prev (ElseIf expr' blk ann' : xs) = do
            cExpr' <- genExpression expr'
            cBlk <- concat <$> mapM genBlocks (blockBody blk)
            mPrev <- genAlternatives prev xs
            case mPrev of
                Nothing -> return . Just $ _if cExpr' (trail_cr . block $ cBlk) |>> location ann'
                Just prev' -> return . Just $ _if_else cExpr' (trail_cr . block $ cBlk) prev' |>> location ann'

genBlocks (ForLoopBlock iterator iteratorTS initValue endValue breakCond body ann) = do
    initExpr <- genExpression initValue
    endExpr <- genExpression endValue
    cIteratorType <- genType noqual iteratorTS
    let cIteratorExpr = iterator @: cIteratorType |>> location ann
    condExpr <-
        case breakCond of
            Nothing -> return $ cIteratorExpr @< endExpr |>> location ann
            Just break' -> do
                    cBreak <- genExpression break'
                    return $ (cIteratorExpr @< endExpr |>> location ann) @&& cBreak |>> location ann

    cBody <- concat <$> mapM genBlocks (blockBody body)
    return 
        [pre_cr $ _for_let 
            (var iterator cIteratorType @:= initExpr)
            condExpr
            (iterator @: cIteratorType @= (cIteratorExpr @+ dec 1 @: cIteratorType) @: cIteratorType |>> location ann) 
            ((trail_cr . block $ cBody) |>> location ann)]
genBlocks match@(MatchBlock expr matchCases ann) = do
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
                    cTy <- genType noqual (TStruct paramsStructName)
                    genMatchCase cObj cTy m
                -- | The first one must add a preceding blank line
                m@(MatchCase identifier _ _ ann') : xs -> do
                    paramsStructName <- genParamsStructName identifier
                    cTy <- genType noqual (TStruct paramsStructName)
                    mRest <- genMatchCases cObj casePrefix genParamsStructName xs
                    cBlk <- flip CSCompound (buildCompoundAnn ann' False True) <$> genMatchCase cObj cTy m
                    -- | TODO: The size of the enum field has been hardcoded, it should be
                    -- platform dependent
                    let cEnumVariantsFieldExpr = cObj @. variant @: uint32_t |>> location ann
                        cCasePrefixIdentExpr = casePrefix identifier @: uint32_t |>> location ann
                    case mRest of 
                        Nothing ->
                            return [pre_cr $ _if (cEnumVariantsFieldExpr @== cCasePrefixIdentExpr |>> location ann') cBlk]
                        Just rest ->
                            return [pre_cr $ _if_else (cEnumVariantsFieldExpr @== cCasePrefixIdentExpr |>> location ann') cBlk rest]
                _ -> throwError $ InternalError $ "Match statement without cases: " ++ show match
        _ -> do
            cExpr <- genExpression expr
            cType <- genType noqual (TStruct structName)
            let decl = var (namefy "match") cType @:= cExpr |>> location ann
                cObj' = namefy "match" @: cType
            case matchCases of
                [m@(MatchCase identifier _ _ _)] -> do
                    paramsStructName <- genParamsStructName identifier
                    paramsStructType <- genType noqual (TStruct paramsStructName)
                    cBlk <- genMatchCase cObj' paramsStructType m
                    return [no_cr $ (trail_cr . block) (pre_cr decl : cBlk) |>> location ann]
                m@(MatchCase identifier _ _ ann') : xs -> do
                    paramsStructName <- genParamsStructName identifier
                    paramsStructType <- genType noqual (TStruct paramsStructName)
                    mRest <- genMatchCases cObj' casePrefix genParamsStructName xs
                    cBlk <- trail_cr . block <$> genMatchCase cObj' paramsStructType m
                    let cEnumVariantsFieldExpr = cObj' @. variant @: uint32_t |>> location ann'
                        cCasePrefixIdentExpr = casePrefix identifier @: uint32_t |>> location ann'
                    case mRest of 
                        Nothing -> return 
                            [pre_cr $ (trail_cr . block) (pre_cr decl : [pre_cr $ 
                                _if (cEnumVariantsFieldExpr @== cCasePrefixIdentExpr |>> location ann') cBlk |>> location ann'])]
                        Just rest -> return
                            [pre_cr $ (trail_cr . block) (pre_cr decl : [pre_cr $ 
                                _if_else (cEnumVariantsFieldExpr @== cCasePrefixIdentExpr |>> location ann') cBlk rest |>> location ann'])]
                _ -> throwError $ InternalError $ "Match statement without cases: " ++ show match

    where

        genMatchCases ::
            -- | The expression to match
            CObject
            -- | A function to prefix the case identifier
            -> (Identifier -> Identifier)
            -- | A function to get the parameter struct name 
            -> (Identifier -> CGenerator Identifier)
            -- | The list of remaining match cases
            -> [MatchCase SemanticAnn]
            -> CGenerator (Maybe CStatement)
        -- | This should never happen
        genMatchCases _ _ _ [] = return Nothing
        -- | The last one does not need to check the variant
        genMatchCases cObj _ genParamsStructName [m@(MatchCase identifier _ _ ann')] = do
            paramsStructName <- genParamsStructName identifier
            cTs <- genType noqual (TStruct paramsStructName)
            cBlk <- genMatchCase cObj cTs m
            return $ Just ((trail_cr . block) cBlk |>> location ann')
        genMatchCases cObj casePrefix genParamsStructName (m@(MatchCase identifier _ _ ann') : xs) = do
            let cEnumVariantsFieldExpr = cObj @. variant @: uint32_t |>> location ann'
                cCasePrefixIdentExpr = casePrefix identifier @: uint32_t |>> location ann'
                cExpr' = cEnumVariantsFieldExpr @== cCasePrefixIdentExpr |>> location ann'
            paramsStructName <- genParamsStructName identifier
            cTs <- genType noqual (TStruct paramsStructName)
            cBlk <- trail_cr . block  <$> genMatchCase cObj cTs m
            mRest <- genMatchCases cObj casePrefix genParamsStructName xs
            case mRest of
                Nothing ->
                    return . Just $ _if cExpr' (cBlk |>> location ann') |>> location ann'
                Just rest ->
                    return . Just $ _if_else cExpr' (cBlk |>> location ann') rest |>> location ann'

        genMatchCase ::
            CObject -> CType
            -> MatchCase SemanticAnn
            -> CGenerator [CCompoundBlockItem]
        genMatchCase cObj cParamsStructType c@(MatchCase _ _ blk' _) = do
            decls <- genMatchCaseParams cObj cParamsStructType c
            cBlk <- concat <$> mapM genBlocks (blockBody blk')
            return $ decls ++ cBlk

        genMatchCaseParams :: CObject -> CType -> MatchCase SemanticAnn -> CGenerator [CCompoundBlockItem]
        genMatchCaseParams cObj cParamsStructType (MatchCase this_variant params _ ann') = do
            cParamTypes <- case getMatchCaseTypes (element ann') of
                Just ts -> traverse (genType noqual) ts
                Nothing -> throwError $ InternalError "Match case without types"
            case params of
                [] -> return []
                [param] -> 
                    return [pre_cr (var param (head cParamTypes) @:= (cObj @. this_variant @: cParamsStructType) @. namefy (show (0 :: Integer)) @: head cParamTypes) |>> location ann]
                (p : xp) -> do
                    let rest = zipWith3
                            (\sym index cParamType -> 
                                no_cr $ var sym cParamType @:= (cObj @. this_variant @: cParamsStructType) @. namefy (show (index :: Integer)) @: cParamType) xp [1..] (tail cParamTypes)
                    return $ pre_cr (var p (head cParamTypes) @:= (cObj @. this_variant @: cParamsStructType) @. namefy (show (0 :: Integer)) @: head cParamTypes) |>> location ann : rest


genBlocks (ReturnBlock mExpr ann) = 
    case mExpr of
        Nothing ->
            return [pre_cr (_return Nothing) |>> location ann]
        Just expr -> do
            cExpr <- genExpression expr
            return [pre_cr (_return (Just cExpr)) |>> location ann]
genBlocks (ContinueBlock expr ann) = do
    cExpr <- genExpression expr
    return [pre_cr (_return (Just cExpr)) |>> location ann]
-- | TODO: Support system calls
genBlocks (SystemCall _obj ident args ann) = do
    -- Generate the C code for the parameters
    cArgs <- mapM genExpression args
    -- | Now we have to get the system calls map to obtain the name
    -- of the function to call
    syscalls <- asks syscallsMap
    case M.lookup ident syscalls of
        Nothing -> throwError $ InternalError $ "System call not found: " ++ show ident
        Just syscall -> 
            return [pre_cr (syscall @@ cArgs) |>> location ann]

genStatement :: Statement SemanticAnn -> CGenerator [CCompoundBlockItem]
genStatement (AssignmentStmt obj expr  _) = do
    typeObj <- getObjType obj
    cObj <- genObject obj
    case typeObj of
        TArray _ _ -> genArrayInitialization True 0 cObj expr
        (TFixedLocation _) -> do
            let ann = getAnnotation obj
            return [pre_cr (addrOf cObj |>> location ann) |>> location ann]
        _ -> case expr of
            (StructInitializer {}) -> genStructInitialization True 0 cObj expr
            (OptionVariantInitializer {}) -> genOptionInitialization True 0 cObj expr
            (EnumVariantInitializer {}) -> genEnumInitialization True 0 cObj expr
            _ -> do
                cExpr <- genExpression expr
                let ann = getAnnotation expr
                return [pre_cr (cObj @= cExpr |>> location ann) |>> location ann]
genStatement (Declaration identifier _ ts expr ann) = do
  cType <- genType noqual ts
  let cObj = CVar identifier cType
  case ts of
    TArray _ _ -> do
        arrayInitialization <- genArrayInitialization False 0 cObj expr
        return $
            pre_cr (var identifier cType) |>> location ann
            : arrayInitialization
    _ -> case expr of
        (StructInitializer {}) -> do
            structInitialization <- genStructInitialization False 0 cObj expr
            return $
                pre_cr (var identifier cType) |>> location ann
                : structInitialization
        (OptionVariantInitializer {}) -> do
            optionInitialization <- genOptionInitialization False 0 cObj expr
            return $
                pre_cr (var identifier cType) |>> location ann
                : optionInitialization
        (EnumVariantInitializer {}) -> do
            enumInitialization <- genEnumInitialization False 0 cObj expr
            return $
                pre_cr (var identifier cType) |>> location ann
                : enumInitialization
        _ -> do
            cExpr <- genExpression expr
            return [pre_cr $ var identifier cType @:= cExpr |>> location ann]
genStatement (SingleExpStmt expr ann) = do
    cExpr <- genExpression expr
    return [pre_cr cExpr |>> location ann]
