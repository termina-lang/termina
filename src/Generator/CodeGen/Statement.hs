module Generator.CodeGen.Statement where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Generator.LanguageC.Embedded
import Semantic.Types
import Control.Monad ( zipWithM )
import Control.Monad.Except
import Generator.CodeGen.Common
import Generator.CodeGen.Expression
import Utils.Annotations
import Generator.CodeGen.Types

genAtomicStore :: Location -> Bool -> CObject -> Expression SemanticAnn -> CGenerator CCompoundBlockItem
genAtomicStore loc before' cObj expr = do
    cInitializationExpr <- genExpression expr
    let cObjType = getCObjType cObj
        cObjExpr = addrOf cObj
        cFuncType = CTFunction (CTVoid noqual) [cObjType]
        methodCallExpr = atomicMethodName "store" @: cFuncType @@ [cObjExpr, cInitializationExpr]
    if before' then
        return $ pre_cr methodCallExpr |>> loc
    else
        return $ no_cr methodCallExpr |>> loc

genAtomicArrayInitialization ::
    Location
    -- | Prepend a line to the initialization expression 
    -> Bool
    -- | Current array nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CGenerator [CCompoundBlockItem]
genAtomicArrayInitialization loc before level cObj expr = do
    case expr of 
        (StructInitializer [FieldValueAssignment "values" assignmentExpr _] _ann) -> do
            case assignmentExpr of
                (ArrayInitializer expr' size _ann) -> do
                    cSize <- genExpression size
                    let iterator = namefy $ "i" ++ show level
                        cIteratorExpr = iterator @: size_t
                        initDecl = var iterator size_t @:= dec 0 @: size_t
                        condExpr = cIteratorExpr @< cSize
                        incrExpr = iterator @: size_t @= (cIteratorExpr @+ dec 1 @: size_t) @: size_t
                        cObjType = getCObjType cObj
                    cObjArrayItemType <- getCArrayItemType cObjType
                    arrayInit <- genAtomicStore loc False (cObj @$$ cIteratorExpr @: cObjArrayItemType) expr'
                    if before then
                        return [pre_cr (_for_let initDecl condExpr incrExpr (block [arrayInit])) |>> loc]
                    else
                        return [no_cr (_for_let initDecl condExpr incrExpr (block [arrayInit])) |>> loc]
                (ArrayExprListInitializer exprs _ann) -> 
                    genAtomicArrayItemsInitialization before level 0 exprs
                _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr
        _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr
    
    where 

        genAtomicArrayItemsInitialization :: Bool -> Integer -> Integer -> [Expression SemanticAnn] -> CGenerator [CCompoundBlockItem]
        genAtomicArrayItemsInitialization _before _level _idx [] = return []
        genAtomicArrayItemsInitialization before' level' idx (x:xs) = do
            rest <- genAtomicArrayItemsInitialization False level' (idx + 1) xs
            let cObjType = getCObjType cObj
            cObjArrayItemType <- getCArrayItemType cObjType
            current <- genAtomicStore loc before' (cObj @$$ (CInteger idx CDecRepr @: size_t) @: cObjArrayItemType) x
            return $ current : rest

genAtomicInitialization ::
    Location
    -- | Prepend a line to the initialization expression 
    -> Bool
    -- | Current array nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CGenerator [CCompoundBlockItem]
genAtomicInitialization loc before _level cObj expr = do
    case expr of 
        (StructInitializer [FieldValueAssignment "value" assignmentExpr _] _ann) -> do
            atomicInit <- genAtomicStore loc before cObj assignmentExpr
            return [atomicInit]
        _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr 

genEnumInitialization ::
    Location
    -- | Prepend a line to the initialization expression 
    -> Bool
    -> Integer ->
    -- | Enum
    CObject ->
    -- |  The initialization expression
    Expression SemanticAnn ->
    CGenerator [CCompoundBlockItem]
genEnumInitialization loc before level cObj expr = do
    case expr of
        -- \| This function can only be called with a field values assignments expressions
        (EnumVariantInitializer ts this_variant params ann) -> do
            let exprCAnn = buildGenericAnn ann
            cParams <- zipWithM (\e index -> do
                cType <- getExprType e >>= genType noqual
                let cFieldObj = cObj @. this_variant @: cType
                genFieldInitialization loc False level cFieldObj (namefy (show (index :: Integer))) e) params [0..]
            let variantsFieldsObj = cObj @. variant @: enumFieldType
            let variantExpr = CExprValOf (CVar (ts <::> this_variant) enumFieldType) enumFieldType exprCAnn
            if before then
                return $ pre_cr (variantsFieldsObj @= variantExpr |>> getLocation ann) |>> getLocation ann : concat cParams
            else
                return $ no_cr (variantsFieldsObj @= variantExpr |>> getLocation ann) |>> getLocation ann : concat cParams
        _ -> error "Incorrect expression"

genStringInitialization ::
    Location
    -- | Prepend a line to the initialization expression 
    -> Bool
    -> Integer
    -> CObject
    -> String
    -> CGenerator [CCompoundBlockItem]
genStringInitialization loc before level cObj value = do
    genStringItemsInitialization before level 0 value

    where

        cObjType = getCObjType cObj

        genStringItemsInitialization :: Bool -> Integer -> Integer -> String -> CGenerator [CCompoundBlockItem]
        genStringItemsInitialization before' level' idx [] = do
            case cObjType of
                CTArray (CTChar _) (CExprConstant (CIntConst (CInteger size _)) _ _) -> do
                    -- If the array has a fixed size, we need to fill the rest of the array with null characters
                    if size > (fromIntegral (length value) + 1) then do
                        -- We need to fill the rest of the array with null characters. We do this with a for loop.
                        let cSize = dec size @: size_t |>> loc
                            iterator = namefy $ "i" ++ show level'
                            cIteratorExpr = iterator @: size_t |>> loc
                            initDecl = var iterator size_t @:= dec idx @: size_t
                            condExpr = cIteratorExpr @< cSize |>> loc
                            incrExpr = iterator @: size_t @= (cIteratorExpr @+ dec 1 @: size_t) @: size_t |>> loc
                        cObjArrayItemType <- getCArrayItemType cObjType
                        let expr' = Constant (C '\0') (SemanticAnn (ETy (SimpleType TChar)) loc)
                        arrayInit <- genArrayInitialization loc False (level' + 1) (cObj @$$ cIteratorExpr @: cObjArrayItemType) expr'
                        if before' then
                            return [pre_cr (_for_let initDecl condExpr incrExpr (block arrayInit)) |>> loc]
                        else
                            return [no_cr (_for_let initDecl condExpr incrExpr (block arrayInit)) |>> loc]
                    else if size == (fromIntegral (length value) + 1) then do
                        -- If the array has a fixed size and it is exactly the
                        -- size of the string plus the null character, we just
                        -- need to add the null character at the end of the
                        -- string.
                        let index = fromIntegral (length value)
                            cIndex = dec index @: size_t |>> loc
                            cChar = '\0' @: char |>> loc
                            cAssignment = (cObj @$$ cIndex @: cObjType) @= cChar
                        if before' then
                            return [pre_cr cAssignment |>> loc]
                        else
                            return [no_cr cAssignment |>> loc]
                    else
                        return []
                CTArray (CTChar _) cArraySize -> do
                    -- | If we have an array whose size is not a literal constant, i.e., it depends on a constant parameter,
                    -- we need to fill the rest of the array with null characters. We do this with a for loop.
                    let iterator = namefy $ "i" ++ show level'
                        cIteratorExpr = iterator @: size_t |>> loc
                        initDecl = var iterator size_t @:= dec idx @: size_t
                        condExpr = cIteratorExpr @< cArraySize |>> loc
                        incrExpr = iterator @: size_t @= (cIteratorExpr @+ dec 1 @: size_t) @: size_t |>> loc
                    cObjArrayItemType <- getCArrayItemType cObjType
                    let expr' = Constant (C '\0') (SemanticAnn (ETy (SimpleType TChar)) loc)
                    arrayInit <- genArrayInitialization loc False (level' + 1) (cObj @$$ cIteratorExpr @: cObjArrayItemType) expr'
                    if before' then
                        return [pre_cr (_for_let initDecl condExpr incrExpr (block arrayInit)) |>> loc]
                    else
                        return [no_cr (_for_let initDecl condExpr incrExpr (block arrayInit)) |>> loc]
                _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show value
        genStringItemsInitialization before' level' idx (x:xs) = do
            rest <- genStringItemsInitialization False level' (idx + 1) xs
            let current = cObj @$$ (dec idx @: size_t) @: char @= x @: char
            if before' then
                return $ (pre_cr current |>> loc) : rest
            else
                return $ (no_cr current |>> loc) : rest

genMonadicTypeInitialization ::
    Location
    -> Bool
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CGenerator [CCompoundBlockItem]
genMonadicTypeInitialization loc before level cObj expr =
    case expr of
        (MonadicVariantInitializer (Some e) ann) -> do
            let cSomeVariantFieldObj = cObj @. optionSomeVariant @: enumFieldType
            fieldInitalization <- genFieldInitialization loc False level cSomeVariantFieldObj (namefy "0") e
            let variantsFieldsObj = cObj @. variant @: enumFieldType
            let someVariantExpr = optionSomeVariant @: enumFieldType |>> getLocation ann
            if before then
                return $ pre_cr (variantsFieldsObj @= someVariantExpr |>> getLocation ann) |>> getLocation ann : fieldInitalization
            else
                return $ no_cr (variantsFieldsObj @= someVariantExpr |>> getLocation ann) |>> getLocation ann : fieldInitalization
        (MonadicVariantInitializer None ann) -> do
            let variantsFieldsObj = cObj @. variant @: enumFieldType
            let noneVariantExpr = optionNoneVariant @: enumFieldType |>> getLocation ann
            if before then
                return [pre_cr (variantsFieldsObj @= noneVariantExpr |>> getLocation ann) |>> getLocation ann]
            else
                return [no_cr (variantsFieldsObj @= noneVariantExpr |>> getLocation ann) |>> getLocation ann]
        (MonadicVariantInitializer Success ann) -> do
            let variantsFieldsObj = cObj @. variant @: enumFieldType
            let successVariantExpr = statusSuccessVariant @: enumFieldType |>> getLocation ann
            if before then
                return [pre_cr (variantsFieldsObj @= successVariantExpr |>> getLocation ann) |>> getLocation ann]
            else
                return [no_cr (variantsFieldsObj @= successVariantExpr |>> getLocation ann) |>> getLocation ann]
        (MonadicVariantInitializer (Failure e) ann) -> do
            let cFailureVariantFieldObj = cObj @. statusFailureVariant @: enumFieldType
            fieldInitalization <- genFieldInitialization loc False level cFailureVariantFieldObj (namefy "0") e
            let variantsFieldsObj = cObj @. variant @: enumFieldType
            let failureVariantExpr = statusFailureVariant @: enumFieldType |>> getLocation ann
            if before then
                return $ pre_cr (variantsFieldsObj @= failureVariantExpr |>> getLocation ann) |>> getLocation ann : fieldInitalization
            else
                return $ no_cr (variantsFieldsObj @= failureVariantExpr |>> getLocation ann) |>> getLocation ann : fieldInitalization
        (MonadicVariantInitializer (Ok e) ann) -> do
            let cOkVariantFieldObj = cObj @. resultOkVariant @: enumFieldType
            fieldInitalization <- genFieldInitialization loc False level cOkVariantFieldObj (namefy "0") e
            let variantsFieldsObj = cObj @. variant @: enumFieldType
            let failureVariantExpr = resultOkVariant @: enumFieldType |>> getLocation ann
            if before then
                return $ pre_cr (variantsFieldsObj @= failureVariantExpr |>> getLocation ann) |>> getLocation ann : fieldInitalization
            else
                return $ no_cr (variantsFieldsObj @= failureVariantExpr |>> getLocation ann) |>> getLocation ann : fieldInitalization
        (MonadicVariantInitializer (Error e) ann) -> do
            let cErrorVariantFieldObj = cObj @. resultErrorVariant @: enumFieldType
            fieldInitalization <- genFieldInitialization loc False level cErrorVariantFieldObj (namefy "0") e
            let variantsFieldsObj = cObj @. variant @: enumFieldType
            let failureVariantExpr = resultErrorVariant @: enumFieldType |>> getLocation ann
            if before then
                return $ pre_cr (variantsFieldsObj @= failureVariantExpr |>> getLocation ann) |>> getLocation ann : fieldInitalization
            else
                return $ no_cr (variantsFieldsObj @= failureVariantExpr |>> getLocation ann) |>> getLocation ann : fieldInitalization
        _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr

genArrayInitialization ::
    Location
    -- | Prepend a line to the initialization expression 
    -> Bool
    -- | Current array nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CGenerator [CCompoundBlockItem]
genArrayInitialization loc before level cObj expr = do
    case expr of
        (ArrayInitializer expr' size ann) -> do
            cSize <- genExpression size
            let iterator = namefy $ "i" ++ show level
                cIteratorExpr = iterator @: size_t |>> getLocation ann
                initDecl = var iterator size_t @:= dec 0 @: size_t
                condExpr = cIteratorExpr @< cSize |>> getLocation ann
                incrExpr = iterator @: size_t @= (cIteratorExpr @+ dec 1 @: size_t) @: size_t |>> getLocation ann
                cObjType = getCObjType cObj
            cObjArrayItemType <- getCArrayItemType cObjType
            arrayInit <- genArrayInitialization loc False (level + 1) (cObj @$$ cIteratorExpr @: cObjArrayItemType) expr'
            if before then
                return [pre_cr (_for_let initDecl condExpr incrExpr (block arrayInit)) |>> loc]
            else
                return [no_cr (_for_let initDecl condExpr incrExpr (block arrayInit)) |>> loc]
        (ArrayExprListInitializer exprs _ann) ->
            genArrayItemsInitialization before level 0 exprs
        (StringInitializer value _) ->
            genStringInitialization loc before level cObj value
        (StructInitializer {}) -> genStructInitialization loc before level cObj expr
        (MonadicVariantInitializer {}) -> genMonadicTypeInitialization loc before level cObj expr
        (EnumVariantInitializer {}) -> genEnumInitialization loc before level cObj expr
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
            current <- genArrayInitialization loc before' level' (cObj @$$ (CInteger idx CDecRepr @: size_t) @: cObjArrayItemType) x
            return $ current ++ rest

        genArrayInitializationFromExpression :: Integer ->
            CObject ->
            CExpression ->
            TerminaType SemanticAnn ->
            SemanticAnn ->
            CGenerator [CCompoundBlockItem]
        genArrayInitializationFromExpression lvl lhsCObj rhsCExpr ts ann = do
            case ts of
                -- | If the initializer is an array, we must iterate
                (TArray ts' arraySize) -> do
                    cSize <- genExpression arraySize
                    let iterator = namefy $ "i" ++ show lvl
                        cIteratorExpr = iterator @: size_t |>> getLocation ann
                        exprCAnn = buildGenericAnn ann
                        initExpr = var iterator size_t @:= dec 0 @: size_t
                        condExpr = cIteratorExpr @< cSize |>> getLocation ann
                        incrExpr = iterator @: size_t @= (cIteratorExpr @+ dec 1 @: size_t) @: size_t |>> getLocation ann
                    cTs' <- genType noqual ts'
                    rhsCObject <- getCObject rhsCExpr
                    arrayInit <- genArrayInitializationFromExpression (lvl + 1) (CIndexOf lhsCObj cIteratorExpr cTs') (CExprValOf (CIndexOf rhsCObject cIteratorExpr cTs') cTs' exprCAnn) ts' ann
                    if before && lvl == 0 then
                        return [pre_cr $ _for_let initExpr condExpr incrExpr (block arrayInit)]
                    else
                        return [no_cr $ _for_let initExpr condExpr incrExpr (block arrayInit)]
                _ ->
                    if before && lvl == 0 then
                        return [pre_cr (lhsCObj @= rhsCExpr) |>> getLocation ann]
                    else
                        return [no_cr (lhsCObj @= rhsCExpr) |>> getLocation ann]


genFieldInitialization ::
    Location
    -- | Prepend a line to the initialization expression 
    -> Bool
    -- | Current array nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Identifier
    -> Expression SemanticAnn
    -> CGenerator [CCompoundBlockItem]
genFieldInitialization loc before level cObj fid expr = do
    cExprType <- getExprType expr >>= genType noqual
    let cFieldObj = cObj @. fid @: cExprType
    case expr of
        StructInitializer {} ->
            genStructInitialization loc before level cFieldObj expr
        MonadicVariantInitializer {} ->
            genMonadicTypeInitialization loc before level cFieldObj expr
        ArrayInitializer {} ->
            genArrayInitialization loc before level cFieldObj expr
        StringInitializer value _ ->
            genStringInitialization loc before level cFieldObj value
        ArrayExprListInitializer {} ->
            genArrayInitialization loc before level cFieldObj expr
        EnumVariantInitializer {} ->
            genEnumInitialization loc before level cFieldObj expr
        _ -> do
            exprType <- getExprType expr
            case exprType of
                TArray _ _ -> genArrayInitialization loc before level cFieldObj expr
                _ -> do
                    cExpr <- genExpression expr
                    if before then
                        return [pre_cr (cFieldObj @= cExpr) |>> loc]
                    else
                        return [no_cr (cFieldObj @= cExpr) |>> loc]

genProcedureType :: [Parameter SemanticAnn] -> CGenerator CType
genProcedureType tsParams = do
    tsParams' <- traverse (genType noqual . paramType) tsParams
    let cEventArgType = ptr __termina_event_t
        cThatArgType = _const . ptr $ void
    return $ CTFunction void (cEventArgType : cThatArgType : tsParams')

genStructInitialization ::
    Location
    -- | Prepend a line to the initialization expression 
    -> Bool
    -- | Current array nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CGenerator [CCompoundBlockItem]
genStructInitialization loc before level cObj expr = do
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (StructInitializer vas ann) -> genFieldAssignments before vas

        where

            genProcedureAssignment :: Identifier -> TerminaType SemanticAnn -> ProcedureSeman SemanticAnn -> CGenerator CCompoundBlockItem
            genProcedureAssignment fid (TGlobal ResourceClass resource) (ProcedureSeman procid ptys _modifiers) = do
                cPortFieldType <- genType noqual (TStruct resource)
                let portFieldObj = cObj @. fid @: cPortFieldType
                clsFunctionName <- genClassFunctionName resource procid
                clsFunctionType <- genProcedureType ptys
                let clsFunctionExpr = clsFunctionName @: clsFunctionType
                if before then
                    return $ pre_cr (portFieldObj @. procid @: clsFunctionType @= clsFunctionExpr) |>> getLocation ann
                else
                    return $ no_cr (portFieldObj @. procid @: clsFunctionType @= clsFunctionExpr) |>> getLocation ann
            genProcedureAssignment f i p = error $ "Invalid procedure assignment: " ++ show (f, i, p)
                -- throwError $ InternalError "Unsupported procedure assignment"

            genFieldAssignments :: Bool -> [FieldAssignment SemanticAnn] -> CGenerator [CCompoundBlockItem]
            genFieldAssignments _ [] = return []
            genFieldAssignments before' (FieldValueAssignment fld expr' _: xs) = do
                fieldInit <- genFieldInitialization loc before' level cObj fld expr'
                rest <- genFieldAssignments False xs
                return $ fieldInit ++ rest
            genFieldAssignments before' (FieldAddressAssignment fld addr (SemanticAnn (ETy (SimpleType ts)) _):xs) = do
                cAddress <- genExpression addr
                cTs <- genType noqual ts
                let fieldObj = cObj @. fld @: cTs
                rest <- genFieldAssignments False xs
                if before' then
                    return $ pre_cr (fieldObj @= cast cTs cAddress) : rest
                else
                    return $ no_cr (fieldObj @= cast cTs cAddress) : rest
            genFieldAssignments before' (FieldPortConnection OutboundPortConnection fld channel (SemanticAnn (STy (PortConnection (OutPConnTy _))) _) : xs) = do
                let cMsgQueue = typeDef msgQueue
                let cPtrMsgQueue = ptr cMsgQueue
                let channelExpr = addrOf (channel @: cMsgQueue)
                rest <- genFieldAssignments False xs
                if before' then
                    return $ pre_cr (cObj @. fld @: cPtrMsgQueue @= channelExpr) : rest
                else
                    return $ no_cr (cObj @. fld @: cPtrMsgQueue @= channelExpr) : rest
            genFieldAssignments before' (FieldPortConnection AccessPortConnection fid resource (SemanticAnn (STy (PortConnection (APConnTy (TInterface RegularInterface iface) rts procedures))) _) : xs) = do
                cResourceType <- genType noqual rts
                let cInterfaceType = typeDef iface
                    resourceExpr = addrOf (resource @: cResourceType)
                rest <- genFieldAssignments False xs
                cProcedures <- mapM (genProcedureAssignment fid rts) procedures
                if before' then
                    return $ pre_cr (((cObj @. fid @: cInterfaceType) @. thatField @: void_ptr) @= resourceExpr) : (cProcedures ++ rest)
                else
                    return $ no_cr (((cObj @. fid @: cInterfaceType) @. thatField @: void_ptr) @= resourceExpr) : (cProcedures ++ rest)
            genFieldAssignments _ (FieldPortConnection AccessPortConnection fid _ (SemanticAnn (STy (PortConnection (APConnTy (TInterface SystemInterface _) rts procedures))) _) : xs) = do
                rest <- genFieldAssignments False xs
                cProcedures <- mapM (genProcedureAssignment fid rts) procedures
                return (cProcedures ++ rest)
            genFieldAssignments before' (FieldPortConnection AccessPortConnection fld resource (SemanticAnn (STy (PortConnection (APPoolConnTy {}))) _) : xs) = do
                rest <- genFieldAssignments False xs
                let allocFunctionType = CTFunction void [void_ptr, ptr __option_box_t]
                    freeFunctionType = CTFunction void [void_ptr, __termina_box_t]
                let resourceExpr = addrOf (resource @: __termina_pool_t)
                    cPoolProcedures = [
                            no_cr $ cObj @. fld @: __termina_allocator_t @. "alloc" @: ptr allocFunctionType @= __termina_pool__alloc,
                            no_cr $ cObj @. fld @: __termina_allocator_t @. "free" @: ptr freeFunctionType @= __termina_pool__free
                        ]
                if before' then
                    return $ pre_cr ((cObj @. fld @: __termina_allocator_t) @. thatField @: void_ptr @= resourceExpr) : (cPoolProcedures ++ rest)
                else
                    return $ no_cr ((cObj @. fld @: __termina_allocator_t) @. thatField @: void_ptr @= resourceExpr) : (cPoolProcedures ++ rest)
            genFieldAssignments before' (FieldPortConnection AccessPortConnection fld res (SemanticAnn (STy (PortConnection (APAtomicArrayConnTy ts size _))) _) : xs) = do
                rest <- genFieldAssignments False xs
                cTs <- genType noqual (TArray ts size)
                let portFieldObj = cObj @. fld @: cTs
                if before' then
                    return $ pre_cr (portFieldObj @= (res @: cTs)) : rest
                else
                    return $ no_cr (portFieldObj @= (res @: cTs)) : rest
            genFieldAssignments before' (FieldPortConnection AccessPortConnection fld res (SemanticAnn (STy (PortConnection (APAtomicConnTy ts))) _) : xs) = do
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
genBlocks (RegularBlock stmts) = concat <$> traverse genStatement stmts
genBlocks (ProcedureInvoke obj ident args ann) = do
    (cFuncType, _) <- case ann of
        SemanticAnn (ETy (AppType pts ts)) _ -> do
            cFuncType <- genProcedureType pts
            cRetType <- genType noqual ts
            return (cFuncType, cRetType)
        _ -> throwError $ InternalError $ "Invalid function annotation: " ++ show ann
    -- Generate the C code for the object
    typeObj <- getObjType obj
    cObj <- case (obj, typeObj) of
        (MemberAccess obj' identifier _ann, TAccessPort (TInterface RegularInterface iface)) -> do
            cObj' <- genObject obj'
            return $ cObj' @. identifier @: CTTypeDef iface noqual
        (DereferenceMemberAccess obj' identifier _ann, TAccessPort (TInterface RegularInterface iface)) -> do
            cObj' <- genObject obj'
            return $ cObj' @. identifier @: CTTypeDef iface noqual
        _ -> throwError $ InternalError $ "Invalid object in procedure call: " ++ show obj
    let cEventArg = eventParam @: ptr __termina_event_t
    -- Generate the C code for the parameters
    cArgs <- mapM genExpression args
    -- | Obtain the type of the object
    return
        [pre_cr ((cObj @. ident @: cFuncType) @@
            (cEventArg : (cObj @. thatField @: ptr void) : cArgs) |>> getLocation ann) |>> getLocation ann]
    
genBlocks (AllocBox obj arg ann) = do
    -- Generate the C code for the object
    cObj <- genObject obj
    -- Generate the C code for the parameters
    cArg <- genExpression arg
    methodCallExpr <- genPoolMethodCallExpr "alloc" cObj cArg (buildGenericAnn ann)
    return [pre_cr methodCallExpr |>> getLocation ann]
genBlocks (FreeBox obj arg ann) = do
    -- Generate the C code for the object
    cObj <- genObject obj
    -- Generate the C code for the parameters
    cArg <- genExpression arg
    methodCallExpr <- genPoolMethodCallExpr "free" cObj cArg (buildGenericAnn ann)
    return [pre_cr methodCallExpr |>> getLocation ann]
genBlocks (AtomicLoad obj arg ann) = do
    -- Generate the C code for the object
    cObj <- genObject obj
    let cObjExpr = cObj @: getCObjType cObj |>> getLocation ann
    -- Generate the C code for the parameters
    cArg <- genExpression arg
    case arg of
        ReferenceExpression _ refObj _ -> do
            cRefObj <- genObject refObj
            mCall <- genAtomicMethodCall "load" cObjExpr [cArg] (buildGenericAnn ann)
            return [pre_cr (cRefObj @= mCall) |>> getLocation ann]
        _ -> throwError $ InternalError $ "invalid params for atomic load: " ++ show arg
genBlocks (AtomicStore obj arg ann) = do
    -- Generate the C code for the object
    cObj <- genObject obj
    let cObjExpr = cObj @: getCObjType cObj |>> getLocation ann
    -- Generate the C code for the parameters
    cArg <- genExpression arg
    methodCallExpr <- genAtomicMethodCall "store" cObjExpr [cArg] (buildGenericAnn ann)
    return [pre_cr methodCallExpr |>> getLocation ann]
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
            return [pre_cr (cRefObj @= mCall) |>> getLocation ann]
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
    return [pre_cr mCall |>> getLocation ann]
genBlocks (SendMessage obj arg ann) = do
    let cAnn = buildGenericAnn ann
    cObj <- genObject obj
    -- Generate the C code for the object
    case arg of
        -- | If the argument is null, we must call the send method with a null pointer
        (Constant Null _) -> do
            mCall <- genMsgQueueSendNULLExpr cObj cAnn
            return [pre_cr mCall |>> getLocation ann]
        -- | If the argument is an access object, we can use it directly
        (AccessObject {}) -> do
            cArg <- genExpression arg
            mCall <- genMsgQueueSendCall cObj cArg cAnn
            return [pre_cr mCall |>> getLocation ann]
        -- | If it is not an object, must store it in a temporary variable
        _ -> do
            cArg <- genExpression arg
            let cArgType = getCExprType cArg
                cTmp = "msg" @: cArgType
            mCall <- genMsgQueueSendCall cObj cTmp cAnn
            return [
                    no_cr $ block [
                        no_cr $ var "msg" cArgType @:= cArg |>> getLocation ann,
                        no_cr mCall
                    ]
                ]
genBlocks (IfElseBlock (CondIf expr ifBlks _) elifsBlks elseBlk ann) = do
    cExpr <- genExpression expr
    cIfBlk <- concat <$> traverse genBlocks (blockBody ifBlks)
    cElseBlk <- maybe (return Nothing) (\elseBlk' -> do
                    blks <- concat <$> traverse genBlocks (blockBody . condElseBody $ elseBlk')
                    return . Just $ (trail_cr . block $ blks) |>> getLocation ann) elseBlk
    mAlts <- genAlternatives cElseBlk elifsBlks
    case mAlts of
        Nothing -> return [pre_cr $ _if cExpr (trail_cr . block $ cIfBlk) |>> getLocation ann]
        Just cAlts -> return [pre_cr $ _if_else cExpr (trail_cr . block $ cIfBlk) cAlts |>> getLocation ann]

    where

        genAlternatives :: Maybe CStatement -> [CondElseIf SemanticAnn] -> CGenerator (Maybe CStatement)
        genAlternatives prev [] = return prev
        genAlternatives prev (CondElseIf expr' blk ann' : xs) = do
            cExpr' <- genExpression expr'
            cBlk <- concat <$> traverse genBlocks (blockBody blk)
            mPrev <- genAlternatives prev xs
            case mPrev of
                Nothing -> return . Just $ _if cExpr' (trail_cr . block $ cBlk) |>> getLocation ann'
                Just prev' -> return . Just $ _if_else cExpr' (trail_cr . block $ cBlk) prev' |>> getLocation ann'

genBlocks (ForLoopBlock iterator iteratorTS initValue endValue breakCond body ann) = do
    initExpr <- genExpression initValue
    endExpr <- genExpression endValue
    cIteratorType <- genType noqual iteratorTS
    let cIteratorExpr = iterator @: cIteratorType |>> getLocation ann
    condExpr <-
        case breakCond of
            Nothing -> return $ cIteratorExpr @< endExpr |>> getLocation ann
            Just break' -> do
                    cBreak <- genExpression break'
                    return $ (cIteratorExpr @< endExpr |>> getLocation ann) @&& cBreak |>> getLocation ann

    cBody <- concat <$> traverse genBlocks (blockBody body)
    return
        [pre_cr (_for_let
            (var iterator cIteratorType @:= initExpr)
            condExpr
            (iterator @: cIteratorType @= (cIteratorExpr @+ dec 1 @: cIteratorType) @: cIteratorType)
            (trail_cr . block $ cBody)) |>> getLocation ann]
genBlocks match@(MatchBlock expr matchCases mDefaultCase ann) = do
    exprType <- getExprType expr
    (casePrefix, structName, genParamsStructName) <-
        case exprType of
            -- | If the expression is an enumeration, the case identifier must 
            -- be prefixed with the enumeration identifier.
            (TEnum enumId) -> do
                enumStructName <- genEnumStructName enumId
                return ((<::>) enumId, enumStructName, genEnumParameterStructName enumId)
            TReference _ (TEnum enumId) -> do
                enumStructName <- genEnumStructName enumId
                return ((<::>) enumId, enumStructName, genEnumParameterStructName enumId)
            (TOption ts) -> do
                sname <- genOptionStructName ts
                pname <- genOptionParameterStructName ts
                return (id, sname, const (return pname))
            (TReference _ (TOption ts)) -> do
                sname <- genOptionStructName ts
                pname <- genOptionParameterStructName ts
                return (id, sname, const (return pname))
            (TStatus ts) -> do
                sname <- genStatusStructName ts
                pname <- genStatusParameterStructName ts
                return (id, sname, const (return pname))
            (TReference _ (TStatus ts)) -> do
                sname <- genStatusStructName ts
                pname <- genStatusParameterStructName ts
                return (id, sname, const (return pname))
            (TReference _ (TResult okTy errorTy)) -> do
                sname <- genResultStructName okTy errorTy
                return (id, sname, genResultParameterStructName okTy errorTy)
            (TResult okTy errorTy) -> do
                sname <- genResultStructName okTy errorTy
                return (id, sname, genResultParameterStructName okTy errorTy)
            _ -> throwError $ InternalError $ "Unsupported match expression type: " ++ show expr
    case expr of
        (ReferenceExpression _ obj _) -> do
            cObj <- genObject obj
            case matchCases of
                -- | If there is only one case, we only need to check the variant if
                -- there is no default case
                [m@(MatchCase identifier _ _ ann')] -> do
                    let loc' = getLocation ann'
                    paramsStructName <- genParamsStructName identifier
                    cTy <- genType noqual (TStruct paramsStructName)
                    case mDefaultCase of
                        Just d@(DefaultCase {}) -> do
                            let cEnumVariantsFieldExpr = cObj @. variant @: uint32_t
                                cCasePrefixIdentExpr = casePrefix identifier @: uint32_t
                            theCase <- flip CSCompound (buildCompoundAnn ann' False True) <$> genMatchCase cObj cTy m
                            defCaseBlk <- genDefaultCase d
                            let cDefaultCase = (trail_cr . block) defCaseBlk
                            return [pre_cr (_if_else (cEnumVariantsFieldExpr @== cCasePrefixIdentExpr) theCase cDefaultCase) |>> loc']
                        Nothing -> genMatchCase cObj cTy m
                -- | The first one must add a preceding blank line
                m@(MatchCase identifier _ _ ann') : xs -> do
                    let loc' = getLocation ann'
                    paramsStructName <- genParamsStructName identifier
                    cTy <- genType noqual (TStruct paramsStructName)
                    rest <- genMatchCases cObj casePrefix genParamsStructName xs
                    cBlk <- flip CSCompound (buildCompoundAnn ann' False True) <$> genMatchCase cObj cTy m
                    -- | TODO: The size of the enum field has been hardcoded, it should be
                    -- platform dependent
                    let cEnumVariantsFieldExpr = cObj @. variant @: uint32_t |>> getLocation ann
                        cCasePrefixIdentExpr = casePrefix identifier @: uint32_t |>> getLocation ann
                    return [pre_cr (_if_else (cEnumVariantsFieldExpr @== cCasePrefixIdentExpr) cBlk rest) |>> loc']
                _ -> throwError $ InternalError $ "Match statement without cases: " ++ show match
        (AccessObject {}) -> do
            cObj <- genExpression expr >>= getCObject
            case matchCases of
                -- | If there is only one case, we only need to check the variant if
                -- there is no default case
                [m@(MatchCase identifier _ _ ann')] -> do
                    let loc' = getLocation ann'
                    paramsStructName <- genParamsStructName identifier
                    cTy <- genType noqual (TStruct paramsStructName)
                    case mDefaultCase of
                        Just d@(DefaultCase {}) -> do
                            let cEnumVariantsFieldExpr = cObj @. variant @: uint32_t
                                cCasePrefixIdentExpr = casePrefix identifier @: uint32_t
                            theCase <- flip CSCompound (buildCompoundAnn ann' False True) <$> genMatchCase cObj cTy m
                            defCaseBlk <- genDefaultCase d
                            let cDefaultCase = (trail_cr . block) defCaseBlk
                            return [pre_cr (_if_else (cEnumVariantsFieldExpr @== cCasePrefixIdentExpr) theCase cDefaultCase) |>> loc']
                        Nothing -> genMatchCase cObj cTy m
                -- | The first one must add a preceding blank line
                m@(MatchCase identifier _ _ ann') : xs -> do
                    let loc' = getLocation ann'
                    paramsStructName <- genParamsStructName identifier
                    cTy <- genType noqual (TStruct paramsStructName)
                    rest <- genMatchCases cObj casePrefix genParamsStructName xs
                    cBlk <- flip CSCompound (buildCompoundAnn ann' False True) <$> genMatchCase cObj cTy m
                    -- | TODO: The size of the enum field has been hardcoded, it should be
                    -- platform dependent
                    let cEnumVariantsFieldExpr = cObj @. variant @: uint32_t
                        cCasePrefixIdentExpr = casePrefix identifier @: uint32_t
                    return [pre_cr (_if_else (cEnumVariantsFieldExpr @== cCasePrefixIdentExpr) cBlk rest) |>> loc']
                _ -> throwError $ InternalError $ "Match statement without cases: " ++ show match
        _ -> do
            cExpr <- genExpression expr
            cType <- genType noqual (TStruct structName)
            let decl = var (namefy "match") cType @:= cExpr
                cObj' = namefy "match" @: cType
            case matchCases of
                [m@(MatchCase identifier _ _ ann')] -> do
                    let loc' = getLocation ann'
                    paramsStructName <- genParamsStructName identifier
                    cTy <- genType noqual (TStruct paramsStructName)
                    case mDefaultCase of
                        Just d@(DefaultCase {}) -> do
                            cBlk <- trail_cr . block <$> genMatchCase cObj' cTy m
                            let cEnumVariantsFieldExpr = cObj' @. variant @: uint32_t
                                cCasePrefixIdentExpr = casePrefix identifier @: uint32_t
                            defCaseBlk <- genDefaultCase d
                            let cDefaultCase = (trail_cr . block) defCaseBlk
                            return [pre_cr $ (trail_cr . block) (pre_cr decl : [pre_cr
                                    (_if_else (cEnumVariantsFieldExpr @== cCasePrefixIdentExpr) cBlk cDefaultCase) |>> loc'])]
                        Nothing -> do
                            cBlk <- genMatchCase cObj' cTy m
                            return [no_cr $ (trail_cr . block) (pre_cr decl : cBlk)]
                m@(MatchCase identifier _ _ _) : xs -> do
                    paramsStructName <- genParamsStructName identifier
                    cTy <- genType noqual (TStruct paramsStructName)
                    rest <- genMatchCases cObj' casePrefix genParamsStructName xs
                    cBlk <- trail_cr . block <$> genMatchCase cObj' cTy m
                    let cEnumVariantsFieldExpr = cObj' @. variant @: uint32_t
                        cCasePrefixIdentExpr = casePrefix identifier @: uint32_t
                    return [pre_cr $ (trail_cr . block) (pre_cr decl : [pre_cr 
                            (_if_else (cEnumVariantsFieldExpr @== cCasePrefixIdentExpr) cBlk rest)])]
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
            -> CGenerator CStatement
        -- | This should never happen
        genMatchCases _ _ _ [] = throwError $ InternalError "Match statement without cases"
        -- | The last one does not need to check the variant
        genMatchCases cObj casePrefix genParamsStructName [m@(MatchCase identifier _ _ ann')] = do
            paramsStructName <- genParamsStructName identifier
            cTy <- genType noqual (TStruct paramsStructName)
            cBlk <- genMatchCase cObj cTy m
            case mDefaultCase of
                Just d@(DefaultCase _ ann'') -> do
                    let cEnumVariantsFieldExpr = cObj @. variant @: uint32_t |>> getLocation ann
                        cCasePrefixIdentExpr = casePrefix identifier @: uint32_t |>> getLocation ann
                        cExpr' = cEnumVariantsFieldExpr @== cCasePrefixIdentExpr |>> getLocation ann'
                    defCaseBlk <- genDefaultCase d
                    let cLastCase = (trail_cr . block) cBlk |>> getLocation ann'
                        cDefaultCase = (trail_cr . block) defCaseBlk |>> getLocation ann''
                    return $ _if_else cExpr' cLastCase cDefaultCase |>> getLocation ann'
                Nothing -> return $ (trail_cr . block) cBlk |>> getLocation ann'
        genMatchCases cObj casePrefix genParamsStructName (m@(MatchCase identifier _ _ ann') : xs) = do
            let cEnumVariantsFieldExpr = cObj @. variant @: uint32_t |>> getLocation ann'
                cCasePrefixIdentExpr = casePrefix identifier @: uint32_t |>> getLocation ann'
                cExpr' = cEnumVariantsFieldExpr @== cCasePrefixIdentExpr |>> getLocation ann'
            paramsStructName <- genParamsStructName identifier
            cTs <- genType noqual (TStruct paramsStructName)
            cBlk <- trail_cr . block  <$> genMatchCase cObj cTs m
            rest <- genMatchCases cObj casePrefix genParamsStructName xs
            return $ _if_else cExpr' (cBlk |>> getLocation ann') rest |>> getLocation ann'

        genMatchCase ::
            CObject -> CType
            -> MatchCase SemanticAnn
            -> CGenerator [CCompoundBlockItem]
        genMatchCase cObj cParamsStructType c@(MatchCase _ _ blk' _) = do
            decls <- genMatchCaseParams cObj cParamsStructType c
            cBlk <- concat <$> traverse genBlocks (blockBody blk')
            return $ decls ++ cBlk

        genDefaultCase :: DefaultCase SemanticAnn -> CGenerator [CCompoundBlockItem]
        genDefaultCase (DefaultCase blk' _) = do
            concat <$> traverse genBlocks (blockBody blk')

        genMatchCaseParams :: CObject -> CType -> MatchCase SemanticAnn -> CGenerator [CCompoundBlockItem]
        genMatchCaseParams cObj cParamsStructType (MatchCase this_variant params _ (SemanticAnn semann loc')) = do
            cParamTypes <- case getMatchCaseTypes semann of
                Just ts -> traverse (genType noqual) ts
                Nothing -> throwError $ InternalError "Match case without types"
            case [used | used@(x : _) <- params, x /= '_' ] of
                [] -> return []
                [param] ->
                    case head cParamTypes of
                        CTPointer {} ->
                            return [pre_cr (var param (head cParamTypes) @:= addrOf ((cObj @. this_variant @: cParamsStructType) @. namefy (show (0 :: Integer)) @: head cParamTypes)) |>> loc']
                        _ ->
                            return [pre_cr (var param (head cParamTypes) @:= (cObj @. this_variant @: cParamsStructType) @. namefy (show (0 :: Integer)) @: head cParamTypes) |>> loc']
                (p : xp) -> do
                    let rest = zipWith3
                            (\sym index cParamType ->
                                case cParamType of
                                    CTPointer {} ->
                                        no_cr (var sym cParamType @:= addrOf ((cObj @. this_variant @: cParamsStructType) @. namefy (show (index :: Integer)) @: cParamType)) |>> loc'
                                    _ ->
                                        no_cr (var sym cParamType @:= (cObj @. this_variant @: cParamsStructType) @. namefy (show (index :: Integer)) @: cParamType) |>> loc') xp [1..] (tail cParamTypes)
                    case head cParamTypes of
                        CTPointer {} ->
                            return $ pre_cr (var p (head cParamTypes) @:= addrOf ((cObj @. this_variant @: cParamsStructType) @. namefy (show (0 :: Integer)) @: head cParamTypes)) |>> loc' : rest
                        _ ->
                            return $ pre_cr (var p (head cParamTypes) @:= (cObj @. this_variant @: cParamsStructType) @. namefy (show (0 :: Integer)) @: head cParamTypes) |>> loc' : rest


genBlocks (ReturnBlock mExpr ann) =
    case mExpr of
        Nothing ->
            return [pre_cr (_return Nothing) |>> getLocation ann]
        Just expr -> do
            cExpr <- genExpression expr
            return [pre_cr (_return (Just cExpr)) |>> getLocation ann]
genBlocks (ContinueBlock expr ann) = do
    cExpr <- genExpression expr
    return [pre_cr (_return (Just cExpr)) |>> getLocation ann]
genBlocks (RebootBlock ann) = do
    return [pre_cr $ __termina_exec__reboot @@ [] |>> getLocation ann]
genBlocks (SystemCall obj ident args ann) = do
    (cFuncType, _) <- case ann of
        SemanticAnn (ETy (AppType pts ts)) _ -> do
            cFuncType <- genSystemCallType pts
            cRetType <- genType noqual ts
            return (cFuncType, cRetType)
        _ -> throwError $ InternalError $ "Invalid function annotation: " ++ show ann
    -- Generate the C code for the object
    typeObj <- getObjType obj
    cObj <- case (obj, typeObj) of
        (MemberAccess obj' identifier _ann, TAccessPort (TInterface SystemInterface iface)) -> do
            cObj' <- genObject obj'
            return $ cObj' @. identifier @: CTTypeDef iface noqual
        (DereferenceMemberAccess obj' identifier _ann, TAccessPort (TInterface SystemInterface iface)) -> do
            cObj' <- genObject obj'
            return $ cObj' @. identifier @: CTTypeDef iface noqual
        _ -> throwError $ InternalError $ "Invalid object in procedure call: " ++ show obj
    -- Generate the C code for the parameters
    let cEventArg = eventParam @: ptr __termina_event_t
    cArgs <- mapM genExpression args
    -- | Obtain the type of the object
    return
        [pre_cr ((cObj @. ident @: cFuncType) @@ (cEventArg : cArgs) |>> getLocation ann) |>> getLocation ann]

    where 

        genSystemCallType :: [Parameter SemanticAnn] -> CGenerator CType
        genSystemCallType tsParams = do
            tsParams' <- traverse (genType noqual . paramType) tsParams
            let cEventArgType = ptr __termina_event_t
            return $ CTFunction void (cEventArgType : tsParams')

genStatement :: Statement SemanticAnn -> CGenerator [CCompoundBlockItem]
genStatement (AssignmentStmt obj expr ann) = do
    let loc = getLocation ann
    typeObj <- getObjType obj
    cObj <- genObject obj
    case typeObj of
        TArray _ _ -> genArrayInitialization loc True 0 cObj expr
        (TFixedLocation _) -> do
            return [pre_cr (addrOf cObj) |>> loc]
        _ -> case expr of
            (StructInitializer {}) -> genStructInitialization loc True 0 cObj expr
            (MonadicVariantInitializer {}) -> genMonadicTypeInitialization loc True 0 cObj expr
            (EnumVariantInitializer {}) -> genEnumInitialization loc True 0 cObj expr
            _ -> do
                cExpr <- genExpression expr
                return [pre_cr (cObj @= cExpr) |>> getLocation ann]
genStatement (Declaration identifier _ ts expr ann) = do
  let loc = getLocation ann
  cType <- genType noqual ts
  let cObj = CVar identifier cType
  case ts of
    TArray _ _ -> do
        arrayInitialization <- genArrayInitialization loc False 0 cObj expr
        return $
            pre_cr (var identifier cType) |>> loc
            : arrayInitialization
    _ -> case expr of
        (StructInitializer {}) -> do
            structInitialization <- genStructInitialization loc False 0 cObj expr
            return $
                pre_cr (var identifier cType) |>> loc
                : structInitialization
        (MonadicVariantInitializer {}) -> do
            optionInitialization <- genMonadicTypeInitialization loc False 0 cObj expr
            return $
                pre_cr (var identifier cType) |>> getLocation ann
                : optionInitialization
        (EnumVariantInitializer {}) -> do
            enumInitialization <- genEnumInitialization loc False 0 cObj expr
            return $
                pre_cr (var identifier cType) |>> getLocation ann
                : enumInitialization
        _ -> do
            cExpr <- genExpression expr
            return [pre_cr (var identifier cType @:= cExpr) |>> getLocation ann]
genStatement (SingleExpStmt expr ann) = do
    let loc = getLocation ann
    cExpr <- genExpression expr
    return [pre_cr cExpr |>> loc]
