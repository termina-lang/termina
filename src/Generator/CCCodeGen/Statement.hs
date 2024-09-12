module Generator.CCCodeGen.Statement where

import AST.Seman
import Generator.LanguageC.CompCertC
import Semantic.Monad
import Control.Monad.Except
import Generator.CCCodeGen.Common
import Generator.CCCodeGen.Expression
import Utils.Annotations
import Data.Map (fromList, union)
import qualified Control.Monad.Reader

genEnumInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -> Integer ->
    -- | Enum
    CObject ->
    -- |  The initialization expression
    Expression SemanticAnn ->
    CSourceGenerator [CStatement]
genEnumInitialization before level cObj expr = do
    case expr of
        -- \| This function can only be called with a field values assignments expressions
        (EnumVariantInitializer ts variant params ann) -> do
            let exprCAnn = buildGenericAnn ann
            let declStmtAnn = buildStatementAnn ann before
            cParams <- zipWithM (\e index -> do
                cType <- getExprType e >>= genType noqual
                let cFieldObj = CField cObj variant cType
                genFieldInitialization False level cFieldObj (namefy (show (index :: Integer))) e) params [0..]
            let variantsFieldsObj = CField cObj enumVariantsField enumFieldType
            let variantExpr = CExprValOf (CVar (ts <::> variant) enumFieldType) enumFieldType exprCAnn
            return $ CSDo (CExprAssign variantsFieldsObj variantExpr enumFieldType exprCAnn) declStmtAnn : concat cParams
        _ -> error "Incorrect expression"

genOptionInitialization ::
    Bool
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CSourceGenerator [CStatement]
genOptionInitialization before level cObj expr =
    case expr of
        (OptionVariantInitializer (Some e) ann) -> do
            let exprCAnn = buildGenericAnn ann
            let declStmtAnn = buildStatementAnn ann before
            
            let cSomeVariantFieldObj = CField cObj optionSomeVariant enumFieldType
            fieldInitalization <- genFieldInitialization False level cSomeVariantFieldObj optionSomeField e
            let variantsFieldsObj = CField cObj enumVariantsField enumFieldType
            let someVariantExpr = CExprValOf (CVar optionSomeVariant enumFieldType) enumFieldType exprCAnn
            return $
                CSDo (CExprAssign variantsFieldsObj someVariantExpr enumFieldType exprCAnn) declStmtAnn :
                fieldInitalization
        (OptionVariantInitializer None ann) -> do
            let exprCAnn = buildGenericAnn ann
            let declStmtAnn = buildStatementAnn ann before
            let variantsFieldsObj = CField cObj enumVariantsField enumFieldType
            let noneVariantExpr = CExprValOf (CVar optionNoneVariant enumFieldType) enumFieldType exprCAnn
            return [CSDo (CExprAssign variantsFieldsObj noneVariantExpr enumFieldType exprCAnn) declStmtAnn]
        _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr

genArrayInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -- | Current vector nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CSourceGenerator [CStatement]
genArrayInitialization before level cObj expr = do
    case expr of
        (ArrayInitializer expr' size ann) -> do
            cSize <- genArraySizeExpr size ann
            let iterator = namefy $ "i" ++ show level
                cIteratorExpr = CExprValOf (CVar iterator (CTSizeT noqual)) (CTSizeT noqual) exprCAnn
                exprCAnn = buildGenericAnn ann
                initExpr = Right $ CDecl
                            (CTypeSpec (CTSizeT noqual)) (Just iterator)
                            (Just (CExprConstant  (CIntConst (CInteger 0 CDecRepr)) (CTSizeT noqual) exprCAnn))
                condExpr = Just $ CExprBinaryOp COpLt cIteratorExpr cSize (CTBool noqual) exprCAnn
                incrExpr = Just $ CExprAssign (CVar iterator (CTSizeT noqual)) (CExprBinaryOp COpAdd cIteratorExpr (CExprConstant (CIntConst (CInteger 1 CDecRepr)) (CTSizeT noqual) exprCAnn) (CTSizeT noqual) exprCAnn) (CTSizeT noqual) exprCAnn
                cObjType = getCObjType cObj
            cObjArrayItemType <- getCArrayItemType cObjType
            arrayInit <- genArrayInitialization False (level + 1) (CIndexOf cObj cIteratorExpr cObjArrayItemType) expr'
            return [CSFor initExpr condExpr incrExpr (CSCompound (CBlockStmt <$> arrayInit) (buildCompoundAnn ann False False)) (buildStatementAnn ann before)]
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

        genArrayItemsInitialization :: Bool -> Integer -> Integer -> [Expression SemanticAnn] -> CSourceGenerator [CStatement]
        genArrayItemsInitialization _before _level _idx [] = return []
        genArrayItemsInitialization before' level' idx (x:xs) = do
            let ann = getAnnotation x
            rest <- genArrayItemsInitialization False level' (idx + 1) xs
            let cObjType = getCObjType cObj
            cObjArrayItemType <- getCArrayItemType cObjType
            current <- genArrayInitialization before' level' (CIndexOf cObj (CExprConstant (CIntConst (CInteger idx CDecRepr)) (CTSizeT noqual) (buildGenericAnn ann)) cObjArrayItemType) x
            return $ current ++ rest
        
        genArrayInitializationFromExpression :: Integer ->
            CObject ->
            CExpression ->
            TypeSpecifier ->
            SemanticAnn ->
            CSourceGenerator [CStatement]
        genArrayInitializationFromExpression lvl lhsCObj rhsCExpr ts ann = do
            case ts of
                -- | If the initializer is a vector, we must iterate
                (Array ts' (K s)) -> do
                    let iterator = namefy $ "i" ++ show lvl
                        cIteratorExpr = CExprValOf (CVar iterator (CTSizeT noqual)) (CTSizeT noqual) exprCAnn
                        exprCAnn = buildGenericAnn ann
                        initExpr = Right $ CDecl
                            (CTypeSpec (CTSizeT noqual)) (Just iterator)
                            (Just (CExprConstant (CIntConst (CInteger 0 CDecRepr)) (CTSizeT noqual) exprCAnn))
                        cSize = genInteger s
                        condExpr = Just $ CExprBinaryOp COpLt cIteratorExpr (CExprConstant (CIntConst cSize) (CTSizeT noqual) exprCAnn) (CTBool noqual) exprCAnn
                        incrExpr = Just $ CExprAssign (CVar iterator (CTSizeT noqual)) (CExprBinaryOp COpAdd cIteratorExpr (CExprConstant (CIntConst (CInteger 1 CDecRepr)) (CTSizeT noqual) exprCAnn) (CTSizeT noqual) exprCAnn) (CTSizeT noqual) exprCAnn
                    cTs' <- genType noqual ts'
                    rhsCObject <- unboxObject rhsCExpr
                    arrayInit <- genArrayInitializationFromExpression (lvl + 1) (CIndexOf lhsCObj cIteratorExpr cTs') (CExprValOf (CIndexOf rhsCObject cIteratorExpr cTs') cTs' exprCAnn) ts' ann
                    return [CSFor initExpr condExpr incrExpr (CSCompound (CBlockStmt <$> arrayInit) (buildCompoundAnn ann False False)) (buildStatementAnn ann (before && lvl == 0))]
                _ -> let cType = getCObjType lhsCObj in
                    return [CSDo (CExprAssign lhsCObj rhsCExpr cType (buildGenericAnn ann)) (buildStatementAnn ann (before && lvl == 0))]


genFieldInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -- | Current vector nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Identifier
    -> Expression SemanticAnn
    -> CSourceGenerator [CStatement]
genFieldInitialization before level cObj field expr = do
    cExprType <- getExprType expr >>= genType noqual
    let cFieldObj = CField cObj field cExprType
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
                Array _ _ -> genArrayInitialization before level cFieldObj expr
                _ -> do
                    let exprCAnn = buildGenericAnn ann
                        declStmtAnn = buildStatementAnn ann before
                    cExpr <- genExpression expr
                    return [CSDo (CExprAssign cFieldObj cExpr cExprType exprCAnn) declStmtAnn]

genStructInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -- | Current vector nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CObject
    -> Expression SemanticAnn
    -> CSourceGenerator [CStatement]
genStructInitialization before level cObj expr = do
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (StructInitializer vas _ ann) -> genFieldAssignments before vas

        where

            genProcedureAssignment :: Identifier -> TypeSpecifier -> SemanProcedure -> CSourceGenerator CStatement
            genProcedureAssignment field (DefinedType interface) (SemanProcedure procid params) = do
                let exprCAnn = buildGenericAnn ann
                    declStmtAnn = buildStatementAnn ann before
                cPortFieldType <- genType noqual (DefinedType interface)
                let portFieldObj = CField cObj field cPortFieldType
                clsFunctionName <- genClassFunctionName interface procid
                clsFunctionType <- genFunctionType Unit (fmap paramTypeSpecifier params)
                let clsFunctionExpr = CExprValOf (CVar clsFunctionName clsFunctionType) clsFunctionType exprCAnn
                return $ CSDo (CExprAssign portFieldObj clsFunctionExpr clsFunctionType exprCAnn) declStmtAnn
            genProcedureAssignment _ _ _ = throwError $ InternalError "Unsupported procedure assignment"

            genFieldAssignments :: Bool -> [FieldAssignment SemanticAnn] -> CSourceGenerator [CStatement]
            genFieldAssignments _ [] = return []
            genFieldAssignments before' (FieldValueAssignment field expr' _: xs) = do
                fieldInit <- genFieldInitialization before' level cObj field expr'
                rest <- genFieldAssignments False xs
                return $ fieldInit ++ rest
            genFieldAssignments before' (FieldAddressAssignment field addr (Located (ETy (SimpleType ts)) _):xs) = do
                let exprCAnn = buildGenericAnn ann
                    declStmtAnn = buildStatementAnn ann before'
                    cAddress = genInteger addr
                cTs <- genType noqual ts
                let fieldObj = CField cObj field cTs
                rest <- genFieldAssignments False xs
                return $ CSDo (CExprAssign fieldObj (CExprCast (CExprConstant (CIntConst cAddress) cTs exprCAnn) cTs exprCAnn) cTs exprCAnn) declStmtAnn : rest
            genFieldAssignments before' (FieldPortConnection OutboundPortConnection field channel (Located (CTy (OutPConnTy _)) _) : xs) = do
                let exprCAnn = buildGenericAnn ann
                    declStmtAnn = buildStatementAnn ann before'
                let cMsgQueue = CTTypeDef msgQueue noqual
                let cPtrMsgQueue = CTPointer cMsgQueue noqual
                let channelExpr = CExprAddrOf (CVar channel cMsgQueue) cPtrMsgQueue exprCAnn
                    fieldObj = CField cObj field cPtrMsgQueue
                rest <- genFieldAssignments False xs
                return $ CSDo (CExprAssign fieldObj channelExpr cPtrMsgQueue exprCAnn) declStmtAnn : rest
            genFieldAssignments before' (FieldPortConnection AccessPortConnection field resource (Located (CTy (APConnTy rts procedures)) _) : xs) = do
                let exprCAnn = buildGenericAnn ann
                    declStmtAnn = buildStatementAnn ann before'
                let thatFieldObj = CField cObj thatField (CTPointer (CTVoid noqual) noqual)
                    cResourceType = CTTypeDef resource noqual
                    resourceExpr = CExprAddrOf (CVar resource cResourceType) (CTPointer cResourceType noqual) exprCAnn
                rest <- genFieldAssignments False xs
                cProcedures <- mapM (genProcedureAssignment field rts) procedures
                return $ CSDo (CExprAssign thatFieldObj resourceExpr (CTPointer cResourceType noqual) exprCAnn) declStmtAnn : (cProcedures ++ rest)
            genFieldAssignments before' (FieldPortConnection AccessPortConnection field res (Located (CTy (APAtomicArrayConnTy ts size)) _) : xs) = do
                let exprCAnn = buildGenericAnn ann
                    declStmtAnn = buildStatementAnn ann before'
                rest <- genFieldAssignments False xs
                cTs <- genType noqual (Array ts size)
                let portFieldObj = CField cObj field cTs
                return $ CSDo (CExprAssign portFieldObj (CExprValOf (CVar res cTs) cTs exprCAnn) cTs exprCAnn) declStmtAnn : rest
            genFieldAssignments before' (FieldPortConnection AccessPortConnection field res (Located (CTy (APAtomicConnTy ts)) _) : xs) = do
                let exprCAnn = buildGenericAnn ann
                    declStmtAnn = buildStatementAnn ann before'
                rest <- genFieldAssignments False xs
                cTs <- genType noqual ts
                let portFieldObj = CField cObj field (CTPointer cTs noqual)
                    cResourceType = CTTypeDef res noqual
                return $ CSDo (CExprAssign portFieldObj (CExprAddrOf (CVar res cResourceType) cResourceType exprCAnn) (CTPointer cResourceType noqual) exprCAnn) declStmtAnn : rest
            genFieldAssignments before' (_ : xs) = genFieldAssignments before' xs

    _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr

genBlockItem :: Statement SemanticAnn -> CSourceGenerator [CCompoundBlockItem]
genBlockItem (AssignmentStmt obj expr  _) = do
    typeObj <- getObjType obj
    cType <- genType noqual typeObj
    cObj <- genObject obj 
    case typeObj of
        Array _ _ -> fmap CBlockStmt <$> genArrayInitialization True 0 cObj expr
        (Location _) -> do
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
genBlockItem (Declaration identifier _ ts expr ann) = do
  cType <- genType noqual ts
  let cObj = CVar identifier cType
  case ts of
    Array _ _ -> do
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
genBlockItem (IfElseStmt expr ifBlk elifsBlks elseBlk ann) = do
    cExpr <- genExpression expr
    cIfBlk <- concat <$> mapM genBlockItem ifBlk
    cElseBlk <- 
        (case elseBlk of
            Nothing -> return Nothing
            Just elseBlk' ->
                mapM genBlockItem elseBlk' >>= (return . Just) . flip CSCompound (buildCompoundAnn ann False True) . concat)
    cAlts <- genAlternatives cElseBlk elifsBlks

    return $ CBlockStmt <$>
        [CSIfThenElse cExpr (CSCompound cIfBlk (buildCompoundAnn ann False True)) cAlts (buildStatementAnn ann True)]

    where
        genAlternatives :: Maybe CStatement -> [ElseIf SemanticAnn] -> CSourceGenerator (Maybe CStatement)
        genAlternatives prev [] = return prev
        genAlternatives prev (ElseIf expr' blk ann' : xs) = do
            prev' <- genAlternatives prev xs
            cExpr' <- genExpression expr'
            cBlk <- concat <$> mapM genBlockItem blk
            return $ Just (CSIfThenElse cExpr' (CSCompound cBlk (buildCompoundAnn ann' False True)) prev' (buildStatementAnn ann' False))
genBlockItem (ForLoopStmt iterator iteratorTS initValue endValue breakCond body ann) = do
    let exprCAnn = buildGenericAnn ann
    initExpr <- genConstExpression initValue
    endExpr <- genConstExpression endValue
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
    cBody <- concat <$> mapM genBlockItem body
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
genBlockItem match@(MatchStmt expr matchCases ann) = do
    let exprCAnn = buildGenericAnn ann
    exprType <- getExprType expr
    (casePrefix, structName, genParamsStructName) <-
        case exprType of
            -- | If the expression is an enumeration, the case identifier must 
            -- be prefixed with the enumeration identifier.
            (DefinedType enumId) -> do
                enumStructName <- genEnumStructName enumId
                return ((<::>) enumId, enumStructName, genEnumParameterStructName enumId)
            (Option ts) -> do
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
                    cTs <- CTypeSpec <$> genType noqual (DefinedType paramsStructName)
                    genMatchCase cTs cObj m
                -- | The first one must add a preceding blank line
                m@(MatchCase identifier _ _ ann') : xs -> do
                    paramsStructName <- genParamsStructName identifier
                    cTs <- CTypeSpec <$> genType noqual (DefinedType paramsStructName)
                    rest <- genMatchCases cObj casePrefix genParamsStructName genMatchCase xs
                    cBlk <- flip CSCompound (buildCompoundAnn ann' False True) <$> genMatchCase cTs cObj m
                    -- | TODO: The size of the enum field has been hardcoded, it should be
                    -- platform dependent
                    let cEnumVariantsFieldExpr = CExprValOf (CField cObj enumVariantsField (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) exprCAnn
                        cCasePrefixIdentExpr = CExprValOf (CVar (casePrefix identifier) (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) exprCAnn
                    return [CBlockStmt $ CSIfThenElse
                        (CExprBinaryOp COpEq cEnumVariantsFieldExpr cCasePrefixIdentExpr (CTBool noqual) (buildGenericAnn ann'))
                        cBlk rest (buildStatementAnn ann' True)]
                _ -> throwError $ InternalError $ "Match statement without cases: " ++ show match
        _ -> do
            cExpr <- genExpression expr
            cType <- genType noqual (DefinedType structName)
            let decl = CDecl (CTypeSpec cType) (Just (namefy "match")) (Just cExpr)
                cObj' = CVar (namefy "match") cType
            case matchCases of
                [m@(MatchCase identifier _ _ _)] -> do
                    paramsStructName <- genParamsStructName identifier
                    paramsStructTypeSpec <- CTypeSpec <$> genType noqual (DefinedType paramsStructName)
                    cBlk <- genAnonymousMatchCase paramsStructTypeSpec cObj' m
                    return [CBlockStmt $ CSCompound (CBlockDecl decl (buildDeclarationAnn ann True) : cBlk) (buildCompoundAnn ann True True)]
                m@(MatchCase identifier _ _ ann') : xs -> do
                    paramsStructName <- genParamsStructName identifier
                    paramsStructTypeSpec <- CTypeSpec <$> genType noqual (DefinedType paramsStructName)
                    rest <- genMatchCases cObj' casePrefix genParamsStructName genAnonymousMatchCase xs
                    cBlk <- flip CSCompound (buildCompoundAnn ann' False True) <$> genAnonymousMatchCase paramsStructTypeSpec cObj' m
                    let cEnumVariantsFieldExpr = CExprValOf (CField cObj' enumVariantsField (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) (buildGenericAnn ann')
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
            -> (Identifier -> CSourceGenerator Identifier)
            -- | A function to generate a match case (inside the monad)
            -> (CTypeSpecifier
                -> CObject
                -> MatchCase SemanticAnn
                -> CSourceGenerator [CCompoundBlockItem])
            -- | The list of remaining match cases
            -> [MatchCase SemanticAnn]
            -> CSourceGenerator (Maybe CStatement)
        -- | This should never happen
        genMatchCases _ _ _ _ [] = return Nothing
        -- | The last one does not need to check the variant
        genMatchCases cObj _ genParamsStructName genCase [m@(MatchCase identifier _ _ ann')] = do
            paramsStructName <- genParamsStructName identifier
            cTs <- CTypeSpec <$> genType noqual (DefinedType paramsStructName)
            cBlk <- genCase cTs cObj m
            return $ Just (CSCompound cBlk (buildCompoundAnn ann' False True))
        genMatchCases cObj casePrefix genParamsStructName genCase (m@(MatchCase identifier _ _ ann') : xs) = do
            let cAnn = buildGenericAnn ann'
                cEnumVariantsFieldExpr = CExprValOf (CField cObj enumVariantsField (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) cAnn
                cCasePrefixIdentExpr = CExprValOf (CVar (casePrefix identifier) (CTInt IntSize32 Unsigned noqual)) (CTInt IntSize32 Unsigned noqual) cAnn
                cExpr' = CExprBinaryOp COpEq cEnumVariantsFieldExpr cCasePrefixIdentExpr (CTBool noqual) cAnn
            paramsStructName <- genParamsStructName identifier
            cTs <- CTypeSpec <$> genType noqual (DefinedType paramsStructName)
            cBlk <- flip CSCompound (buildCompoundAnn ann' False True) <$> genCase cTs cObj m
            rest <- genMatchCases cObj casePrefix genParamsStructName genCase xs
            return $ Just (CSIfThenElse cExpr' cBlk rest (buildStatementAnn ann' False))

        genAnonymousMatchCase ::
            CTypeSpecifier
            -> CObject
            -> MatchCase SemanticAnn -> CSourceGenerator [CCompoundBlockItem]
        genAnonymousMatchCase _ _ (MatchCase _ [] blk' _) = do
            concat <$> mapM genBlockItem blk'
        genAnonymousMatchCase (CTypeSpec cParamsStructType) cObj (MatchCase variant params blk' ann') = do
            let cObj' = CField cObj variant cParamsStructType
            cParamTypes <- case getMatchCaseTypes (element ann') of
                Just ts -> traverse (genType noqual) ts
                Nothing -> throwError $ InternalError "Match case without types"
            let newKeyVals = fromList $ zipWith3
                    (\sym index cParamType -> (sym, CField cObj' (namefy (show (index :: Integer))) cParamType)) params [0..] cParamTypes
            Control.Monad.Reader.local (union newKeyVals) $ concat <$> mapM genBlockItem blk'
        genAnonymousMatchCase _ _ _ = throwError $ InternalError "Invalid match case"

        genMatchCase ::
            CTypeSpecifier
            -> CObject
            -> MatchCase SemanticAnn
            -> CSourceGenerator [CCompoundBlockItem]
        genMatchCase _ _ (MatchCase _ [] blk' _) = do
            concat <$> mapM genBlockItem blk'
        genMatchCase cTs@(CTypeSpec cParamsStructType) cExpr (MatchCase variant params blk' ann') = do
            let cAnn = buildGenericAnn ann'
                cObj' = CVar (namefy variant) cParamsStructType
            cParamTypes <- case getMatchCaseTypes (element ann') of
                Just ts -> traverse (genType noqual) ts
                Nothing -> throwError $ InternalError "Match case without types"
            let newKeyVals = fromList $ zipWith3
                    (\sym index cParamType -> (sym, CField cObj' (namefy (show (index :: Integer))) cParamType)) params [0..] cParamTypes
                decl = CDecl cTs (Just (namefy variant))
                    (Just $ CExprValOf (CField cExpr variant cParamsStructType) cParamsStructType cAnn)
            cBlk <- Control.Monad.Reader.local (union newKeyVals) $ concat <$> mapM genBlockItem blk'
            return $ CBlockDecl decl (buildDeclarationAnn ann True) : cBlk
        genMatchCase _ _ _ = throwError $ InternalError "Invalid match case"
genBlockItem (SingleExpStmt expr ann) = do
    cExpr <- genExpression expr
    return [CBlockStmt $ CSDo cExpr (buildStatementAnn ann True)]

genReturnStatement :: ReturnStmt SemanticAnn -> CSourceGenerator [CCompoundBlockItem]
genReturnStatement (ReturnStmt (Just expr) ann) = do
    let cAnn = buildStatementAnn ann True
    cExpr <- genExpression expr
    return [CBlockStmt $ CSReturn (Just cExpr) cAnn]
genReturnStatement (ReturnStmt Nothing ann) = do
    let cAnn = buildStatementAnn ann True
    return [CBlockStmt $ CSReturn Nothing cAnn]