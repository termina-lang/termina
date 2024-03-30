module Generator.Statement where

import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Control.Monad.Except
import Generator.Common
import Generator.Expression
import Annotations
import Data.Map (fromList, union)
import qualified Control.Monad.Reader

genEnumInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -> Integer ->
    -- | Enum
    CExpression ->
    -- |  The initialization expression
    Expression SemanticAnns ->
    CGenerator [CStatement]
genEnumInitialization before level cObj expr = do
    case expr of
        -- \| This function can only be called with a field values assignments expressions
        (EnumVariantExpression ts variant params ann) -> do
            let exprCAnn = buildGenericAnn ann
            let declStmtAnn = buildStatementAnn ann before
            cParams <- zipWithM (\e index -> genFieldInitialization False level (CMember cObj variant False exprCAnn) (namefy (show (index :: Integer))) e) params [0..]
            return $ CExpr (Just (CAssignment (CMember cObj enumVariantsField False exprCAnn) (CVar (ts <::> variant) exprCAnn) exprCAnn)) declStmtAnn : concat cParams
        _ -> error "Incorrect expression"

genOptionInitialization ::
    Bool
    -> Integer
    -> CExpression
    -> Expression SemanticAnns
    -> CGenerator [CStatement]
genOptionInitialization before level cObj expr =
    case expr of
        (OptionVariantExpression (Some e) ann) -> do
            let exprCAnn = buildGenericAnn ann
            let declStmtAnn = buildStatementAnn ann before
            fieldInitalization <- genFieldInitialization False level (CMember cObj optionSomeVariant False exprCAnn) optionSomeField e
            return $
                CExpr (Just (CAssignment (CMember cObj enumVariantsField False exprCAnn) (CVar optionSomeVariant exprCAnn) exprCAnn)) declStmtAnn :
                fieldInitalization
        (OptionVariantExpression None ann) -> do
            let exprCAnn = buildGenericAnn ann
            let declStmtAnn = buildStatementAnn ann before
            return [CExpr (Just (CAssignment (CMember cObj enumVariantsField False exprCAnn) (CVar optionNoneVariant exprCAnn) exprCAnn)) declStmtAnn]
        _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr

genArrayInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -- | Current vector nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CExpression
    -> Expression SemanticAnns
    -> CGenerator [CStatement]
genArrayInitialization before level cObj expr = do
    case expr of
        (VectorInitExpression expr' (K s) ann) -> do
            let iterator = namefy $ "i" ++ show level
                exprCAnn = buildGenericAnn ann
                initExpr = Right $ CDeclaration
                    [CTypeSpec CSizeTType]
                    [(Just (CDeclarator (Just iterator) [] [] exprCAnn),
                      Just (CInitExpr (CConst (CIntConst (CInteger 0 DecRepr)) exprCAnn) exprCAnn),
                      Nothing)]
                    (buildDeclarationAnn ann False)
                condExpr = Just $ CBinary CLeOp (CVar iterator exprCAnn) (CConst (CIntConst (CInteger s DecRepr)) exprCAnn) exprCAnn
                incrExpr = Just $ CAssignment (CVar iterator exprCAnn) (CBinary CAddOp (CVar iterator exprCAnn) (CConst (CIntConst (CInteger 1 DecRepr)) exprCAnn) exprCAnn) exprCAnn
            arrayInit <- genArrayInitialization False (level + 1) (CIndex cObj (CVar iterator exprCAnn) exprCAnn) expr'
            return [CFor initExpr condExpr incrExpr (CCompound (CBlockStmt <$> arrayInit) (buildCompoundAnn ann False False)) (buildStatementAnn ann before)]
        (FieldAssignmentsExpression {}) -> genStructInitialization False level cObj expr
        (OptionVariantExpression {}) -> genOptionInitialization False level cObj expr
        (EnumVariantExpression {}) -> genEnumInitialization False level cObj expr
        (FunctionExpression _ _ ann) -> do
            exprType <- getExprType expr
            structName <- genArrayWrapStructName exprType
            cExpr <- genExpression expr
            let exprCAnn = buildGenericAnn ann
                stmtAnn = buildStatementAnn ann before
                declAnn = buildDeclarationAnn ann False
                left = CUnary CIndOp (CCast
                    (CDeclaration [CTypeSpec $ CTypeDef structName] [(Just (CDeclarator Nothing [CPtrDeclr [] exprCAnn] [] exprCAnn), Nothing, Nothing)] declAnn)
                    cObj exprCAnn) exprCAnn
            return [CExpr (Just (CAssignment left cExpr exprCAnn)) stmtAnn]
        _ -> do
            cExpr <- genExpression expr
            exprType <- getExprType expr
            let ann = getAnnotation expr
            genArrayInitializationFromExpression level cObj cExpr exprType ann
    where
        genArrayInitializationFromExpression :: Integer ->
            CExpression ->
            CExpression ->
            TypeSpecifier ->
            SemanticAnns ->
            CGenerator [CStatement]
        genArrayInitializationFromExpression lvl cObj' cExpr ts ann = do
            case ts of
                -- | If the initializer is a vector, we must iterate
                (Vector ts' (K s)) -> do
                    let iterator = namefy $ "i" ++ show lvl
                        exprCAnn = buildGenericAnn ann
                        initExpr = Right $ CDeclaration
                            [CTypeSpec CSizeTType]
                            [(Just (CDeclarator (Just iterator) [] [] exprCAnn), Just (CInitExpr (CConst (CIntConst (CInteger 0 DecRepr)) exprCAnn) exprCAnn), Nothing)]
                            (buildDeclarationAnn ann False)
                        condExpr = Just $ CBinary CLeOp (CVar iterator exprCAnn) (CConst (CIntConst (CInteger s DecRepr)) exprCAnn) exprCAnn
                        incrExpr = Just $ CAssignment (CVar iterator exprCAnn) (CBinary CAddOp (CVar iterator exprCAnn) (CConst (CIntConst (CInteger 1 DecRepr)) exprCAnn) exprCAnn) exprCAnn
                    arrayInit <- genArrayInitializationFromExpression (lvl + 1) (CIndex cObj' (CVar iterator exprCAnn) exprCAnn) (CIndex cExpr (CVar iterator exprCAnn) exprCAnn) ts' ann
                    return [CFor initExpr condExpr incrExpr (CCompound (CBlockStmt <$> arrayInit) (buildCompoundAnn ann False False)) (buildStatementAnn ann (before && lvl == 0))]
                _ -> return [CExpr (Just (CAssignment cObj' cExpr (buildGenericAnn ann))) (buildStatementAnn ann (before && lvl == 0))]


genFieldInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -- | Current vector nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CExpression
    -> Identifier
    -> Expression SemanticAnns
    -> CGenerator [CStatement]
genFieldInitialization before level cObj field expr =
    case expr of
        FieldAssignmentsExpression _ _ ann ->
            genStructInitialization before level (CMember cObj field False (buildGenericAnn ann)) expr
        OptionVariantExpression _ ann ->
            genOptionInitialization before level (CMember cObj field False (buildGenericAnn ann)) expr
        VectorInitExpression _ _ ann ->
            genArrayInitialization before level (CMember cObj field False (buildGenericAnn ann)) expr
        EnumVariantExpression _ _ _ ann ->
            genEnumInitialization before level (CMember cObj field False (buildGenericAnn ann)) expr
        _ -> do
            exprType <- getExprType expr
            let ann = getAnnotation expr
            case exprType of
                Vector _ _ -> genArrayInitialization before level (CMember cObj field False (buildGenericAnn ann)) expr
                _ -> do
                    let exprCAnn = buildGenericAnn ann
                        declStmtAnn = buildStatementAnn ann before
                    cExpr <- genExpression expr
                    return [CExpr (Just $ CAssignment (CMember cObj field False exprCAnn) cExpr exprCAnn) declStmtAnn]

genStructInitialization ::
    -- | Prepend a line to the initialization expression 
    Bool
    -- | Current vector nesting level. This argument is used to
    -- generate the name of the iterator variable.
    -> Integer
    -> CExpression
    -> Expression SemanticAnns
    -> CGenerator [CStatement]
genStructInitialization before level cObj expr =
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (FieldAssignmentsExpression _ vas ann) -> genFieldAssignments before vas

        where

            genFieldAssignments :: Bool -> [FieldAssignment SemanticAnns] -> CGenerator [CStatement]
            genFieldAssignments _ [] = return []
            genFieldAssignments before' (FieldValueAssignment field expr' _: xs) = do
                fieldInit <- genFieldInitialization before' level cObj field expr'
                rest <- genFieldAssignments False xs
                return $ fieldInit ++ rest
            genFieldAssignments before' (FieldAddressAssignment field addr (SemAnn _ (ETy (SimpleType ts))):xs) = do
                let exprCAnn = buildGenericAnn ann
                    declStmtAnn = buildStatementAnn ann before'
                decl <- genCastDeclaration ts ann
                rest <- genFieldAssignments False xs
                return $ CExpr (Just $ CAssignment (
                                CMember cObj field False exprCAnn
                            ) (CCast decl (CConst (CIntConst (CInteger addr HexRepr)) exprCAnn) exprCAnn) exprCAnn) declStmtAnn : rest
            genFieldAssignments _ expr' = throwError $ InternalError $ "Unsupported initialization expression: " ++ show expr'

    _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr

genBlockItem :: Statement SemanticAnns -> CGenerator [CCompoundBlockItem]
genBlockItem (AssignmentStmt obj expr  _) = do
    objType <- getObjectType obj
    cObj <- genObject obj
    case objType of
        Vector _ _ ->
            case expr of
                (FunctionExpression _ _ ann) -> do
                    exprType <- getExprType expr
                    structName <- genArrayWrapStructName exprType
                    cExpr <- genExpression expr
                    let exprCAnn = buildGenericAnn ann
                        stmtAnn = buildStatementAnn ann True
                        declAnn = buildDeclarationAnn ann False
                        left = CUnary CIndOp (CCast
                            (CDeclaration [CTypeSpec $ CTypeDef structName] [(Just (CDeclarator Nothing [CPtrDeclr [] exprCAnn] [] exprCAnn), Nothing, Nothing)] declAnn) cObj exprCAnn) exprCAnn
                    return $ CBlockStmt <$> [CExpr (Just (CAssignment left cExpr exprCAnn)) stmtAnn]
                _ -> fmap CBlockStmt <$> genArrayInitialization True 0 cObj expr
        (Location _) -> do
            let ann = getAnnotation obj
            return $ CBlockStmt <$> [CExpr (Just (CUnary CIndOp cObj (buildGenericAnn ann))) (buildStatementAnn ann True)]
        _ -> case expr of
            (FieldAssignmentsExpression {}) -> fmap CBlockStmt <$> genStructInitialization True 0 cObj expr
            (OptionVariantExpression {}) -> fmap CBlockStmt <$> genOptionInitialization True 0 cObj expr
            (EnumVariantExpression {}) -> fmap CBlockStmt <$> genEnumInitialization True 0 cObj expr
            _ -> do
                cExpr <- genExpression expr
                let ann = getAnnotation expr
                return $ CBlockStmt <$>
                    [CExpr (Just (CAssignment cObj cExpr (buildGenericAnn ann))) (buildStatementAnn ann True)]
genBlockItem (Declaration identifier _ ts expr ann) =
  let exprCAnn = buildGenericAnn ann in
  case ts of
    Vector _ _ -> do
        let declStmt = buildDeclarationAnn ann True
            arrayDecl = genArraySizeDeclarator ts ann
        arrayInitialization <- fmap CBlockStmt <$> genArrayInitialization False 0 (CVar identifier exprCAnn) expr
        decls <- genDeclSpecifiers ts
        return $
            CBlockDecl (CDeclaration decls [
                (Just (CDeclarator (Just identifier) arrayDecl [] exprCAnn), Nothing, Nothing)] declStmt)
            : arrayInitialization
    _ -> case expr of
        (FieldAssignmentsExpression {}) -> do
            let declStmt = buildDeclarationAnn ann True
            structInitialization <- fmap CBlockStmt <$> genStructInitialization False 0 (CVar identifier exprCAnn) expr
            decls <- genDeclSpecifiers ts
            return $
                CBlockDecl (CDeclaration decls [(Just (CDeclarator (Just identifier) [] [] exprCAnn), Nothing, Nothing)] declStmt)
                : structInitialization
        (OptionVariantExpression {}) -> do
            let declStmt = buildDeclarationAnn ann True
            optionInitialization <- fmap CBlockStmt <$> genOptionInitialization False 0 (CVar identifier exprCAnn) expr
            decls <- genDeclSpecifiers ts
            return $
                CBlockDecl (CDeclaration decls [(Just (CDeclarator (Just identifier) [] [] exprCAnn), Nothing, Nothing)] declStmt)
                : optionInitialization
        (EnumVariantExpression {}) -> do
            let declStmt = buildDeclarationAnn ann True
            enumInitialization <- fmap CBlockStmt <$> genEnumInitialization False 0 (CVar identifier exprCAnn) expr
            decls <- genDeclSpecifiers ts
            return $
                CBlockDecl (CDeclaration decls [(Just (CDeclarator (Just identifier) [] [] exprCAnn), Nothing, Nothing)] declStmt)
                : enumInitialization
        _ -> do
            let declStmt = buildDeclarationAnn ann True
            decls <- genDeclSpecifiers ts
            cExpr <- genExpression expr
            return $ CBlockDecl <$>
                [CDeclaration decls [(Just (CDeclarator (Just identifier) [] [] exprCAnn), Just (CInitExpr cExpr exprCAnn), Nothing)] declStmt]
genBlockItem (IfElseStmt expr ifBlk elifsBlks elseBlk ann) = do
    cExpr <- genExpression expr
    cIfBlk <- concat <$> mapM genBlockItem ifBlk
    cElseBlk <- if null elseBlk then return Nothing
        else
            mapM genBlockItem elseBlk >>= (return . Just) . flip CCompound (buildCompoundAnn ann False True) . concat
    cAlts <- genAlternatives cElseBlk elifsBlks

    return $ CBlockStmt <$>
        [CIf cExpr (CCompound cIfBlk (buildCompoundAnn ann False True)) cAlts (buildStatementAnn ann True)]

    where
        genAlternatives :: Maybe CStatement -> [ElseIf SemanticAnns] -> CGenerator (Maybe CStatement)
        genAlternatives prev [] = return prev
        genAlternatives prev (ElseIf expr' blk ann' : xs) = do
            prev' <- genAlternatives prev xs
            cExpr' <- genExpression expr'
            cBlk <- concat <$> mapM genBlockItem blk
            return $ Just (CIf cExpr' (CCompound cBlk (buildCompoundAnn ann' False True)) prev' (buildStatementAnn ann' False))
genBlockItem (ForLoopStmt iterator iteratorTS initValue endValue breakCond body ann) = do
    let exprCAnn = buildGenericAnn ann
        declAnn = buildDeclarationAnn ann False
    initExpr <- genExpression initValue
    endExpr <- genExpression endValue
    condExpr <-
        case breakCond of
            Nothing -> return $ CBinary CLeOp (CVar iterator exprCAnn) endExpr exprCAnn
            Just break' -> do
                    cBreak <- genExpression break'
                    return $ CBinary CLndOp
                        (CBinary CLeOp (CVar iterator exprCAnn) endExpr exprCAnn) cBreak exprCAnn
    cBody <- concat <$> mapM genBlockItem body
    decls <- genDeclSpecifiers iteratorTS
    return $ CBlockStmt <$>
        [CFor
            -- | Initialization expression
            (Right $ CDeclaration decls
                [(Just (CDeclarator (Just iterator) [] [] exprCAnn), Just (CInitExpr initExpr exprCAnn), Nothing)]
                declAnn)
            -- | Condition expression 
            (Just condExpr)
            -- | Increment expression
            (Just $ CAssignment (CVar iterator exprCAnn)
                (CBinary CAddOp
                    (CVar iterator exprCAnn)
                    (CConst (CIntConst (CInteger 1 DecRepr)) exprCAnn)
                    exprCAnn)
                exprCAnn)
            -- | Body
            (CCompound cBody (buildCompoundAnn ann False True))
            (buildStatementAnn ann True)]
genBlockItem match@(MatchStmt expr matchCases ann) = do
    let exprCAnn = buildGenericAnn ann
    exprType <- getExprType expr
    (casePrefix, structName, paramsStructName) <-
        case exprType of
            -- | If the expression is an enumeration, the case identifier must 
            -- be prefixed with the enumeration identifier.
            (DefinedType enumId) -> return ((<::>) enumId, enumStructName enumId, enumParameterStructName enumId)
            (Option ts) -> do
                sname <- genOptionStructName ts
                pname <- genOptionParameterStructName ts
                return (id, sname, const pname)
            _ -> throwError $ InternalError $ "Unsupported match expression type: " ++ show expr
    case expr of
        (AccessObject {}) -> do
            cExpr <- genExpression expr
            case matchCases of
                -- | If there is only one case, we do not need to check the variant
                [m@(MatchCase identifier _ _ _)] -> do
                    cTs <- genDeclSpecifiers (DefinedType (paramsStructName identifier))
                    genMatchCase cTs cExpr m
                -- | The first one must add a preceding blank line
                m@(MatchCase identifier _ _ ann') : xs -> do
                    cTs <- genDeclSpecifiers (DefinedType (paramsStructName identifier))
                    rest <- genMatchCases cExpr casePrefix paramsStructName genMatchCase xs
                    cBlk <- flip CCompound (buildCompoundAnn ann' False True) <$> genMatchCase cTs cExpr m
                    return [CBlockStmt $ CIf
                        (CBinary CEqOp (CMember cExpr enumVariantsField False exprCAnn) (CVar (casePrefix identifier) (buildGenericAnn ann')) (buildGenericAnn ann'))
                        cBlk rest (buildStatementAnn ann' True)]
                _ -> throwError $ InternalError $ "Match statement without cases: " ++ show match
        _ -> do
            cExpr <- genExpression expr
            cTs <- genDeclSpecifiers (DefinedType structName)
            let decl = CDeclaration cTs
                    [(Just (CDeclarator (Just (namefy "match")) [] [] exprCAnn),
                    Just (CInitExpr cExpr exprCAnn),
                    Nothing)]
                    (buildDeclarationAnn ann True)
                cExpr' = CVar (namefy "match") exprCAnn
            case matchCases of
                [m@(MatchCase {})] -> do

                    -- | The annonymous match case uses temporary structure to hold the parameters
                    -- so we do not need to copy the parameters into a new structure.
                    -- Thus, we pass "undefined" to the function, since we can safely assume that
                    -- the function will not use them
                    cBlk <- genAnonymousMatchCase undefined cExpr' m
                    return [CBlockStmt $ CCompound (CBlockDecl decl : cBlk) (buildCompoundAnn ann True True)]
                m@(MatchCase identifier _ _ ann') : xs -> do
                    rest <- genMatchCases cExpr' casePrefix paramsStructName genAnonymousMatchCase xs
                    cBlk <- flip CCompound (buildCompoundAnn ann' False True) <$> genAnonymousMatchCase undefined cExpr' m
                    return [CBlockStmt $ CCompound (CBlockDecl decl : [CBlockStmt $ CIf
                        (CBinary CEqOp (CMember cExpr' enumVariantsField False exprCAnn) (CVar (casePrefix identifier) (buildGenericAnn ann')) (buildGenericAnn ann'))
                        cBlk rest (buildStatementAnn ann' True)]) (buildCompoundAnn ann True True)]
                _ -> throwError $ InternalError $ "Match statement without cases: " ++ show match

    where

        genMatchCases ::
            -- | The expression to match
            CExpression
            -- | A function to prefix the case identifier
            -> (Identifier -> Identifier)
            -- | A function to get the parameter struct name 
            -> (Identifier -> Identifier)
            -- | A function to generate a match case (inside the monad)
            -> ([CDeclarationSpecifier]
                -> CExpression
                -> MatchCase SemanticAnns
                -> CGenerator [CCompoundBlockItem])
            -- | The list of remaining match cases
            -> [MatchCase SemanticAnns]
            -> CGenerator (Maybe CStatement)
        -- | This should never happen
        genMatchCases _ _ _ _ [] = return Nothing
        -- | The last one does not need to check the variant
        genMatchCases cExpr _ paramsStructName genCase [m@(MatchCase identifier _ _ ann')] = do
            cTs <- genDeclSpecifiers (DefinedType (paramsStructName identifier))
            cBlk <- genCase cTs cExpr m
            return $ Just (CCompound cBlk (buildCompoundAnn ann' False True))
        genMatchCases cExpr casePrefix paramsStructName genCase (m@(MatchCase identifier _ _ ann') : xs) = do
            let cAnn = buildGenericAnn ann'
                cExpr' = CBinary CEqOp (CMember cExpr identifier False cAnn) (CVar (casePrefix identifier) cAnn) cAnn
            cTs <- genDeclSpecifiers (DefinedType (paramsStructName identifier))
            cBlk <- flip CCompound (buildCompoundAnn ann' False True) <$> genCase cTs cExpr m
            rest <- genMatchCases cExpr casePrefix paramsStructName genCase xs
            return $ Just (CIf cExpr' cBlk rest (buildStatementAnn ann' False))

        genAnonymousMatchCase ::
            [CDeclarationSpecifier]
            -> CExpression
            -> MatchCase SemanticAnns -> CGenerator [CCompoundBlockItem]
        genAnonymousMatchCase _ _ (MatchCase _ [] blk' _) = do
            concat <$> mapM genBlockItem blk'
        genAnonymousMatchCase _ cExpr (MatchCase variant params blk' _) = do
            let cAnn = buildGenericAnn ann
                cExpr' = CMember cExpr variant False cAnn
                newKeyVals = fromList $ zipWith
                    (\sym index -> (sym, CMember cExpr' (namefy (show (index :: Integer))) False cAnn)) params [0..]
            Control.Monad.Reader.local (union newKeyVals) $ concat <$> mapM genBlockItem blk'

        genMatchCase ::
            [CDeclarationSpecifier]
            -> CExpression
            -> MatchCase SemanticAnns
            -> CGenerator [CCompoundBlockItem]
        genMatchCase _ _ (MatchCase _ [] blk' _) = do
            concat <$> mapM genBlockItem blk'
        genMatchCase cTs cExpr (MatchCase variant params blk' ann') = do
            let cAnn = buildGenericAnn ann'
                cExpr' = CVar (namefy variant) cAnn
                newKeyVals = fromList $ zipWith
                    (\sym index -> (sym, CMember cExpr' (namefy (show (index :: Integer))) False cAnn)) params [0..]
                decl = CDeclaration cTs
                    [(Just (CDeclarator (Just (namefy variant)) [] [] cAnn),
                      Just (CInitExpr (CMember cExpr variant False cAnn) cAnn),
                      Nothing)]
                    (buildDeclarationAnn ann True)
            cBlk <- Control.Monad.Reader.local (union newKeyVals) $ concat <$> mapM genBlockItem blk'
            return $ CBlockDecl decl : cBlk

genBlockItem stmt = throwError $ InternalError $ "Unsupported statement: " ++ show stmt