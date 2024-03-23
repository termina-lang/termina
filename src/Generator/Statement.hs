module Generator.Statement where

import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Control.Monad.Except
import Generator.Common

import Generator.Expression
import Annotations

genEnumInitialization ::
  Integer ->
  -- | Enum
  CExpression ->
  -- |  The initialization expression
  Expression SemanticAnns ->
  CGenerator [CStatement]
genEnumInitialization level cObj expr =
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (EnumVariantExpression ts variant params ann) -> do
        cParams <- zipWithM (\e index -> genFieldInitialization level (CMember cObj variant False ann) (namefy (show (index :: Integer))) e) params [0..]
        return $ CExpr (Just (CAssignment (CMember cObj enumVariantsField False ann) (CVar (ts <::> variant) ann) ann)) ann : concat cParams
    _ -> error "Incorrect expression"

genOptionInitialization ::
    Integer
    -> CExpression
    -> Expression SemanticAnns
    -> CGenerator [CStatement]
genOptionInitialization level cObj expr =
    case expr of
        (OptionVariantExpression (Some e) ann) -> do
            fieldInitalization <- genFieldInitialization level (CMember cObj optionSomeVariant False ann) optionSomeField e
            return $
                CExpr (Just (CAssignment (CMember cObj enumVariantsField False ann) (CVar optionSomeVariant ann) ann)) ann :
                fieldInitalization
        (OptionVariantExpression None ann) ->
            return [CExpr (Just (CAssignment (CMember cObj enumVariantsField False ann) (CVar optionNoneVariant ann) ann)) ann]
        _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr

genArrayInitialization ::
    -- | Current vector nesting level. This argument is used to
    -- generate the name of the iterator variable.
    Integer
    -> CExpression
    -> Expression SemanticAnns
    -> CGenerator [CStatement]
genArrayInitialization level cObj expr = do
    let iterator = namefy $ "i" ++ show level
    case expr of
        (FieldAssignmentsExpression {}) -> genStructInitialization level cObj expr
        (OptionVariantExpression {}) -> genOptionInitialization level cObj expr
        (VectorInitExpression expr' (K s) ann) -> do
            let initExpr = Right $ CDeclaration [CTypeSpec CSizeTType] [(Just (CDeclarator (Just iterator) [] [] ann), Just (CInitExpr (CConst (CIntConst (CInteger 0 DecRepr)) ann) ann), Nothing)] ann
                condExpr = Just $ CBinary CLeOp (CVar iterator ann) (CConst (CIntConst (CInteger s DecRepr)) ann) ann
                incrExpr = Just $ CAssignment (CVar iterator ann) (CBinary CAddOp (CVar iterator ann) (CConst (CIntConst (CInteger 1 DecRepr)) ann) ann) ann
            arrayInit <- genArrayInitialization (level + 1) (CIndex cObj (CVar iterator ann) ann) expr'
            return [CFor initExpr condExpr incrExpr (CCompound (CBlockStmt <$> arrayInit) ann) ann]
        (EnumVariantExpression {}) -> genEnumInitialization level cObj expr
        (FunctionExpression _ _ ann) -> do
            exprType <- getExprType expr
            structName <- genArrayWrapStructName exprType
            cExpr <- genExpression expr
            let left = CUnary CIndOp (CCast (CDeclaration [CTypeSpec $ CTypeDef structName] [(Just (CDeclarator Nothing [CPtrDeclr [] ann] [] ann), Nothing, Nothing)] ann) cObj ann) ann
            return [CExpr (Just (CAssignment left cExpr ann)) ann]
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
            let iterator = namefy $ "i" ++ show lvl
            case ts of
                -- \| If the initializer is a vector, we must iterate
                (Vector ts' (K s)) -> do
                    let initExpr = Right $ CDeclaration [CTypeSpec CSizeTType] [(Just (CDeclarator (Just iterator) [] [] ann), Just (CInitExpr (CConst (CIntConst (CInteger 0 DecRepr)) ann) ann), Nothing)] ann
                        condExpr = Just $ CBinary CLeOp (CVar iterator ann) (CConst (CIntConst (CInteger s DecRepr)) ann) ann
                        incrExpr = Just $ CAssignment (CVar iterator ann) (CBinary CAddOp (CVar iterator ann) (CConst (CIntConst (CInteger 1 DecRepr)) ann) ann) ann
                    arrayInit <- genArrayInitializationFromExpression (lvl + 1) (CIndex cObj' (CVar iterator ann) ann) (CIndex cExpr (CVar iterator ann) ann) ts' ann
                    return [CFor initExpr condExpr incrExpr (CCompound (CBlockStmt <$> arrayInit) ann) ann]
                _ -> return [CExpr (Just (CAssignment cObj' cExpr ann)) ann]


genFieldInitialization ::
    -- | Current vector nesting level. This argument is used to
    -- generate the name of the iterator variable.
    Integer
    -> CExpression
    -> Identifier
    -> Expression SemanticAnns
    -> CGenerator [CStatement]
genFieldInitialization level cObj field expr =
    case expr of
        FieldAssignmentsExpression _ _ ann ->
            genStructInitialization level (CMember cObj field False ann) expr
        OptionVariantExpression _ ann ->
            genOptionInitialization level (CMember cObj field False ann) expr
        VectorInitExpression _ _ ann ->
            genArrayInitialization level (CMember cObj field False ann) expr
        EnumVariantExpression _ _ _ ann ->
            genEnumInitialization level (CMember cObj field False ann) expr
        _ -> do
            exprType <- getExprType expr
            let ann = getAnnotation expr
            case exprType of
                Vector _ _ -> genArrayInitialization level (CMember cObj field False ann) expr
                _ -> do
                    cExpr <- genExpression expr
                    return [CExpr (Just $ CAssignment (CMember cObj field False ann) cExpr ann) ann]

genStructInitialization ::
    -- | Current vector nesting level. This argument is used to
    -- generate the name of the iterator variable.
    Integer
    -> CExpression
    -> Expression SemanticAnns
    -> CGenerator [CStatement]
genStructInitialization level cObj expr =
  case expr of
    -- \| This function can only be called with a field values assignments expressions
    (FieldAssignmentsExpression _ vas ann) ->
        foldM (
            \acc assignment ->
                case assignment of
                FieldValueAssignment field expr' _ -> do
                    fieldInit <- genFieldInitialization level cObj field expr'
                    return $ acc ++ fieldInit
                FieldAddressAssignment field addr (SemAnn _ (ETy (SimpleType ts))) -> do
                    decl <- genCastDeclaration ts ann
                    return $ CExpr (Just $ CAssignment (
                                    CMember cObj field False ann
                                ) (CCast decl (CConst (CIntConst (CInteger addr HexRepr)) ann) ann) ann) ann : acc
                _ -> throwError $ InternalError $ "Unsupported initialization expression: " ++ show expr
            ) [] vas
    _ -> throwError $ InternalError $ "Incorrect initialization expression: " ++ show expr


genStatement :: Statement SemanticAnns -> CGenerator [CStatement]
genStatement (AssignmentStmt obj expr  _) = do
    objType <- getObjectType obj
    cObj <- genObject obj
    case objType of
        Vector _ _ ->
            case expr of
                (FunctionExpression _ _ ann) -> do
                    exprType <- getExprType expr
                    structName <- genArrayWrapStructName exprType
                    cExpr <- genExpression expr
                    let left = CUnary CIndOp (CCast (CDeclaration [CTypeSpec $ CTypeDef structName] [(Just (CDeclarator Nothing [CPtrDeclr [] ann] [] ann), Nothing, Nothing)] ann) cObj ann) ann
                    return [CExpr (Just (CAssignment left cExpr ann)) ann]
                _ -> genArrayInitialization 0 cObj expr
        (Location _) -> do
            let ann = getAnnotation obj
            return [CExpr (Just (CUnary CIndOp cObj ann)) ann]
        _ -> case expr of
            (FieldAssignmentsExpression {}) -> genStructInitialization 0 cObj expr
            (OptionVariantExpression {}) -> genOptionInitialization 0 cObj expr
            (EnumVariantExpression {}) -> genEnumInitialization 0 cObj expr
            _ -> do
                cExpr <- genExpression expr
                let ann = getAnnotation expr
                return [CExpr (Just (CAssignment cObj cExpr ann)) ann]
genStatement stmt = throwError $ InternalError $ "Unsupported statement: " ++ show stmt