module Generator.Expression where
import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Map
import Generator.Common
import Annotations


cBinOp :: Op -> CBinaryOp
cBinOp Multiplication = CMulOp
cBinOp Division = CDivOp
cBinOp Addition = CAddOp
cBinOp Subtraction = CSubOp
cBinOp Modulo = CRmdOp
cBinOp BitwiseLeftShift = CShlOp
cBinOp BitwiseRightShift = CShrOp
cBinOp RelationalLT = CLeOp
cBinOp RelationalLTE = CLeqOp
cBinOp RelationalGT = CGrOp
cBinOp RelationalGTE = CGeqOp
cBinOp RelationalEqual = CEqOp
cBinOp RelationalNotEqual = CNeqOp
cBinOp BitwiseAnd = CAndOp
cBinOp BitwiseOr = COrOp
cBinOp BitwiseXor = CXorOp
cBinOp LogicalAnd = CLndOp
cBinOp LogicalOr = CLorOp

genMemberFunctionAccess :: 
    Object SemanticAnns 
    -> Identifier 
    -> [ConstExpression SemanticAnns]
    -> [Expression SemanticAnns] 
    -> SemanticAnns 
    -> CSourceGenerator CExpression
genMemberFunctionAccess obj ident constArgs args ann = do
    let cAnn = buildGenericAnn ann
        declAnn = buildDeclarationAnn ann False
    -- Generate the C code for the object
    cObj <- genObject obj
    -- Generate the C code for the const generics
    cConstArgs <- mapM genConstExpression constArgs
    -- Generate the C code for the parameters
    cArgs <- mapM
            (\pExpr -> do
                cParamExpr <- genExpression pExpr
                cParamExprTs <- getExprType pExpr
                case cParamExprTs of
                    Vector {} -> do
                        structName <- genArrayWrapStructName cParamExprTs
                        return $  CUnary CIndOp
                            (CCast (
                                CDeclaration [CTypeSpec $ CTypeDef structName]
                                    [(Just (CDeclarator Nothing [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] declAnn
                                ) cParamExpr cAnn) cAnn
                    _ -> return cParamExpr) args
    objType <- getObjectType obj
    case objType of
        (Reference _ ts) ->
            case ts of
                -- | If the left hand size is a class:
                (DefinedType classId) ->
                    return $ CCall (CVar (classId <::> ident) cAnn) (cObj : (cConstArgs ++ cArgs)) cAnn
                -- | Anything else should not happen
                _ -> throwError $ InternalError $ "unsupported member function access to object: " ++ show obj
        (DefinedType classId) ->
            case obj of
                (Dereference _ _) ->
                    -- | If we are here, it means that we are dereferencing the self object
                    return $ CCall (CVar (classId <::> ident) cAnn) (CVar "self" cAnn : cArgs) cAnn
                    -- | If the left hand size is a class:
                _ -> return $ CCall (CVar (classId <::> ident) cAnn) (cObj : (cConstArgs ++ cArgs)) cAnn
        AccessPort (DefinedType {}) ->
            return $
                CCall (CMember cObj ident False cAnn)
                      (CMember cObj thatField False cAnn : cArgs) cAnn
        -- | If the left hand side is a pool:
        AccessPort (Allocator {}) ->
            return $
                CCall (CVar (poolMethodName ident) cAnn) (cObj : (cConstArgs ++ cArgs)) cAnn
        -- | If the left hand side is a message queue:
        OutPort {} ->
            -- | If it is a send, the first parameter is the object to be sent. The
            -- function is expecting to receive a reference to that object.
            case args of
                [element] -> do
                    paramTs <- getExprType element
                    case paramTs of
                        Vector {} ->
                            return $
                                CCall (CVar (msgQueueMethodName ident) cAnn) (cObj :
                                    (flip (CCast (CDeclaration
                                        [CTypeSpec CVoidType]
                                        [(Just (CDeclarator Nothing [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)]
                                        declAnn)) cAnn <$> cArgs))  cAnn
                        _ -> return $
                                CCall (CVar (msgQueueMethodName ident) cAnn) (cObj: 
                                    (flip (CCast (CDeclaration
                                        [CTypeSpec CVoidType]
                                        [(Just (CDeclarator Nothing [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)]
                                        declAnn)) cAnn . flip (CUnary CAdrOp) cAnn <$> cArgs))  cAnn
                _ -> throwError $ InternalError $ "invalid params for message queue send: " ++ show args
        -- | Anything else should not happen
        _ -> throwError $ InternalError $ "unsupported member function access to object: " ++ show obj

genExpression :: Expression SemanticAnns -> CSourceGenerator CExpression
genExpression (AccessObject obj) = do
    cObj <- genObject obj
    cObjType <- getObjectType obj
    case cObjType of
        (Location _) ->
            return $ CUnary CIndOp cObj (buildGenericAnn (getAnnotation obj))
        _ -> return cObj
genExpression (BinOp op left right ann) = do
    let cAnn = buildGenericAnn ann
    -- | We need to check if the left and right expressions are binary operations
    -- If they are, we need to cast them to ensure that the resulting value
    -- is truncated to the correct type
    cLeft <-
        case left of
            (BinOp {}) -> do
                ts <- getExprType left
                case ts of
                    Bool -> genExpression left
                    _ -> do
                        decl <- genCastDeclaration ts ann
                        flip (CCast decl) cAnn <$> genExpression left
            _ -> genExpression left
    cRight <-
        case right of
            (BinOp {}) -> do
                ts <- getExprType right
                case ts of
                    Bool -> genExpression right
                    _ -> do
                        decl <- genCastDeclaration ts ann
                        flip (CCast decl) cAnn <$> genExpression right
            _ -> genExpression right
    return $ CBinary (cBinOp op) cLeft cRight cAnn
genExpression (Constant c ann) = do
    let cAnn = buildGenericAnn ann 
    case c of
        (I i _) -> 
            let cInteger = genInteger i in
            return $ CConst (CIntConst cInteger) cAnn
        (B True) -> return $ CConst (CIntConst (CInteger 1 CDecRepr)) cAnn
        (B False) -> return $ CConst (CIntConst (CInteger 0 CDecRepr)) cAnn
        (C char) -> return $ CConst (CCharConst (CChar char)) cAnn
genExpression (Casting expr ts ann) = do
    let cAnn = buildGenericAnn ann 
    decl <- genCastDeclaration ts ann
    cExpr <- genExpression expr
    return $ CCast decl cExpr cAnn
genExpression (ReferenceExpression _ obj ann) = do
    let cAnn = buildGenericAnn ann 
    objType <- getObjectType obj
    cObj <- genObject obj
    case objType of
        -- | If it is a vector, we need to generate the address of the data
        (DynamicSubtype (Vector _ _)) -> do
            -- We must obtain the declaration specifier of the vector
            decl <- genCastDeclaration objType ann
            return $ CCast decl (CMember cObj "data" False cAnn) cAnn
            -- | Else, we print the address to the data
        (DynamicSubtype _) -> do
            decl <- genCastDeclaration objType ann
            return $ CCast decl (CMember cObj "data" False cAnn) cAnn
        (Vector {}) -> return cObj
        _ -> return $ CUnary CAdrOp cObj cAnn
genExpression expr@(FunctionExpression name constArgs args ann) = do
    let cAnn = buildGenericAnn ann
        declAnn = buildDeclarationAnn ann False
    -- Obtain the substitutions map
    subs <- ask
    -- If the identifier is in the substitutions map, use the substituted identifier
    let identifier = fromMaybe (CVar name cAnn) (Data.Map.lookup name subs)
    argsAnns <- getParameters expr
    cConstArgs <- mapM genConstExpression constArgs
    cArgs <- zipWithM
            (\pExpr (Parameter _ ts) -> do
                cParamExpr <- genExpression pExpr
                case ts of
                    Vector {} -> do
                        structName <- genArrayWrapStructName ts
                        return $  CUnary CIndOp
                            (CCast (
                                CDeclaration [CTypeSpec $ CTypeDef structName]
                                    [(Just (CDeclarator Nothing [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] declAnn
                                ) cParamExpr cAnn) cAnn
                    _ -> return cParamExpr) args argsAnns
    expType <- getExprType expr
    case expType of
        Vector {} -> return $ CMember (CCall identifier cArgs cAnn) "array" False cAnn
        _ -> return $ CCall identifier (cConstArgs ++ cArgs) cAnn
genExpression (MemberFunctionAccess obj ident constArgs args ann) = do
    genMemberFunctionAccess obj ident constArgs args ann
genExpression (DerefMemberFunctionAccess obj ident constArgs args ann) =
    genMemberFunctionAccess obj ident constArgs args ann
genExpression (IsEnumVariantExpression obj enum variant ann) = do
    let cAnn = buildGenericAnn ann 
    cObj <- genObject obj
    return $ CBinary CEqOp (CMember cObj enumVariantsField False cAnn) (CVar (enum <::> variant) cAnn) cAnn
genExpression (IsOptionVariantExpression obj NoneLabel ann) = do
    let cAnn = buildGenericAnn ann 
    cObj <- genObject obj
    return $ CBinary CEqOp (CMember cObj enumVariantsField False cAnn) (CVar optionNoneVariant cAnn) cAnn
genExpression (IsOptionVariantExpression obj SomeLabel ann) = do
    let cAnn = buildGenericAnn ann 
    cObj <- genObject obj
    return $ CBinary CEqOp (CMember cObj enumVariantsField False cAnn) (CVar optionSomeVariant cAnn) cAnn
genExpression o = throwError $ InternalError $ "Unsupported expression: " ++ show o

genConstExpression :: ConstExpression SemanticAnns -> CSourceGenerator CExpression
genConstExpression (KC (I i _) ann) = do
    let cInteger = genInteger i
    return $ CConst (CIntConst cInteger) (buildGenericAnn ann)
genConstExpression (KC (B True) ann) = return $ CConst (CIntConst (CInteger 1 CDecRepr)) (buildGenericAnn ann)
genConstExpression (KC (B False) ann) = return $ CConst (CIntConst (CInteger 0 CDecRepr)) (buildGenericAnn ann)
genConstExpression (KC (C char) ann) = return $ CConst (CCharConst (CChar char)) (buildGenericAnn ann)
genConstExpression (KV ident ann) = return $ CVar ident (buildGenericAnn ann)

genObject :: Object SemanticAnns -> CSourceGenerator CExpression
genObject (Variable identifier ann) = do
    let cAnn = buildGenericAnn ann
    -- Obtain the substitutions map
    subs <- ask
    -- If the identifier is in the substitutions map, use the substituted identifier
    let ident = fromMaybe (CVar identifier cAnn) (Data.Map.lookup identifier subs)
    -- Return the C identifier
    return ident
genObject (VectorIndexExpression obj index ann) = do
    let cAnn = buildGenericAnn ann
    -- Generate the C code for the object
    cObj <- genObject obj
    -- Generate the C code for the index
    cIndex <- genExpression index
    -- Return the C code for the vector index expression
    return $ CIndex cObj cIndex cAnn
genObject (VectorSliceExpression obj lower _ ann) = do
    let cAnn = buildGenericAnn ann
    cLower <- genExpression lower
    cObj <- genObject obj
    return $ CUnary CAdrOp (CIndex cObj cLower cAnn) cAnn
genObject (MemberAccess obj identifier ann) = do
    let cAnn = buildGenericAnn ann
    cObj <- genObject obj
    objType <- getObjectType obj
    case objType of
        (Location {}) -> return $ CMember cObj identifier True cAnn
        _ -> return $ CMember cObj identifier False cAnn
genObject (DereferenceMemberAccess obj identifier ann) = do
    let cAnn = buildGenericAnn ann
    cObj <- genObject obj
    return $ CMember cObj identifier True cAnn
genObject (Dereference obj ann) = do
    let cAnn = buildGenericAnn ann
    objType <- getObjectType obj
    case objType of
        -- | A dereference to a vector is printed as the name of the vector
        (Reference _ (Vector _ _)) -> genObject obj
        _ -> do
            cObj <- genObject obj
            return $ CUnary CIndOp cObj cAnn
-- | If the expression is a dynamic subtype treated as its base type, we need to
-- check if it is a vector
genObject o@(Undyn obj ann) = do
    let cAnn = buildGenericAnn ann
    objType <- getObjectType obj
    cObj <- genObject obj
    case objType of
        -- | If it is a vector, we need to generate the address of the data
        (DynamicSubtype (Vector _ _)) -> do
            -- We must obtain the declaration specifier of the vector
            decl <- genCastDeclaration objType ann
            return $ CCast decl (CMember cObj "data" False cAnn) cAnn
            -- | Else, we print the derefence to the data
        (DynamicSubtype _) -> do
            decl <- genCastDeclaration objType ann
            return $ CUnary CIndOp (CCast decl (CMember cObj "data" False cAnn) cAnn) cAnn
        -- | An undyn can only be applied to a dynamic subtype. We are not
        -- supposed to reach here. If we are here, it means that the semantic
        -- analysis is wrong.
        _ -> throwError $ InternalError $ "Unsupported object: " ++ show o
