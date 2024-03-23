module Generator.Expression where
import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Map
import Generator.Common


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


-- | Obtains the corresponding C type of a primitive type
genDeclSpecifiers :: TypeSpecifier -> CGenerator [CDeclarationSpecifier]
-- |  Unsigned integer types
genDeclSpecifiers UInt8  = return [CTypeSpec CUInt8Type]
genDeclSpecifiers UInt16 = return [CTypeSpec CUInt16Type]
genDeclSpecifiers UInt32 = return [CTypeSpec CUInt32Type]
genDeclSpecifiers UInt64 = return [CTypeSpec CUInt64Type]
-- | Signed integer types
genDeclSpecifiers Int8   = return [CTypeSpec CInt8Type]
genDeclSpecifiers Int16  = return [CTypeSpec CInt16Type]
genDeclSpecifiers Int32  = return [CTypeSpec CInt32Type]
genDeclSpecifiers Int64  = return [CTypeSpec CInt64Type]
-- | Other primitive types
genDeclSpecifiers USize  = return [CTypeSpec CSizeTType]
genDeclSpecifiers Bool   = return [CTypeSpec CBoolType]
genDeclSpecifiers Char   = return [CTypeSpec CCharType]
-- | Primitive type
genDeclSpecifiers (DefinedType typeIdentifier) = return [CTypeSpec $ CTypeDef typeIdentifier]
-- | Vector type
-- The type of the vector is the type of the elements
genDeclSpecifiers (Vector ts _) = genDeclSpecifiers ts
-- | Option type
genDeclSpecifiers (Option (DynamicSubtype _))  = return [CTypeSpec $ CTypeDef optionDyn]
genDeclSpecifiers (Option ts)                  = do
    optName <- genOptionStructName ts
    return [CTypeSpec $ CTypeDef optName]
-- Non-primitive types:
-- | Dynamic subtype
genDeclSpecifiers (DynamicSubtype _)           = return [CTypeSpec $ CTypeDef dynamicStruct]
-- | Pool type
genDeclSpecifiers (Pool _ _)                   = return [CTypeSpec $ CTypeDef pool]
genDeclSpecifiers (MsgQueue _ _)               = return [CTypeSpec $ CTypeDef msgQueue]
genDeclSpecifiers (Location ts)                = genDeclSpecifiers ts
genDeclSpecifiers (AccessPort ts)              = genDeclSpecifiers ts
genDeclSpecifiers (Allocator _)                = return [CTypeSpec $ CTypeDef pool]
-- | Type of the ports
genDeclSpecifiers (SinkPort {})                = return [CTypeSpec $ CTypeDef sinkPort]
genDeclSpecifiers (OutPort {})                 = return [CTypeSpec $ CTypeDef outPort]
genDeclSpecifiers (InPort {})                  = return [CTypeSpec $ CTypeDef inPort]
genDeclSpecifiers t                            = throwError $ InternalError $ "Unsupported type: " ++ show t

genArrayDeclarator :: TypeSpecifier -> SemanticAnns -> CGenerator CDeclarator
genArrayDeclarator (Vector ts _) ann = do
    return $ CDeclarator Nothing (CPtrDeclr [] ann : reverse (genArraySizeDeclarator ts)) [] ann
    where
        genArraySizeDeclarator :: TypeSpecifier -> [CDerivedDeclarator]
        genArraySizeDeclarator (Vector ts' (K s)) = 
            CArrDeclr [] (CArrSize False (CConst (CIntConst (CInteger s DecRepr)) ann)) ann : genArraySizeDeclarator ts'
        genArraySizeDeclarator _ = []
genArrayDeclarator ts _ = throwError $ InternalError $ "Invalid type specifier, not an array: " ++ show ts

genCastDeclaration :: TypeSpecifier -> SemanticAnns -> CGenerator CDeclaration
genCastDeclaration (DynamicSubtype ts@(Vector _ _)) ann = do
    -- We must obtain the declaration specifier of the vector
    specs <- genDeclSpecifiers ts
    decls <- genArrayDeclarator ts ann
    return $ CDeclaration specs [(Just decls, Nothing, Nothing)] ann
genCastDeclaration (DynamicSubtype ts) ann = do
    specs <- genDeclSpecifiers ts
    return $ CDeclaration specs [(Just (CDeclarator Nothing [CPtrDeclr [] ann] [] ann), Nothing, Nothing)] ann
genCastDeclaration ts ann = do
    specs <- genDeclSpecifiers ts
    return $ CDeclaration specs [] ann

genExpression :: Expression SemanticAnns -> CGenerator CExpression
genExpression (AccessObject obj) = genObject obj
genExpression (BinOp op left right ann) = do
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
                        flip (CCast decl) ann <$> genExpression left
            _ -> genExpression left
    cRight <-
        case right of
            (BinOp {}) -> do
                ts <- getExprType right
                case ts of
                    Bool -> genExpression right
                    _ -> do
                        decl <- genCastDeclaration ts ann
                        flip (CCast decl) ann <$> genExpression right
            _ -> genExpression right
    return $ CBinary (cBinOp op) cLeft cRight ann
genExpression (Constant c anns) =
    case c of
        (I _ i) -> return $ CConst (CIntConst (CInteger i DecRepr)) anns
        (B True) -> return $ CConst (CIntConst (CInteger 1 DecRepr)) anns
        (B False) -> return $ CConst (CIntConst (CInteger 0 DecRepr)) anns
        (C char) -> return $ CConst (CCharConst (CChar char)) anns
genExpression (Casting expr ts ann) = do
    decl <- genCastDeclaration ts ann
    cExpr <- genExpression expr
    return $ CCast decl cExpr ann
genExpression (ReferenceExpression _ obj ann) = do
    objType <- getObjectType obj
    cObj <- genObject obj
    case objType of
        -- | If it is a vector, we need to generate the address of the data
        (DynamicSubtype (Vector _ _)) -> do
            -- We must obtain the declaration specifier of the vector
            decl <- genCastDeclaration objType ann
            return $ CCast decl (CMember cObj "data" False ann) ann
            -- | Else, we print the address to the data
        (DynamicSubtype _) -> do
            decl <- genCastDeclaration objType ann
            return $ CCast decl (CMember cObj "data" False ann) ann
        (Vector {}) -> return cObj
        _ -> return $ CUnary CAdrOp cObj ann
genExpression expr@(FunctionExpression name args ann) = do
    -- Obtain the substitutions map
    subs <- ask
    -- If the identifier is in the substitutions map, use the substituted identifier
    let identifier = fromMaybe name (Data.Map.lookup name subs)
    argsAnns <- getParameters expr
    cArgs <- zipWithM
            (\pExpr (Parameter _ ts) -> do
                cParamExpr <- genExpression pExpr
                case ts of
                    Vector {} -> do
                        structName <- genArrayWrapStructName ts
                        return $  CUnary CIndOp
                            (CCast (
                                CDeclaration [CTypeSpec $ CTypeDef structName]
                                    [(Just (CDeclarator Nothing [CPtrDeclr [] ann] [] ann), Nothing, Nothing)] ann
                                ) cParamExpr ann) ann
                    _ -> return cParamExpr) args argsAnns
    expType <- getExprType expr
    case expType of
        Vector {} -> return $ CMember (CCall (CVar identifier ann) cArgs ann) "array" False ann
        _ -> return $ CCall (CVar identifier ann) cArgs ann
genExpression (MemberFunctionAccess obj ident args ann) = do
    -- Generate the C code for the object
    cObj <- genObject obj
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
                                    [(Just (CDeclarator Nothing [CPtrDeclr [] ann] [] ann), Nothing, Nothing)] ann
                                ) cParamExpr ann) ann
                    _ -> return cParamExpr) args
    objType <- getObjectType obj
    case objType of
        (DefinedType classId) ->
            -- For the time being, the only way to access the member function of a class
            -- is through the self object. Since the function in C expeects a reference to
            -- the object, we would need to pass the address of the object. In order to avoid
            -- derefecerencing a reference (&*self), we shortcut the process and pass the object
            -- directly. This might change in the future if we finally implement traits.
            return $ CCall (CVar (classId <::> ident) ann) (CVar "self" ann : cArgs) ann
        -- | If the left hand size is a class:
        AccessPort (DefinedType {}) ->
            return $
                CCall (CMember cObj ident False ann)
                      (CMember cObj thatField False ann : cArgs) ann
        -- | If the left hand side is a pool:
        AccessPort (Allocator {}) ->
            return $
                CCall (CVar (poolMethodName ident) ann) (cObj : cArgs) ann
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
                                CCall (CVar (msgQueueMethodName ident) ann) (cObj :
                                    (flip (CCast (CDeclaration
                                        [CTypeSpec CVoidType]
                                        [(Just (CDeclarator Nothing [CPtrDeclr [] ann] [] ann), Nothing, Nothing)]
                                        ann)) ann <$> cArgs))  ann
                        _ -> return $
                                CCall (CVar (msgQueueMethodName ident) ann) (cObj: 
                                    (flip (CCast (CDeclaration
                                        [CTypeSpec CVoidType]
                                        [(Just (CDeclarator Nothing [CPtrDeclr [] ann] [] ann), Nothing, Nothing)]
                                        ann)) ann . flip (CUnary CAdrOp) ann <$> cArgs))  ann
                _ -> throwError $ InternalError $ "invalid params for message queue send: " ++ show args
        -- | Anything else should not happen
        _ -> throwError $ InternalError $ "unsupported member function access to object: " ++ show obj
genExpression o = throwError $ InternalError $ "Unsupported expression: " ++ show o

genObject :: Object SemanticAnns -> CGenerator CExpression
genObject (Variable identifier ann) = do
    -- Obtain the substitutions map
    subs <- ask
    -- If the identifier is in the substitutions map, use the substituted identifier
    let ident = fromMaybe identifier (Data.Map.lookup identifier subs)
    -- Return the C identifier
    return $ CVar ident ann
genObject (VectorIndexExpression obj index anns) = do
    -- Generate the C code for the object
    cObj <- genObject obj
    -- Generate the C code for the index
    cIndex <- genExpression index
    -- Return the C code for the vector index expression
    return $ CIndex cObj cIndex anns
genObject (VectorSliceExpression obj lower _ ann) =
    case lower of
        KC (I _ lowInteger) -> do
            cObj <- genObject obj
            return $ CUnary CAdrOp (CIndex cObj (CConst (CIntConst (CInteger lowInteger DecRepr)) ann) ann) ann
        _ -> throwError $ InternalError ("Invalid constant expression: " ++ show lower)
genObject (MemberAccess obj identifier ann) = do
    cObj <- genObject obj
    objType <- getObjectType obj
    case objType of
        (Location {}) -> return $ CMember cObj identifier True ann
        _ -> return $ CMember cObj identifier False ann
genObject (DereferenceMemberAccess obj identifier ann) = do
    cObj <- genObject obj
    return $ CMember cObj identifier True ann
genObject (Dereference obj ann) = do
    objType <- getObjectType obj
    case objType of
        -- | A dereference to a vector is printed as the name of the vector
        (Reference _ (Vector _ _)) -> genObject obj
        _ -> do
            cObj <- genObject obj
            return $ CUnary CIndOp cObj ann
-- | If the expression is a dynamic subtype treated as its base type, we need to
-- check if it is a vector
genObject o@(Undyn obj ann) = do
    objType <- getObjectType obj
    cObj <- genObject obj
    case objType of
        -- | If it is a vector, we need to generate the address of the data
        (DynamicSubtype (Vector _ _)) -> do
            -- We must obtain the declaration specifier of the vector
            decl <- genCastDeclaration objType ann
            return $ CCast decl (CMember cObj "data" False ann) ann
            -- | Else, we print the derefence to the data
        (DynamicSubtype _) -> do
            decl <- genCastDeclaration objType ann
            return $ CUnary CIndOp (CCast decl (CMember cObj "data" False ann) ann) ann
        -- | An undyn can only be applied to a dynamic subtype. We are not
        -- supposed to reach here. If we are here, it means that the semantic
        -- analysis is wrong.
        _ -> throwError $ InternalError $ "Unsupported object: " ++ show o
