module Generator.CCCodeGen.Expression where

import AST.Seman
import Generator.LanguageC.CompCertC
import Semantic.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Map
import Generator.CCCodeGen.Common
import Utils.Annotations


cBinOp :: Op -> CBinaryOp
cBinOp Multiplication = COpMul
cBinOp Division = COpDiv
cBinOp Addition = COpAdd
cBinOp Subtraction = COpSub
cBinOp Modulo = COpMod
cBinOp BitwiseLeftShift = COpShl
cBinOp BitwiseRightShift = COpShr
cBinOp RelationalLT = COpLt
cBinOp RelationalLTE = COpLe
cBinOp RelationalGT = COpGt
cBinOp RelationalGTE = COpGe
cBinOp RelationalEqual = COpEq
cBinOp RelationalNotEqual = COpNe
cBinOp BitwiseAnd = COpAnd
cBinOp BitwiseOr = COpOr
cBinOp BitwiseXor = COpXor
cBinOp LogicalAnd = error "Logical and is codified as a sequential expression"
cBinOp LogicalOr = error "Logical or is codified as a sequential expression"

genConstExpression :: ConstExpression SemanticAnn -> CSourceGenerator CExpression
genConstExpression c@(KC (I i _) ann) = do
    let cInteger = getCInteger i
    ctype <- getConstExprType c >>= genType noqual
    return $ CExprConstant (CIntConst cInteger) ctype (buildGenericAnn ann)
genConstExpression (KC (B True) ann) = 
    return $ CExprConstant (CIntConst (CInteger 1 CDecRepr)) (CTBool noqual) (buildGenericAnn ann)
genConstExpression (KC (B False) ann) = 
    return $ CExprConstant (CIntConst (CInteger 0 CDecRepr)) (CTBool noqual) (buildGenericAnn ann)
genConstExpression (KC (C char) ann) = 
    return $ CExprConstant (CCharConst (CChar char)) (CTChar noqual) (buildGenericAnn ann)
genConstExpression c@(KV ident ann) = do
    ctype <- getConstExprType c >>= genType noqual
    return $ CExprValOf (CVar ident ctype) ctype (buildGenericAnn ann)

genObject :: Object SemanticAnn -> CSourceGenerator CObject
genObject o@(Variable identifier _ann) = do
    cType <- getObjType o >>= genType noqual
    -- Obtain the substitutions map
    subs <- ask
    -- If the identifier is in the substitutions map, use the substituted identifier
    let ident = fromMaybe (CVar identifier cType) (Data.Map.lookup identifier subs)
    -- Return the C identifier
    return ident
genObject o@(ArrayIndexExpression obj index _ann) = do
    -- Generate the C code for the object
    cExpr <- genObject obj
    -- Extract the C object from the expression
    ctype <- getObjType o >>= genType noqual
    -- Generate the C code for the index
    cIndex <- genExpression index
    -- Return the C code for the vector index expression
    return $ CIndexOf cExpr cIndex ctype
genObject o@(MemberAccess obj identifier _ann) = do
    cObj <- genObject obj
    ctype <- getObjType o >>= genType noqual
    return $ CField cObj identifier ctype
genObject o@(DereferenceMemberAccess obj identifier _ann) = do
    cObj <- genObject obj
    ctype <- getObjType o >>= genType noqual
    return $ CField cObj identifier ctype
genObject o@(Dereference obj _ann) = do
    typeObj <- getObjType obj
    cObj <- genObject obj
    case typeObj of
        -- | A dereference to a vector is printed as the name of the vector
        (Reference _ (Array _ _)) -> return cObj
        _ -> do
            ctype <- getObjType o >>= genType noqual
            return $ CDeref cObj ctype
-- | If the expression is a box subtype treated as its base type, we need to
-- check if it is a vector
genObject o@(Unbox obj ann) = do
    let cAnn = buildGenericAnn ann
    let dataFieldCType = CTPointer (CTInt IntSize8 Unsigned noqual) noqual
    typeObj <- getObjType obj
    cObj <- genObject obj
    case typeObj of
        -- | If it is an arrayy, we need to generate the address of the data
        (BoxSubtype ty@(Array _ _)) -> do
            -- We must obtain the declaration specifier of the vector
            ctype <- genType noqual ty
            return $ CObjCast (CField cObj "data" dataFieldCType) ctype cAnn
            -- | Else, we print the derefence to the data
        (BoxSubtype ty) -> do
            ctype <- genType noqual ty
            let cptrtype = CTPointer ctype noqual
            return $ CDeref (CObjCast (CField cObj "data" dataFieldCType) cptrtype cAnn) ctype
        -- | An unbox can only be applied to a box subtype. We are not
        -- supposed to reach here. If we are here, it means that the semantic
        -- analysis is wrong.
        _ -> throwError $ InternalError $ "Unsupported object: " ++ show o

genMemberFunctionAccess :: 
    Object SemanticAnn 
    -> Identifier 
    -> [Expression SemanticAnn] 
    -> SemanticAnn 
    -> CSourceGenerator CExpression
genMemberFunctionAccess obj ident args ann = do
    let cAnn = buildGenericAnn ann
    -- | Obtain the function type
    (cFuncType, cRetType) <- case ann of
        Located (ETy (AppType pts ts)) _ -> do
            cFuncType <- genFunctionType ts (paramTypeSpecifier <$> pts)
            cRetType <- genType noqual ts
            return (cFuncType, cRetType)
        _ -> throwError $ InternalError $ "Invalid function annotation: " ++ show ann
    -- Generate the C code for the object
    cObj <- genObject obj
    let cObjExpr = CExprValOf cObj (getCObjType cObj) cAnn
    -- Generate the C code for the parameters
    cArgs <- mapM genExpression args
    -- | Obtain the type of the object
    typeObj <- getObjType obj
    case typeObj of
        (Reference _ ts) ->
            case ts of
                -- | If the left hand size is a class:
                (DefinedType classId) ->
                    return $ CExprCall (CExprValOf (CVar (classId <::> ident) cFuncType) cFuncType cAnn) (cObjExpr : cArgs) cRetType cAnn
                -- | Anything else should not happen
                _ -> throwError $ InternalError $ "unsupported member function access to object: " ++ show obj
        (DefinedType classId) ->
            case obj of
                (Dereference _ _) ->
                    let selfCType = CTPointer (CTStruct CStructTag classId noqual) noqual in
                    -- | If we are here, it means that we are dereferencing the self object
                    return $ CExprCall (CExprValOf (CVar (classId <::> ident) cFuncType) cFuncType cAnn) (CExprValOf (CVar "self" selfCType) selfCType cAnn : cArgs) cRetType cAnn
                    -- | If the left hand size is a class:
                _ -> 
                    return $ CExprCall (CExprValOf (CVar (classId <::> ident) cFuncType) cFuncType cAnn) (cObjExpr : cArgs) cRetType cAnn
        AccessPort (DefinedType iface) ->
            let thatFieldCType = CTPointer (CTStruct CStructTag iface noqual) noqual in
            return $
                CExprCall (CExprValOf (CField cObj ident cFuncType) cFuncType cAnn)
                      (CExprValOf (CField cObj thatField thatFieldCType) thatFieldCType cAnn : cArgs) cRetType cAnn
        -- | If the left hand side is a pool:
        AccessPort (Allocator {}) ->
            genPoolMethodCallExpr ident cObjExpr cArgs cAnn
        AccessPort (AtomicAccess {}) ->
            case ident of
                "load" -> 
                    case args of 
                        [ReferenceExpression _ refObj _] -> do
                            cRefObj <- genObject refObj
                            let cRefObjType = getCObjType cRefObj
                            mCall <- genAtomicMethodCall ident cObjExpr cArgs cAnn
                            return $ CExprAssign cRefObj mCall cRefObjType cAnn
                        _ -> throwError $ InternalError $ "invalid params for atomic load: " ++ show args
                "store" -> genAtomicMethodCall ident cObjExpr cArgs cAnn
                _ -> throwError $ InternalError $ "This should not happen. Unsupported atomic access method: " ++ ident
        AccessPort (AtomicArrayAccess {}) ->
            case ident of
                "load_index" -> 
                    case args of 
                        [_, ReferenceExpression _ refObj _] -> do
                            cIndexedObj <- genIndexOf cObj (head cArgs)
                            cRefIndexedObj <- genAddrOf cIndexedObj noqual cAnn
                            cRefObj <- genObject refObj
                            let cRefObjType = getCObjType cRefObj
                            mCall <- genAtomicMethodCall "load" cRefIndexedObj cArgs cAnn
                            return $ CExprAssign cRefObj mCall cRefObjType cAnn
                        _ -> throwError $ InternalError $ "invalid params for atomic load_index: " ++ show args
                "store_index" -> 
                    case cArgs of
                        [idx, value] -> do
                            cIndexedObj <- genIndexOf cObj idx
                            cRefIndexedObj <- genAddrOf cIndexedObj noqual cAnn
                            genAtomicMethodCall "store" cRefIndexedObj [value] cAnn
                        _ -> throwError $ InternalError $ "invalid params for atomic store_index: " ++ show args
                _ -> throwError $ InternalError $ "This should not happen. Unsupported atomic access method: " ++ ident
        -- | If the left hand side is a message queue:
        OutPort {} -> do
            genMsgQueueMethodCall ident cObj cArgs cAnn
        -- | Anything else should not happen
        _ -> throwError $ InternalError $ "unsupported member function access to object: " ++ show obj

genExpression :: Expression SemanticAnn -> CSourceGenerator CExpression
genExpression (AccessObject obj) = do
    cObj <- genObject obj
    cObjType <- getObjType obj
    case cObjType of
        (Location _) -> do
            return $ CExprValOf (CDeref cObj (getCObjType cObj)) (getCObjType cObj) (buildGenericAnn (getAnnotation obj))
        _ -> 
            return $ CExprValOf cObj (getCObjType cObj) (buildGenericAnn (getAnnotation obj))
genExpression (BinOp op left right ann) = 
    let cAnn = buildGenericAnn ann in
    case op of
        LogicalAnd -> do
            cLeft <- genExpression left
            cRight <- genExpression right
            return $ CExprSeqAnd cLeft cRight (CTBool noqual) cAnn
        LogicalOr -> do
            cLeft <- genExpression left
            cRight <- genExpression right
            return $ CExprSeqOr cLeft cRight (CTBool noqual) cAnn
        _ -> do
            -- | We need to check if the left and right expressions are binary operations
            -- If they are, we need to cast them to ensure that the resulting value
            -- is truncated to the correct type
            cLeft <- (do
                leftExpr <- genExpression left
                case left of
                    (BinOp {}) -> do
                        let leftExprType = getCExprType leftExpr
                        case leftExprType of
                            CTBool _ -> return leftExpr
                            _ -> return $ CExprCast leftExpr leftExprType cAnn
                    _ -> return leftExpr)
            cRight <- (do
                rightExpr <- genExpression right
                case right of
                    (BinOp {}) -> do
                        let rightExprType = getCExprType rightExpr
                        case rightExprType of
                            CTBool _ -> return rightExpr
                            _ -> return $ CExprCast rightExpr rightExprType cAnn
                    _ -> genExpression right)
            return $ CExprBinaryOp (cBinOp op) cLeft cRight (getCExprType cLeft) cAnn

genExpression e@(Constant c ann) = do
    let cAnn = buildGenericAnn ann 
    cType <- getExprType e >>= genType noqual
    case c of
        (I i _) -> 
            let cInteger = genInteger i in
            return $ CExprConstant (CIntConst cInteger) cType cAnn
        (B True) -> return $ CExprConstant (CIntConst (CInteger 1 CDecRepr)) cType cAnn
        (B False) -> return $ CExprConstant (CIntConst (CInteger 0 CDecRepr)) cType cAnn
        (C char) -> return $ CExprConstant (CCharConst (CChar char)) cType cAnn
genExpression (Casting expr ts ann) = do
    let cAnn = buildGenericAnn ann 
    cType <- genType noqual ts
    cExpr <- genExpression expr
    return $ CExprCast cExpr cType cAnn
genExpression (ReferenceExpression _ obj ann) = do
    let cAnn = buildGenericAnn ann 
    typeObj <- getObjType obj
    cObj <- genObject obj
    case typeObj of
        -- | If it is a vector, we need to generate the address of the data
        (BoxSubtype ty@(Array {})) -> do
            -- We must obtain the declaration specifier of the vector
            cType <- genType noqual ty
            let ptrToVoidCType = CTPointer (CTVoid noqual) noqual
            return $ CExprCast (CExprValOf (CField cObj "data" ptrToVoidCType) ptrToVoidCType cAnn) cType cAnn
            -- | Else, we print the address to the data
        (BoxSubtype ty) -> do
            cType <- genType noqual ty
            let ptrToVoidCType = CTPointer (CTVoid noqual) noqual
                ptrTy = CTPointer cType noqual
            return $ CExprCast (CExprValOf (CField cObj "data" ptrToVoidCType) ptrToVoidCType cAnn) ptrTy cAnn
        (Array {}) -> return $ CExprValOf cObj (getCObjType cObj) cAnn
        ty -> do
            cType <- genType noqual ty
            return $ CExprAddrOf cObj (CTPointer cType noqual) cAnn
genExpression e@(FunctionCall name args ann) = do
    let cAnn = buildGenericAnn ann
    cRetType <- getExprType e >>= genType noqual
    cArgs <- mapM genExpression args
    let cFunctionType = CTFunction cRetType . fmap getCExprType $ cArgs
    -- Obtain the substitutions map
    subs <- ask
    -- If the identifier is in the substitutions map, use the substituted identifier
    let ident = fromMaybe (CVar name cFunctionType) (Data.Map.lookup name subs)
    return $ CExprCall (CExprValOf ident (getCObjType ident) cAnn) cArgs cFunctionType cAnn
genExpression (MemberFunctionCall obj ident args ann) = do
    genMemberFunctionAccess obj ident args ann
genExpression (DerefMemberFunctionCall obj ident args ann) =
    genMemberFunctionAccess obj ident args ann
genExpression (IsEnumVariantExpression obj enum this_variant ann) = do
    let cAnn = buildGenericAnn ann 
    cObj <- genObject obj

    let leftExpr = CExprValOf (CField cObj variant enumFieldType) enumFieldType cAnn
    let rightExpr = CExprValOf (CVar (enum <::> this_variant) enumFieldType) enumFieldType cAnn
    return $ CExprBinaryOp COpEq leftExpr rightExpr (CTBool noqual) cAnn
genExpression (IsOptionVariantExpression obj NoneLabel ann) = do
    let cAnn = buildGenericAnn ann 
    cObj <- genObject obj
    let leftExpr = CExprValOf (CField cObj variant enumFieldType) enumFieldType cAnn
    let rightExpr = CExprValOf (CVar optionNoneVariant enumFieldType) enumFieldType cAnn
    return $ CExprBinaryOp COpEq leftExpr rightExpr (CTBool noqual) cAnn
genExpression (IsOptionVariantExpression obj SomeLabel ann) = do
    let cAnn = buildGenericAnn ann 
    cObj <- genObject obj
    let leftExpr = CExprValOf (CField cObj variant enumFieldType) enumFieldType cAnn
    let rightExpr = CExprValOf (CVar optionSomeVariant enumFieldType) enumFieldType cAnn
    return $ CExprBinaryOp COpEq leftExpr rightExpr (CTBool noqual) cAnn
genExpression (ArraySliceExpression _ak obj lower _rb _ann) = do
    cObjType <- getObjType obj
    case cObjType of
        (Array ty _) -> do
            cObj <- genObject obj
            cLower <- genExpression lower
            cType <- genType noqual ty
            return $ CExprAddrOf (CIndexOf cObj cLower cType) (getCObjType cObj) (buildGenericAnn (getAnnotation obj))
        _ -> throwError $ InternalError $ "Unsupported object. Not an array: " ++ show obj
genExpression o = throwError $ InternalError $ "Unsupported expression: " ++ show o
