module Generator.CodeGen.Expression where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Semantic.Types
import Control.Monad.Except
import Generator.CodeGen.Common
import Utils.Annotations
import Generator.LanguageC.Embedded


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

genObject :: Object SemanticAnn -> CGenerator CObject
genObject o@(Variable identifier _ann) = do
    cType <- getObjType o >>= genType noqual
    -- Return the C identifier
    return (identifier @: cType)
genObject o@(ArrayIndexExpression obj index _ann) = do
    -- Generate the C code for the object
    cExpr <- genObject obj
    -- Extract the C object from the expression
    ctype <- getObjType o >>= genType noqual
    -- Generate the C code for the index
    cIndex <- genExpression index
    -- Return the C code for the array index expression
    return $ cExpr @$$ cIndex @: ctype
genObject o@(MemberAccess obj identifier _ann) = do
    cObj <- genObject obj
    ctype <- getObjType o >>= genType noqual
    return $ cObj @. identifier @: ctype
genObject o@(DereferenceMemberAccess obj identifier _ann) = do
    cObj <- genObject obj
    ctype <- getObjType o >>= genType noqual
    return $ cObj @. identifier @: ctype
genObject (Dereference obj _ann) = do
    typeObj <- getObjType obj
    cObj <- genObject obj
    case typeObj of
        -- | A dereference to an array is printed as the name of the array
        (TReference _ (TArray _ _)) -> return cObj
        _ -> do
            return $ deref cObj
-- | If the expression is a box subtype treated as its base type, we need to
-- check if it is an array
genObject o@(Unbox obj _ann) = do
    let dataFieldCType = ptr uint8_t
    typeObj <- getObjType obj
    cObj <- genObject obj
    case typeObj of
        -- | If it is an arrayy, we need to generate the address of the data
        (TBoxSubtype ty@(TArray _ _)) -> do
            -- We must obtain the declaration specifier of the array
            ctype <- genType noqual ty
            return $ cast ctype ((cObj @. "data") @: dataFieldCType)
            -- | Else, we print the derefence to the data
        (TBoxSubtype ty) -> do
            ctype <- genType noqual ty
            return $ deref (cast (ptr ctype) (cObj @. "data" @: dataFieldCType))
        -- | An unbox can only be applied to a box subtype. We are not
        -- supposed to reach here. If we are here, it means that the semantic
        -- analysis is wrong.
        _ -> throwError $ InternalError $ "Unsupported object: " ++ show o

genMemberFunctionAccess :: 
    Object SemanticAnn 
    -> Identifier 
    -> [Expression SemanticAnn] 
    -> SemanticAnn 
    -> CGenerator CExpression
genMemberFunctionAccess obj ident args ann = do
    let cAnn = buildGenericAnn ann
    -- | Obtain the function type
    (cFuncType, _) <- case ann of
        LocatedElement (ETy (AppType pts ts)) _ -> do
            cFuncType <- genFunctionType ts pts
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
        (TReference _ ts) ->
            case ts of
                -- | If the left hand size is a class:
                (TGlobal _ classId) ->
                    return $ ((classId <::> ident) @: cFuncType) @@ (cObjExpr : cArgs) |>> location ann
                -- | Anything else should not happen
                _ -> throwError $ InternalError $ "unsupported member function access to object reference: " ++ show obj
        (TGlobal _ classId) ->
            case obj of
                (Dereference _ _) ->
                    let selfCType = ptr (struct classId) in
                    -- | If we are here, it means that we are dereferencing the self object
                    return $ ((classId <::> ident) @: cFuncType) @@ ("self" @: selfCType : cArgs) |>> location ann 
                    -- | If the left hand size is a class:
                _ -> 
                    return $ ((classId <::> ident) @: cFuncType) @@ (cObjExpr : cArgs) |>> location ann
        -- | Anything else should not happen
        _ -> throwError $ InternalError $ "unsupported member function access to object: " ++ show obj

genExpression :: Expression SemanticAnn -> CGenerator CExpression
genExpression (AccessObject obj) = do
    cObj <- genObject obj
    objType <- getObjType obj
    case objType of
        (TFixedLocation _) -> do
            return $ deref cObj |>> location (getAnnotation obj)
        _ -> 
            return $ cObj @: getCObjType cObj |>> location (getAnnotation obj)
genExpression (BinOp op left right ann) = 
    let cAnn = buildGenericAnn ann in
    case op of
        LogicalAnd -> do
            cLeft <- genExpression left
            cRight <- genExpression right
            return $ cLeft @&& cRight |>> location cAnn
        LogicalOr -> do
            cLeft <- genExpression left
            cRight <- genExpression right
            return $ cLeft @|| cRight |>> location cAnn
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
                            _ -> return $ cast leftExprType leftExpr |>> location ann
                    _ -> return leftExpr)
            cRight <- (do
                rightExpr <- genExpression right
                case right of
                    (BinOp {}) -> do
                        let rightExprType = getCExprType rightExpr
                        case rightExprType of
                            CTBool _ -> return rightExpr
                            _ -> return $ cast rightExprType rightExpr |>> location ann
                    _ -> genExpression right)
            return $ CExprBinaryOp (cBinOp op) cLeft cRight (getCExprType cLeft) cAnn

genExpression e@(Constant c ann) = do
    cType <- getExprType e >>= genType noqual
    case c of
        (I i _) -> 
            let cInteger = genInteger i in
            return $ cInteger @: cType |>> location ann
        (B True) -> return $ dec 1 @: cType |>> location ann
        (B False) -> return $ dec 0 @: cType |>> location ann
        (C chr) -> return $ chr @: cType |>> location ann
genExpression (Casting expr ts ann) = do
    cType <- genType noqual ts
    cExpr <- genExpression expr
    return $ cast cType cExpr |>> location ann
genExpression (ReferenceExpression _ obj ann) = do
    typeObj <- getObjType obj
    cObj <- genObject obj
    case typeObj of
        -- | If it is an array, we need to generate the address of the data
        (TBoxSubtype ty@(TArray {})) -> do
            -- We must obtain the declaration specifier of the array
            cType <- genType noqual ty
            return $ cast cType (cObj @. "data" @: void_ptr) |>> location ann
            -- | Else, we print the address to the data
        (TBoxSubtype ty) -> do
            cType <- genType noqual ty
            return $ cast (ptr cType) (cObj @. "data" @: void_ptr) |>> location ann
        (TArray {}) -> return $ cObj @: getCObjType cObj |>> location ann
        _ -> do
            return $ addrOf cObj |>> location ann
genExpression e@(FunctionCall name args ann) = do
    cRetType <- getExprType e >>= genType noqual
    cArgs <- mapM genExpression args
    let cFunctionType = CTFunction cRetType . fmap getCExprType $ cArgs
    return $ (name @: cFunctionType) @@ cArgs |>> location ann
genExpression (MemberFunctionCall obj ident args ann) = do
    genMemberFunctionAccess obj ident args ann
genExpression (DerefMemberFunctionCall obj ident args ann) =
    genMemberFunctionAccess obj ident args ann
genExpression (IsEnumVariantExpression obj enum this_variant ann) = do
    cObj <- genObject obj
    let leftExpr = cObj @. variant @: enumFieldType |>> location ann
    let rightExpr = (enum <::> this_variant) @: enumFieldType |>> location ann
    return $ leftExpr @== rightExpr |>> location ann
genExpression (IsOptionVariantExpression obj NoneLabel ann) = do
    cObj <- genObject obj
    let leftExpr = cObj @. variant @: enumFieldType |>> location ann
    let rightExpr = optionNoneVariant @: enumFieldType |>> location ann
    return $ leftExpr @== rightExpr |>> location ann
genExpression (IsOptionVariantExpression obj SomeLabel ann) = do
    cObj <- genObject obj
    let leftExpr = cObj @. variant @: enumFieldType |>> location ann
    let rightExpr = optionSomeVariant @: enumFieldType |>> location ann
    return $ leftExpr @== rightExpr |>> location ann
genExpression (ArraySliceExpression _ak obj lower _rb ann) = do
    cObjType <- getObjType obj
    case cObjType of
        (TArray ty _) -> do
            cObj <- genObject obj
            cLower <- genExpression lower
            cType <- genType noqual ty
            return $ addrOf (cObj @$$ cLower @: cType) |>> location ann
        _ -> throwError $ InternalError $ "Unsupported object. Not an array: " ++ show obj
genExpression o = throwError $ InternalError $ "Unsupported expression: " ++ show o
