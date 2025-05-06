{-# LANGUAGE FlexibleContexts #-}
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

-- | Translate type annotation to C type
genType :: CQualifier 
    -> TerminaType SemanticAnn 
    -> CGenerator CType
-- |  Unsigned integer types
genType qual TUInt8 = return (CTInt IntSize8 Unsigned qual)
genType qual TUInt16 = return (CTInt IntSize16 Unsigned qual)
genType qual TUInt32 = return (CTInt IntSize32 Unsigned qual)
genType qual TUInt64 = return (CTInt IntSize64 Unsigned qual)
-- | Signed integer types
genType qual TInt8 = return (CTInt IntSize8 Signed qual)
genType qual TInt16 = return (CTInt IntSize16 Signed qual)
genType qual TInt32 = return (CTInt IntSize32 Signed qual)
genType qual TInt64 = return (CTInt IntSize64 Signed qual)
-- | Other primitive typess
genType qual TUSize = return (CTSizeT qual)
genType qual TBool = return (CTBool qual)
genType qual TChar = return (CTChar qual)
-- | Primitive type
genType qual (TGlobal _ clsIdentifier) = return (CTTypeDef clsIdentifier qual)
-- | TArray type
genType qual (TArray ts' s) = do
    ts <- genType qual ts'
    arraySize <- genExpression s
    return (CTArray ts arraySize)
-- | Option types
genType _qual (TOption (TBoxSubtype _)) = return (CTTypeDef optionBox noqual)
genType _qual (TOption ts) = do
    optName <- genOptionStructName ts
    return (CTTypeDef optName noqual)
genType _qual (TResult tyOk tyError) = do
    resultName <- genResultStructName tyOk tyError
    return (CTTypeDef resultName noqual)
genType _qual (TStatus ts) = do
    optName <- genStatusStructName ts
    return (CTTypeDef optName noqual)
-- Non-primitive types:
-- | Box subtype
genType _qual (TBoxSubtype _) = return (CTTypeDef boxStruct noqual)
-- | Const subtype
genType _qual (TConstSubtype ty) = genType constqual ty
-- | TPool type
genType _qual (TPool _ _) = return (CTTypeDef pool noqual)
genType _qual (TMsgQueue _ _) = return (CTTypeDef msgQueue noqual)
genType qual (TFixedLocation ts) = do
    ts' <- genType volatile ts
    return (CTPointer ts' qual)
genType _qual (TAccessPort (TInterface _ _)) = throwError $ InternalError "Access ports shall not be translated to C types"
genType _qual (TAccessPort ts) = genType noqual ts
genType _qual (TAllocator _) = return (CTTypeDef allocator noqual)
genType _qual (TAtomic ts) = genType atomic ts
genType _qual (TAtomicArray ts s) = do
    ts' <- genType atomic ts
    arraySize <- genExpression s
    return (CTArray ts' arraySize)
genType _qual (TAtomicAccess ts) = do
    ts' <- genType atomic ts
    return (CTPointer ts' noqual)
genType _qual (TAtomicArrayAccess ts _) = do
    ts' <- genType atomic ts
    return (CTPointer ts' noqual)
-- | Type of the ports
genType _qual (TSinkPort {}) = return (CTTypeDef sinkPort noqual)
genType _qual (TOutPort {}) = return (CTTypeDef outPort noqual)
genType _qual (TInPort {}) = return (CTTypeDef inPort noqual)
genType qual (TReference Immutable ts) = do
    case ts of
        TArray {} -> genType constqual ts
        _ -> do
            ts' <- genType qual{qual_const = True} ts
            return (CTPointer ts' constqual)
genType _qual (TReference _ ts) = do
    case ts of
        TArray {} -> genType noqual ts
        _ -> do
            ts' <- genType noqual ts
            return (CTPointer ts' constqual)
genType _noqual TUnit = return (CTVoid noqual)
genType qual (TEnum ident) = return (CTTypeDef ident qual)
genType qual (TStruct ident) = return (CTTypeDef ident qual)
genType qual (TInterface RegularInterface ident) = return (CTTypeDef ident qual)
genType _qual (TInterface SystemInterface _) = throwError $ InternalError "System interfaces shall not be translated to C types"

genFunctionType :: TerminaType SemanticAnn -> [Parameter SemanticAnn] -> CGenerator CType
genFunctionType ts tsParams = do
    ts' <- genType noqual ts
    tsParams' <- traverse (genType noqual . paramType) tsParams
    return (CTFunction ts' tsParams')

genParameterDeclaration :: Parameter SemanticAnn -> CGenerator CDeclaration
genParameterDeclaration (Parameter identifier ts) = do
    cParamType <- genType noqual ts
    return $ CDecl (CTypeSpec cParamType) (Just identifier) Nothing


genObject :: Object SemanticAnn -> CGenerator CObject
genObject o@(Variable identifier _ann) = do
    cType <- getObjType o >>= genType noqual
    -- Return the C identifier
    return (identifier @: cType)
genObject (ArrayIndexExpression obj index ann) = do
    objType <- getObjType obj
    (ty, arraySize) <- case objType of 
        (TArray ty arraySize) -> return (ty, arraySize)
        (TReference _ (TArray ty arraySize)) -> return (ty, arraySize)
        _ -> throwError $ InternalError $ "Invalid object type: " ++ show obj ++ ". Expected an array."
    -- Generate the C code for the object
    cObj <- genObject obj
    -- Extract the C object from the expression
    ctype <- genType noqual ty
    -- Check the type of the index expression
    indexType <- getExprType index
    -- Generate the C code for the index
    cIndex <- genExpression index
    case indexType of
        (TConstSubtype _) -> do
            -- If the index is constant, we just need to generate the C code
            -- for the index expression
            -- Return the C code for the array index expression
            return $ cObj @$$ cIndex @: ctype
        _ -> do
            -- If the index is not a constant, we need to call the index
            -- checker function
            cArraySize <- genExpression arraySize
            let cAnn = buildGenericAnn ann
                cFuncType = CTFunction (CTSizeT noqual) [_const size_t, _const size_t]
                cFunctionCall = CExprCall (CExprValOf (CVar "__termina_array__index" cFuncType) cFuncType cAnn) [cArraySize, cIndex] (CTSizeT noqual) cAnn
            return $ cObj @$$ cFunctionCall @: ctype
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
        SemanticAnn (ETy (AppType pts ts)) _ -> do
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
                    return $ ((classId <::> ident) @: cFuncType) @@ (cObjExpr : cArgs) |>> getLocation ann
                -- | Anything else should not happen
                _ -> throwError $ InternalError $ "unsupported member function access to object reference: " ++ show obj
        (TGlobal _ classId) ->
            case obj of
                (Dereference _ _) ->
                    let selfCType = ptr (typeDef classId) in
                    -- | If we are here, it means that we are dereferencing the self object
                    return $ ((classId <::> ident) @: cFuncType) @@ ("self" @: selfCType : cArgs) |>> getLocation ann 
                    -- | If the left hand size is a class:
                _ -> 
                    return $ ((classId <::> ident) @: cFuncType) @@ (cObjExpr : cArgs) |>> getLocation ann
        -- | Anything else should not happen
        _ -> throwError $ InternalError $ "unsupported member function access to object: " ++ show obj
        

genExpression :: Expression SemanticAnn -> CGenerator CExpression
genExpression (AccessObject obj) = do
    cObj <- genObject obj
    objType <- getObjType obj
    case objType of
        (TFixedLocation _) -> do
            return $ deref cObj |>> getLocation (getAnnotation obj)
        _ -> 
            return $ cObj @: getCObjType cObj |>> getLocation (getAnnotation obj)
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
                            CTInt intSize intSign _  -> return $ cast (CTInt intSize intSign noqual) leftExpr |>> getLocation ann
                            CTSizeT _ -> return $ cast (CTSizeT noqual) leftExpr |>> getLocation ann
                            _ -> throwError $ InternalError $ "Unsupported left expression type: " ++ show leftExprType
                    _ -> return leftExpr)
            cRight <- (do
                rightExpr <- genExpression right
                case right of
                    (BinOp {}) -> do
                        let rightExprType = getCExprType rightExpr
                        case rightExprType of
                            CTBool _ -> return rightExpr
                            CTInt intSize intSign _  -> return $ cast (CTInt intSize intSign noqual) rightExpr |>> getLocation ann
                            CTSizeT _ -> return $ cast (CTSizeT noqual) rightExpr |>> getLocation ann
                            _ -> throwError $ InternalError $ "Unsupported right expression type: " ++ show rightExpr
                    _ -> genExpression right)
            return $ CExprBinaryOp (cBinOp op) cLeft cRight (getCExprType cLeft) cAnn

genExpression e@(Constant c ann) = do
    cType <- getExprType e >>= genType noqual
    case c of
        (I i _) -> 
            let cInteger = genInteger i in
            return $ cInteger @: cType |>> getLocation ann
        (B True) -> return $ dec 1 @: cType |>> getLocation ann
        (B False) -> return $ dec 0 @: cType |>> getLocation ann
        (C chr) -> return $ chr @: cType |>> getLocation ann
genExpression (Casting expr ts ann) = do
    cType <- genType noqual ts
    cExpr <- genExpression expr
    return $ cast cType cExpr |>> getLocation ann
genExpression (ReferenceExpression _ obj ann) = do
    typeObj <- getObjType obj
    cObj <- genObject obj
    case typeObj of
        -- | If it is an array, we need to generate the address of the data
        (TBoxSubtype ty@(TArray {})) -> do
            -- We must obtain the declaration specifier of the array
            cType <- genType noqual ty
            return $ cast cType (cObj @. "data" @: void_ptr) |>> getLocation ann
            -- | Else, we print the address to the data
        (TBoxSubtype ty) -> do
            cType <- genType noqual ty
            return $ cast (ptr cType) (cObj @. "data" @: void_ptr) |>> getLocation ann
        (TArray {}) -> return $ cObj @: getCObjType cObj |>> getLocation ann
        _ -> do
            return $ addrOf cObj |>> getLocation ann
genExpression e@(FunctionCall name args ann) = do
    cRetType <- getExprType e >>= genType noqual
    cArgs <- mapM genExpression args
    let cFunctionType = CTFunction cRetType . fmap getCExprType $ cArgs
    return $ (name @: cFunctionType) @@ cArgs |>> getLocation ann
genExpression (MemberFunctionCall obj ident args ann) = do
    genMemberFunctionAccess obj ident args ann
genExpression (DerefMemberFunctionCall obj ident args ann) =
    genMemberFunctionAccess obj ident args ann
genExpression (IsEnumVariantExpression obj enum this_variant ann) = do
    cObj <- genObject obj
    let leftExpr = cObj @. variant @: enumFieldType |>> getLocation ann
    let rightExpr = (enum <::> this_variant) @: enumFieldType |>> getLocation ann
    return $ leftExpr @== rightExpr |>> getLocation ann
genExpression (IsMonadicVariantExpression obj this_variant ann) = do
    cObj <- genObject obj
    let leftExpr = cObj @. variant @: enumFieldType |>> getLocation ann
    let rightExpr = case this_variant of 
            NoneLabel -> optionNoneVariant @: enumFieldType |>> getLocation ann
            SomeLabel -> optionSomeVariant @: enumFieldType |>> getLocation ann
            SuccessLabel -> statusSuccessVariant @: enumFieldType |>> getLocation ann
            FailureLabel -> statusFailureVariant @: enumFieldType |>> getLocation ann
            OkLabel -> resultOkVariant @: enumFieldType |>> getLocation ann
            ErrorLabel -> resultErrorVariant @: enumFieldType |>> getLocation ann
    return $ leftExpr @== rightExpr |>> getLocation ann
genExpression expr@(ArraySliceExpression _ak obj lower upper ann) = do
    objType <- getObjType obj
    expectedType <- getExprType expr
    lowerType <- getExprType lower
    upperType <- getExprType upper
    cLower <- genExpression lower
    cObj <- genObject obj
    case (objType, expectedType, lowerType, upperType) of
        (TArray ty _, _, TConstSubtype _, TConstSubtype _) -> do
            cType <- genType noqual ty
            return $ addrOf (cObj @$$ cLower @: cType) |>> getLocation ann
        (TArray ty arraySize, TReference _ (TArray _ expectedSize), _, _) -> do
            cType <- genType noqual ty
            cUpper <- genExpression upper
            cArraySize <- genExpression arraySize
            cExpectedSize <- genExpression expectedSize
            let cAnn = buildGenericAnn ann
                cFuncType = CTFunction (CTSizeT noqual) [_const size_t, _const size_t, _const size_t, _const size_t]
                cFunctionCall = 
                    CExprCall (CExprValOf (CVar "__termina_array__slice" cFuncType) cFuncType cAnn) 
                            [cArraySize, cExpectedSize, cLower, cUpper] (CTSizeT noqual) cAnn
            return $ addrOf (cObj @$$ cFunctionCall @: cType) |>> getLocation ann
        (ty, _,  _, _) -> throwError $ InternalError $ "Unsupported object. Not a reference to an array: " ++ show ty
genExpression o = throwError $ InternalError $ "Unsupported expression: " ++ show o
