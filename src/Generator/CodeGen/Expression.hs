{-# LANGUAGE FlexibleContexts #-}
module Generator.CodeGen.Expression where

import Elaboration.AST
import Elaboration (elaborateConstant)
import qualified Lowering.AST as L
import Generator.LanguageC.AST
import Semantic.Types
import Control.Monad.Except
import Control.Monad (zipWithM)
import Control.Monad.State (gets)
import Generator.CodeGen.Common
import Utils.Annotations
import Generator.LanguageC.Embedded
import Core.Utils (shiftWidth, arrayOf)
import Configuration.Platform (Platform, intWidth)


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
-- |  Unsigned integer types
genType qual TUInt8 = return (CTInt IntSize8 Unsigned qual)
genType qual TUInt16 = return (CTInt IntSize16 Unsigned qual)
genType qual TUInt32 = return (CTInt IntSize32 Unsigned qual)
genType qual TUInt64 = return (CTInt IntSize64 Unsigned qual)
-- | Signed integer types
genType qual TInt8 = return (CTInt IntSize8 Signed qual)
genType qual TInt16 = return (CTInt IntSize16 Signed qual)
genType qual TInt32 = return (CTInt IntSize32 Signed qual)
genType qual TInt64 = return (CTInt IntSize64 Signed qual)
-- | Other primitive typess
genType qual TUSize = return (CTSizeT qual)
genType qual TBool = return (CTBool qual)
genType qual TChar = return (CTChar qual)
-- | Floating-point types
genType qual TFloat32 = return (CTFloat FloatSize32 qual)
genType qual TFloat64 = return (CTFloat FloatSize64 qual)
-- | Primitive type
genType qual (TGlobal _ clsIdentifier) = return (CTTypeDef clsIdentifier qual)
-- | TArray type
genType qual (TArray ts' s) = do
    ts <- genType qual ts'
    arraySize <- genConstExpression s
    return (CTArray ts arraySize)
-- | Option types
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
    arraySize <- genConstExpression s
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

-- | The C type of a parameter, which every place that emits one takes from
-- here: the declaration of a function, its definition, and the prototype of a
-- function pointer. A value is taken const, since a parameter of Termina
-- cannot be assigned, and a reference already comes out qualified from
-- genType.
genParameterType :: TerminaType SemanticAnn -> CGenerator CType
genParameterType ts = _const <$> genType noqual ts

genParameterDeclaration :: Parameter SemanticAnn -> CGenerator CDeclaration
genParameterDeclaration (Parameter identifier ts _) = do
    cParamType <- genParameterType ts
    return $ CDecl (CTypeSpec cParamType) (Just (genParameterIdentifier identifier)) Nothing

-- | The same parameter as part of the prototype of a function pointer, where
-- C asks for the name as well as the type.
genCParameter :: Parameter SemanticAnn -> CGenerator CParameter
genCParameter (Parameter identifier ts _) =
    CParameter (genParameterIdentifier identifier) <$> genParameterType ts


-- | Generates a constant expression of the semantic AST, such as the size of
-- an array type.
genConstExpression :: L.Expression SemanticAnn -> CGenerator CExpression
genConstExpression = genExpression . elaborateConstant

genObject :: Object SemanticAnn -> CGenerator CObject
genObject o@(Variable identifier _ann) = do
    cType <- getObjType o >>= genType noqual
    -- Return the C identifier
    return (identifier @: cType)
genObject (CheckedArrayIndex obj index ann) = do
    (_, arraySize) <- getArrayType obj
    cArraySize <- genConstExpression arraySize
    cIndex <- genExpression index
    let cFuncType = CTFunction size_t [_const size_t, _const size_t]
        cFunctionCall = ("termina__check__array_index" @: cFuncType |>> getLocation ann)
            @@ [cArraySize, cIndex] |>> getLocation ann
    genArrayAccess obj cFunctionCall
genObject (UncheckedArrayIndex obj index _ann) = do
    cIndex <- genExpression index
    genArrayAccess obj cIndex
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
            -- | Else, we print the derefence to the data
        (TBoxSubtype ty) -> do
            ctype <- genType noqual ty
            return $ deref (cast (ptr ctype) (cObj @. "data" @: dataFieldCType))
        -- | An unbox can only be applied to a box subtype. We are not
        -- supposed to reach here. If we are here, it means that the semantic
        -- analysis is wrong.
        _ -> throwError $ InternalError $ "Unsupported object: " ++ show o

-- | Accesses an element of an array object with the given C index.
genArrayAccess :: Object SemanticAnn -> CExpression -> CGenerator CObject
genArrayAccess obj cIndex = do
    (ty, _) <- getArrayType obj
    cObj <- genObject obj
    ctype <- genType noqual ty
    return $ cObj @$$ cIndex @: ctype

-- | The element type and the size of an array object.
getArrayType :: Object SemanticAnn -> CGenerator (TerminaType SemanticAnn, L.Expression SemanticAnn)
getArrayType obj = do
    objType <- getObjType obj
    maybe (throwError $ InternalError $ "Invalid object type: " ++ show obj ++ ". Expected an array.")
        return (arrayOf objType)

genMemberFunctionAccess :: 
    Object SemanticAnn 
    -> Identifier 
    -> [Expression SemanticAnn] 
    -> SemanticAnn 
    -> CGenerator CExpression
genMemberFunctionAccess obj ident args ann = do
    -- | Obtain the function type
    (cFuncType, _) <- case ann of
        SemanticAnn (ETy (AppType pts ts)) _ -> do
            cFuncType <- genFunctionType ts pts
            cRetType <- genType noqual ts
            return (cFuncType, cRetType)
        _ -> throwError $ InternalError $ "Invalid function annotation: " ++ show ann
    -- Generate the C code for the object
    cObj <- genObject obj
    let cObjExpr = cObj @: getCObjType cObj |>> getLocation ann
    -- Generate the C code for the parameters
    cArgs <- mapM genExpression args
    -- | Obtain the type of the object
    typeObj <- getObjType obj
    cEventArg <- genEventParamArg (internalAnn CGenericAnn)
    case typeObj of
        (TReference _ ts) ->
            case ts of
                -- | If the left hand size is a class:
                (TGlobal _ classId) ->
                    return $ ((classId <::> ident) @: cFuncType) @@ (cEventArg : cObjExpr : cArgs) |>> getLocation ann
                -- | Anything else should not happen
                _ -> throwError $ InternalError $ "unsupported member function access to object reference: " ++ show obj
        (TGlobal _ classId) ->
            case obj of
                (Dereference _ _) ->
                    let selfCType = ptr (typeDef classId) in
                    -- | If we are here, it means that we are dereferencing the self object
                    return $ ((classId <::> ident) @: cFuncType) @@ (cEventArg : "self" @: selfCType : cArgs) |>> getLocation ann 
                    -- | If the left hand size is a class:
                _ -> 
                    return $ ((classId <::> ident) @: cFuncType) @@ (cEventArg : cObjExpr : cArgs) |>> getLocation ann
        -- | Anything else should not happen
        _ -> throwError $ InternalError $ "unsupported member function access to object: " ++ show obj
        

-- | Casts the C expression generated for a binary operation to the type of the
-- operation, so that its value is truncated to that type: integer operands are
-- promoted to int in C, and the result could otherwise keep bits that the
-- Termina type does not have. Any other expression is returned unchanged.
castBinOpToOwnType :: Location -> Expression SemanticAnn -> CExpression -> CGenerator CExpression
castBinOpToOwnType loc expr cExpr =
    case (binaryOperation expr, cExpr) of
        -- | checkedArithmetic has already brought the result to its type.
        (Just _, CExprCall {}) -> return cExpr
        (Just (op, _, _), _) -> castOperation op
        (Nothing, _) -> return cExpr

  where

    castOperation op = do
        plt <- gets targetPlatform
        if productLeavesInt plt op cExpr then
            -- | multiplyInUnsignedInt has already cast the product to its type.
            return cExpr
        else if widens op then
            castToOwnType loc (dropBitsAbove plt cExpr)
        else
            castToOwnType loc cExpr

-- | Whether the product of two operands of this unsigned type can leave the
-- range of the signed int they are promoted to, which is undefined in C.
productLeavesInt :: Platform -> Op -> CExpression -> Bool
productLeavesInt plt Multiplication cExpr =
    case getCExprType cExpr of
        CTInt size Unsigned _ ->
            let width = intSizeWidth size in
            width < intWidth plt && 2 * width >= intWidth plt
        _ -> False
productLeavesInt _ _ _ = False

-- | Multiplies in the unsigned type as wide as the int of the platform, so that
-- the operation is carried out without promotion to a signed int, and brings
-- the product back to the type of the operands. Converting one operand is
-- enough, since the usual arithmetic conversions convert the other one.
multiplyInUnsignedInt :: Platform -> CExpression -> CExpression -> Location -> CExpression
multiplyInUnsignedInt plt cLeft cRight loc =
    let ownType = getCExprType cLeft
        uintType = CTInt (intSizeOfWidth (intWidth plt)) Unsigned noqual
        cProduct = (cast uintType cLeft @* cRight) @: uintType |>> loc
    in
    case ownType of
        CTInt size Unsigned _ ->
            cast (CTInt size Unsigned noqual) (maskToWidth size cProduct)
        _ -> cProduct

-- | The run-time check of an integer operation, for an operation that the
-- elaboration left checked. A signed operation goes to the function of the
-- OSAL that carries it out once it has checked that its result is
-- representable, and the divisor of an unsigned division or remainder is
-- checked not to be zero.
checkedArithmetic :: Op -> CExpression -> CExpression -> Location -> Maybe CExpression
checkedArithmetic op cLeft cRight loc =
    case getCExprType cLeft of
        CTInt size Signed _ ->
            let suffix = "i" ++ show (intSizeWidth size)
                ownType = CTInt size Signed noqual
            in
            case op of
                Addition -> Just $ checkOperation "add" suffix ownType
                Subtraction -> Just $ checkOperation "sub" suffix ownType
                Multiplication -> Just $ checkOperation "mul" suffix ownType
                Division -> Just $ checkOperation "div" suffix ownType
                Modulo -> Just $ checkOperation "mod" suffix ownType
                _ -> Nothing
        CTInt size Unsigned _ ->
            checkUnsignedDivisor ("u" ++ show (intSizeWidth size)) (CTInt size Unsigned noqual)
        CTSizeT _ ->
            checkUnsignedDivisor "usize" (CTSizeT noqual)
        _ -> Nothing

    where

        checkCall :: String -> [CType] -> CType -> [CExpression] -> CExpression
        checkCall name paramTypes retType args =
            let cFuncType = CTFunction retType paramTypes in
            (name @: cFuncType |>> loc) @@ args |>> loc

        checkOperation :: String -> String -> CType -> CExpression
        checkOperation name suffix ownType =
            checkCall ("termina__check__" ++ name ++ "_" ++ suffix) [ownType, ownType] ownType [cLeft, cRight]

        checkUnsignedDivisor :: String -> CType -> Maybe CExpression
        checkUnsignedDivisor suffix ownType =
            case op of
                Division -> divided
                Modulo -> divided
                _ -> Nothing

            where

                divided =
                    Just $ binOp (cBinOp op) cLeft
                        (checkCall ("termina__check__divisor_" ++ suffix) [ownType] ownType [cRight])
                        @: getCExprType cLeft |>> loc

-- | Whether the result of the operation can need more bits than its operands
-- hold. The rest give a result that fits: a quotient and a remainder are no
-- greater than the dividend, a right shift no greater than the value shifted,
-- and the bitwise operations work within the bits they are given.
widens :: Op -> Bool
widens Addition = True
widens Subtraction = True
widens Multiplication = True
widens BitwiseLeftShift = True
widens _ = False

-- | Drops the bits that the width of the type does not hold, which is what an
-- operation on a type narrower than the @int@ of the platform means: C
-- promotes the operands of such a type, so the operation is carried out in
-- @int@ and the result comes back with the bits above the width still in it.
-- Masking them off says that they are dropped and leaves a value that provably
-- fits the type, where the cast alone would be a conversion that an analyser
-- reads as losing them. A type as wide as @int@, or wider, is not promoted and
-- wraps on its own, and a signed type is left alone, since dropping the bits is
-- not the value the operation gives.
dropBitsAbove :: Platform -> CExpression -> CExpression
dropBitsAbove plt cExpr = case getCExprType cExpr of
    CTInt size Unsigned _ ->
        if intSizeWidth size < intWidth plt then maskToWidth size cExpr else cExpr
    _ -> cExpr

-- | Masks an integer C expression with the bits of the given width, keeping the
-- type of the expression.
maskToWidth :: CIntSize -> CExpression -> CExpression
maskToWidth size cExpr =
    let cType = getCExprType cExpr
        mask = 2 ^ intSizeWidth size - 1
    in (cExpr @& (CInteger mask CHexRepr @: cType)) @: cType

intSizeWidth :: CIntSize -> Integer
intSizeWidth IntSize8   = 8
intSizeWidth IntSize16  = 16
intSizeWidth IntSize32  = 32
intSizeWidth IntSize64  = 64
intSizeWidth IntSize128 = 128

intSizeOfWidth :: Integer -> CIntSize
intSizeOfWidth width =
    case width of
        8 -> IntSize8
        16 -> IntSize16
        32 -> IntSize32
        64 -> IntSize64
        _ -> IntSize128

-- | Casts an integer C expression to its own type, without qualifiers.
castToOwnType :: Location -> CExpression -> CGenerator CExpression
castToOwnType loc cExpr =
    case getCExprType cExpr of
        CTBool _ -> return cExpr
        CTInt intSize intSign _  -> return $ cast (CTInt intSize intSign noqual) cExpr |>> loc
        CTSizeT _ -> return $ cast (CTSizeT noqual) cExpr |>> loc
        -- | Float operands need no cast: same-type float arithmetic
        -- does not promote (float + float stays float), unlike integer
        -- promotion to int. 
        CTFloat _ _ -> return cExpr
        cType -> throwError $ InternalError $ "Unsupported expression type: " ++ show cType

-- | Generates a binary operation, with the run-time check of its operator when
-- the elaboration left it checked.
genBinOp :: Bool -> Op -> Expression SemanticAnn -> Expression SemanticAnn -> SemanticAnn -> CGenerator CExpression
genBinOp checked op left right ann =
    case op of
        LogicalAnd -> do
            cLeft <- genExpression left
            cRight <- genExpression right
            return $ cLeft @&& cRight |>> loc
        LogicalOr -> do
            cLeft <- genExpression left
            cRight <- genExpression right
            return $ cLeft @|| cRight |>> loc
        _ -> do
            -- | We need to check if the left and right expressions are binary operations
            -- If they are, we need to cast them to ensure that the resulting value
            -- is truncated to the correct type
            cLeft <- genExpression left >>= castBinOpToOwnType loc left
            cRight <- genExpression right >>= castBinOpToOwnType loc right
            -- | The type of a shift is the one of its left operand. A constant
            -- left operand is cast to its type, so that the type written in the
            -- source is the one of the shift, regardless of the value.
            let castShiftConstant =
                    case left of
                        Constant {} -> castToOwnType loc cLeft
                        _ -> return cLeft
            cLeft' <- case op of
                BitwiseLeftShift  -> castShiftConstant
                BitwiseRightShift -> castShiftConstant
                _ -> return cLeft
            let boundShift = do
                    leftTy <- getExprType left
                    plt <- gets targetPlatform
                    let cFuncType = CTFunction size_t [_const size_t, _const size_t]
                        cWidth = dec (shiftWidth plt leftTy) @: size_t |>> loc
                    return $ ("termina__check__shift_amount" @: cFuncType |>> loc)
                        @@ [cWidth, cRight] |>> loc
            cRight' <- case op of
                BitwiseLeftShift | checked -> boundShift
                BitwiseRightShift | checked -> boundShift
                _ -> return cRight
            plt <- gets targetPlatform
            case (if checked then checkedArithmetic op cLeft' cRight' loc else Nothing) of
                Just cChecked -> return cChecked
                Nothing ->
                    if productLeavesInt plt op cLeft' then
                        return $ multiplyInUnsignedInt plt cLeft' cRight' loc
                    else
                        return $ binOp (cBinOp op) cLeft' cRight' @: getCExprType cLeft |>> loc

  where

    loc = getLocation ann

genExpression :: Expression SemanticAnn -> CGenerator CExpression
genExpression (AccessObject obj) = do
    cObj <- genObject obj
    objType <- getObjType obj
    case objType of
        (TFixedLocation _) -> do
            return $ deref cObj |>> getLocation (getAnnotation obj)
        _ -> 
            return $ cObj @: getCObjType cObj |>> getLocation (getAnnotation obj)
genExpression (CheckedBinOp op left right ann) = genBinOp True op left right ann
genExpression (UncheckedBinOp op left right ann) = genBinOp False op left right ann
genExpression e@(Constant c ann) = do
    cType <- getExprType e >>= genType noqual
    case c of
        (I i _) ->
            let cInteger = genInteger i in
            return $ cInteger @: cType |>> getLocation ann
        (F f _) ->
            let cFloat = genFloat f in
            return $ cFloat @: cType |>> getLocation ann
        (B b) -> return $ b @: cType |>> getLocation ann
        (C chr) -> return $ chr @: cType |>> getLocation ann
        Null -> throwError $ InternalError "Null constant should not be translated to C"
genExpression (Casting expr ts ann) = do
    cType <- genType noqual ts
    -- | A binary operation is first cast to its own type, so that the value
    -- converted to the target type is the one of the operation.
    cExpr <- genExpression expr >>= castBinOpToOwnType (getLocation ann) expr
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
            -- | Else, we print the address to the data
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
            NoneLabel -> optionNoneTag @: enumFieldType |>> getLocation ann
            SomeLabel -> optionSomeTag @: enumFieldType |>> getLocation ann
            SuccessLabel -> statusSuccessTag @: enumFieldType |>> getLocation ann
            FailureLabel -> statusFailureTag @: enumFieldType |>> getLocation ann
            OkLabel -> resultOkTag @: enumFieldType |>> getLocation ann
            ErrorLabel -> resultErrorTag @: enumFieldType |>> getLocation ann
    return $ leftExpr @== rightExpr |>> getLocation ann
genExpression (UncheckedArraySlice _ak obj lower _upper ann) = do
    objType <- getObjType obj
    cLower <- genExpression lower
    cObj <- genObject obj
    case objType of
        TArray ty _ -> do
            cType <- genType noqual ty
            return $ addrOf (cObj @$$ cLower @: cType) |>> getLocation ann
        ty -> throwError $ InternalError $ "Unsupported object. Not a reference to an array: " ++ show ty
genExpression expr@(CheckedArraySlice _ak obj lower upper ann) = do
    objType <- getObjType obj
    expectedType <- getExprType expr
    cLower <- genExpression lower
    cObj <- genObject obj
    case (objType, expectedType) of
        (TArray ty arraySize, TReference _ (TArray _ expectedSize)) -> do
            cType <- genType noqual ty
            cUpper <- genExpression upper
            cArraySize <- genConstExpression arraySize
            cExpectedSize <- genConstExpression expectedSize
            let cFuncType = CTFunction size_t [_const size_t, _const size_t, _const size_t, _const size_t]
                cFunctionCall = ("termina__check__array_slice" @: cFuncType |>> getLocation ann)
                    @@ [cArraySize, cExpectedSize, cLower, cUpper] |>> getLocation ann
            return $ addrOf (cObj @$$ cFunctionCall @: cType) |>> getLocation ann
        (ty, _) -> throwError $ InternalError $ "Unsupported object. Not a reference to an array: " ++ show ty
genExpression o = throwError $ InternalError $ "Unsupported expression: " ++ show o

-- | Lowers an array or string initializer to a C initializer list { ... }.
-- These forms are not expressions (typeExpression rejects their use as such):
-- they appear only as the initializer of a declaration or const, so they get
-- their own lowering instead of living in genExpression. Nested array/string
-- elements recurse; scalar elements fall back to genExpression.
genInitializerExpr :: Expression SemanticAnn -> CGenerator CExpression
genInitializerExpr e@(ArrayExprListInitializer exprs ann) = do
    cType <- getExprType e >>= genType noqual
    cElems <- mapM genInitializerExpr exprs
    return $ cElems @:: cType |>> getLocation ann
genInitializerExpr e@(ArrayInitializer iexpr _size ann) = do
    -- | A fill [e; N] nested in an initializer position must be expanded to an
    -- explicit list { e, ..., e } (N copies): a C initializer list cannot hold
    -- a loop. Top-level fills are still emitted as for loops (see genStatement).
    cType <- getExprType e >>= genType noqual
    case cType of
        CTArray _ (CExprConstant (CIntConst (CInteger n _)) _ _) -> do
            cElem <- genInitializerExpr iexpr
            return $ replicate (fromIntegral n) cElem @:: cType |>> getLocation ann
        _ -> throwError $ InternalError $ "array fill initializer with non-literal size: " ++ show cType
genInitializerExpr e@(StringInitializer value ann) = do
    -- | A char-array string initializer is emitted as an explicit list of
    -- character constants { 'h', 'e', ..., '\0', ... }, padded with nulls up to
    -- the array size. We avoid the C string-literal form (char s[N] = "..."):
    -- it makes the no-trailing-null exact-fit case explicit instead of relying
    -- on string-literal truncation (which MISRA flags), and keeps declarations
    -- uniformly initializer lists. Requires a literal array size; const-sized
    -- (VLA) char arrays are initialized element-wise instead (see genStatement).
    cType <- getExprType e >>= genType noqual
    case cType of
        CTArray _ (CExprConstant (CIntConst (CInteger n _)) _ _) -> do
            let cChars = map (@: char) value
                cPadding = replicate (max 0 (fromIntegral n - length value)) ('\0' @: char)
            return $ (cChars ++ cPadding) @:: cType |>> getLocation ann
        _ -> throwError $ InternalError $ "string initializer with non-literal size: " ++ show cType
genInitializerExpr e@(StructInitializer fas ann) = do
    cType <- getExprType e >>= genType noqual
    cFields <- mapM genFieldInit fas
    return $ cFields @.: cType |>> getLocation ann
    where
        genFieldInit (FieldValueAssignment fld fexpr _) = do
            ce <- genInitializerExpr fexpr
            return (fld @.= ce)
        genFieldInit (FieldAddressAssignment fld addr (SemanticAnn (ETy (SimpleType ts)) _)) = do
            cTs <- genType noqual ts
            cAddr <- genExpression addr
            return (fld @.= cast cTs cAddr)
        genFieldInit fa = throwError $ InternalError $ "Unsupported field in data struct initializer: " ++ show fa
genInitializerExpr e@(MonadicVariantInitializer mv ann) = do
    cType <- getExprType e >>= genType noqual
    let variantTag name = name @: enumFieldType |>> getLocation ann
        singlePayload tag fieldName v = do
            cv <- genInitializerExpr v
            let inner = [variantParamField 0 @.= cv] @.: cType |>> getLocation ann
            return $ [variant @.= variantTag tag, fieldName @.= inner] @.: cType |>> getLocation ann
        noPayload tag =
            return $ [variant @.= variantTag tag] @.: cType |>> getLocation ann
    case mv of
        Some v -> singlePayload optionSomeTag optionSomeVariant v
        None -> noPayload optionNoneTag
        Success -> noPayload statusSuccessTag
        Failure v -> singlePayload statusFailureTag statusFailureVariant v
        Ok v -> singlePayload resultOkTag resultOkVariant v
        Error v -> singlePayload resultErrorTag resultErrorVariant v
genInitializerExpr e@(EnumVariantInitializer ts this_variant params ann) = do
    cType <- getExprType e >>= genType noqual
    let tagExpr = (ts <::> this_variant) @: enumFieldType |>> getLocation ann
    cParams <- zipWithM (\p i -> do
        cp <- genInitializerExpr p
        return (variantParamField (i :: Integer) @.= cp)) params [0..]
    let designators = (variant @.= tagExpr) :
            [this_variant @.= cParams @.: cType |>> getLocation ann | not (null cParams)]
    return $ designators @.: cType |>> getLocation ann
genInitializerExpr e = genExpression e
