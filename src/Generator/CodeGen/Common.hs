{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Common where

import ControlFlow.BasicBlocks.AST
import Semantic.Types
import Control.Monad.Except
import Generator.LanguageC.AST
import Utils.Annotations
import Configuration.Configuration
import Configuration.Platform (Platform)
import Generator.Monadic
import qualified Control.Monad.State as ST
import qualified Data.Set as S
import Data.Maybe (maybeToList)

newtype CGeneratorError = InternalError String
    deriving (Show)

data CGeneratorEnv = CGeneratorEnv { 
    currentModule :: QualifiedName,
    extraImports :: S.Set QualifiedName,
    monadicTypes :: MonadicTypes,
    configParams :: TerminaConfig,
    targetPlatform :: Platform,
    eventParamUsed :: Bool
  }

type CGenerator = ExceptT CGeneratorError (ST.State CGeneratorEnv)

(<::>) :: Identifier -> Identifier -> Identifier
(<::>) id0 id1 = id0 <> "__" <> id1

(<:>) :: Identifier -> Identifier -> Identifier
(<:>) id0 id1 = id0 <> "_" <> id1

-- | Every identifier a generated translation unit introduces: the file-scope
-- declarations (functions, objects, typedef and tag names, enumeration
-- constants, macro names) plus the function-scope identifiers inside each
-- definition (parameters and block-scope locals, including a @for@ init
-- declaration). File-scope external names are bound by the C limit on
-- significant characters in an external identifier; block-scope locals and
-- macros by the looser internal-identifier limit. All are collected and checked
-- against a single per-platform bound, which is conservative; a linkage-aware
-- split (external vs internal) is left as a future refinement. Struct/union
-- member names are not collected (a separate namespace, rarely long).
generatedIdentifiers :: CFile -> [Ident]
generatedIdentifiers file = concatMap itemIdents (fileItems file)
  where
    fileItems (CSourceFile _ its) = its
    fileItems (CHeaderFile _ its) = its

    itemIdents :: CFileItem -> [Ident]
    itemIdents (CFunctionDef _ (CFunction _ ident params body) _) =
        ident : paramIdents params ++ stmtIdents body
    itemIdents (CExtDecl ed _) = extDeclIdents ed
    itemIdents (CPPDirective d _) = ppIdents d

    extDeclIdents :: CExternalDeclaration -> [Ident]
    extDeclIdents (CEDVariable _ (CDecl _ mid _)) = maybeToList mid
    extDeclIdents (CEDFunction _ _ ident params) = ident : paramIdents params
    extDeclIdents (CEDEnum mid (CEnum _ constants _)) = maybeToList mid ++ map fst constants
    extDeclIdents (CEDStructUnion mid _) = maybeToList mid
    extDeclIdents (CEDTypeDef ident _) = [ident]

    ppIdents :: CPreprocessorDirective -> [Ident]
    ppIdents (CPPDefine ident _) = [ident]
    ppIdents _ = []

    paramIdents :: [CDeclaration] -> [Ident]
    paramIdents = concatMap (\(CDecl _ mid _) -> maybeToList mid)

    stmtIdents :: CStatement -> [Ident]
    stmtIdents (CSCompound items _)         = concatMap blockItemIdents items
    stmtIdents (CSIfThenElse _ s1 ms2 _)    = stmtIdents s1 ++ maybe [] stmtIdents ms2
    stmtIdents (CSFor initPart _ _ body _)  = forInitIdents initPart ++ stmtIdents body
    stmtIdents (CSCase _ s _)               = stmtIdents s
    stmtIdents (CSDefault s _)              = stmtIdents s
    stmtIdents (CSSwitch _ s _)             = stmtIdents s
    stmtIdents _                            = []

    blockItemIdents :: CCompoundBlockItem -> [Ident]
    blockItemIdents (CBlockStmt s)              = stmtIdents s
    blockItemIdents (CBlockDecl (CDecl _ mid _) _) = maybeToList mid

    forInitIdents :: Either (Maybe CExpression) CDeclaration -> [Ident]
    forInitIdents (Right (CDecl _ mid _)) = maybeToList mid
    forInitIdents (Left _)                = []

-- | Reject any generated identifier longer than the platform's
-- significant-character limit. @Nothing@ (the toolchain treats all characters as
-- significant, as GCC does) is a no-op, which is the case for every currently
-- supported platform. A per-identifier cap at or below the limit is sufficient
-- to rule out significant-character collisions, so no pairwise analysis is
-- needed. See 'Configuration.Platform.maxIdentifierLength'.
checkIdentifierLengths :: (MonadError CGeneratorError m) => Maybe Integer -> CFile -> m ()
checkIdentifierLengths Nothing _ = return ()
checkIdentifierLengths (Just limit) file =
    case filter ((> limit) . toInteger . length) (generatedIdentifiers file) of
        [] -> return ()
        (ident : _) -> throwError . InternalError $
            "generated identifier '" <> ident <> "' has " <> show (length ident) <>
            " significant characters, exceeding the " <> show limit <>
            "-character limit declared for the target platform"

-- | Termina's pretty builtin types
optionBox, boxStruct, sinkPort, inPort, outPort :: Identifier
optionBox = optionType <::> "box"
boxStruct = terminafy "box_t"
sinkPort = terminaID
inPort = terminaID
outPort = terminafy "out_port_t"

terminaID :: Identifier
terminaID = terminafy "id_t"

pool, allocator, msgQueue, periodicTimer :: Identifier
pool = terminafy "pool_t"
allocator = terminafy "allocator_t"
msgQueue = terminafy "msg_queue_t"
periodicTimer = terminafy "periodic_timer_t"

atomicMethodName :: Identifier -> Identifier
atomicMethodName mName = "atomic" <:> mName

poolMemoryArea :: Identifier -> Identifier
poolMemoryArea identifier = terminafy $ "pool_memory" <::> identifier

msgQueueSendMethodName :: Identifier
msgQueueSendMethodName = terminafy $ "out_port" <::> "send"

-- | Name of a field that the generator adds to a struct.
fieldify :: Identifier -> Identifier
fieldify = ("_" <>)

-- | Name of a symbol that the generator adds to the generated code.
terminafy :: Identifier -> Identifier
terminafy = ("termina" <::>)

-- | The names of the types of Termina that take type arguments, which the
-- generated code carries as they are written.
optionType, statusType, resultType :: Identifier
optionType = "Option"
statusType = "Status"
resultType = "Result"

-- | Name of the field that holds the parameter of a variant at the given
-- position.
variantParamField :: Integer -> Identifier
variantParamField = fieldify . show

thatField, thisParam, eventParam, selfParam, lockVar :: Identifier
thatField = fieldify "that"
thisParam = terminafy "this"
eventParam = terminafy "ev"
selfParam = "self"
lockVar = terminafy "lock"

resourceLockTypeField, taskMsgQueueIDField,
    taskIDField, handlerIDField :: Identifier
resourceLockTypeField = fieldify $ "lock" <:> "type"
taskMsgQueueIDField = fieldify $ "task" <:> "msg_queue" <:> "id"
taskIDField = fieldify $ "task" <:> "id"
handlerIDField = fieldify $ "handler" <:> "id"

genEnumStructName :: (MonadError CGeneratorError m) => Identifier -> m Identifier
genEnumStructName identifier = return $ terminafy $ "enum" <::> identifier <:> "t"

genEnumVariantName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genEnumVariantName enumId this_variant = return $ enumId <::> this_variant

genEnumParameterStructName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genEnumParameterStructName enumId this_variant = return $ terminafy $ "enum" <::> enumId <::> this_variant <:> "params" <:> "t"

genClassFunctionName :: (MonadError CGeneratorError m) => Identifier -> Identifier -> m Identifier
genClassFunctionName className functionName = return $ className <::> functionName

genTypeSpecName :: (MonadError CGeneratorError m) => TerminaType SemanticAnn -> m Identifier
genTypeSpecName TBool = return "bool"
genTypeSpecName TChar = return "char"
genTypeSpecName TUInt8 = return "u8"
genTypeSpecName TUInt16 = return "u16"
genTypeSpecName TUInt32 = return "u32"
genTypeSpecName TUInt64 = return "u64"
genTypeSpecName TInt8 = return "i8"
genTypeSpecName TInt16 = return "i16"
genTypeSpecName TInt32 = return "i32"
genTypeSpecName TInt64 = return "i64"
genTypeSpecName TUSize = return "usize"
genTypeSpecName TFloat32 = return "f32"
genTypeSpecName TFloat64 = return "f64"
genTypeSpecName (TStruct ident) = return ident
genTypeSpecName (TEnum ident) = return ident
genTypeSpecName ts' = throwError $ InternalError $ "invalid option type specifier: " ++ show ts'

-- | This function returns the name of the struct that represents the parameters
-- of an option type. 
genOptionParameterStructName :: (MonadError CGeneratorError m) => TerminaType SemanticAnn -> m Identifier
genOptionParameterStructName (TBoxSubtype _) = return $ terminafy $ "enum" <::> optionBox <::> optionSomeVariant <:> "params" <:> "t"
genOptionParameterStructName ty = do
    tyName <- genTypeSpecName ty
    return $ terminafy $ "enum" <::> optionType <::> tyName <::> optionSomeVariant <:> "params" <:> "t"

genStatusParameterStructName :: (MonadError CGeneratorError m) => TerminaType SemanticAnn -> m Identifier
genStatusParameterStructName ty = do
    tyName <- genTypeSpecName ty
    return $ terminafy $ "enum" <::> statusType <::> tyName <::> statusFailureVariant <:> "params" <:> "t"

genResultParameterStructName :: (MonadError CGeneratorError m) => 
    TerminaType SemanticAnn 
    -> TerminaType SemanticAnn
    -> Identifier
    -> m Identifier
genResultParameterStructName okTy errorTy this_variant = do
    okTyName <- genTypeSpecName okTy
    errorTyName <- genTypeSpecName errorTy
    return $ terminafy $ "enum" <::> resultType <::> okTyName <::> errorTyName <::> this_variant <:> "params" <:> "t"

variant :: Identifier
variant = fieldify "variant"

-- | The name of a variant, which is the name of the field that holds its
-- parameters.
optionSomeVariant, optionNoneVariant :: Identifier
optionSomeVariant = "Some"
optionNoneVariant = "None"

resultOkVariant, resultErrorVariant :: Identifier
resultOkVariant = "Ok"
resultErrorVariant = "Error"

statusSuccessVariant, statusFailureVariant :: Identifier
statusSuccessVariant = "Success"
statusFailureVariant = "Failure"

-- | The enum constant of a variant, qualified with the name of its type as the
-- variants of an enum of the program are.
optionSomeTag, optionNoneTag :: Identifier
optionSomeTag = optionType <::> optionSomeVariant
optionNoneTag = optionType <::> optionNoneVariant

resultOkTag, resultErrorTag :: Identifier
resultOkTag = resultType <::> resultOkVariant
resultErrorTag = resultType <::> resultErrorVariant

statusSuccessTag, statusFailureTag :: Identifier
statusSuccessTag = statusType <::> statusSuccessVariant
statusFailureTag = statusType <::> statusFailureVariant

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
getObjType :: (MonadError CGeneratorError m) => Object SemanticAnn -> m (TerminaType SemanticAnn)
getObjType (Variable _ (SemanticAnn (ETy (ObjectType _ ts)) _))                  = return ts
getObjType (ArrayIndexExpression _ _ (SemanticAnn (ETy (ObjectType _ ts)) _))    = return ts
getObjType (MemberAccess _ _ (SemanticAnn (ETy (ObjectType _ ts)) _))            = return ts
getObjType (MemberAccess _ _ (SemanticAnn (ETy (AccessPortObjType _ _ ts)) _))     = return ts 
getObjType (Dereference _ (SemanticAnn (ETy (ObjectType _ ts)) _))               = return ts
getObjType (Unbox _ (SemanticAnn (ETy (ObjectType _ ts)) _))                     = return ts
getObjType (DereferenceMemberAccess _ _ (SemanticAnn (ETy (ObjectType _ ts)) _)) = return ts
getObjType (DereferenceMemberAccess _ _ (SemanticAnn (ETy (AccessPortObjType _ _ ts)) _)) = return ts
getObjType ann = throwError $ InternalError $ "invalid object annotation: " ++ show ann

getExprType :: (MonadError CGeneratorError m) => Expression SemanticAnn -> m (TerminaType SemanticAnn)
getExprType (AccessObject obj) = getObjType obj
getExprType (Constant _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (MonadicVariantInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (BinOp _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ReferenceExpression _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (Casting _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (FunctionCall _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (MemberFunctionCall _ _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (DerefMemberFunctionCall _ _ _ (SemanticAnn (ETy (AppType _ ts)) _)) = return ts
getExprType (StructInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (EnumVariantInitializer _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayInitializer _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArrayExprListInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (StringInitializer _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (ArraySliceExpression _ _ _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (IsEnumVariantExpression _ _ _  (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType (IsMonadicVariantExpression _ _ (SemanticAnn (ETy (SimpleType ts)) _)) = return ts
getExprType ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

-- | Extracts the 'CObject' from a 'CExpression'.
-- This function is typically used when an expression is known to be a direct
-- representation of an object, i.e., an 'CExprValOf'.
-- It throws an 'InternalError' if the expression is not a 'CExprValOf'.
getCObject :: (MonadError CGeneratorError m) => CExpression -> m CObject
getCObject (CExprValOf obj _ _) = return obj
getCObject e = throwError $ InternalError ("getCObject: Expected CExprValOf, but received: " ++ show e)

-- | Generates the name of the option struct type.
-- This function is used to generate the name of the struct that represents the
-- option type. The function assumes that the option type is well-typed and that
-- the semantic annotation is correct. If the option type is not well-typed, the
-- function will throw an internal error.
genOptionStructName :: (MonadError CGeneratorError m) => TerminaType SemanticAnn -> m Identifier
genOptionStructName (TBoxSubtype _) = return optionBox
genOptionStructName ty = do
    tyName <- genTypeSpecName ty
    return $ optionType <::> tyName

genStatusStructName :: (MonadError CGeneratorError m) => TerminaType SemanticAnn -> m Identifier
genStatusStructName ty = do
    tyName <- genTypeSpecName ty
    return $ statusType <::> tyName


genResultStructName :: (MonadError CGeneratorError m) => 
    TerminaType SemanticAnn 
    -> TerminaType SemanticAnn -> m Identifier
genResultStructName tyOk tyError = do
    tyOkName <- genTypeSpecName tyOk
    tyErrorName <- genTypeSpecName tyError
    return $ resultType <::> tyOkName <::> tyErrorName

getCInteger :: TInteger -> CInteger
getCInteger (TInteger i DecRepr) = CInteger i CDecRepr
getCInteger (TInteger i HexRepr) = CInteger i CHexRepr

genIndexOf :: (MonadError CGeneratorError m) => CObject -> CExpression -> m CObject
genIndexOf obj index = 
    let cObjType = getCObjType obj in
    case cObjType of
        CTArray ty _ -> return $ CIndexOf obj index ty
        CTPointer ty _ -> return $ CIndexOf obj index ty
        _ -> throwError $ InternalError $ "invalid object type. Not indexable: " ++ show cObjType

genAddrOf :: (MonadError CGeneratorError m) => CObject -> CQualifier -> CAnns -> m CExpression
genAddrOf obj qual cAnn =
    let cObjType = getCObjType obj in
    case cObjType of
        CTArray {} -> return $ CExprValOf obj cObjType cAnn
        ty -> return $ CExprAddrOf obj (CTPointer ty qual) cAnn

genPoolMethodCallExpr :: (MonadError CGeneratorError m, ST.MonadState CGeneratorEnv m) => Identifier -> CObject -> CExpression -> CAnns ->  m CExpression
genPoolMethodCallExpr mName cObj cArg cAnn = do
    cEventArg <- genEventParamArg cAnn
    let cEventArgType = getCExprType cEventArg
    case mName of
        "alloc" -> do
            let cObjExpr = CExprValOf (CField cObj thatField (CTPointer (getCObjType cObj) noqual)) (CTPointer (getCObjType cObj) noqual) cAnn
                cFuncType = CTFunction (CTVoid noqual) [cEventArgType, getCExprType cObjExpr, getCExprType cArg]
                cFunctionCall = CField cObj "alloc" cFuncType
            return $ CExprCall (CExprValOf cFunctionCall cFuncType cAnn) [cEventArg, cObjExpr, cArg] (CTVoid noqual) cAnn
        "free" -> do
            let cObjExpr = CExprValOf (CField cObj thatField (CTPointer (getCObjType cObj) noqual)) (CTPointer (getCObjType cObj) noqual) cAnn
                cFuncType = CTFunction (CTVoid noqual) [cEventArgType, getCExprType cObjExpr]
                cFunctionCall = CField cObj "free" cFuncType
            return $ CExprCall (CExprValOf cFunctionCall cFuncType cAnn) [cEventArg, cObjExpr, cArg] (CTVoid noqual) cAnn
        _ -> throwError $ InternalError $ "invalid pool method name: " ++ mName

genMsgQueueSendNULLExpr :: (MonadError CGeneratorError m, ST.MonadState CGeneratorEnv m) => CObject -> CAnns -> m CExpression
genMsgQueueSendNULLExpr cObj cAnn = do
    cEventArg <- genEventParamArg cAnn
    let cEventArgType = getCExprType cEventArg
        cFuncType = CTFunction (CTVoid noqual) [cEventArgType, getCObjType cObj, CTPointer (CTVoid noqual) noqual]
        cObjExpr = CExprValOf cObj (getCObjType cObj) cAnn
        cDataArg = CExprValOf (CVar "NULL" (CTPointer (CTVoid noqual) noqual)) (CTPointer (CTVoid noqual) noqual) cAnn
    return $
        CExprCall (CExprValOf (CVar msgQueueSendMethodName cFuncType) cFuncType cAnn) [cEventArg, cObjExpr, cDataArg] (CTVoid noqual) cAnn

genMsgQueueSendCall :: (MonadError CGeneratorError m, ST.MonadState CGeneratorEnv m) => CObject -> CExpression -> CAnns -> m CExpression
genMsgQueueSendCall cObj cArg cAnn = do
    cEventArg <- genEventParamArg cAnn
    let cEventArgType = getCExprType cEventArg
        cArgType = getCExprType cArg
        cFuncType = CTFunction (CTVoid noqual) [cEventArgType, cArgType, CTPointer (CTVoid noqual) noqual]
        cObjExpr = CExprValOf cObj (getCObjType cObj) cAnn
    -- | If it is a send, the first parameter is the object to be sent. The
    -- function is expecting to receive a reference to that object.
    cArgObj <- getCObject cArg
    let cDataArg = CExprCast (CExprAddrOf cArgObj (CTPointer cArgType noqual) cAnn) (CTPointer (CTVoid noqual) noqual) cAnn
    return $
        CExprCall (CExprValOf (CVar msgQueueSendMethodName cFuncType) cFuncType cAnn) [cEventArg, cObjExpr, cDataArg] (CTVoid noqual) cAnn

genAtomicMethodCall :: (MonadError CGeneratorError m) => Identifier -> CExpression -> [CExpression] -> CAnns ->  m CExpression
genAtomicMethodCall mName cObj cArgs cAnn =
    case mName of
        "load" -> do
            let cFuncType = CTFunction (CTVoid noqual) [getCExprType cObj]
            return $ CExprCall (CExprValOf (CVar (atomicMethodName mName) cFuncType) cFuncType cAnn) [cObj] (CTVoid noqual) cAnn
        "store" -> do
            let cFuncType = CTFunction (CTVoid noqual) (getCExprType cObj : fmap getCExprType cArgs)
            return $ CExprCall (CExprValOf (CVar (atomicMethodName mName) cFuncType) cFuncType cAnn) (cObj : cArgs) (CTVoid noqual) cAnn
        _ -> throwError $ InternalError $ "invalid atomic method name: " ++ mName

genInteger :: TInteger -> CInteger
genInteger (TInteger i DecRepr) = CInteger i CDecRepr
genInteger (TInteger i HexRepr) = CInteger i CHexRepr

genFloat :: TFloat -> CFloat
genFloat (TFloat d FPDecimal)    = CFloat d CFloatDec
genFloat (TFloat d FPScientific) = CFloat d CFloatSci

getCArrayItemType :: (MonadError CGeneratorError m) => CType -> m CType
getCArrayItemType (CTArray ty _) = return ty
getCArrayItemType ty = throwError $ InternalError $ "invalid array type: " ++ show ty

internalAnn :: CItemAnn -> CAnns
internalAnn = flip LocatedElement Internal

buildGenericAnn :: SemanticAnn -> CAnns
buildGenericAnn (SemanticAnn _ loc) = LocatedElement CGenericAnn loc

buildStatementAnn :: SemanticAnn -> Bool -> CAnns
buildStatementAnn (SemanticAnn _ loc) before = LocatedElement (CStatementAnn before False) loc

buildDeclarationAnn :: SemanticAnn -> Bool -> CAnns
buildDeclarationAnn (SemanticAnn _ loc) before = LocatedElement (CDeclarationAnn before) loc

buildCompoundAnn :: SemanticAnn -> Bool -> Bool -> CAnns
buildCompoundAnn (SemanticAnn _ loc) before trailing = LocatedElement (CCompoundAnn before trailing) loc

buildCPPDirectiveAnn :: SemanticAnn -> Bool -> CAnns
buildCPPDirectiveAnn (SemanticAnn _ loc) before = LocatedElement (CPPDirectiveAnn before) loc

-- | TODO: The size of the enum field has been hardcoded, it should be
-- platform dependent
enumFieldType :: CType
enumFieldType = CTInt IntSize32 Unsigned noqual

taskFunctionName :: (MonadError CGeneratorError m) => Identifier -> m Identifier
taskFunctionName classId = return $ terminafy $ "task_entry" <::> classId

-- | Object that refers to the event parameter of the class member being
-- generated. It records that the member uses the parameter, so that its
-- definition does not discard it.
genEventParamObj :: (ST.MonadState CGeneratorEnv m) => m CObject
genEventParamObj = do
    ST.modify (\env -> env { eventParamUsed = True })
    return $ CVar eventParam (CTPointer (CTTypeDef "termina__event_t" noqual) noqual)

-- | Expression that passes the event parameter of the class member being
-- generated as an argument.
genEventParamArg :: (ST.MonadState CGeneratorEnv m) => CAnns -> m CExpression
genEventParamArg cAnn = do
    cEventObj <- genEventParamObj
    return $ CExprValOf cEventObj (getCObjType cEventObj) cAnn

-- | Parameters whose name starts with a single underscore are ignored: the
-- body of the function cannot use them.
isIgnoredParameter :: Identifier -> Bool
isIgnoredParameter ('_' : c : _) = c /= '_'
isIgnoredParameter _ = False

-- | Statements that discard the given parameters with a cast to void, to be
-- placed at the beginning of a function body.
genDiscardedParameters :: [(Identifier, CType)] -> [CCompoundBlockItem]
genDiscardedParameters params =
    zipWith discard (True : repeat False) params

    where

        discard before (identifier, cType) =
            let cAnn = internalAnn CGenericAnn in
            CBlockStmt $ CSDo
                (CExprCast (CExprValOf (CVar identifier cType) cType cAnn) (CTVoid noqual) cAnn)
                (internalAnn (CStatementAnn before False))