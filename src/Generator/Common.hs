{-# LANGUAGE FlexibleContexts #-}

module Generator.Common where

import AST.Seman
import Semantic.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map
import Data.Set
import Generator.LanguageC.AST

newtype CGeneratorError = InternalError String
    deriving (Show)

type Substitutions = Map Identifier CExpression
type OptionTypes = Map TypeSpecifier (Set TypeSpecifier)

type CSourceGenerator = ReaderT Substitutions (Either CGeneratorError)
type CHeaderGenerator = ReaderT OptionTypes (Either CGeneratorError)

-- |  This function is used to create the names of temporal variables
--  and symbols.
namefy :: Identifier -> Identifier
namefy = ("__" <>)

(<::>) :: Identifier -> Identifier -> Identifier
(<::>) id0 id1 = id0 <> "__" <> id1

-- | Termina's pretty builtin types
pool, msgQueue, optionDyn, dynamicStruct, taskID, resourceID, sinkPort, inPort, outPort :: Identifier
pool = namefy $ "termina" <::> "pool_t"
msgQueue = namefy $ "termina" <::> "msg_queue_t"
optionDyn = namefy $ "option" <::> "dyn_t"
dynamicStruct = namefy $ "termina" <::> "dyn_t"
taskID = namefy $ "termina" <::> "task_t"
resourceID = namefy $ "termina" <::> "resource_t"
sinkPort = namefy $ "termina" <::> "sink_port_t"
inPort = namefy $ "termina" <::> "in_port_t"
outPort = namefy $ "termina" <::> "out_port_t"

poolMethodName :: Identifier -> Identifier
poolMethodName mName = namefy $ "termina" <::> "pool" <::> mName

msgQueueMethodName :: Identifier -> Identifier
msgQueueMethodName mName = namefy $ "termina" <::> "msg_queue" <::> mName

thatField :: Identifier
thatField = "__that"

enumStructName :: Identifier -> Identifier
enumStructName identifier = namefy ("enum_" <> identifier <> "_t")

enumParameterStructName :: Identifier -> Identifier -> Identifier
enumParameterStructName enumId variant = namefy $ enumId <::> variant <> "_params_t"

-- | This function returns the name of the struct that represents the parameters
-- of an option type. 
genOptionParameterStructName :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
genOptionParameterStructName Bool = return $ namefy $ "option" <::> "bool_params_t"
genOptionParameterStructName Char = return $ namefy $ "option" <::> "char_params_t"
genOptionParameterStructName UInt8 = return $ namefy $ "option" <::> "uint8_params_t"
genOptionParameterStructName UInt16 = return $ namefy $ "option" <::> "uint16_params_t"
genOptionParameterStructName UInt32 = return $ namefy $ "option" <::> "uint32_params_t"
genOptionParameterStructName UInt64 = return $ namefy $ "option" <::> "uint64_params_t"
genOptionParameterStructName Int8 = return $ namefy $ "option" <::> "int8_params_t"
genOptionParameterStructName Int16 = return $ namefy $ "option" <::> "int16_params_t"
genOptionParameterStructName Int32 = return $ namefy $ "option" <::> "int32_params_t"
genOptionParameterStructName Int64 = return $ namefy $ "option" <::> "int64_params_t"
genOptionParameterStructName ts@(Option _) = throwError $ InternalError $ "invalid recursive option type: " ++ show ts
genOptionParameterStructName (DynamicSubtype _) = return $ namefy $ "option" <::> "dyn_params_t"
genOptionParameterStructName ts@(Vector {}) = do
    tsName <- genTypeSpecName ts
    tsDimension <- genDimensionOptionTS ts
    return $ namefy $ "option" <::> tsName <> tsDimension <> "_params_t"

    where
        genTypeSpecName :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
        genTypeSpecName UInt8 = return "uint8"
        genTypeSpecName UInt16 = return "uint16"
        genTypeSpecName UInt32 = return "uint32"
        genTypeSpecName UInt64 = return "uint64"
        genTypeSpecName Int8 = return "int8"
        genTypeSpecName Int16 = return "int16"
        genTypeSpecName Int32 = return "int32"
        genTypeSpecName Int64 = return "int64"
        genTypeSpecName (Vector ts' _) = genTypeSpecName ts'
        genTypeSpecName (DefinedType typeIdentifier) = return typeIdentifier
        genTypeSpecName ts' = throwError $ InternalError $ "invalid option type specifier: " ++ show ts'

        genDimensionOptionTS :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
        genDimensionOptionTS (Vector ts' (K s)) = (("_" <> show s) <>) <$> genDimensionOptionTS ts'
        genDimensionOptionTS _ = return ""
genOptionParameterStructName ts = throwError $ InternalError $ "invalid option type specifier: " ++ show ts

enumVariantsField :: Identifier
enumVariantsField = namefy "variant"

optionSomeVariant, optionNoneVariant :: Identifier
optionSomeVariant = "Some"
optionNoneVariant = "None"

optionSomeField :: Identifier
optionSomeField = "__0"

-- | This function returns the type of an object. The type is extracted from the
-- object's semantic annotation. The function assumes that the object is well-typed
-- and that the semantic annotation is correct. If the object is not well-typed, the
-- function will throw an error.
getObjectType :: (MonadError CGeneratorError m) => Object SemanticAnns -> m TypeSpecifier
getObjectType (Variable _ (SemAnn _ (ETy (ObjectType _ ts))))                  = return $ ts
getObjectType (VectorIndexExpression _ _ (SemAnn _ (ETy (ObjectType _ ts))))   = return $ ts
getObjectType (MemberAccess _ _ (SemAnn _ (ETy (ObjectType _ ts))))            = return $ ts
getObjectType (Dereference _ (SemAnn _ (ETy (ObjectType _ ts))))               = return $ ts
getObjectType (VectorSliceExpression _ _ _ (SemAnn _ (ETy (ObjectType _ ts)))) = return $ ts
getObjectType (Undyn _ (SemAnn _ (ETy (ObjectType _ ts))))                     = return $ ts
getObjectType (DereferenceMemberAccess _ _ (SemAnn _ (ETy (ObjectType _ ts)))) = return $ ts
getObjectType ann = throwError $ InternalError $ "invalid object annotation: " ++ show ann

getParameters :: (MonadError CGeneratorError m) => Expression SemanticAnns -> m [Parameter]
getParameters (FunctionExpression _ _ (SemAnn _ (ETy (AppType params _)))) = return params
getParameters ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

getExprType :: (MonadError CGeneratorError m) => Expression SemanticAnns -> m TypeSpecifier
getExprType (AccessObject obj) = getObjectType obj
getExprType (Constant _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (OptionVariantExpression _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (BinOp _ _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (ReferenceExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (Casting _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (FunctionExpression _ _ (SemAnn _ (ETy (AppType _ ts)))) = return $ ts
getExprType (MemberFunctionAccess _ _ _ (SemAnn _ (ETy (AppType _ ts)))) = return $ ts
getExprType (FieldAssignmentsExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (EnumVariantExpression _ _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType (VectorInitExpression _ _ (SemAnn _ (ETy (SimpleType ts)))) = return $ ts
getExprType ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

-- | Generates the name of the option struct type
genOptionStructName :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
genOptionStructName Bool = return $ namefy $ "option" <::> "bool_t"
genOptionStructName Char = return $ namefy $ "option" <::> "char_t"
genOptionStructName UInt8 = return $ namefy $ "option" <::> "uint8_t"
genOptionStructName UInt16 = return $ namefy $ "option" <::> "uint16_t"
genOptionStructName UInt32 = return $ namefy $ "option" <::> "uint32_t"
genOptionStructName UInt64 = return $ namefy $ "option" <::> "uint64_t"
genOptionStructName Int8 = return $ namefy $ "option" <::> "int8_t"
genOptionStructName Int16 = return $ namefy $ "option" <::> "int16_t"
genOptionStructName Int32 = return $ namefy $ "option" <::> "int32_t"
genOptionStructName Int64 = return $ namefy $ "option" <::> "int64_t"
genOptionStructName ts@(Option _) = throwError $ InternalError $ "invalid recursive option type: " ++ show ts
genOptionStructName (DynamicSubtype _) = return optionDyn
genOptionStructName ts@(Vector {}) = do
    tsName <- genTypeSpecName ts
    tsDimension <- genDimensionOptionTS ts
    return $ namefy $ "option" <::> tsName <> tsDimension <> "_t"

    where
        genTypeSpecName :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
        genTypeSpecName UInt8 = return "uint8"
        genTypeSpecName UInt16 = return "uint16"
        genTypeSpecName UInt32 = return "uint32"
        genTypeSpecName UInt64 = return "uint64"
        genTypeSpecName Int8 = return "int8"
        genTypeSpecName Int16 = return "int16"
        genTypeSpecName Int32 = return "int32"
        genTypeSpecName Int64 = return "int64"
        genTypeSpecName (Vector ts' _) = genTypeSpecName ts'
        genTypeSpecName (DefinedType typeIdentifier) = return typeIdentifier
        genTypeSpecName ts' = throwError $ InternalError $ "invalid option type specifier: " ++ show ts'

        genDimensionOptionTS :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
        genDimensionOptionTS (Vector ts' (K s)) = (("_" <> show s) <>) <$> genDimensionOptionTS ts'
        genDimensionOptionTS _ = return ""
genOptionStructName ts = throwError $ InternalError $ "invalid option type specifier: " ++ show ts

genArrayWrapStructName :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
genArrayWrapStructName ts@(Vector {}) = do
    tsName <- genTypeSpecName ts
    tsDimension <- genDimensionOptionTS ts
    return $ namefy $ "wrapper" <::> tsName <> tsDimension <> "_t"

    where
        genTypeSpecName :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
        genTypeSpecName UInt8 = return "uint8"
        genTypeSpecName UInt16 = return "uint16"
        genTypeSpecName UInt32 = return "uint32"
        genTypeSpecName UInt64 = return "uint64"
        genTypeSpecName Int8 = return "int8"
        genTypeSpecName Int16 = return "int16"
        genTypeSpecName Int32 = return "int32"
        genTypeSpecName Int64 = return "int64"
        genTypeSpecName (Vector ts' _) = genTypeSpecName ts'
        genTypeSpecName (DefinedType typeIdentifier) = return typeIdentifier
        genTypeSpecName ts' = throwError $ InternalError $ "invalid option type specifier: " ++ show ts'

        genDimensionOptionTS :: (MonadError CGeneratorError m) => TypeSpecifier -> m Identifier
        genDimensionOptionTS (Vector ts' (K s)) = (("_" <> show s) <>) <$> genDimensionOptionTS ts'
        genDimensionOptionTS _ = return ""
genArrayWrapStructName ts = throwError $ InternalError $ "invalid option type specifier: " ++ show ts

-- | Obtains the corresponding C type of a primitive type
genDeclSpecifiers :: (MonadError CGeneratorError m) => TypeSpecifier -> m [CDeclarationSpecifier]
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

genArraySizeDeclarator :: TypeSpecifier -> SemanticAnns -> [CDerivedDeclarator]
genArraySizeDeclarator (Vector ts' (K s)) ann = 
    let cAnn = buildGenericAnn ann in
    CArrDeclr [] (CArrSize False (CConst (CIntConst (CInteger s DecRepr)) cAnn)) cAnn : genArraySizeDeclarator ts' ann
genArraySizeDeclarator _ _ = []

genCastDeclaration :: (MonadError CGeneratorError m) => TypeSpecifier -> SemanticAnns -> m CDeclaration
genCastDeclaration (DynamicSubtype ts@(Vector _ _)) ann = do
    -- We must obtain the declaration specifier of the vector
    specs <- genDeclSpecifiers ts
    decls <- genPtrArrayDeclarator ts ann
    return $ CDeclaration specs [(Just decls, Nothing, Nothing)] declAnn

    where

        cAnn, declAnn :: CAnns
        cAnn = buildGenericAnn ann
        declAnn = buildDeclarationAnn ann False

        genPtrArrayDeclarator :: (MonadError CGeneratorError m) => TypeSpecifier -> SemanticAnns -> m CDeclarator
        genPtrArrayDeclarator (Vector ts' _) ann' = do
            return $ CDeclarator Nothing (CPtrDeclr [] cAnn : genArraySizeDeclarator ts' ann') [] cAnn
        genPtrArrayDeclarator ts' _ = throwError $ InternalError $ "Invalid type specifier, not an array: " ++ show ts'

genCastDeclaration (DynamicSubtype ts) ann = do
    let cAnn = buildGenericAnn ann
        declAnn = buildDeclarationAnn ann False
    specs <- genDeclSpecifiers ts
    return $ CDeclaration specs [(Just (CDeclarator Nothing [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] declAnn
genCastDeclaration ts ann = do
    let declAnn = buildDeclarationAnn ann False
    specs <- genDeclSpecifiers ts
    return $ CDeclaration specs [] declAnn

buildGenericAnn :: SemanticAnns -> CAnns
buildGenericAnn ann = CAnnotations (Semantic.Monad.location ann) CGenericAnn

buildStatementAnn :: SemanticAnns -> Bool -> CAnns
buildStatementAnn ann before = CAnnotations (Semantic.Monad.location ann) (CStatementAnn before)

buildDeclarationAnn :: SemanticAnns -> Bool -> CAnns
buildDeclarationAnn ann before = CAnnotations (Semantic.Monad.location ann) (CDeclarationAnn before)

buildCompoundAnn :: SemanticAnns -> Bool -> Bool -> CAnns
buildCompoundAnn ann before trailing = CAnnotations (Semantic.Monad.location ann) (CCompoundAnn before trailing)