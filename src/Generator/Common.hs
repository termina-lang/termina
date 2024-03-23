module Generator.Common where

import AST.Seman
import Semantic.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map

newtype CGeneratorError = InternalError String
    deriving (Show)

type Substitutions = Map Identifier Identifier

type CGenerator = ReaderT Substitutions (Either CGeneratorError)

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
getObjectType :: Object SemanticAnns -> CGenerator TypeSpecifier
getObjectType (Variable _ (SemAnn _ (ETy (ObjectType _ ts))))                  = return $ ts
getObjectType (VectorIndexExpression _ _ (SemAnn _ (ETy (ObjectType _ ts))))   = return $ ts
getObjectType (MemberAccess _ _ (SemAnn _ (ETy (ObjectType _ ts))))            = return $ ts
getObjectType (Dereference _ (SemAnn _ (ETy (ObjectType _ ts))))               = return $ ts
getObjectType (VectorSliceExpression _ _ _ (SemAnn _ (ETy (ObjectType _ ts)))) = return $ ts
getObjectType (Undyn _ (SemAnn _ (ETy (ObjectType _ ts))))                     = return $ ts
getObjectType (DereferenceMemberAccess _ _ (SemAnn _ (ETy (ObjectType _ ts)))) = return $ ts
getObjectType ann = throwError $ InternalError $ "invalid object annotation: " ++ show ann

getParameters :: Expression SemanticAnns -> CGenerator [Parameter]
getParameters (FunctionExpression _ _ (SemAnn _ (ETy (AppType params _)))) = return params
getParameters ann = throwError $ InternalError $ "invalid expression annotation: " ++ show ann

getExprType :: Expression SemanticAnns -> CGenerator TypeSpecifier
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
genOptionStructName :: TypeSpecifier -> CGenerator Identifier
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
genOptionStructName ts@(Vector {}) = do
    tsName <- genTypeSpecName ts
    tsDimension <- genDimensionOptionTS ts
    return $ namefy $ "option" <::> tsName <> tsDimension <> "_t"

    where
        genTypeSpecName :: TypeSpecifier -> CGenerator Identifier
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

        genDimensionOptionTS :: TypeSpecifier -> CGenerator Identifier
        genDimensionOptionTS (Vector ts' (K s)) = (("_" <> show s) <>) <$> genDimensionOptionTS ts'
        genDimensionOptionTS _ = return ""
genOptionStructName ts = throwError $ InternalError $ "invalid option type specifier: " ++ show ts

genArrayWrapStructName :: TypeSpecifier -> CGenerator Identifier
genArrayWrapStructName ts@(Vector {}) = do
    tsName <- genTypeSpecName ts
    tsDimension <- genDimensionOptionTS ts
    return $ namefy $ "wrapper" <::> tsName <> tsDimension <> "_t"

    where
        genTypeSpecName :: TypeSpecifier -> CGenerator Identifier
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

        genDimensionOptionTS :: TypeSpecifier -> CGenerator Identifier
        genDimensionOptionTS (Vector ts' (K s)) = (("_" <> show s) <>) <$> genDimensionOptionTS ts'
        genDimensionOptionTS _ = return ""
genArrayWrapStructName ts = throwError $ InternalError $ "invalid option type specifier: " ++ show ts