-- | Helper functions for types

module Utils.TypeSpecifier where

import           AST.Parser
import GHC.Generics (UInt)

-- Primitive Types definition. Assuming |TypeSpecifier| is well-formed.
primitiveTypes :: TypeSpecifier -> Bool
primitiveTypes UInt8           = True
primitiveTypes UInt16          = True
primitiveTypes UInt32          = True
primitiveTypes UInt64          = True
primitiveTypes Int8            = True
primitiveTypes Int16           = True
primitiveTypes Int32           = True
primitiveTypes Int64           = True
primitiveTypes USize           = True
primitiveTypes Bool            = True
primitiveTypes Char            = True
primitiveTypes (DefinedType _) = True
primitiveTypes (Array _ _)     = True
primitiveTypes  _              = False

parameterTy :: TypeSpecifier -> Bool
parameterTy UInt8           = True
parameterTy UInt16          = True
parameterTy UInt32          = True
parameterTy UInt64          = True
parameterTy Int8            = True
parameterTy Int16           = True
parameterTy Int32           = True
parameterTy Int64           = True
parameterTy USize           = True
parameterTy Bool            = True
parameterTy Char            = True
parameterTy (DefinedType _) = True
parameterTy (Reference _ (DynamicSubtype _)) = False
parameterTy (Reference _ (Option (DynamicSubtype _))) = False
parameterTy (Reference {})  = True
parameterTy (Option (DynamicSubtype _)) = False
parameterTy (Option _)      = True
parameterTy _               = False

procedureParamTy :: TypeSpecifier -> Bool
procedureParamTy UInt8           = True
procedureParamTy UInt16          = True
procedureParamTy UInt32          = True
procedureParamTy UInt64          = True
procedureParamTy Int8            = True
procedureParamTy Int16           = True
procedureParamTy Int32           = True
procedureParamTy Int64           = True
procedureParamTy USize           = True
procedureParamTy Bool            = True
procedureParamTy Char            = True
procedureParamTy (DefinedType _) = True
procedureParamTy (Reference {})  = True
procedureParamTy (Option _)      = True
procedureParamTy _               = False

-- | The following function defines what we consider to be simple types.
-- Simple types can be used at variable creation, inside arrays and user defined structures.
-- Also at dynamic object and function returned values.
-- Definition https://hackmd.io/a4CZIjogTi6dXy3RZtyhCA?view#Simple-types
simpleType :: TypeSpecifier -> Bool
simpleType Unit                = False
simpleType (Option (DynamicSubtype {})) = False
simpleType (DynamicSubtype {}) = False
simpleType (MsgQueue {})       = False
simpleType (Pool {})           = False
simpleType (Reference {})      = False
simpleType (Location {})       = False
simpleType (SinkPort {})       = False
simpleType (AccessPort {})     = False
simpleType _                   = True

classFieldType :: TypeSpecifier -> Bool
classFieldType Unit                         = False
classFieldType (DynamicSubtype {})          = False
classFieldType (Option (DynamicSubtype {})) = False
classFieldType (MsgQueue {})                = False
classFieldType (Pool {})                    = False
classFieldType (Reference {})               = False
classFieldType _                            = True

boolTy :: TypeSpecifier -> Bool
boolTy Bool = True
boolTy _    = False

-- | Predicate definining when a |TypeSpecifier| is numeric.
numTy :: TypeSpecifier -> Bool
numTy UInt8  = True
numTy UInt16 = True
numTy UInt32 = True
numTy UInt64 = True
numTy Int8   = True
numTy Int16  = True
numTy Int32  = True
numTy Int64  = True
numTy USize  = True
numTy _      = False

posTy :: TypeSpecifier -> Bool
posTy UInt8  = True
posTy UInt16 = True
posTy UInt32 = True
posTy UInt64 = True
posTy USize  = True
posTy _      = False

memberIntCons :: Integer -> TypeSpecifier -> Bool
memberIntCons i UInt8  = ( 0 <= i ) && ( i <= 255)
memberIntCons i UInt16 = ( 0 <= i ) && ( i <= 65536)
memberIntCons i UInt32 = ( 0 <= i ) && ( i <= 4294967295)
memberIntCons i UInt64 = ( 0 <= i ) && ( i <= 18446744073709551615)
memberIntCons i Int8   = ( -128 <= i ) && ( i <= 127 )
memberIntCons i Int16  = ( -32768 <= i ) && ( i <= 32767 )
memberIntCons i Int32  = ( -2147483648 <= i ) && ( i <= 2147483647 )
memberIntCons i Int64  = ( -9223372036854775808 <= i ) && ( i <= 9223372036854775807 )
-- | TODO: This value depends on the target architecture and shall be selected
-- accordingly. Since we are currently targeting 32-bit systems, we assume that
-- usize is a 32-bit unsigned integer.
memberIntCons i USize  = ( 0 <= i ) && ( i <= 4294967295)
memberIntCons _ _      = False

identifierType :: TypeDef' expr lho a -> Identifier
identifierType (Struct ident _ _) = ident
identifierType (Enum ident _ _)   = ident
identifierType (Class _ ident _ _ _)  = ident
identifierType (Interface ident _ _) = ident

referenceType :: TypeSpecifier -> Bool
referenceType Unit            = False
referenceType (MsgQueue {})   = False
referenceType (Pool {})       = False
referenceType (Reference {})  = False
referenceType (Location {})   = False
referenceType (SinkPort {})   = False
referenceType (AccessPort {}) = False
referenceType _               = True

----------------------------------------
-- Dynamic Helpers
isDyn :: TypeSpecifier -> Maybe TypeSpecifier
isDyn (DynamicSubtype t) = Just t
isDyn _ = Nothing

isNonDynOption :: TypeSpecifier -> Bool
isNonDynOption (Option (DynamicSubtype _)) = False
isNonDynOption (Option _) = True
isNonDynOption _ = False

hasDynOrDep :: TypeSpecifier -> Either Identifier Bool
hasDynOrDep (DefinedType ident) = Left ident
hasDynOrDep UInt8 = Right False
hasDynOrDep UInt16 = Right False
hasDynOrDep UInt32 = Right False
hasDynOrDep UInt64 = Right False
hasDynOrDep Int8 = Right False
hasDynOrDep Int16 = Right False
hasDynOrDep Int32 = Right False
hasDynOrDep Int64 = Right False
hasDynOrDep USize = Right False
hasDynOrDep Bool = Right False
hasDynOrDep Char = Right False
--
hasDynOrDep (Array ty _s) = hasDynOrDep ty
hasDynOrDep (Option ty) = hasDynOrDep ty
hasDynOrDep (Reference _accK ty) = hasDynOrDep ty
hasDynOrDep (DynamicSubtype _) = Right True
hasDynOrDep _ = Right False
----------------------------------------

rootType :: TypeSpecifier -> TypeSpecifier
rootType (Option ts) = rootType ts
rootType (Array ts _) = rootType ts
rootType (MsgQueue ts _) = rootType ts
rootType (Pool ts _) = rootType ts
rootType (Reference _ ts) = rootType ts
rootType (DynamicSubtype ts) = rootType ts
rootType (Location ts) = rootType ts
rootType t = t
