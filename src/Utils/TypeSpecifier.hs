-- | Helper functions for types

module Utils.TypeSpecifier where

import           AST.Parser

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

returnValueTy :: TypeSpecifier -> Bool
returnValueTy UInt8           = True
returnValueTy UInt16          = True
returnValueTy UInt32          = True
returnValueTy UInt64          = True
returnValueTy Int8            = True
returnValueTy Int16           = True
returnValueTy Int32           = True
returnValueTy Int64           = True
returnValueTy USize           = True
returnValueTy Bool            = True
returnValueTy Char            = True
returnValueTy (DefinedType _) = True
returnValueTy _               = False

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
parameterTy (Reference _ (BoxSubtype _)) = False
parameterTy (Reference _ (Option (BoxSubtype _))) = False
parameterTy (Reference {})  = True
parameterTy (Option (BoxSubtype _)) = False
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
-- Also at box object and function returned values.
-- Definition https://hackmd.io/a4CZIjogTi6dXy3RZtyhCA?view#Simple-types
simpleType :: TypeSpecifier -> Bool
simpleType Unit                = False
simpleType (Option (BoxSubtype {})) = False
simpleType (BoxSubtype {}) = False
simpleType (MsgQueue {})       = False
simpleType (Pool {})           = False
simpleType (Reference {})      = False
simpleType (Location {})       = False
simpleType (SinkPort {})       = False
simpleType (AccessPort {})     = False
simpleType _                   = True

classFieldType :: TypeSpecifier -> Bool
classFieldType Unit                         = False
classFieldType (BoxSubtype {})          = False
classFieldType (Option (BoxSubtype {})) = False
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

-- | Predicate defining when a |TypeSpecifier| can be used in a comparison.
equatableTy :: TypeSpecifier -> Bool
equatableTy UInt8  = True
equatableTy UInt16 = True
equatableTy UInt32 = True
equatableTy UInt64 = True
equatableTy Int8   = True
equatableTy Int16  = True
equatableTy Int32  = True
equatableTy Int64  = True
equatableTy USize  = True
equatableTy Bool   = True
equatableTy Char   = True
equatableTy _      = False

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
-- Box Helpers
isBox :: TypeSpecifier -> Maybe TypeSpecifier
isBox (BoxSubtype t) = Just t
isBox _ = Nothing

isNonBoxOption :: TypeSpecifier -> Bool
isNonBoxOption (Option (BoxSubtype _)) = False
isNonBoxOption (Option _) = True
isNonBoxOption _ = False

hasBoxOrDep :: TypeSpecifier -> Either Identifier Bool
hasBoxOrDep (DefinedType ident) = Left ident
hasBoxOrDep UInt8 = Right False
hasBoxOrDep UInt16 = Right False
hasBoxOrDep UInt32 = Right False
hasBoxOrDep UInt64 = Right False
hasBoxOrDep Int8 = Right False
hasBoxOrDep Int16 = Right False
hasBoxOrDep Int32 = Right False
hasBoxOrDep Int64 = Right False
hasBoxOrDep USize = Right False
hasBoxOrDep Bool = Right False
hasBoxOrDep Char = Right False
--
hasBoxOrDep (Array ty _s) = hasBoxOrDep ty
hasBoxOrDep (Option ty) = hasBoxOrDep ty
hasBoxOrDep (Reference _accK ty) = hasBoxOrDep ty
hasBoxOrDep (BoxSubtype _) = Right True
hasBoxOrDep _ = Right False
----------------------------------------

rootType :: TypeSpecifier -> TypeSpecifier
rootType (Option ts) = rootType ts
rootType (Array ts _) = rootType ts
rootType (MsgQueue ts _) = rootType ts
rootType (Pool ts _) = rootType ts
rootType (Reference _ ts) = rootType ts
rootType (BoxSubtype ts) = rootType ts
rootType (Location ts) = rootType ts
rootType t = t
