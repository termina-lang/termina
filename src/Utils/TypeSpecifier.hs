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
primitiveTypes (Vector _ _)    = True
primitiveTypes  _              = False

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
simpleType (Port {})           = False
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

numCons :: Const -> Bool
numCons (I _ _) = True
numCons _       = False

numConstExpression :: ConstExpression -> Bool
numConstExpression (KC c) = numCons c
numConstExpression _      = error "TODO this is a bit weird."

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
identifierType (Class _ ident _ _)  = ident

referenceType :: TypeSpecifier -> Bool
referenceType Unit           = False
referenceType (MsgQueue {})  = False
referenceType (Pool {})      = False
referenceType (Reference {}) = False
referenceType (Location {})  = False
referenceType (Port {})      = False
referenceType _              = True

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
hasDynOrDep (Vector ty _s) = hasDynOrDep ty
hasDynOrDep (Option ty) = hasDynOrDep ty
hasDynOrDep (MsgQueue ty _s) = hasDynOrDep ty
hasDynOrDep (Pool ty _s) = hasDynOrDep ty
hasDynOrDep (Reference _accK ty) = hasDynOrDep ty
hasDynOrDep (DynamicSubtype _) = Right True
hasDynOrDep (Location ty) = hasDynOrDep ty
hasDynOrDep (Port ty) = hasDynOrDep ty
hasDynOrDep Unit = Right False
----------------------------------------

rootType :: TypeSpecifier -> TypeSpecifier
rootType (Option ts) = rootType ts
rootType (Vector ts _) = rootType ts
rootType (MsgQueue ts _) = rootType ts
rootType (Pool ts _) = rootType ts
rootType (Reference _ ts) = rootType ts
rootType (DynamicSubtype ts) = rootType ts
rootType (Location ts) = rootType ts
rootType (Port ts) = rootType ts
rootType t = t
