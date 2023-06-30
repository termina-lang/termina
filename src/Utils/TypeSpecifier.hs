-- | Helper functions for types

module Utils.TypeSpecifier where

import           AST

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
primitiveTypes Bool            = True
primitiveTypes Char            = True
primitiveTypes (DefinedType _) = True
primitiveTypes (Vector _ _)    = True
-- primitiveTypes (Option _)    = True
primitiveTypes  _              = False

boolTy :: TypeSpecifier -> Bool
boolTy Bool = True
boolTy _    = False

numTy :: TypeSpecifier -> Bool
numTy UInt8  = True
numTy UInt16 = True
numTy UInt32 = True
numTy UInt64 = True
numTy Int8   = True
numTy Int16  = True
numTy Int32  = True
numTy Int64  = True
numTy _      = False

memberIntCons :: Integer -> TypeSpecifier -> Bool
memberIntCons i UInt8  = ( 0 <= i ) && ( i <= 255)
memberIntCons i UInt16 = ( 0 <= i ) && ( i <= 65536)
memberIntCons i UInt32 = ( 0 <= i ) && ( i <= 4294967295)
memberIntCons i UInt64 = ( 0 <= i ) && ( i <= 18446744073709551615)
memberIntCons i Int8   = ( -128 <= i ) && ( i <= 127 )
memberIntCons i Int16  = ( -32768 <= i ) && ( i <= 32767 )
memberIntCons i Int32  = ( -2147483648 <= i ) && ( i <= 2147483647 )
memberIntCons i Int64  = ( -9223372036854775808 <= i ) && ( i <= 9223372036854775807 )
memberIntCons _ _      = False

identifierType :: TypeDef a -> Identifier
identifierType (Struct ident _ _ _) = ident
identifierType (Union ident _ _ _)  = ident
identifierType (Enum ident _ _ _)   = ident
identifierType (Class ident _ _ _)  = ident
