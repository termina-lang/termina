-- | Helper functions for types

module Utils.TypeSpecifier where

import AST

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
