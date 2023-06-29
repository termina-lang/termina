module UT.PPrinter.Expression.Common where

import AST
import Semantic.Monad

uint32Const0, uint8Const0x8, uint16Const1024, uint32Const0xFFFF0000, uint64Const1800000000, 
    int8ConstMinux128, int16Const1024, int32ConstMinux1024, int64ConstMinux3000000000, 
    charConsta, trueBool, falseBool :: Expression SemanticAnns
uint32Const0 = Constant (I UInt32 0) (SemAnn undefined UInt32)
uint8Const0x8 = Constant (I UInt8 0x08) (SemAnn undefined UInt8)
uint16Const1024 = Constant (I UInt16 1024) (SemAnn undefined UInt16)
uint32Const0xFFFF0000 = Constant (I UInt32 0xFFFF0000) (SemAnn undefined UInt32)
uint64Const1800000000 = Constant (I UInt64 1800000000) (SemAnn undefined UInt64)
int8ConstMinux128 = Constant (I Int8 (-128)) (SemAnn undefined Int8)
int16Const1024 = Constant (I Int16 1024) (SemAnn undefined Int16)
int32ConstMinux1024 = Constant (I Int32 (-1024)) (SemAnn undefined Int32)
int64ConstMinux3000000000 = Constant (I Int64 (-3000000000)) (SemAnn undefined Int64)
charConsta = Constant (C 'a') (SemAnn undefined Char)
trueBool = Constant (B True) (SemAnn undefined Bool)
falseBool = Constant (B False) (SemAnn undefined Bool)

uint16TS, uint32TS, vectorTS, twoDimVectorTS, threeDimVectorTS :: TypeSpecifier
uint16TS = UInt16
uint32TS = UInt32
vectorTS = Vector UInt32 (KC (I UInt32 10))
twoDimVectorTS = Vector (Vector Int64 (KC (I UInt32 5))) (KC (I UInt32 10))
threeDimVectorTS = Vector (Vector (Vector Char (KC (I UInt32 40))) (KC (I UInt32 5))) (KC (I UInt32 10))

tmDescriptorTS, dynamicTMDescriptorTS :: TypeSpecifier
tmDescriptorTS = DefinedType "TMDescriptor"
dynamicTMDescriptorTS = DynamicSubtype tmDescriptorTS