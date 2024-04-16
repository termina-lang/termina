module UT.PPrinter.Expression.Common where

import AST.Core
import Semantic.Monad
import Parser.Parsing

objSemAnn :: AccessKind -> TypeSpecifier -> SemanticAnns
objSemAnn ak ts = SemAnn Internal (ETy (ObjectType ak ts))

stmtSemAnn :: SemanticAnns
stmtSemAnn = SemAnn Internal STy

simpleTySemAnn :: TypeSpecifier -> SemanticAnns
simpleTySemAnn ts = SemAnn Internal (ETy (SimpleType ts))

unitSemAnn :: SemanticAnns
unitSemAnn = simpleTySemAnn Unit

uint8SemAnn, uint32SemAnn, uint16SemAnn, uint64SemAnn, int8SemAnn,
    int16SemAnn, int32SemAnn, int64SemAnn, usizeSemAnn, charSemAnn, boolSemAnn :: SemanticAnns
uint8SemAnn = simpleTySemAnn UInt8
uint32SemAnn = simpleTySemAnn UInt32
uint16SemAnn = simpleTySemAnn UInt16
uint64SemAnn = simpleTySemAnn UInt64
int8SemAnn = simpleTySemAnn Int8
int16SemAnn = simpleTySemAnn Int16
int32SemAnn = simpleTySemAnn Int32
int64SemAnn = simpleTySemAnn Int64
usizeSemAnn = simpleTySemAnn USize
charSemAnn = simpleTySemAnn Char
boolSemAnn = simpleTySemAnn Bool

dynTySemAnn :: TypeSpecifier -> SemanticAnns
dynTySemAnn ts = objSemAnn Mutable (DynamicSubtype ts)

dynUInt8SemAnn, dynUInt16SemAnn, dynUInt32SemAnn, dynUInt64SemAnn, dynInt8SemAnn,
    dynInt16SemAnn, dynInt32SemAnn, dynInt64SemAnn, dynCharSemAnn, dynBoolSemAnn :: SemanticAnns
dynUInt8SemAnn = dynTySemAnn UInt8
dynUInt16SemAnn = dynTySemAnn UInt16
dynUInt32SemAnn = dynTySemAnn UInt32
dynUInt64SemAnn = dynTySemAnn UInt64
dynInt8SemAnn = dynTySemAnn Int8
dynInt16SemAnn = dynTySemAnn Int16
dynInt32SemAnn = dynTySemAnn Int32
dynInt64SemAnn = dynTySemAnn Int64
dynCharSemAnn = dynTySemAnn Char
dynBoolSemAnn = dynTySemAnn Bool

optionDynSemAnn :: AccessKind -> TypeSpecifier -> SemanticAnns
optionDynSemAnn ak ts = objSemAnn ak (Option (DynamicSubtype ts))

refSemAnn :: TypeSpecifier -> SemanticAnns
refSemAnn ts = objSemAnn Immutable (Reference Mutable ts)

refUInt8SemAnn, refUInt16SemAnn, refUInt32SemAnn, refUInt64SemAnn, refInt8SemAnn,
    refInt16SemAnn, refInt32SemAnn, refInt64SemAnn, refCharSemAnn, refBoolSemAnn :: SemanticAnns
refUInt8SemAnn = refSemAnn UInt8
refUInt16SemAnn = refSemAnn UInt16
refUInt32SemAnn = refSemAnn UInt32
refUInt64SemAnn = refSemAnn UInt64
refInt8SemAnn = refSemAnn Int8
refInt16SemAnn = refSemAnn Int16
refInt32SemAnn = refSemAnn Int32
refInt64SemAnn = refSemAnn Int64
refCharSemAnn = refSemAnn Char
refBoolSemAnn = refSemAnn Bool

vectorSemAnn :: AccessKind -> TypeSpecifier -> Size -> SemanticAnns
vectorSemAnn ak ts size = objSemAnn ak (Vector ts size)

dynVectorSemAnn :: TypeSpecifier -> Size -> SemanticAnns
dynVectorSemAnn ts size = objSemAnn Mutable (DynamicSubtype (Vector ts size))

refVectorSemAnn :: TypeSpecifier -> Size -> SemanticAnns
refVectorSemAnn ts size = objSemAnn Immutable (Reference Mutable (Vector ts size))

refTwoDymVectorSemAnn :: TypeSpecifier -> Size -> Size -> SemanticAnns
refTwoDymVectorSemAnn ts size1 size2 = objSemAnn Immutable (Reference Mutable (Vector (Vector ts size1) size2))

uint16VecSemAnn, uint32VecSemAnn :: AccessKind -> Size -> SemanticAnns
uint16VecSemAnn ak = vectorSemAnn ak UInt16
uint32VecSemAnn ak = vectorSemAnn ak UInt32

twoDymVectorSemAnn :: AccessKind -> TypeSpecifier -> Size -> Size -> SemanticAnns
twoDymVectorSemAnn ak ts size1 size2 = objSemAnn ak (Vector (Vector ts size1) size2)

uint16TwoDymVecSemAnn, uint32TwoDymVecSemAnn :: AccessKind -> Size -> Size -> SemanticAnns
uint16TwoDymVecSemAnn ak = twoDymVectorSemAnn ak UInt16
uint32TwoDymVecSemAnn ak = twoDymVectorSemAnn ak UInt32

dynTwoDymVectorSemAnn :: TypeSpecifier -> Size -> Size -> SemanticAnns
dynTwoDymVectorSemAnn ts size1 size2 = objSemAnn Mutable (DynamicSubtype (Vector (Vector ts size1) size2))

dynThreeDymVectorSemAnn :: TypeSpecifier -> Size -> Size -> Size -> SemanticAnns
dynThreeDymVectorSemAnn ts size1 size2 size3 = objSemAnn Mutable (DynamicSubtype (Vector (Vector (Vector ts size1) size2) size3))

uint16DynTwoDymVecSemAnn, uint32DynTwoDymVecSemAnn :: Size -> Size -> SemanticAnns
uint16DynTwoDymVecSemAnn = dynTwoDymVectorSemAnn UInt16
uint32DynTwoDymVecSemAnn = dynTwoDymVectorSemAnn UInt32

threeDymVectorSemAnn :: AccessKind -> TypeSpecifier -> Size -> Size -> Size -> SemanticAnns
threeDymVectorSemAnn ak ts size1 size2 size3 = objSemAnn ak (Vector (Vector (Vector ts size1) size2) size3)

uint16ThreeDymVecSemAnn, uint32ThreeDymVecSemAnn :: AccessKind -> Size -> Size -> Size -> SemanticAnns
uint16ThreeDymVecSemAnn ak = threeDymVectorSemAnn ak UInt16
uint32ThreeDymVecSemAnn ak = threeDymVectorSemAnn ak UInt32

definedTypeSemAnn :: AccessKind -> Identifier -> SemanticAnns
definedTypeSemAnn ak name = objSemAnn ak (DefinedType name)

dynDefinedTypeSemAnn :: Identifier -> SemanticAnns
dynDefinedTypeSemAnn name = dynTySemAnn (DefinedType name)

refDefinedTypeSemAnn :: Identifier -> SemanticAnns
refDefinedTypeSemAnn name = refSemAnn (DefinedType name)

poolSemAnn :: TypeSpecifier -> SemanticAnns
poolSemAnn ts = objSemAnn Mutable (AccessPort (Allocator ts))

msgQueueSemAnn :: TypeSpecifier -> SemanticAnns
msgQueueSemAnn ts = objSemAnn Mutable (OutPort ts)

funSemAnn :: [Parameter] -> [Parameter] -> TypeSpecifier -> SemanticAnns
funSemAnn constParams params ts = SemAnn Internal (ETy (AppType constParams params ts))