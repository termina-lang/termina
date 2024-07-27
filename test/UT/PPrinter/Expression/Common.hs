module UT.PPrinter.Expression.Common where

import AST.Core
import Utils.Annotations
import Semantic.Monad

objSemAnn :: AccessKind -> TypeSpecifier -> SemanticAnn
objSemAnn ak ts = Located (ETy (ObjectType ak ts)) Internal

stmtSemAnn :: SemanticAnn
stmtSemAnn = Located STy Internal

simpleTySemAnn :: TypeSpecifier -> SemanticAnn
simpleTySemAnn ts = Located (ETy (SimpleType ts)) Internal

unitSemAnn :: SemanticAnn
unitSemAnn = simpleTySemAnn Unit

uint8SemAnn, uint32SemAnn, uint16SemAnn, uint64SemAnn, int8SemAnn,
    int16SemAnn, int32SemAnn, int64SemAnn, usizeSemAnn, charSemAnn, boolSemAnn :: SemanticAnn
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

dynTySemAnn :: TypeSpecifier -> SemanticAnn
dynTySemAnn ts = objSemAnn Mutable (DynamicSubtype ts)

dynUInt8SemAnn, dynUInt16SemAnn, dynUInt32SemAnn, dynUInt64SemAnn, dynInt8SemAnn,
    dynInt16SemAnn, dynInt32SemAnn, dynInt64SemAnn, dynCharSemAnn, dynBoolSemAnn :: SemanticAnn
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

optionDynSemAnn :: AccessKind -> TypeSpecifier -> SemanticAnn
optionDynSemAnn ak ts = objSemAnn ak (Option (DynamicSubtype ts))

refSemAnn :: TypeSpecifier -> SemanticAnn
refSemAnn ts = objSemAnn Immutable (Reference Mutable ts)

refUInt8SemAnn, refUInt16SemAnn, refUInt32SemAnn, refUInt64SemAnn, refInt8SemAnn,
    refInt16SemAnn, refInt32SemAnn, refInt64SemAnn, refCharSemAnn, refBoolSemAnn :: SemanticAnn
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

vectorSemAnn :: AccessKind -> TypeSpecifier -> Size -> SemanticAnn
vectorSemAnn ak ts size = objSemAnn ak (Array ts size)

dynArraySemAnn :: TypeSpecifier -> Size -> SemanticAnn
dynArraySemAnn ts size = objSemAnn Mutable (DynamicSubtype (Array ts size))

refArraySemAnn :: TypeSpecifier -> Size -> SemanticAnn
refArraySemAnn ts size = objSemAnn Immutable (Reference Mutable (Array ts size))

refTwoDymArraySemAnn :: TypeSpecifier -> Size -> Size -> SemanticAnn
refTwoDymArraySemAnn ts size1 size2 = objSemAnn Immutable (Reference Mutable (Array (Array ts size1) size2))

uint16VecSemAnn, uint32VecSemAnn :: AccessKind -> Size -> SemanticAnn
uint16VecSemAnn ak = vectorSemAnn ak UInt16
uint32VecSemAnn ak = vectorSemAnn ak UInt32

twoDymArraySemAnn :: AccessKind -> TypeSpecifier -> Size -> Size -> SemanticAnn
twoDymArraySemAnn ak ts size1 size2 = objSemAnn ak (Array (Array ts size1) size2)

uint16TwoDymVecSemAnn, uint32TwoDymVecSemAnn :: AccessKind -> Size -> Size -> SemanticAnn
uint16TwoDymVecSemAnn ak = twoDymArraySemAnn ak UInt16
uint32TwoDymVecSemAnn ak = twoDymArraySemAnn ak UInt32

dynTwoDymArraySemAnn :: TypeSpecifier -> Size -> Size -> SemanticAnn
dynTwoDymArraySemAnn ts size1 size2 = objSemAnn Mutable (DynamicSubtype (Array (Array ts size1) size2))

dynThreeDymArraySemAnn :: TypeSpecifier -> Size -> Size -> Size -> SemanticAnn
dynThreeDymArraySemAnn ts size1 size2 size3 = objSemAnn Mutable (DynamicSubtype (Array (Array (Array ts size1) size2) size3))

uint16DynTwoDymVecSemAnn, uint32DynTwoDymVecSemAnn :: Size -> Size -> SemanticAnn
uint16DynTwoDymVecSemAnn = dynTwoDymArraySemAnn UInt16
uint32DynTwoDymVecSemAnn = dynTwoDymArraySemAnn UInt32

threeDymArraySemAnn :: AccessKind -> TypeSpecifier -> Size -> Size -> Size -> SemanticAnn
threeDymArraySemAnn ak ts size1 size2 size3 = objSemAnn ak (Array (Array (Array ts size1) size2) size3)

uint16ThreeDymVecSemAnn, uint32ThreeDymVecSemAnn :: AccessKind -> Size -> Size -> Size -> SemanticAnn
uint16ThreeDymVecSemAnn ak = threeDymArraySemAnn ak UInt16
uint32ThreeDymVecSemAnn ak = threeDymArraySemAnn ak UInt32

definedTypeSemAnn :: AccessKind -> Identifier -> SemanticAnn
definedTypeSemAnn ak name = objSemAnn ak (DefinedType name)

dynDefinedTypeSemAnn :: Identifier -> SemanticAnn
dynDefinedTypeSemAnn name = dynTySemAnn (DefinedType name)

refDefinedTypeSemAnn :: Identifier -> SemanticAnn
refDefinedTypeSemAnn name = refSemAnn (DefinedType name)

poolSemAnn :: TypeSpecifier -> SemanticAnn
poolSemAnn ts = objSemAnn Mutable (AccessPort (Allocator ts))

msgQueueSemAnn :: TypeSpecifier -> SemanticAnn
msgQueueSemAnn ts = objSemAnn Mutable (OutPort ts)

funSemAnn :: [Parameter] -> TypeSpecifier -> SemanticAnn
funSemAnn params ts = Located (ETy (AppType params ts)) Internal