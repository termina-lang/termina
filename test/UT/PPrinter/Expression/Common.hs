module UT.PPrinter.Expression.Common where

import SemanAST
import Semantic.Monad

tySemAnn :: TypeSpecifier -> SemanticAnns
tySemAnn ts = SemAnn undefined (ETy ts)

unitSemAnn :: SemanticAnns
unitSemAnn = tySemAnn Unit

uint8SemAnn, uint32SemAnn, uint16SemAnn, uint64SemAnn, int8SemAnn,
    int16SemAnn, int32SemAnn, int64SemAnn, charSemAnn, boolSemAnn :: SemanticAnns
uint8SemAnn = tySemAnn UInt8
uint32SemAnn = tySemAnn UInt32
uint16SemAnn = tySemAnn UInt16
uint64SemAnn = tySemAnn UInt64
int8SemAnn = tySemAnn Int8
int16SemAnn = tySemAnn Int16
int32SemAnn = tySemAnn Int32
int64SemAnn = tySemAnn Int64
charSemAnn = tySemAnn Char
boolSemAnn = tySemAnn Bool

dynTySemAnn :: TypeSpecifier -> SemanticAnns
dynTySemAnn ts = tySemAnn (DynamicSubtype ts)

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

refSemAnn :: TypeSpecifier -> SemanticAnns
refSemAnn ts = tySemAnn (Reference ts)

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

vectorSemAnn :: TypeSpecifier -> Const -> SemanticAnns
vectorSemAnn ts size = tySemAnn (Vector ts (KC size))

dynVectorSemAnn :: TypeSpecifier -> Const -> SemanticAnns
dynVectorSemAnn ts size = tySemAnn (DynamicSubtype (Vector ts (KC size)))

refVectorSemAnn :: TypeSpecifier -> Const -> SemanticAnns
refVectorSemAnn ts size = tySemAnn (Reference (Vector ts (KC size)))

refTwoDymVectorSemAnn :: TypeSpecifier -> Const -> Const -> SemanticAnns
refTwoDymVectorSemAnn ts size1 size2 = tySemAnn (Reference (Vector (Vector ts (KC size1)) (KC size2)))

uint16VecSemAnn, uint32VecSemAnn :: Const -> SemanticAnns
uint16VecSemAnn = vectorSemAnn UInt16
uint32VecSemAnn = vectorSemAnn UInt32

twoDymVectorSemAnn :: TypeSpecifier -> Const -> Const -> SemanticAnns
twoDymVectorSemAnn ts size1 size2 = tySemAnn (Vector (Vector ts (KC size1)) (KC size2))

uint16TwoDymVecSemAnn, uint32TwoDymVecSemAnn :: Const -> Const -> SemanticAnns
uint16TwoDymVecSemAnn = twoDymVectorSemAnn UInt16
uint32TwoDymVecSemAnn = twoDymVectorSemAnn UInt32

dynTwoDymVectorSemAnn :: TypeSpecifier -> Const -> Const -> SemanticAnns
dynTwoDymVectorSemAnn ts size1 size2 = tySemAnn (DynamicSubtype (Vector (Vector ts (KC size1)) (KC size2)))

dynThreeDymVectorSemAnn :: TypeSpecifier -> Const -> Const -> Const -> SemanticAnns
dynThreeDymVectorSemAnn ts size1 size2 size3 = tySemAnn (DynamicSubtype (Vector (Vector (Vector ts (KC size1)) (KC size2)) (KC size3)))

uint16DynTwoDymVecSemAnn, uint32DynTwoDymVecSemAnn :: Const -> Const -> SemanticAnns
uint16DynTwoDymVecSemAnn = dynTwoDymVectorSemAnn UInt16
uint32DynTwoDymVecSemAnn = dynTwoDymVectorSemAnn UInt32

threeDymVectorSemAnn :: TypeSpecifier -> Const -> Const -> Const -> SemanticAnns
threeDymVectorSemAnn ts size1 size2 size3 = tySemAnn (Vector (Vector (Vector ts (KC size1)) (KC size2)) (KC size3))

uint16ThreeDymVecSemAnn, uint32ThreeDymVecSemAnn :: Const -> Const -> Const -> SemanticAnns
uint16ThreeDymVecSemAnn = threeDymVectorSemAnn UInt16
uint32ThreeDymVecSemAnn = threeDymVectorSemAnn UInt32

definedTypeSemAnn :: Identifier -> SemanticAnns
definedTypeSemAnn name = tySemAnn (DefinedType name)

dynDefinedTypeSemAnn :: Identifier -> SemanticAnns
dynDefinedTypeSemAnn name = dynTySemAnn (DefinedType name)

refDefinedTypeSemAnn :: Identifier -> SemanticAnns
refDefinedTypeSemAnn name = refSemAnn (DefinedType name)

poolSemAnn :: TypeSpecifier -> Integer -> SemanticAnns
poolSemAnn ts size = tySemAnn (Pool ts (K size))

msgQueueSemAnn :: TypeSpecifier -> Integer -> SemanticAnns
msgQueueSemAnn ts size = tySemAnn (MsgQueue ts (K size))