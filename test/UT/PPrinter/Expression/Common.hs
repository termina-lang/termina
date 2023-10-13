module UT.PPrinter.Expression.Common where

import AST.Core
import Semantic.Monad
import Parser.Parsing

objSemAnn :: AccessKind -> TypeSpecifier -> SemanticAnns
objSemAnn ak ts = SemAnn Internal (ETy (ObjectType ak ts))

simpleTySemAnn :: TypeSpecifier -> SemanticAnns
simpleTySemAnn ts = SemAnn Internal (ETy (SimpleType ts))

unitSemAnn :: SemanticAnns
unitSemAnn = simpleTySemAnn Unit

uint8SemAnn, uint32SemAnn, uint16SemAnn, uint64SemAnn, int8SemAnn,
    int16SemAnn, int32SemAnn, int64SemAnn, charSemAnn, boolSemAnn :: SemanticAnns
uint8SemAnn = simpleTySemAnn UInt8
uint32SemAnn = simpleTySemAnn UInt32
uint16SemAnn = simpleTySemAnn UInt16
uint64SemAnn = simpleTySemAnn UInt64
int8SemAnn = simpleTySemAnn Int8
int16SemAnn = simpleTySemAnn Int16
int32SemAnn = simpleTySemAnn Int32
int64SemAnn = simpleTySemAnn Int64
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

vectorSemAnn :: AccessKind -> TypeSpecifier -> Const -> SemanticAnns
vectorSemAnn ak ts size = objSemAnn ak (Vector ts (KC size))

dynVectorSemAnn :: TypeSpecifier -> Const -> SemanticAnns
dynVectorSemAnn ts size = objSemAnn Mutable (DynamicSubtype (Vector ts (KC size)))

refVectorSemAnn :: TypeSpecifier -> Const -> SemanticAnns
refVectorSemAnn ts size = objSemAnn Immutable (Reference Mutable (Vector ts (KC size)))

refTwoDymVectorSemAnn :: TypeSpecifier -> Const -> Const -> SemanticAnns
refTwoDymVectorSemAnn ts size1 size2 = objSemAnn Immutable (Reference Mutable (Vector (Vector ts (KC size1)) (KC size2)))

uint16VecSemAnn, uint32VecSemAnn :: AccessKind -> Const -> SemanticAnns
uint16VecSemAnn ak = vectorSemAnn ak UInt16
uint32VecSemAnn ak = vectorSemAnn ak UInt32

twoDymVectorSemAnn :: AccessKind -> TypeSpecifier -> Const -> Const -> SemanticAnns
twoDymVectorSemAnn ak ts size1 size2 = objSemAnn ak (Vector (Vector ts (KC size1)) (KC size2))

uint16TwoDymVecSemAnn, uint32TwoDymVecSemAnn :: AccessKind -> Const -> Const -> SemanticAnns
uint16TwoDymVecSemAnn ak = twoDymVectorSemAnn ak UInt16
uint32TwoDymVecSemAnn ak = twoDymVectorSemAnn ak UInt32

dynTwoDymVectorSemAnn :: TypeSpecifier -> Const -> Const -> SemanticAnns
dynTwoDymVectorSemAnn ts size1 size2 = objSemAnn Mutable (DynamicSubtype (Vector (Vector ts (KC size1)) (KC size2)))

dynThreeDymVectorSemAnn :: TypeSpecifier -> Const -> Const -> Const -> SemanticAnns
dynThreeDymVectorSemAnn ts size1 size2 size3 = objSemAnn Mutable (DynamicSubtype (Vector (Vector (Vector ts (KC size1)) (KC size2)) (KC size3)))

uint16DynTwoDymVecSemAnn, uint32DynTwoDymVecSemAnn :: Const -> Const -> SemanticAnns
uint16DynTwoDymVecSemAnn = dynTwoDymVectorSemAnn UInt16
uint32DynTwoDymVecSemAnn = dynTwoDymVectorSemAnn UInt32

threeDymVectorSemAnn :: AccessKind -> TypeSpecifier -> Const -> Const -> Const -> SemanticAnns
threeDymVectorSemAnn ak ts size1 size2 size3 = objSemAnn ak (Vector (Vector (Vector ts (KC size1)) (KC size2)) (KC size3))

uint16ThreeDymVecSemAnn, uint32ThreeDymVecSemAnn :: AccessKind -> Const -> Const -> Const -> SemanticAnns
uint16ThreeDymVecSemAnn ak = threeDymVectorSemAnn ak UInt16
uint32ThreeDymVecSemAnn ak = threeDymVectorSemAnn ak UInt32

definedTypeSemAnn :: AccessKind -> Identifier -> SemanticAnns
definedTypeSemAnn ak name = objSemAnn ak (DefinedType name)

dynDefinedTypeSemAnn :: Identifier -> SemanticAnns
dynDefinedTypeSemAnn name = dynTySemAnn (DefinedType name)

refDefinedTypeSemAnn :: Identifier -> SemanticAnns
refDefinedTypeSemAnn name = refSemAnn (DefinedType name)

poolSemAnn :: TypeSpecifier -> Integer -> SemanticAnns
poolSemAnn ts size = objSemAnn Immutable (Pool ts (K size))

msgQueueSemAnn :: TypeSpecifier -> Integer -> SemanticAnns
msgQueueSemAnn ts size = objSemAnn Immutable (MsgQueue ts (K size))

funSemAnn :: [Parameter] -> TypeSpecifier -> SemanticAnns
funSemAnn params ts = SemAnn undefined (ETy (AppType params ts))