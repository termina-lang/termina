module UT.PPrinter.Expression.Common where

import AST.Core
import Utils.Annotations
import Semantic.Monad

objSemAnn :: AccessKind -> TypeSpecifier -> SemanticAnn
objSemAnn ak ts = Located (ETy (ObjectType ak ts)) Internal

stmtSemAnn :: SemanticAnn
stmtSemAnn = Located (STy SimpleStmtType) Internal

simpleTySemAnn :: TypeSpecifier -> SemanticAnn
simpleTySemAnn ts = Located (ETy (SimpleType ts)) Internal

matchCaseSemAnn :: [TypeSpecifier] -> SemanticAnn
matchCaseSemAnn ts = Located (STy (MatchCaseStmtType ts)) Internal

unitSemAnn :: SemanticAnn
unitSemAnn = simpleTySemAnn Unit

uint8ExprSemAnn, uint32ExprSemAnn, uint16ExprSemAnn, uint64ExprSemAnn, int8ExprSemAnn,
    int16ExprSemAnn, int32ExprSemAnn, int64ExprSemAnn,
    usizeExprSemAnn, charExprSemAnn, boolExprSemAnn :: SemanticAnn
uint8ExprSemAnn = simpleTySemAnn UInt8
uint32ExprSemAnn = simpleTySemAnn UInt32
uint16ExprSemAnn = simpleTySemAnn UInt16
uint64ExprSemAnn = simpleTySemAnn UInt64
int8ExprSemAnn = simpleTySemAnn Int8
int16ExprSemAnn = simpleTySemAnn Int16
int32ExprSemAnn = simpleTySemAnn Int32
int64ExprSemAnn = simpleTySemAnn Int64
usizeExprSemAnn = simpleTySemAnn USize
charExprSemAnn = simpleTySemAnn Char
boolExprSemAnn = simpleTySemAnn Bool

boxTySemAnn :: TypeSpecifier -> SemanticAnn
boxTySemAnn ts = objSemAnn Mutable (BoxSubtype ts)

boxUInt8SemAnn, boxUInt16SemAnn, boxUInt32SemAnn, boxUInt64SemAnn, boxInt8SemAnn,
    boxInt16SemAnn, boxInt32SemAnn, boxInt64SemAnn, boxCharSemAnn, boxBoolSemAnn :: SemanticAnn
boxUInt8SemAnn = boxTySemAnn UInt8
boxUInt16SemAnn = boxTySemAnn UInt16
boxUInt32SemAnn = boxTySemAnn UInt32
boxUInt64SemAnn = boxTySemAnn UInt64
boxInt8SemAnn = boxTySemAnn Int8
boxInt16SemAnn = boxTySemAnn Int16
boxInt32SemAnn = boxTySemAnn Int32
boxInt64SemAnn = boxTySemAnn Int64
boxCharSemAnn = boxTySemAnn Char
boxBoolSemAnn = boxTySemAnn Bool

optionBoxObjSemAnn :: AccessKind -> TypeSpecifier -> SemanticAnn
optionBoxObjSemAnn ak ts = objSemAnn ak (Option (BoxSubtype ts))

optionBoxExprSemAnn :: TypeSpecifier -> SemanticAnn
optionBoxExprSemAnn ts = simpleTySemAnn (Option (BoxSubtype ts))

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

vectorObjSemAnn :: AccessKind -> TypeSpecifier -> Size -> SemanticAnn
vectorObjSemAnn ak ts size = objSemAnn ak (Array ts size)

vectorExprSemAnn :: TypeSpecifier -> Size -> SemanticAnn
vectorExprSemAnn ts size = simpleTySemAnn $ Array ts size

boxArrayObjSemAnn :: TypeSpecifier -> Size -> SemanticAnn
boxArrayObjSemAnn ts size = objSemAnn Mutable (BoxSubtype (Array ts size))

refArraySemAnn :: TypeSpecifier -> Size -> SemanticAnn
refArraySemAnn ts size = objSemAnn Immutable (Reference Mutable (Array ts size))

refTwoDymArraySemAnn :: TypeSpecifier -> Size -> Size -> SemanticAnn
refTwoDymArraySemAnn ts size1 size2 = objSemAnn Immutable (Reference Mutable (Array (Array ts size1) size2))

uint16VecObjSemAnn, uint32VecObjSemAnn :: AccessKind -> Size -> SemanticAnn
uint16VecObjSemAnn ak = vectorObjSemAnn ak UInt16
uint32VecObjSemAnn ak = vectorObjSemAnn ak UInt32

twoDymArrayObjSemAnn :: AccessKind -> TypeSpecifier -> Size -> Size -> SemanticAnn
twoDymArrayObjSemAnn ak ts size1 size2 = objSemAnn ak (Array (Array ts size1) size2)

twoDymArrayExprSemAnn :: TypeSpecifier -> Size -> Size -> SemanticAnn
twoDymArrayExprSemAnn ts size1 size2 = simpleTySemAnn (Array (Array ts size1) size2)

uint16TwoDymVecObjSemAnn, uint32TwoDymVecObjSemAnn :: AccessKind -> Size -> Size -> SemanticAnn
uint16TwoDymVecObjSemAnn ak = twoDymArrayObjSemAnn ak UInt16
uint32TwoDymVecObjSemAnn ak = twoDymArrayObjSemAnn ak UInt32

boxTwoDymArrayObjSemAnn :: TypeSpecifier -> Size -> Size -> SemanticAnn
boxTwoDymArrayObjSemAnn ts size1 size2 = objSemAnn Mutable (BoxSubtype (Array (Array ts size1) size2))

boxThreeDymArrayObjSemAnn :: TypeSpecifier -> Size -> Size -> Size -> SemanticAnn
boxThreeDymArrayObjSemAnn ts size1 size2 size3 = objSemAnn Mutable (BoxSubtype (Array (Array (Array ts size1) size2) size3))

uint16BoxTwoDymVecObjSemAnn, uint32BoxTwoDymVecObjSemAnn :: Size -> Size -> SemanticAnn
uint16BoxTwoDymVecObjSemAnn = boxTwoDymArrayObjSemAnn UInt16
uint32BoxTwoDymVecObjSemAnn = boxTwoDymArrayObjSemAnn UInt32

threeDymArrayObjSemAnn :: AccessKind -> TypeSpecifier -> Size -> Size -> Size -> SemanticAnn
threeDymArrayObjSemAnn ak ts size1 size2 size3 = objSemAnn ak (Array (Array (Array ts size1) size2) size3)

uint16ThreeDymVecObjSemAnn, uint32ThreeDymVecObjSemAnn :: AccessKind -> Size -> Size -> Size -> SemanticAnn
uint16ThreeDymVecObjSemAnn ak = threeDymArrayObjSemAnn ak UInt16
uint32ThreeDymVecObjSemAnn ak = threeDymArrayObjSemAnn ak UInt32

definedTypeObjSemAnn :: AccessKind -> Identifier -> SemanticAnn
definedTypeObjSemAnn ak name = objSemAnn ak (DefinedType name)

definedTypeExprSemAnn :: Identifier -> SemanticAnn
definedTypeExprSemAnn name = simpleTySemAnn (DefinedType name)

boxDefinedTypeSemAnn :: Identifier -> SemanticAnn
boxDefinedTypeSemAnn name = boxTySemAnn (DefinedType name)

refDefinedTypeSemAnn :: Identifier -> SemanticAnn
refDefinedTypeSemAnn name = refSemAnn (DefinedType name)

poolSemAnn :: TypeSpecifier -> SemanticAnn
poolSemAnn ts = objSemAnn Mutable (AccessPort (Allocator ts))

msgQueueSemAnn :: TypeSpecifier -> SemanticAnn
msgQueueSemAnn ts = objSemAnn Mutable (OutPort ts)

funSemAnn :: [Parameter] -> TypeSpecifier -> SemanticAnn
funSemAnn params ts = Located (ETy (AppType params ts)) Internal