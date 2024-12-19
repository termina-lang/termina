{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module Encapsulating Semantic Errors

module Semantic.Errors where

-- Termina AST
import Semantic.AST
import Utils.Annotations
import Utils.Errors
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Text.Parsec
import Errata 
import qualified Data.Text.Lazy as TL
import Errata.Styles

----------------------------------------
-- Type checker error handling
----------------------------------------
data Error
  -- | Expected /similar/ types?
  = 
    EMismatch TerminaType TerminaType -- ^ Type mismatch (Internal)
  | ENoStructFound Identifier -- ^ Struct not found (Internal)
  | EUnboxingObject -- ^ Error when unboxing an annotated object to get its type (Internal)
  | EUnboxingExpression -- ^ Error when unboxing an expression to get its type (Internal)
  | EUnboxingStructType -- ^ Error when unboxing a struct type (Internal)
  | EUnboxingEnumType -- ^ Error when unboxing an enum type (Internal)
  | EUnboxingClassType -- ^ Error when unboxing a class type (Internal)
  | EUnboxingMemberFunctionType -- ^ Error when unboxing a member function type (Internal)
  | EUnboxingInterface -- ^ Error when unboxing an interface (Internal)
  | EUnboxingEmittterClass -- ^ Error when unboxing an emitter class (Internal)
  | EUnboxingIntConst -- ^ Error when unboxing an integer constant (Internal)
  | EExpectedArrayTy TerminaType -- ^ Expected a valid type for the elements of an array (Internal)
  | EExpectedCopyType TerminaType -- ^ Expected a copiable type (Internal)
  | EExpectedNumType TerminaType -- ^ Expected a numeric type (Internal)
  | EInvalidObjectDeclaration Identifier -- ^ Invalid object declaration (Internal)
  | EMalformedSlice -- ^ Malformed slice (Internal)
  | EMalformedClassTyping -- ^ Malformed class typing (Internal)
  | EExpressionNotConstant -- ^ Expression not constant (Internal)
  | EContinueActionNotFound -- ^ Action not found in continue statement (Internal)
  | EMissingIdentifier -- ^ Missing identifier (Internal)
  | EMatchCaseInternalError -- ^ Internal error in match case (Internal)
  | EStructDefEmpty Identifier -- ^ Empty struct definition (Internal)
  | EEnumDefEmpty Identifier -- ^ Empty enum definition (Internal)
  | EInterfaceEmpty Identifier -- ^ Empty interface definition (Internal)
  | EInvalidArrayIndexing TerminaType -- ^ Invalid array indexing (SE-001)
  | ENotNamedObject Identifier -- ^ Object not found (SE-002)
  | ENotConstant Identifier -- ^ Invalid use of a non-constant object (SE-003)
  | EAssignmentToImmutable -- ^ Assignment to immutable variable (SE-004)
  | EIfElseNoOtherwise -- ^ Missing else clause (SE-005)
  | ENotCasteable TerminaType TerminaType -- ^ Casting error (SE-006)
  | EInvalidParameterType Parameter -- ^ Invalid parameter type (SE-007)
  | EInvalidReturnType TerminaType -- ^ Invalid return type (SE-008)
  | EProcedureCallExtraArgs (Identifier, [TerminaType], Location) Integer -- ^ Extra parameters in procedure call (SE-009)
  | EProcedureCallMissingArgs (Identifier, [TerminaType], Location) Integer -- ^ Missing parameters in procedure call (SE-010)
  | EProcedureCallArgTypeMismatch (Identifier, TerminaType, Location) Integer TerminaType -- ^ Parameter type mismatch in procedure call (SE-011)
  | EUnknownProcedure Identifier -- ^ Unknown procedure (SE-012)
  | EResourceClassNoProvides Identifier -- ^ Resource class does not provide any interface (SE-013)
  | EResourceClassAction (Identifier, Location) Identifier -- ^ Resource class defines an action (SE-014)
  | EResourceClassInPort (Identifier, Location) Identifier -- ^ Resource class defines an in port (SE-015)
  | EResourceClassOutPort (Identifier, Location) Identifier -- ^ Resource class defines an out port (SE-016)
  | EInterfaceNotFound Identifier -- ^ Interface not found (SE-017)
  | EGlobalNotInterface Identifier -- ^ The type is not an interface (SE-018)
  | EProcedureNotFromProvidedInterfaces (Identifier, Location) Identifier -- ^ Procedure not from provided interfaces (SE-019)
  | EMissingProcedure Identifier Identifier -- ^ Missing procedure (SE-020)
  | EProcedureExtraParams (Identifier, Identifier, [TerminaType], Location) Integer -- ^ Extra parameters in procedure definition (SE-021)
  | EProcedureMissingParams (Identifier, Identifier, [TerminaType], Location) Integer -- ^ Missing parameters in procedure definition (SE-022)
  | EProcedureParamTypeMismatch (Identifier, Identifier, TerminaType, Location) TerminaType -- ^ Parameter type mismatch in procedure definition (SE-023)
  | ETaskClassProvides Identifier -- ^ Task class provides an interface (SE-024)
  | ETaskClassProcedure (Identifier, Location) Identifier -- ^ Task class defines a procedure (SE-025)
  | ETaskClassNoActions Identifier -- ^ Task class does not define any actions (SE-026)
  | EHandlerClassProvides Identifier -- ^ Handler class provides an interface (SE-027)
  | EHandlerClassProcedure (Identifier, Location) Identifier -- ^ Handler class defines a procedure (SE-028)
  | EHandlerClassNoAction Identifier -- ^ Handler class does not define any actions (SE-029)
  | EHandlerClassMultipleActions Identifier Location -- ^ Handler class defines multiple actions (SE-030)
  | EHandlerClassNoSinkPort Identifier -- ^ Handler class does not define a sink port (SE-031)
  | EHandlerClassMultipleSinkPorts Identifier Location -- ^ Handler class defines multiple sink ports (SE-032)
  | EHandlerClassInPort (Identifier, Location) Identifier -- ^ Handler class defines an in port (SE-033)
  | EIfElseIfCondNotBool TerminaType -- ^ If-else-if condition is not a boolean (SE-034)
  | EFunctionCallExtraArgs (Identifier, [TerminaType], Location) Integer -- ^ Extra parameters in function call (SE-035)
  | EFunctionCallMissingArgs (Identifier, [TerminaType], Location) Integer -- ^ Missing parameters in function call (SE-036)
  | EFunctionCallArgTypeMismatch (Identifier, TerminaType, Location) Integer TerminaType -- ^ Parameter type mismatch in function call (SE-037)
  | EMemberAccessNotFunction Identifier -- ^ Access to a member that is not a function (SE-038)
  | EMutableReferenceToImmutable -- ^ Mutable reference to immutable object (SE-039)
  | EMutableReferenceToPrivate -- ^ Mutable reference to immutable object (SE-040)
  | EBinOpExpectedTypeLeft Op TerminaType TerminaType -- ^ Binary operation expected type on the left (SE-041)
  | EBinOpExpectedTypeRight Op TerminaType TerminaType -- ^ Binary operation expected type on the right (SE-042)
  | EBinOpTypeMismatch Op TerminaType TerminaType -- ^ Binary operation type mismatch (SE-043)
  | EBinOpExpectedTypeNotBool Op TerminaType -- ^ Binary operation expected result type not boolean (SE-044)
  | EBinOpLeftTypeNotBool Op TerminaType -- ^ Binary operation expected boolean type on the left (SE-045)
  | EBinOpRightTypeNotBool Op TerminaType -- ^ Binary operation expected boolean type on the right (SE-046)
  | EBinOpExpectedTypeNotNum Op TerminaType -- ^ Binary operation expected result type not numeric (SE-047)
  | EBinOpLeftTypeNotNum Op TerminaType -- ^ Binary operation expected numeric type on the left (SE-048)
  | EBinOpRightTypeNotNum Op TerminaType -- ^ Binary operation expected numeric type on the right (SE-049)
  | EBinOpRightTypeNotPos Op TerminaType -- ^ Binary operation expected positive numeric type on the right (SE-050)
  | EBinOpLeftTypeNotEq Op TerminaType -- ^ Binary operation expected equatable type on the left (SE-051)
  | EBinOpRightTypeNotEq Op TerminaType -- ^ Binary operation expected equatable type on the right (SE-052)
  | EAtomicAccessInvalidType TerminaType -- ^ Invalid type for the atomic access interface (SE-053)
  | EAtomicArrayAccessInvalidType TerminaType -- ^ Invalid type for the atomic array access interface (SE-054)
  | EAtomicInvalidType TerminaType -- ^ Invalid atomic type (SE-055)
  | EAtomicArrayInvalidType TerminaType -- ^ Invalid atomic array type (SE-056)
  | EAtomicConnectionTypeMismatch TerminaType TerminaType -- ^ Atomic connection type mismatch (SE-057)
  | EAtomicArrayConnectionTypeMismatch TerminaType TerminaType -- ^ Atomic array connection type mismatch (SE-058)
  | EAtomicArrayConnectionSizeMismatch Size Size -- ^ Atomic array connection size mismatch (SE-059)
  | EConstantWithoutKnownType Const -- ^ Constant without known type (SE-060)
  | EStructInitializerInvalidUse -- ^ Invalid use of a struct initializer (SE-061)
  | EStructInitializerTypeMismatch TerminaType TerminaType -- ^ Struct initializer type mismatch (SE-062)
  | EEnumInitializerExpectedTypeMismatch TerminaType TerminaType -- ^ Enum initializer expected type mismatch (SE-063)
  | ESliceInvalidUse -- ^ Invalid use of a slice (SE-064)
  | EArrayInitializerInvalidUse -- ^ Invalid use of an array initializer (SE-065)
  | EArrayInitializerNotArray TerminaType -- ^ Assignment of an array initializer to a non-array type (SE-066)
  | EArrayExprListInitializerInvalidUse -- ^ Invalid use of an expression list array initializer (SE-067)
  | EArrayExprListInitializerNotArray TerminaType -- ^ Assignment of an expression list array initializer to a non-array type (SE-068)
  | EOptionVariantInitializerInvalidUse -- ^ Invalid use of an option variant initializer (SE-069)
  | EArrayInitializerSizeMismatch Size Size -- ^ Array initializer size mismatch (SE-070)
  | EArrayExprListInitializerSizeMismatch Integer Integer -- ^ Array expression list array initializer size mismatch (SE-071)
  | EArrayExprListInitializerExprTypeMismatch TerminaType TerminaType -- ^ List of initializing expressions type mismatch (SE-072)
  | EReturnValueExpected TerminaType -- ^ Expected return value (SE-073)
  | EReturnValueNotUnit -- ^ Return value not expected (SE-074)
  | EInvalidArrayType TerminaType -- ^ Invalid array type (SE-075)
  | EInvalidBoxType TerminaType -- ^ Invalid box type (SE-076)
  | ENoTypeFound Identifier -- ^ Type not found (SE-077)
  | EGlobalNotType (Identifier, Location) -- ^ Global object but not a type (SE-078)
  | EInvalidAccessToGlobal Identifier -- ^ Invalid access to global object (SE-079)
  | EConstantIsReadOnly Identifier -- ^ Invalid write to a constant (SE-080)
  | ESymbolAlreadyDefined (Identifier, Location) -- ^ Symbol already defined (SE-081)
  | EContinueInvalidExpression -- ^ Invalid expression in continue statement (SE-082)
  | EContinueInvalidMethodOrViewerCall Identifier -- ^ Invalid method or viewer call in continue statement (SE-083)
  | EContinueInvalidMemberCall TerminaType -- ^ Invalid member call in continue statement (SE-084)
  | EContinueActionExtraArgs (Identifier, [TerminaType], Location) Integer -- ^ Extra parameters in action call in continue statement (SE-085)
  | EContinueActionMissingArgs (Identifier, Location) -- ^ Missing parameters in action call in continue statement (SE-086)
  | EEnumVariantInitializerInvalidUse -- ^ Invalid use of an enum variant initializer (SE-087)
  | EEnumVariantNotFound Identifier Identifier -- ^ Enum variant not found (SE-088)
  | EEnumVariantExtraParams (Identifier, Location) (Identifier, [TerminaType]) Integer -- ^ Extra parameters in enum variant (SE-089)
  | EEnumVariantMissingParams (Identifier, Location) (Identifier, [TerminaType]) Integer -- ^ Missing parameters in enum variant (SE-090)
  | EEnumVariantParamTypeMismatch (Identifier, Location) (Identifier, Integer, TerminaType) TerminaType -- ^ Parameter type mismatch in enum variant (SE-091)
  | EFunctionNotFound Identifier -- ^ Function not found (SE-092)
  | EGlobalNotFunction (Identifier, Location) -- ^ Global object but not a function (SE-093)
  | EUnexpectedNumericConstant TerminaType -- ^ Unexpected numeric constant (SE-094)
  | EInvalidAssignmentExprType TerminaType -- ^ Invalid assignment expression type (SE-095)
  | EInvalidMessageType TerminaType -- ^ Invalid message type (SE-096)
  | EInvalidOptionType TerminaType -- ^ Invalid option type (SE-097)
  | EInvalidReferenceType TerminaType -- ^ Invalid reference type (SE-098)
  | EInvalidFixedLocationType TerminaType -- ^ Invalid fixed-location type (SE-099)
  | EInvalidAllocatorType TerminaType -- ^ Invalid allocator type (SE-100)
  | EInvalidClassFieldType TerminaType -- ^ Invalid class field type (SE-101)
  | EInvalidStructFieldType TerminaType -- ^ Invalid struct field type (SE-102)
  | EInvalidEnumParameterType TerminaType -- ^ Invalid enum parameter type (SE-103)
  | EInvalidAccessPortType TerminaType -- ^ Invalid access port type (SE-104)
  | EInvalidDeclarationType TerminaType -- ^ Invalid declaration type (SE-105)
  | EInvalidTypeSpecifier TypeSpecifier -- ^ Invalid type specifier (SE-106)
  | EInvalidNumericConstantType TerminaType -- ^ Invalid numeric constant type (SE-107)
  | EInvalidActionParameterType TerminaType -- ^ Invalid action parameter type (SE-108)
  | EInvalidProcedureParameterType TerminaType -- ^ Invalid procedure parameter type (SE-109)
  | EMemberFunctionCallExtraArgs (Identifier, [TerminaType], Location) Integer -- ^ Extra arguments in member function call (SE-110)
  | EMemberFunctionCallMissingArgs (Identifier, [TerminaType], Location) Integer -- ^ Missing arguments in member function call (SE-111)
  | EMemberFunctionCallArgTypeMismatch (Identifier, TerminaType, Location) Integer TerminaType -- ^ Parameter type mismatch in member function call (SE-112)
  | EArrayIndexNotUSize TerminaType -- ^ Invalid array index type (SE-113)
  | EArraySliceLowerBoundNotUSize TerminaType -- ^ Invalid array slice lower bound type (SE-114)
  | EArraySliceUpperBoundNotUSize TerminaType -- ^ Invalid array slice upper bound type (SE-115)
  | EOutboundPortSendInvalidNumArgs Integer -- ^ Invalid number of arguments in outbound port send (SE-116)
  | EOutboundPortArgTypeMismatch TerminaType TerminaType -- ^ Parameter type mismatch in output port (SE-117)
  | EAssignmentExprMismatch TerminaType TerminaType -- ^ Assignment expression type mismatch (SE-118)
  | EFieldValueAssignmentMissingFields (TerminaType, Location) [Identifier] -- ^ Missing field/s in field assignment expression (SE-119)
  | EFieldValueAssignmentUnknownFields (TerminaType, Location) [Identifier] -- ^ Unknown field/s in field assignment expression (SE-120)
  | EFieldNotFixedLocation Identifier TerminaType -- ^ Field is not a fixed-location (SE-121)
  | EFieldNotAccessPort Identifier TerminaType -- ^ Field is not an access port (SE-122)
  | EFieldNotSinkOrInboundPort Identifier TerminaType -- ^ Field is not a sink or in port (SE-123)
  | EFieldNotOutboundPort Identifier TerminaType -- ^ Field is not an out port (SE-124)
  | EMemberAccessInvalidType TerminaType -- ^ Invalid member access type (SE-125)
  | EMemberFunctionCallInvalidType TerminaType -- ^ Invalid member function call type (SE-126)
  | EMemberAccessUnknownField (Identifier, Location) Identifier -- ^ Unknown field in member access (SE-127)
  | EInvalidProcedureCallInsideMemberFunction -- ^ Invalid procedure call inside member function (SE-128)
  | EConstantOutRange Const -- ^ Numeric constant out of range (SE-129)
  | EForIteratorInvalidType TerminaType -- ^ Invalid for iterator type (SE-130)
  | EUsedTypeName Identifier Location -- ^ Type name already used (SE-131)
  | EUsedGlobalName Identifier Location -- ^ Global object name already used (SE-132)
  | EUsedFunName Identifier Location -- ^ Function name already used (SE-133)
  | EAccessPortConnectionInvalidGlobal Identifier -- ^ Invalid access port connection (SE-134)
  | EAccessPortConnectionInterfaceNotProvided Identifier Identifier -- ^ Resource does not provide the interface (SE-135)
  | ESinkPortConnectionInvalidGlobal Identifier -- ^ Invalid sink port connection (SE-136)
  | EInboundPortConnectionInvalidObject Identifier -- ^ Invalid inbound port connection (SE-137)
  | EOutboundPortConnectionInvalidGlobal Identifier -- ^ Invalid outbound port connection (SE-138)
  | EAllocatorPortConnectionInvalidGlobal Identifier -- ^ Invalid allocator port connection (SE-139)
  | EAtomicAccessPortConnectionInvalidGlobal Identifier -- ^ Invalid atomic access port connection (SE-140)
  | EAtomicArrayAccessPortConnectionInvalidGlobal Identifier -- ^ Invalid atomic array access port connection (SE-141)
  | EStructDefNotUniqueField [Identifier] -- ^ Repeated field in struct definition (SE-142)
  | EEnumDefNotUniqueVariant [Identifier] -- ^ Repeated variant in enum definition (SE-143)
  | EInterfaceNotUniqueProcedure [Identifier] -- ^ Repeated procedure in interface definition (SE-144)
  | EClassLoop [Identifier] -- ^ Loop between member function calls in class definition (SE-145)
  | EDereferenceInvalidType TerminaType -- ^ Invalid dereference type (SE-146)
  | EMatchInvalidType TerminaType -- ^ Invalid match type (SE-147)
  | EMatchCaseDuplicate Identifier Location -- ^ Duplicate case in match statement (SE-148)
  | EMatchCaseUnknownVariants [Identifier] -- ^ Unknown variant/s in match case (SE-149)
  | EMatchMissingCases [Identifier] -- ^ Missing case/s in match statement (SE-150)
  | EIsVariantInvalidType TerminaType -- ^ Invalid type for is-variant expression (SE-151)
  | EIsOptionVariantInvalidType TerminaType -- ^ Invalid type for is-option-variant expression (SE-152)
  | EIsVariantEnumTypeMismatch Identifier Identifier -- ^ Enum type mismatch in is variant expression (SE-153)
  | EOutboundPortInvalidProcedure Identifier -- ^ Invalid procedure in outbound port (SE-154)
  | EInvalidPoolInitialization -- ^ Invalid pool initialization (SE-155)
  | EInvalidMsgQueueInitialization -- ^ Invalid message queue initialization (SE-156)
  | EUnknownGlobal Identifier -- ^ Unknown global object (SE-157)
  | EInvalidInterruptEmitterType TerminaType -- ^ Invalid interrupt emitter type (SE-158)
  | EInvalidPeriodicTimerEmitterType TerminaType -- ^ Invalid periodic timer emitter type (SE-159)
  | EInvalidSystemInitEmitterType TerminaType -- ^ Invalid system init emitter type (SE-160)
  | EInboundPortConnectionMsgQueueTypeMismatch Identifier TerminaType TerminaType -- ^ Message queue type mismatch in inbound port connection (SE-161)
  | EOutboundPortConnectionMsgQueueTypeMismatch Identifier TerminaType TerminaType -- ^ Message queue type mismatch in outbound port connection (SE-162)
  | EAllocatorPortConnectionPoolTypeMismatch Identifier TerminaType TerminaType -- ^ Pool type mismatch in allocator port connection (SE-163)
  | EInvalidTaskType TerminaType -- ^ Invalid task type (SE-164)
  | EInvalidHandlerType TerminaType -- ^ Invalid handler type (SE-165)
  | EInvalidResourceType TerminaType -- ^ Invalid resource type (SE-166)
  | EInvalidEmitterType TerminaType -- ^ Invalid emitter type (SE-167)
  | EInvalidChannelType TerminaType -- ^ Invalid channel type (SE-168)
  | EEmitterClassNotInstantiable Identifier -- ^ Emitter class not instantiable (SE-169)
  | ESingleExpressionTypeNotUnit TerminaType -- ^ Single expression type not unit (SE-170)
  deriving Show

type SemanticErrors = AnnotatedError Error Location

instance ErrorMessage SemanticErrors where

    errorIdent (AnnotatedError (EInvalidArrayIndexing _ty) _pos) = "SE-001"
    errorIdent (AnnotatedError (ENotNamedObject _ident) _pos) = "SE-002"
    errorIdent (AnnotatedError (ENotConstant _ident) _pos) = "SE-003"
    errorIdent (AnnotatedError EAssignmentToImmutable _pos) = "SE-004"
    errorIdent (AnnotatedError EIfElseNoOtherwise _pos) = "SE-005"
    errorIdent (AnnotatedError (ENotCasteable _ty1 _ty2) _pos) = "SE-006"
    errorIdent (AnnotatedError (EInvalidParameterType _param) _pos) = "SE-007"
    errorIdent (AnnotatedError (EInvalidReturnType _ty) _pos) = "SE-008"
    errorIdent (AnnotatedError (EProcedureCallExtraArgs _def _numArgs) _pos) = "SE-009"
    errorIdent (AnnotatedError (EProcedureCallMissingArgs _def _numArgs) _pos) = "SE-010"
    errorIdent (AnnotatedError (EProcedureCallArgTypeMismatch _def _num _actualTy) _pos) = "SE-011"
    errorIdent (AnnotatedError (EUnknownProcedure _ident) _pos) = "SE-012"
    errorIdent (AnnotatedError (EResourceClassNoProvides _ident) _pos) = "SE-013"
    errorIdent (AnnotatedError (EResourceClassAction _def _ident) _pos) = "SE-014"
    errorIdent (AnnotatedError (EResourceClassInPort _def _ident) _pos) = "SE-015"
    errorIdent (AnnotatedError (EResourceClassOutPort _def _port) _pos) = "SE-016"
    errorIdent (AnnotatedError (EInterfaceNotFound _ident) _pos) = "SE-017"
    errorIdent (AnnotatedError (EGlobalNotInterface _ident) _pos) = "SE-018"
    errorIdent (AnnotatedError (EProcedureNotFromProvidedInterfaces _def _ident) _pos) = "SE-019"
    errorIdent (AnnotatedError (EMissingProcedure _ifaceId _procId) _pos) = "SE-020"
    errorIdent (AnnotatedError (EProcedureExtraParams _def _paramNumber) _pos) = "SE-021"
    errorIdent (AnnotatedError (EProcedureMissingParams _def _paramNumber) _pos) = "SE-022"
    errorIdent (AnnotatedError (EProcedureParamTypeMismatch _def _ty) _pos) = "SE-023"
    errorIdent (AnnotatedError (ETaskClassProvides _ident) _pos) = "SE-024"
    errorIdent (AnnotatedError (ETaskClassProcedure _def _ident) _pos) = "SE-025"
    errorIdent (AnnotatedError (ETaskClassNoActions _ident) _pos) = "SE-026"
    errorIdent (AnnotatedError (EHandlerClassProvides _ident) _pos) = "SE-027"
    errorIdent (AnnotatedError (EHandlerClassProcedure _def _ident) _pos) = "SE-028"
    errorIdent (AnnotatedError (EHandlerClassNoAction _ident) _pos) = "SE-029"
    errorIdent (AnnotatedError (EHandlerClassMultipleActions _ident _loc) _pos) = "SE-030"
    errorIdent (AnnotatedError (EHandlerClassNoSinkPort _ident) _pos) = "SE-031"
    errorIdent (AnnotatedError (EHandlerClassMultipleSinkPorts _ident _loc) _pos) = "SE-032"
    errorIdent (AnnotatedError (EHandlerClassInPort _def _ident) _pos) = "SE-033"
    errorIdent (AnnotatedError (EIfElseIfCondNotBool _ty) _pos) = "SE-034"
    errorIdent (AnnotatedError (EFunctionCallExtraArgs _def _paramNumber) _pos) = "SE-035"
    errorIdent (AnnotatedError (EFunctionCallMissingArgs _def _paramNumber) _pos) = "SE-036"
    errorIdent (AnnotatedError (EFunctionCallArgTypeMismatch _def _paramNumber _ty) _pos) = "SE-037"
    errorIdent (AnnotatedError (EMemberAccessNotFunction _ident) _pos) = "SE-038"
    errorIdent (AnnotatedError EMutableReferenceToImmutable _pos) = "SE-039"
    errorIdent (AnnotatedError EMutableReferenceToPrivate _pos) = "SE-040"
    errorIdent (AnnotatedError (EBinOpExpectedTypeLeft _op _ty1 _ty2) _pos) = "SE-041"
    errorIdent (AnnotatedError (EBinOpExpectedTypeRight _op _ty1 _ty2) _pos) = "SE-042"
    errorIdent (AnnotatedError (EBinOpTypeMismatch _op _ty1 _ty2) _pos) = "SE-043"
    errorIdent (AnnotatedError (EBinOpExpectedTypeNotBool _op _ty) _pos) = "SE-044"
    errorIdent (AnnotatedError (EBinOpLeftTypeNotBool _op _ty) _pos) = "SE-045"
    errorIdent (AnnotatedError (EBinOpRightTypeNotBool _op _ty) _pos) = "SE-046"
    errorIdent (AnnotatedError (EBinOpExpectedTypeNotNum _op _ty) _pos) = "SE-047"
    errorIdent (AnnotatedError (EBinOpLeftTypeNotNum _op _ty) _pos) = "SE-048"
    errorIdent (AnnotatedError (EBinOpRightTypeNotNum _op _ty) _pos) = "SE-049"
    errorIdent (AnnotatedError (EBinOpRightTypeNotPos _op _ty) _pos) = "SE-050"
    errorIdent (AnnotatedError (EBinOpLeftTypeNotEq _op _ty) _pos) = "SE-051"
    errorIdent (AnnotatedError (EBinOpRightTypeNotEq _op _ty) _pos) = "SE-052"
    errorIdent (AnnotatedError (EAtomicAccessInvalidType _ty) _pos) = "SE-053"
    errorIdent (AnnotatedError (EAtomicArrayAccessInvalidType _ty) _pos) = "SE-054"
    errorIdent (AnnotatedError (EAtomicInvalidType _ty) _pos) = "SE-055"
    errorIdent (AnnotatedError (EAtomicArrayInvalidType _ty) _pos) = "SE-056"
    errorIdent (AnnotatedError (EAtomicConnectionTypeMismatch _ty1 _ty2) _pos) = "SE-057"
    errorIdent (AnnotatedError (EAtomicArrayConnectionTypeMismatch _ty1 _ty2) _pos) = "SE-058"
    errorIdent (AnnotatedError (EAtomicArrayConnectionSizeMismatch _size1 _size2) _pos) = "SE-059"
    errorIdent (AnnotatedError (EConstantWithoutKnownType _const) _pos) = "SE-060"
    errorIdent (AnnotatedError EStructInitializerInvalidUse _pos) = "SE-061"
    errorIdent (AnnotatedError (EStructInitializerTypeMismatch _ty1 _ty2) _pos) = "SE-062"
    errorIdent (AnnotatedError (EEnumInitializerExpectedTypeMismatch _ty1 _ty2) _pos) = "SE-063"
    errorIdent (AnnotatedError ESliceInvalidUse _pos) = "SE-064"
    errorIdent (AnnotatedError EArrayInitializerInvalidUse _pos) = "SE-065"
    errorIdent (AnnotatedError (EArrayInitializerNotArray _ty) _pos) = "SE-066"
    errorIdent (AnnotatedError EArrayExprListInitializerInvalidUse _pos) = "SE-067"
    errorIdent (AnnotatedError (EArrayExprListInitializerNotArray _ty) _pos) = "SE-068"
    errorIdent (AnnotatedError EOptionVariantInitializerInvalidUse _pos) = "SE-069"
    errorIdent (AnnotatedError (EArrayInitializerSizeMismatch _size1 _size2) _pos) = "SE-070"
    errorIdent (AnnotatedError (EArrayExprListInitializerSizeMismatch _size1 _size2) _pos) = "SE-071"
    errorIdent (AnnotatedError (EArrayExprListInitializerExprTypeMismatch _ty1 _ty2) _pos) = "SE-072"
    errorIdent (AnnotatedError (EReturnValueExpected _ty) _pos) = "SE-073"
    errorIdent (AnnotatedError EReturnValueNotUnit _pos) = "SE-074"
    errorIdent (AnnotatedError (EInvalidArrayType _ty) _pos) = "SE-075"
    errorIdent (AnnotatedError (EInvalidBoxType _ty) _pos) = "SE-076"
    errorIdent (AnnotatedError (ENoTypeFound _ident) _pos) = "SE-077"
    errorIdent (AnnotatedError (EGlobalNotType _def) _pos) = "SE-078"
    errorIdent (AnnotatedError (EInvalidAccessToGlobal _ident) _pos) = "SE-079"
    errorIdent (AnnotatedError (EConstantIsReadOnly _ident) _pos) = "SE-080"
    errorIdent (AnnotatedError (ESymbolAlreadyDefined _def) _pos) = "SE-081"
    errorIdent (AnnotatedError EContinueInvalidExpression _pos) = "SE-082"
    errorIdent (AnnotatedError (EContinueInvalidMethodOrViewerCall _ident) _pos) = "SE-083"
    errorIdent (AnnotatedError (EContinueInvalidMemberCall _ty) _pos) = "SE-084"
    errorIdent (AnnotatedError (EContinueActionExtraArgs _def _num) _pos) = "SE-085"
    errorIdent (AnnotatedError (EContinueActionMissingArgs _def) _pos) = "SE-086"
    errorIdent (AnnotatedError EEnumVariantInitializerInvalidUse _pos) = "SE-087"
    errorIdent (AnnotatedError (EEnumVariantNotFound _enum _variant) _pos) = "SE-088"
    errorIdent (AnnotatedError (EEnumVariantExtraParams _def _params _paramNumber) _pos) = "SE-089"
    errorIdent (AnnotatedError (EEnumVariantMissingParams _def _params _paramNumber) _pos) = "SE-090"
    errorIdent (AnnotatedError (EEnumVariantParamTypeMismatch _def _param _ty) _pos) = "SE-091"
    errorIdent (AnnotatedError (EFunctionNotFound _ident) _pos) = "SE-092"
    errorIdent (AnnotatedError (EGlobalNotFunction _def) _pos) = "SE-093"
    errorIdent (AnnotatedError (EUnexpectedNumericConstant _ty) _pos) = "SE-094"
    errorIdent (AnnotatedError (EInvalidAssignmentExprType _ty) _pos) = "SE-095"
    errorIdent (AnnotatedError (EInvalidMessageType _ty) _pos) = "SE-096"
    errorIdent (AnnotatedError (EInvalidOptionType _ty) _pos) = "SE-097"
    errorIdent (AnnotatedError (EInvalidReferenceType _ty) _pos) = "SE-098"
    errorIdent (AnnotatedError (EInvalidFixedLocationType _ty) _pos) = "SE-099"
    errorIdent (AnnotatedError (EInvalidAllocatorType _ty) _pos) = "SE-100"
    errorIdent (AnnotatedError (EInvalidClassFieldType _ty) _pos) = "SE-101"
    errorIdent (AnnotatedError (EInvalidStructFieldType _ty) _pos) = "SE-102"
    errorIdent (AnnotatedError (EInvalidEnumParameterType _ty) _pos) = "SE-103"
    errorIdent (AnnotatedError (EInvalidAccessPortType _ty) _pos) = "SE-104"
    errorIdent (AnnotatedError (EInvalidDeclarationType _ty) _pos) = "SE-105"
    errorIdent (AnnotatedError (EInvalidTypeSpecifier _ty) _pos) = "SE-106"
    errorIdent (AnnotatedError (EInvalidNumericConstantType _ty) _pos) = "SE-107"
    errorIdent (AnnotatedError (EInvalidActionParameterType _ty) _pos) = "SE-108"
    errorIdent (AnnotatedError (EInvalidProcedureParameterType _ty) _pos) = "SE-109"
    errorIdent (AnnotatedError (EMemberFunctionCallExtraArgs _def _num) _pos) = "SE-110"
    errorIdent (AnnotatedError (EMemberFunctionCallMissingArgs _def _num) _pos) = "SE-111"
    errorIdent (AnnotatedError (EMemberFunctionCallArgTypeMismatch _def _num _ty) _pos) = "SE-112"
    errorIdent (AnnotatedError (EArrayIndexNotUSize _ty) _pos) = "SE-113"
    errorIdent (AnnotatedError (EArraySliceLowerBoundNotUSize _ty) _pos) = "SE-114"
    errorIdent (AnnotatedError (EArraySliceUpperBoundNotUSize _ty) _pos) = "SE-115"
    errorIdent (AnnotatedError (EOutboundPortSendInvalidNumArgs _num) _pos) = "SE-116"
    errorIdent (AnnotatedError (EOutboundPortArgTypeMismatch _ty1 _ty2) _pos) = "SE-117"
    errorIdent (AnnotatedError (EAssignmentExprMismatch _ty1 _ty2) _pos) = "SE-118"
    errorIdent (AnnotatedError (EFieldValueAssignmentMissingFields _def _fields) _pos) = "SE-119"
    errorIdent (AnnotatedError (EFieldValueAssignmentUnknownFields _def _fields) _pos) = "SE-120"
    errorIdent (AnnotatedError (EFieldNotFixedLocation _ident _ty) _pos) = "SE-121"
    errorIdent (AnnotatedError (EFieldNotAccessPort _ident _ty) _pos) = "SE-122"
    errorIdent (AnnotatedError (EFieldNotSinkOrInboundPort _ident _ty) _pos) = "SE-123"
    errorIdent (AnnotatedError (EFieldNotOutboundPort _ident _ty) _pos) = "SE-124"
    errorIdent (AnnotatedError (EMemberAccessInvalidType _ty) _pos) = "SE-125"
    errorIdent (AnnotatedError (EMemberFunctionCallInvalidType _ty) _pos) = "SE-126"
    errorIdent (AnnotatedError (EMemberAccessUnknownField _def _ident) _pos) = "SE-127"
    errorIdent (AnnotatedError EInvalidProcedureCallInsideMemberFunction _pos) = "SE-128"
    errorIdent (AnnotatedError (EConstantOutRange _const) _pos) = "SE-129"
    errorIdent (AnnotatedError (EForIteratorInvalidType _ty) _pos) = "SE-130"
    errorIdent (AnnotatedError (EUsedTypeName _ident _loc) _pos) = "SE-131"
    errorIdent (AnnotatedError (EUsedGlobalName _ident _loc) _pos) = "SE-132"
    errorIdent (AnnotatedError (EUsedFunName _ident _loc) _pos) = "SE-133"
    errorIdent (AnnotatedError (EAccessPortConnectionInvalidGlobal _ident) _pos) = "SE-134"
    errorIdent (AnnotatedError (EAccessPortConnectionInterfaceNotProvided _res _iface) _pos) = "SE-135"
    errorIdent (AnnotatedError (ESinkPortConnectionInvalidGlobal _ident) _pos) = "SE-136"
    errorIdent (AnnotatedError (EInboundPortConnectionInvalidObject _ident) _pos) = "SE-137"
    errorIdent (AnnotatedError (EOutboundPortConnectionInvalidGlobal _ident) _pos) = "SE-138"
    errorIdent (AnnotatedError (EAllocatorPortConnectionInvalidGlobal _ident) _pos) = "SE-139"
    errorIdent (AnnotatedError (EAtomicAccessPortConnectionInvalidGlobal _ident) _pos) = "SE-140"
    errorIdent (AnnotatedError (EAtomicArrayAccessPortConnectionInvalidGlobal _ident) _pos) = "SE-141"
    errorIdent (AnnotatedError (EStructDefNotUniqueField _fields) _pos) = "SE-142"
    errorIdent (AnnotatedError (EEnumDefNotUniqueVariant _variants) _pos) = "SE-143"
    errorIdent (AnnotatedError (EInterfaceNotUniqueProcedure _procedures) _pos) = "SE-144"
    errorIdent (AnnotatedError (EClassLoop _members) _pos) = "SE-145"
    errorIdent (AnnotatedError (EDereferenceInvalidType _ty) _pos) = "SE-146"
    errorIdent (AnnotatedError (EMatchInvalidType _ty) _pos) = "SE-147"
    errorIdent (AnnotatedError (EMatchCaseDuplicate _ident _loc) _pos) = "SE-148"
    errorIdent (AnnotatedError (EMatchCaseUnknownVariants _variants) _pos) = "SE-149"
    errorIdent (AnnotatedError (EMatchMissingCases _variants) _pos) = "SE-150"
    errorIdent (AnnotatedError (EIsVariantInvalidType _ty) _pos) = "SE-151"
    errorIdent (AnnotatedError (EIsOptionVariantInvalidType _ty) _pos) = "SE-152"
    errorIdent (AnnotatedError (EIsVariantEnumTypeMismatch _enum _variant) _pos) = "SE-153"
    errorIdent (AnnotatedError (EOutboundPortInvalidProcedure _ident) _pos) = "SE-154"
    errorIdent (AnnotatedError EInvalidPoolInitialization _pos) = "SE-155"
    errorIdent (AnnotatedError EInvalidMsgQueueInitialization _pos) = "SE-156"
    errorIdent (AnnotatedError (EUnknownGlobal _ident) _pos) = "SE-157"
    errorIdent (AnnotatedError (EInvalidInterruptEmitterType _ty) _pos) = "SE-158"
    errorIdent (AnnotatedError (EInvalidPeriodicTimerEmitterType _ty) _pos) = "SE-159"
    errorIdent (AnnotatedError (EInvalidSystemInitEmitterType _ty) _pos) = "SE-160"
    errorIdent (AnnotatedError (EInboundPortConnectionMsgQueueTypeMismatch _ident _ty1 _ty2) _pos) = "SE-161"
    errorIdent (AnnotatedError (EOutboundPortConnectionMsgQueueTypeMismatch _ident _ty1 _ty2) _pos) = "SE-162"
    errorIdent (AnnotatedError (EAllocatorPortConnectionPoolTypeMismatch _ident _ty1 _ty2) _pos) = "SE-163"
    errorIdent (AnnotatedError (EInvalidTaskType _ty) _pos) = "SE-164"
    errorIdent (AnnotatedError (EInvalidHandlerType _ty) _pos) = "SE-165"
    errorIdent (AnnotatedError (EInvalidResourceType _ty) _pos) = "SE-166"
    errorIdent (AnnotatedError (EInvalidEmitterType _ty) _pos) = "SE-167"
    errorIdent (AnnotatedError (EInvalidChannelType _ty) _pos) = "SE-168"
    errorIdent (AnnotatedError (EEmitterClassNotInstantiable _ident) _pos) = "SE-169"
    errorIdent (AnnotatedError (ESingleExpressionTypeNotUnit _ty) _pos) = "SE-170"
    errorIdent _ = "Internal"

{--
ppError :: M.Map FilePath TL.Text ->
    SemanticErrors -> IO ()
ppError toModuleAST (AnnotatedError e pos@(Position start end)) =
  let fileName = sourceName start
      sourceLines = toModuleAST M.! fileName
  in
  case e of
    EProcedureNotFromProvidedInterfaces (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [SE-019]\x1b[0m: procedure not from provided interfaces."
            portStartLine = sourceLine start
            portEndLine = sourceLine end
            portStartColumn = sourceColumn start
            portEndColumn = 
                if portStartLine == portEndLine then 
                    sourceColumn end 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (portStartLine - 1)) + 1
            classStartLine = sourceLine startPosClass
            classEndLine = sourceLine endPosClass
            classStartColumn = sourceColumn startPosClass
            classEndColumn = 
                if classStartLine == classEndLine then 
                    sourceColumn endPosClass 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (classStartLine - 1)) + 1
        in
            TLIO.putStrLn $ prettyErrors
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Errata.Block
                                fancyRedStyle
                                (sourceName start, classStartLine, classStartColumn)
                                Nothing
                                [
                                    Pointer classStartLine classStartColumn
                                            classEndColumn
                                            True Nothing fancyRedPointer,
                                    Pointer portStartLine portStartColumn portEndColumn
                                            True (Just " \x1b[31munknown procedure\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just
                            ("The procedure \x1b[31m" <> T.pack ident
                                <> "\x1b[0m does not belong to any of the provided interfaces of resource class \x1b[31m"
                                <> T.pack classId <> "\x1b[0m."))
                ]
    EMissingProcedure ifaceId procId ->
        let title = "\x1b[31merror [SE-020]\x1b[0m: missing procedure."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procId <> "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> "\x1b[0m is not being provided."))
    EProcedureExtraParams (ifaceId, procId, params, procPos@(Position procStart _procEnd)) paramNumber ->
        let title = "\x1b[31merror [SE-021]\x1b[0m: extra parameters in procedure definition."
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procId <>
                    "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                procSourceLines "The interface of the procedure is defined here:" procFileName
                procPos Nothing
    EProcedureMissingParams (ifaceId, procId, params, procPos@(Position procStart _procEnd)) paramNumber ->
        let title = "\x1b[31merror [SE-022]\x1b[0m: missing parameters in procedure definition."
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procId <>
                    "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                procSourceLines "The interface of the procedure is defined here:" procFileName
                procPos Nothing
    EProcedureParamTypeMismatch (ifaceId, procId, expectedTy, procPos@(Position procStart _procEnd)) actualTy ->
        let title = "\x1b[31merror [SE-023]\x1b[0m: parameter type mismatch in procedure definition."
            procFileName = sourceName procStart
            procSourceLines = toModuleAST M.! procFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Parameter is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but you are defining it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError
                procSourceLines
                    ("The procedure \x1b[31m" <> T.pack procId <>
                     "\x1b[0m of the interface \x1b[31m" <> T.pack ifaceId <>
                     "\x1b[0m is defined here:")
                    procFileName procPos Nothing
    ETaskClassProvides ident ->
        let title = "\x1b[31merror [SE-024]\x1b[0m: task class provides an interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Task class \x1b[31m" <> T.pack ident <> "\x1b[0m provides an interface.\n" <>
                       "Task classes must not provide any interface."))
    ETaskClassProcedure (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [SE-025]\x1b[0m: task class defines a procedure."
            procStartLine = sourceLine start
            procEndLine = sourceLine end
            procStartColumn = sourceColumn start
            procEndColumn = 
                if procStartLine == procEndLine then 
                    sourceColumn end 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (procStartLine - 1)) + 1
            classStartLine = sourceLine startPosClass
            classEndLine = sourceLine endPosClass
            classStartColumn = sourceColumn startPosClass
            classEndColumn = 
                if classStartLine == classEndLine then 
                    sourceColumn endPosClass 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (classStartLine - 1)) + 1

        in
            TLIO.putStrLn $ prettyErrors
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Errata.Block
                                fancyRedStyle
                                (sourceName start, classStartLine, classStartColumn)
                                Nothing
                                [
                                    Pointer classStartLine classStartColumn
                                            classEndColumn
                                            True Nothing fancyRedPointer,
                                    Pointer procStartLine procStartColumn procEndColumn
                                            True (Just " \x1b[31minvalid procedure definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just
                            ("Task class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the procedure \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Task classes cannot define procedures."))
                ]
    ETaskClassNoActions ident ->
        let title = "\x1b[31merror [SE-026]\x1b[0m: task class does not define any actions."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Task class \x1b[31m" <> T.pack ident <> "\x1b[0m does not define any actions.\n" <>
                       "Task classes must define at least one action."))
    EHandlerClassProvides ident ->
        let title = "\x1b[31merror [SE-027]\x1b[0m: handler class provides an interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Handler class \x1b[31m" <> T.pack ident <> "\x1b[0m provides an interface.\n" <>
                       "Handler classes must not provide any interface."))
    EHandlerClassProcedure (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [SE-028]\x1b[0m: handler class defines a procedure."
            procStartLine = sourceLine start
            procEndLine = sourceLine end
            procStartColumn = sourceColumn start
            procEndColumn = 
                if procStartLine == procEndLine then 
                    sourceColumn end 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (procStartLine - 1)) + 1
            classStartLine = sourceLine startPosClass
            classEndLine = sourceLine endPosClass
            classStartColumn = sourceColumn startPosClass
            classEndColumn = 
                if classStartLine == classEndLine then 
                    sourceColumn endPosClass 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (classStartLine - 1)) + 1

        in
            TLIO.putStrLn $ prettyErrors
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Errata.Block
                                fancyRedStyle
                                (sourceName start, classStartLine, classStartColumn)
                                Nothing
                                [
                                    Pointer classStartLine classStartColumn
                                            classEndColumn
                                            True Nothing fancyRedPointer,
                                    Pointer procStartLine procStartColumn procEndColumn
                                            True (Just " \x1b[31minvalid procedure definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just
                            ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the procedure \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Handler classes cannot define procedures."))
                ]
    EHandlerClassNoAction ident ->
        let title = "\x1b[31merror [SE-029]\x1b[0m: handler class does not define any actions."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Handler class \x1b[31m" <> T.pack ident <> "\x1b[0m does not define any actions.\n" <>
                       "Handler classes must define exactly one action."))
    EHandlerClassMultipleActions classId prevActPos@(Position actStartPos _actEndPos) ->
        let title = "\x1b[31merror [SE-030]\x1b[0m: handler class defines multiple actions."
            actFileName = sourceName actStartPos
            actSourceLines = toModuleAST M.! actFileName 
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines multiple actions.")) >>
            printSimpleError
                actSourceLines "Another action is defined here:" actFileName
                prevActPos Nothing
    EHandlerClassNoSinkPort classId ->
        let title = "\x1b[31merror [SE-031]\x1b[0m: handler class does not define any sink port."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m does not define any sink port.\n" <>
                       "Handler classes must define exactly one sink port."))
    EHandlerClassMultipleSinkPorts classId prevPortPos@(Position portStartPos _portEndPos) ->
        let title = "\x1b[31merror [SE-032]\x1b[0m: handler class defines multiple sink ports."
            portFileName = sourceName portStartPos
            portSourceLines = toModuleAST M.! portFileName 
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines multiple sink ports.")) >>
            printSimpleError
                portSourceLines "Another sink port is defined here:" portFileName
                prevPortPos Nothing
    EHandlerClassInPort (classId, Position startPosClass endPosClass) ident ->
        let title = "\x1b[31merror [SE-033]\x1b[0m: handler class defines an in port."
            portStartLine = sourceLine start
            portEndLine = sourceLine end
            portStartColumn = sourceColumn start
            portEndColumn = 
                if portStartLine == portEndLine then 
                    sourceColumn end 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (portStartLine - 1)) + 1
            classStartLine = sourceLine startPosClass
            classEndLine = sourceLine endPosClass
            classStartColumn = sourceColumn startPosClass
            classEndColumn = 
                if classStartLine == classEndLine then 
                    sourceColumn endPosClass 
                else 
                    fromIntegral $ TL.length (TL.lines sourceLines !! (classStartLine - 1)) + 1

        in
            TLIO.putStrLn $ prettyErrors
                sourceLines
                [
                    Errata
                        (Just title)
                        [
                            Errata.Block
                                fancyRedStyle
                                (sourceName start, classStartLine, classStartColumn)
                                Nothing
                                [
                                    Pointer classStartLine classStartColumn
                                            classEndColumn
                                            True Nothing fancyRedPointer,
                                    Pointer portStartLine portStartColumn portEndColumn
                                            True (Just " \x1b[31minvalid port definition\x1b[0m") fancyRedPointer
                                ]
                                Nothing
                        ]
                        (Just
                            ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the in port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                            <> "Handler classes cannot define in ports."))
                ]
    EIfElseIfCondNotBool ts ->
        let title = "\x1b[31merror [SE-034]\x1b[0m: if-else-if condition not boolean."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The condition in the statement is expected to be of type \x1b[31mbool\x1b[0m but it is of type \x1b[31m" <> showText ts <> "\x1b[0m."))
    EFunctionCallExtraArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
        let title = "\x1b[31merror [SE-035]\x1b[0m: extra arguments in function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EFunctionCallMissingArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
        let title = "\x1b[31merror [SE-036]\x1b[0m: missing arguments in function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EFunctionCallArgTypeMismatch (funcId, expectedTy, funcPos@(Position funcStart _procEnd)) argNumber actualTy ->
        let title = "\x1b[31merror [SE-037]\x1b[0m: argument type mismatch in function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Argument \x1b[31m#" <> T.pack (show argNumber) <> "\x1b[0m of function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EMemberAccessNotFunction ident ->
        let title = "\x1b[31merror [SE-038]\x1b[0m: Access to a member that is not a function."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not a valid member function."))
    EMutableReferenceToImmutable ->
        let title = "\x1b[31merror [SE-039]\x1b[0m: mutable reference to immutable object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "You are trying to create a mutable reference to an immutable object.")
    EMutableReferenceToPrivate ->
        let title = "\x1b[31merror [SE-040]\x1b[0m: mutable reference to private object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "You are trying to create a mutable reference to a private object.")
    EBinOpExpectedTypeLeft op expectedTy actualTy ->
        let title = "\x1b[31merror [SE-041]\x1b[0m: Binary operation expected type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The result of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the left operand you are providing is of type \x1b[31m" <>
                    showText actualTy <> "\x1b[0m."))
    EBinOpExpectedTypeRight op expectedTy actualTy ->
        let title = "\x1b[31merror [SE-042]\x1b[0m: Binary operation expected type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The result of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the right operand you are providing is of type \x1b[31m" <>
                    showText actualTy <> "\x1b[0m."))
    EBinOpTypeMismatch op ty_le ty_re ->
        let title = "\x1b[31merror [SE-043]\x1b[0m: binary operation type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m expects operands of the same type but the left one is of type \x1b[31m" <>
                    showText ty_le <> "\x1b[0m and the right one is of type \x1b[31m" <> showText ty_re <> "\x1b[0m."))
    EBinOpExpectedTypeNotBool op ty ->
        let title = "\x1b[31merror [SE-044]\x1b[0m: binary operation expected result type not boolean."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m will result in a value of type \x1b[31m" <> showText TBool <>
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EBinOpLeftTypeNotBool op ty ->
        let title = "\x1b[31merror [SE-045]\x1b[0m: binary operation expected boolean type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TBool <> "\x1b[0m."))
    EBinOpRightTypeNotBool op ty ->
        let title = "\x1b[31merror [SE-046]\x1b[0m: binary operation expected boolean type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TBool <> "\x1b[0m."))
    EBinOpExpectedTypeNotNum op ty ->
        let title = "\x1b[31merror [SE-047]\x1b[0m: binary operation expected result type not numeric."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m will result in a numeric value but the expected type is \x1b[31m" <> showText ty <> "\x1b[0m."))
    EBinOpLeftTypeNotNum op ty ->
        let title = "\x1b[31merror [SE-048]\x1b[0m: binary operation expected numeric type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of numeric type."))
    EBinOpRightTypeNotNum op ty ->
        let title = "\x1b[31merror [SE-049]\x1b[0m: binary operation expected numeric type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of numeric type."))
    EBinOpRightTypeNotPos op ty ->
        let title = "\x1b[31merror [SE-050]\x1b[0m: binary operation expected positive numeric type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of positive numeric type."))
    EBinOpLeftTypeNotEq op ty ->
        let title = "\x1b[31merror [SE-051]\x1b[0m: binary operation expected equatable type on the left."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of equatable type."))
    EBinOpRightTypeNotEq op ty ->
        let title = "\x1b[31merror [SE-052]\x1b[0m: binary operation expected equatable type on the right."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                    "\x1b[0m is of type \x1b[31m" <> showText ty <>
                    "\x1b[0m but it is expected to be of equatable type."))
    EAtomicAccessInvalidType ty ->
        let title = "\x1b[31merror [SE-053]\x1b[0m: invalid type for the atomic access interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic access, only numeric types are allowed."))
    EAtomicArrayAccessInvalidType ty ->
        let title = "\x1b[31merror [SE-054]\x1b[0m: invalid type for the atomic array access interface."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array access, only numeric types are allowed."))
    EAtomicInvalidType ty ->
        let title = "\x1b[31merror [SE-055]\x1b[0m: invalid atomic type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic."))
    EAtomicArrayInvalidType ty ->
        let title = "\x1b[31merror [SE-056]\x1b[0m: invalid atomic array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array."))
    EAtomicConnectionTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-057]\x1b[0m: atomic connection type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the connected atomic resource is expected to be \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAtomicArrayConnectionTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-058]\x1b[0m: atomic array connection type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the elements of the connected atomic array is expected to be \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but the array is of elements of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAtomicArrayConnectionSizeMismatch expectedSize actualSize ->
        let title = "\x1b[31merror [SE-059]\x1b[0m: atomic array connection size mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The size of the connected atomic array is expected to be \x1b[31m" <> showText expectedSize <>
                    "\x1b[0m but the array has size \x1b[31m" <> showText actualSize <> "\x1b[0m."))
    EConstantWithoutKnownType c ->
        let title = "\x1b[31merror [SE-060]\x1b[0m: constant without known type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the constant \x1b[31m" <> showText c <>
                    "\x1b[0m cannot be inferred from the environment and must be explicitly defined."))
    EStructInitializerInvalidUse ->
        let title = "\x1b[31merror [SE-061]\x1b[0m: invalid use of struct initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use a struct initializer in an invalid context.\n" <>
                        "Struct initializers can only be used to initialize struct objects.")
    EStructInitializerTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-062]\x1b[0m: struct initializer type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The struct initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EEnumInitializerExpectedTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-063]\x1b[0m: enum initializer expected type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The enum initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    ESliceInvalidUse ->
        let title = "\x1b[31merror [SE-064]\x1b[0m: invalid use of slice."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use a slice in an invalid context.\n" <>
                        "Slices can only be used to create references to a part of an array.")
    EArrayInitializerInvalidUse ->
        let title = "\x1b[31merror [SE-065]\x1b[0m: invalid use of an array initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an array initializer in an invalid context.\n" <>
                        "Array initializers can only be used to initialize array objects.")
    EArrayInitializerNotArray ty ->
        let title = "\x1b[31merror [SE-066]\x1b[0m: assignment of an array initializer to a non-array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid use of an array initializer.\n" <>
                    "You are trying to assign an array initializer to an object of type \x1b[31m" <>
                    showText ty <> "\x1b[0m."))
    EArrayExprListInitializerInvalidUse ->
        let title = "\x1b[31merror [SE-067]\x1b[0m: invalid use of an expression list array initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an array expression list initializer in an invalid context.\n" <>
                        "TArray expression list initializers can only be used to initialize array objects.")
    EArrayExprListInitializerNotArray ty ->
        let title = "\x1b[31merror [SE-068]\x1b[0m: assignment of an array expression list initializer to a non-array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Invalid use of an array expression list initializer.\n" <>
                    "You are trying to assign an array expression list initializer to an object of type \x1b[31m" <>
                    showText ty <> "\x1b[0m."))
    EOptionVariantInitializerInvalidUse ->
        let title = "\x1b[31merror [SE-069]\x1b[0m: invalid use of an option variant initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an option variant initializer in an invalid context.\n" <>
                        "Option variant initializers can only be used to initialize option objects.")
    EArrayInitializerSizeMismatch expectedSize initializerSize ->
        let title = "\x1b[31merror [SE-070]\x1b[0m: array initializer size mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The size of the array initializer is \x1b[31m" <> showText initializerSize <>
                    "\x1b[0m but the expected size is \x1b[31m" <> showText expectedSize <> "\x1b[0m."))
    EArrayExprListInitializerSizeMismatch expectedSize initializerSize ->
        let title = "\x1b[31merror [SE-071]\x1b[0m: array expression list initializer size mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The size of the array expression list initializer is \x1b[31m" <> T.pack (show initializerSize) <>
                    "\x1b[0m but the expected size is \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
    EArrayExprListInitializerExprTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-072]\x1b[0m: list of initializing expressions type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The expression in the array expression list initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EReturnValueExpected ty ->
        let title = "\x1b[31merror [SE-073]\x1b[0m: expected return value."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The function is expected to return a value of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EReturnValueNotUnit ->
        let title = "\x1b[31merror [SE-074]\x1b[0m: return value not expected."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "The function is not expected to return a value.")
    EInvalidArrayType ty ->
        let title = "\x1b[31merror [SE-075]\x1b[0m: invalid array type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid array type."))
    EInvalidBoxType ty ->
        let title = "\x1b[31merror [SE-076]\x1b[0m: invalid box type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid box type."))
    ENoTypeFound ident ->
        let title = "\x1b[31merror [SE-077]\x1b[0m: no type found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> T.pack ident <> "\x1b[0m is not found."))
    EGlobalNotType (ident, globalPos@(Position globalStart _)) ->
        let title = "\x1b[31merror [SE-078]\x1b[0m: global object but not a type."
            globalFileName = sourceName globalStart
            globalSourceLines = toModuleAST M.! globalFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a type.")) >>
            printSimpleError
                globalSourceLines "The global object is defined here:" globalFileName
                globalPos Nothing
    EInvalidAccessToGlobal ident ->
        let title = "\x1b[31merror [SE-079]\x1b[0m: invalid access to global object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be accessed from within this context."))
    EConstantIsReadOnly ident ->
        let title = "\x1b[31merror [SE-080]\x1b[0m: invalid write to a constant."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The constant \x1b[31m" <> T.pack ident <> "\x1b[0m is read-only and cannot be modified."))
    ESymbolAlreadyDefined (ident, symbolPos@(Position symbolStart _symbolEnd)) ->
        let title = "\x1b[31merror [SE-081]\x1b[0m: symbol already defined."
            symbolFileName = sourceName symbolStart
            symbolSourceLines = toModuleAST M.! symbolFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already defined.")) >>
            printSimpleError
                symbolSourceLines "The symbol was previoulsy defined here:" symbolFileName
                symbolPos Nothing
    EContinueInvalidExpression -> 
        let title = "\x1b[31merror [SE-082]\x1b[0m: invalid expression in continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "The expression in a continue statement must be a call to a member action.")
    EContinueInvalidMethodOrViewerCall ident -> 
        let title = "\x1b[31merror [SE-083]\x1b[0m: invalid method or viewer call in continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("This statement can only be used to call a continuation action.\n" <>
                       "The member function call \x1b[31m" <> T.pack ident <> "\x1b[0m in a continue statement is invalid."))
    EContinueInvalidMemberCall ts ->
        let title = "\x1b[31merror [SE-084]\x1b[0m: invalid member call in continue statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("This statement can only be used to call a continuation action.\n" <>
                       "Calling a procedure of an object of type \x1b[31m" <> showText ts <> "\x1b[0m in a continue statement is invalid."))
    EContinueActionExtraArgs (ident, params, actionPos@(Position actStartPos _endPos)) argNumber ->
        let title = "\x1b[31merror [SE-085]\x1b[0m: extra arguments in continuation action."
            actFileName = sourceName actStartPos
            actSourceLines = toModuleAST M.! actFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Action \x1b[31m" <> T.pack ident <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            printSimpleError
                actSourceLines "The action is defined here:" actFileName
                actionPos Nothing
    EContinueActionMissingArgs (ident, actionPos@(Position actStartPos _endPos)) ->
        let title = "\x1b[31merror [SE-086]\x1b[0m: missing arguments in continuation action."
            actFileName = sourceName actStartPos
            actSourceLines = toModuleAST M.! actFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Action \x1b[31m" <> T.pack ident <>
                    "\x1b[0m requires \x1b[31mone\x1b[0m parameter but you are providing \x1b[31mnone\x1b[0m.")) >>
            printSimpleError
                actSourceLines "The action is defined here:" actFileName
                actionPos Nothing
    EEnumVariantInitializerInvalidUse ->
        let title = "\x1b[31merror [SE-087]\x1b[0m: invalid use of an enum variant initializer."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just $ "You are trying to use an enum variant initializer in an invalid context.\n" <>
                        "Enum variant initializers can only be used to initialize enum objects.")
    EEnumVariantNotFound enumId variant ->
        let title = "\x1b[31merror [SE-088]\x1b[0m: enum variant not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Enum \x1b[31m" <> T.pack enumId <> "\x1b[0m does not have a variant named \x1b[31m" <> T.pack variant <> "\x1b[0m."))
    EEnumVariantExtraParams (enumId, enumPos@(Position enumStart _end)) (variant, params) paramNumber ->
        let title = "\x1b[31merror [SE-089]\x1b[0m: extra parameters in enum variant."
            enumFileName = sourceName enumStart
            enumSourceLines = toModuleAST M.! enumFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Enum variant \x1b[31m" <> T.pack variant <>
                    "\x1b[0m of enum \x1b[31m" <> T.pack enumId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                enumSourceLines "The enum is defined here:" enumFileName
                enumPos Nothing
    EEnumVariantMissingParams (enumId, enumPos@(Position enumStart _end)) (variant, params) paramNumber ->
        let title = "\x1b[31merror [SE-090]\x1b[0m: missing parameters in enum variant."
            enumFileName = sourceName enumStart
            enumSourceLines = toModuleAST M.! enumFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Enum variant \x1b[31m" <> T.pack variant <>
                    "\x1b[0m of enum \x1b[31m" <> T.pack enumId <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.")) >>
            printSimpleError
                enumSourceLines "The enum is defined here:" enumFileName
                enumPos Nothing
    EEnumVariantParamTypeMismatch (enumId, enumPos@(Position enumStart _end)) (variant, paramNumber, expectedTy) actualTy ->
        let title = "\x1b[31merror [SE-091]\x1b[0m: enum variant parameter type mismatch."
            enumFileName = sourceName enumStart
            enumSourceLines = toModuleAST M.! enumFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Parameter \x1b[31m" <> T.pack (show paramNumber) <>
                    "\x1b[0m of enum variant \x1b[31m" <> T.pack variant <>
                    "\x1b[0m of enum \x1b[31m" <> T.pack enumId <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError
                enumSourceLines "The enum is defined here:" enumFileName
                enumPos Nothing
    EFunctionNotFound ident ->
        let title = "\x1b[31merror [SE-092]\x1b[0m: function not found."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Function \x1b[31m" <> T.pack ident <> "\x1b[0m not found."))
    EGlobalNotFunction (ident, globalPos@(Position globalStart _)) ->
        let title = "\x1b[31merror [SE-093]\x1b[0m: global object but not a function."
            globalFileName = sourceName globalStart
            globalSourceLines = toModuleAST M.! globalFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a function.")) >>
            printSimpleError
                globalSourceLines "The global object is defined here:" globalFileName
                globalPos Nothing
    EUnexpectedNumericConstant ty ->
        let title = "\x1b[31merror [SE-094]\x1b[0m: unexpected numeric constant."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Expected a value of type \x1b[31m" <> showText ty <> "\x1b[0m but found a numeric constant."))
    EInvalidAssignmentExprType ty ->
        let title = "\x1b[31merror [SE-095]\x1b[0m: invalid assignment expression type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Objects of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be copied."))
    EInvalidMessageType ty ->
        let title = "\x1b[31merror [SE-096]\x1b[0m: invalid message type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid message type."))
    EInvalidOptionType ty ->
        let title = "\x1b[31merror [SE-097]\x1b[0m: invalid option type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid option type."))
    EInvalidReferenceType ty ->
        let title = "\x1b[31merror [SE-098]\x1b[0m: invalid reference type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("References to objects of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be created."))
    EInvalidFixedLocationType ty ->
        let title = "\x1b[31merror [SE-099]\x1b[0m: invalid fixed-location type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Fixed-location fields of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be defined."))
    EInvalidAllocatorType ty ->
        let title = "\x1b[31merror [SE-100]\x1b[0m: invalid allocator type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid allocator type."))
    EInvalidClassFieldType ty ->
        let title = "\x1b[31merror [SE-101]\x1b[0m: invalid class field type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid class field type."))
    EInvalidStructFieldType ty ->
        let title = "\x1b[31merror [SE-102]\x1b[0m: invalid struct field type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid struct field type."))
    EInvalidEnumParameterType ty ->
        let title = "\x1b[31merror [SE-103]\x1b[0m: invalid enum parameter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for an enum variant."))
    EInvalidAccessPortType ty ->
        let title = "\x1b[31merror [SE-104]\x1b[0m: invalid access port type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid access port type."))
    EInvalidDeclarationType ty ->
        let title = "\x1b[31merror [SE-105]\x1b[0m: invalid declaration type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid object declaration type."))
    EInvalidTypeSpecifier ts ->
        let title = "\x1b[31merror [SE-106]\x1b[0m: invalid type specifier."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type specifier \x1b[31m" <> showText ts <> "\x1b[0m is not valid."))
    EInvalidNumericConstantType ty ->
        let title = "\x1b[31merror [SE-107]\x1b[0m: invalid numeric constant type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The expected type of this expression is \x1b[31m" <> showText ty <> "\x1b[0m but it is a numeric constant."))
    EInvalidActionParameterType ty ->
        let title = "\x1b[31merror [SE-108]\x1b[0m: invalid action parameter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for an action."))
    EInvalidProcedureParameterType ty ->
        let title = "\x1b[31merror [SE-109]\x1b[0m: invalid procedure parameter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for a procedure."))
    EMemberFunctionCallExtraArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
        let title = "\x1b[31merror [SE-110]\x1b[0m: extra arguments in member function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Member function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EMemberFunctionCallMissingArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
        let title = "\x1b[31merror [SE-111]\x1b[0m: missing arguments in member function call."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Member function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EMemberFunctionCallArgTypeMismatch (funcId, expectedTy, funcPos@(Position funcStart _procEnd)) argNumber actualTy ->
        let title = "\x1b[31merror [SE-112]\x1b[0m: member function call argument type mismatch."
            funcFileName = sourceName funcStart
            funcSourceLines = toModuleAST M.! funcFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Argument \x1b[31m#" <> T.pack (show argNumber) <>
                    "\x1b[0m of member function \x1b[31m" <> T.pack funcId <>
                    "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) >>
            printSimpleError
                funcSourceLines ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                funcPos Nothing
    EArrayIndexNotUSize ty ->
        let title = "\x1b[31merror [SE-113]\x1b[0m: invalid array index type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the array index is \x1b[31m" <> showText ty <>
                 "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TUSize <> "\x1b[0m."))
    EArraySliceLowerBoundNotUSize ty ->
        let title = "\x1b[31merror [SE-114]\x1b[0m: invalid array slice lower bound type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the lower bound of the array slice is \x1b[31m" <> showText ty <>
                 "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TUSize <> "\x1b[0m."))
    EArraySliceUpperBoundNotUSize ty ->
        let title = "\x1b[31merror [SE-115]\x1b[0m: invalid array slice upper bound type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type of the upper bound of the array slice is \x1b[31m" <> showText ty <>
                 "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TUSize <> "\x1b[0m."))
    EOutboundPortSendInvalidNumArgs argNumber ->
        let title = "\x1b[31merror [SE-116]\x1b[0m: invalid number of arguments in outbound port send."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The send procedure of an outbound port expects \x1b[31mone\x1b[0m argument but you are providing \x1b[31m" <>
                    T.pack (show argNumber) <> "\x1b[0m."))
    EOutboundPortArgTypeMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-117]\x1b[0m: output port argument type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The output data is expected to be of type \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but you are sending data of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAssignmentExprMismatch expectedTy actualTy ->
        let title = "\x1b[31merror [SE-118]\x1b[0m: assignment expression type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The expected type of the assignment is \x1b[31m" <> showText expectedTy <>
                    "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EFieldValueAssignmentMissingFields (record, recordPos) [field] ->
        let title = "\x1b[31merror [SE-119]\x1b[0m: missing field/s in field assignment expression."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack field <>
                    "\x1b[0m is not being assigned a value in the field assignment expression.")) >>
            case recordPos of
                Position recordStart _end ->
                    let recordFileName = sourceName recordStart
                        recordSourceLines = toModuleAST M.! recordFileName
                    in
                    printSimpleError
                        recordSourceLines ("The type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") recordFileName
                        recordPos Nothing
                _ -> return ()
    EFieldValueAssignmentMissingFields (record, recordPos) fields ->
        let title = "\x1b[31merror [SE-119]\x1b[0m: missing field/s in field assignment expression."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fields) <>
                    "\x1b[0m are not being assigned a value in the field assignment expression.")) >>
            case recordPos of
                Position recordStart _end ->
                    let recordFileName = sourceName recordStart
                        recordSourceLines = toModuleAST M.! recordFileName
                    in
                    printSimpleError
                        recordSourceLines ("The type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") recordFileName
                        recordPos Nothing
                _ -> return ()
    EFieldValueAssignmentUnknownFields (record, recordPos) [field] ->
        let title = "\x1b[31merror [SE-120]\x1b[0m: unknown field/s in field assignment expression."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack field <>
                    "\x1b[0m is not a field of the type \x1b[31m" <> showText record <> "\x1b[0m.")) >>
            case recordPos of
                Position recordStart _end ->
                    let recordFileName = sourceName recordStart
                        recordSourceLines = toModuleAST M.! recordFileName
                    in
                    printSimpleError
                        recordSourceLines ("The type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") recordFileName
                        recordPos Nothing
                _ -> return ()
    EFieldValueAssignmentUnknownFields (record, recordPos) fields ->
        let title = "\x1b[31merror [SE-120]\x1b[0m: unknown field/s in field assignment expression."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fields) <>
                    "\x1b[0m are not fields of the type \x1b[31m" <> showText record <> "\x1b[0m.")) >>
            case recordPos of
                Position recordStart _end ->
                    let recordFileName = sourceName recordStart
                        recordSourceLines = toModuleAST M.! recordFileName
                    in
                    printSimpleError
                        recordSourceLines ("The type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") recordFileName
                        recordPos Nothing
                _ -> return ()
    EFieldNotFixedLocation fieldName ty ->
        let title = "\x1b[31merror [SE-121]\x1b[0m: field is not a fixed-location field."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack fieldName <>
                    "\x1b[0m of type \x1b[31m" <> showText ty <>
                    "\x1b[0m is not a fixed-location field."))
    EFieldNotAccessPort fieldName ty ->
        let title = "\x1b[31merror [SE-122]\x1b[0m: field is not an access port field."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack fieldName <>
                    "\x1b[0m of type \x1b[31m" <> showText ty <>
                    "\x1b[0m is not an access port field."))
    EFieldNotSinkOrInboundPort fieldName ty ->
        let title = "\x1b[31merror [SE-123]\x1b[0m: field is not a sink or inbound port field."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack fieldName <>
                    "\x1b[0m of type \x1b[31m" <> showText ty <>
                    "\x1b[0m is not a sink or inbound port field."))
    EFieldNotOutboundPort fieldName ty ->
        let title = "\x1b[31merror [SE-124]\x1b[0m: field is not an outbound port field."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack fieldName <>
                    "\x1b[0m of type \x1b[31m" <> showText ty <>
                    "\x1b[0m is not an outbound port field."))
    EMemberAccessInvalidType ty ->
        let title = "\x1b[31merror [SE-125]\x1b[0m: invalid member access type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for member access."))
    EMemberFunctionCallInvalidType ty -> 
        let title = "\x1b[31merror [SE-126]\x1b[0m: invalid member function call type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for member function call."))
    EMemberAccessUnknownField (recordId, recordPos@(Position recordStart _end)) field ->
        let title = "\x1b[31merror [SE-127]\x1b[0m: unknown field in member access."
            recordFileName = sourceName recordStart
            recordSourceLines = toModuleAST M.! recordFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack field <>
                    "\x1b[0m is not a field of the type \x1b[31m" <> T.pack recordId <> "\x1b[0m.")) >>
            printSimpleError
                recordSourceLines ("The type \x1b[31m" <> T.pack recordId <> "\x1b[0m is defined here:") recordFileName
                recordPos Nothing
    EInvalidProcedureCallInsideMemberFunction -> 
        let title = "\x1b[31merror [SE-128]\x1b[0m: invalid procedure call inside member function."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "Procedure calls are not allowed inside member functions.")
    EConstantOutRange ty ->
        let title = "\x1b[31merror [SE-129]\x1b[0m: constant out of range."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The constant value \x1b[31m" <> showText ty <> "\x1b[0m is out of range for its type."))
    EForIteratorInvalidType ty -> 
        let title = "\x1b[31merror [SE-130]\x1b[0m: invalid type for for-loop iterator."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for a for-loop iterator."))
    EUsedTypeName ident prevPos@(Position prevStart _) ->
        let title = "\x1b[31merror [SE-131]\x1b[0m: type name already used."
            prevFileName = sourceName prevStart
            prevSourceLines = toModuleAST M.! prevFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type cannot be defined because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.")) >>
            printSimpleError
                prevSourceLines "The symbol is previously used here:" prevFileName
                prevPos Nothing
    EUsedGlobalName ident prevPos@(Position prevStart _) ->
        let title = "\x1b[31merror [SE-132]\x1b[0m: global name already used."
            prevFileName = sourceName prevStart
            prevSourceLines = toModuleAST M.! prevFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object cannot be declared because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.")) >>
            printSimpleError
                prevSourceLines "The symbol is previously used here:" prevFileName
                prevPos Nothing
    EUsedFunName ident prevPos@(Position prevStart _) ->
        let title = "\x1b[31merror [SE-133]\x1b[0m: function name already used."
            prevFileName = sourceName prevStart
            prevSourceLines = toModuleAST M.! prevFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The function cannot be declared because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.")) >>
            printSimpleError
                prevSourceLines "The symbol is previously used here:" prevFileName
                prevPos Nothing
    EAccessPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-134]\x1b[0m: invalid global object in access port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be used in an access port connection."))
    EAccessPortConnectionInterfaceNotProvided ident iface ->
        let title = "\x1b[31merror [SE-135]\x1b[0m: resource does not provide the interface"
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Resource \x1b[31m" <> T.pack ident <>
                    "\x1b[0m does not provide the interface \x1b[31m" <> T.pack iface <> "\x1b[0m."))
    ESinkPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-136]\x1b[0m: invalid sink port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to a sink port."))
    EInboundPortConnectionInvalidObject ident ->
        let title = "\x1b[31merror [SE-137]\x1b[0m: invalid inbound port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an inbound port."))
    EOutboundPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-138]\x1b[0m: invalid outbound port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an outbound port."))
    EAllocatorPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-139]\x1b[0m: invalid allocator port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an allocator port."))
    EAtomicAccessPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-140]\x1b[0m: invalid atomic access port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an atomic access port."))
    EAtomicArrayAccessPortConnectionInvalidGlobal ident ->
        let title = "\x1b[31merror [SE-141]\x1b[0m: invalid atomic array access port connection."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an atomic array access port."))
    EStructDefNotUniqueField [fieldName] ->
        let title = "\x1b[31merror [SE-142]\x1b[0m: duplicate field in struct definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Field \x1b[31m" <> T.pack fieldName <> "\x1b[0m is duplicated in the struct definition."))
    EStructDefNotUniqueField fieldNames ->
        let title = "\x1b[31merror [SE-142]\x1b[0m: duplicate field in struct definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fieldNames) <>
                    "\x1b[0m are duplicated in the struct definition."))
    EEnumDefNotUniqueVariant [variantName] ->
        let title = "\x1b[31merror [SE-143]\x1b[0m: duplicate variant in enum definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is duplicated in the enum definition."))
    EEnumDefNotUniqueVariant variantNames ->
        let title = "\x1b[31merror [SE-143]\x1b[0m: duplicate variant in enum definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variants \x1b[31m" <> T.intercalate ", " (map T.pack variantNames) <>
                    "\x1b[0m are duplicated in the enum definition."))
    EInterfaceNotUniqueProcedure [procName] ->
        let title = "\x1b[31merror [SE-144]\x1b[0m: duplicate procedure in interface definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedure \x1b[31m" <> T.pack procName <> "\x1b[0m is duplicated in the interface definition."))
    EInterfaceNotUniqueProcedure procNames ->
        let title = "\x1b[31merror [SE-144]\x1b[0m: duplicate procedure in interface definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Procedures \x1b[31m" <> T.intercalate ", " (map T.pack procNames) <>
                    "\x1b[0m are duplicated in the interface definition."))
    EClassLoop funcNames ->
        let title = "\x1b[31merror [SE-145]\x1b[0m: Loop between member function calls in class definition."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The member functions \x1b[31m" <> T.intercalate " -> " (map T.pack funcNames) <>
                    "\x1b[0m form a recursive calling loop in the class definition."))
    EDereferenceInvalidType ty ->
        let title = "\x1b[31merror [SE-146]\x1b[0m: invalid type for dereference."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m cannot be dereferenced."))
    EMatchInvalidType ty ->
        let title = "\x1b[31merror [SE-147]\x1b[0m: invalid type for match statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for match statement."))
    EMatchCaseDuplicate variantName prevCase@(Position prevStart _) ->
        let title = "\x1b[31merror [SE-148]\x1b[0m: duplicate case in match statement."
            prevFileName = sourceName prevStart
            prevSourceLines = toModuleAST M.! prevFileName
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is duplicated in the match statement.")) >>
            printSimpleError
                prevSourceLines "The variant is previously used here:" prevFileName
                prevCase Nothing
    EMatchCaseUnknownVariants [variantName] ->
        let title = "\x1b[31merror [SE-149]\x1b[0m: unknown variant/s in match case."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is not a valid variant of the enum or option."))
    EMatchCaseUnknownVariants variantNames ->
        let title = "\x1b[31merror [SE-149]\x1b[0m: unknown variant/s in match case."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Variants \x1b[31m" <> T.intercalate ", " (map T.pack variantNames) <>
                    "\x1b[0m are not valid variants of the enum or option."))
    EMatchMissingCases [caseIdent] -> 
        let title = "\x1b[31merror [SE-150]\x1b[0m: missing case/s in match statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Case \x1b[31m" <> T.pack caseIdent <> "\x1b[0m is missing in the match statement."))
    EMatchMissingCases caseIdents ->
        let title = "\x1b[31merror [SE-150]\x1b[0m: missing case/s in match statement."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Cases \x1b[31m" <> T.intercalate ", " (map T.pack caseIdents) <>
                    "\x1b[0m are missing in the match statement."))
    EIsVariantInvalidType ty ->
        let title = "\x1b[31merror [SE-151]\x1b[0m: invalid type for is-variant expression."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for is-variant expression."))
    EIsOptionVariantInvalidType ty ->
        let title = "\x1b[31merror [SE-152]\x1b[0m: invalid type for is-option-variant expression."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not an option type."))
    EIsVariantEnumTypeMismatch expectedEnum actualEnum ->
        let title = "\x1b[31merror [SE-153]\x1b[0m: type mismatch in is-variant expression."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The expected enum type is \x1b[31m" <> T.pack expectedEnum <>
                    "\x1b[0m but the actual type is \x1b[31m" <> T.pack actualEnum <> "\x1b[0m."))
    EOutboundPortInvalidProcedure ident ->
        let title = "\x1b[31merror [SE-154]\x1b[0m: invalid procedure in outbound port."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is not a valid procedure for an outbound port."))
    EInvalidPoolInitialization -> 
        let title = "\x1b[31merror [SE-155]\x1b[0m: invalid pool initialization."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "A pool object cannot be initialized with a value.")
    EInvalidMsgQueueInitialization -> 
        let title = "\x1b[31merror [SE-156]\x1b[0m: invalid message queue initialization."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just "A message queue object cannot be initialized with a value.")
    EUnknownGlobal ident ->
        let title = "\x1b[31merror [SE-157]\x1b[0m: unknown global object."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not defined."))
    EInvalidInterruptEmitterType ty ->
        let title = "\x1b[31merror [SE-158]\x1b[0m: invalid interrupt emitter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Interrupts emit data of type \x1b[31m" <> showText TUInt32 <> 
                       "\x1b[0m but you are expecting data of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EInvalidPeriodicTimerEmitterType ty ->
        let title = "\x1b[31merror [SE-159]\x1b[0m: invalid periodic timer emitter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Periodic timers emit data of type \x1b[31m" <> showText (TStruct "TimeVal") <> 
                       "\x1b[0m but you are expecting data of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EInvalidSystemInitEmitterType ty ->
        let title = "\x1b[31merror [SE-160]\x1b[0m: invalid system init emitter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("System init emitters emit data of type \x1b[31m" <> showText (TStruct "TimeVal") <> 
                       "\x1b[0m but you are expecting data of type \x1b[31m" <> showText ty <> "\x1b[0m."))
    EInboundPortConnectionMsgQueueTypeMismatch msgQueueId expectedTy actualTy ->
        let title = "\x1b[31merror [SE-161]\x1b[0m: message queue type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The message queue \x1b[31m" <> T.pack msgQueueId <> 
                       "\x1b[0m exchanges data messages of type \x1b[31m" <> showText expectedTy <> 
                       "\x1b[0m but you are expecting data of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EOutboundPortConnectionMsgQueueTypeMismatch msgQueueId expectedTy actualTy ->
        let title = "\x1b[31merror [SE-162]\x1b[0m: message queue type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The message queue \x1b[31m" <> T.pack msgQueueId <> 
                       "\x1b[0m exchanges data messages of type \x1b[31m" <> showText expectedTy <> 
                       "\x1b[0m but you are sending data of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EAllocatorPortConnectionPoolTypeMismatch poolId expectedTy actualTy ->
        let title = "\x1b[31merror [SE-163]\x1b[0m: pool type mismatch."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The pool \x1b[31m" <> T.pack poolId <> 
                       "\x1b[0m serves data of type \x1b[31m" <> showText expectedTy <> 
                       "\x1b[0m but you are expecting data of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
    EInvalidTaskType ty ->
        let title = "\x1b[31merror [SE-164]\x1b[0m: invalid task type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid task type."))
    EInvalidHandlerType ty ->
        let title = "\x1b[31merror [SE-165]\x1b[0m: invalid handler type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid handler type."))
    EInvalidResourceType ty ->
        let title = "\x1b[31merror [SE-166]\x1b[0m: invalid resource type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid resource type."))
    EInvalidEmitterType ty ->
        let title = "\x1b[31merror [SE-167]\x1b[0m: invalid emitter type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid emitter type."))
    EInvalidChannelType ty ->
        let title = "\x1b[31merror [SE-168]\x1b[0m: invalid channel type."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid channel type."))
    EEmitterClassNotInstantiable ident ->
        let title = "\x1b[31merror [SE-169]\x1b[0m: emitter class is not instantiable."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Applications cannot instantiate event emitters of class \x1b[31m" <> T.pack ident <> "\x1b[0m."))
    ESingleExpressionTypeNotUnit ty ->
        let title = "\x1b[31merror [SE-170]\x1b[0m: single expression type is not unit."
        in
            printSimpleError
                sourceLines title fileName pos
                (Just ("Expressions used in single-expression statements must have type \x1b[31m" <> showText TUnit <> 
                       "\x1b[0m but the expression has type \x1b[31m" <> showText ty <> "\x1b[0m. Return values of functions cannot be ignored."))
    _ -> putStrLn $ show pos ++ ": " ++ show e
-- | Print the error as is
ppError _ (AnnotatedError e pos) = putStrLn $ show pos ++ ": " ++ show e
--}

    errorTitle (AnnotatedError (EInvalidArrayIndexing _ty) _pos) = "invalid array indexing"
    errorTitle (AnnotatedError (ENotNamedObject _ident) _pos) = "object not found"
    errorTitle (AnnotatedError (ENotConstant _ident) _pos) = "invalid use of a non-constant object"
    errorTitle (AnnotatedError EAssignmentToImmutable _pos) = "assignment to immutable variable"
    errorTitle (AnnotatedError EIfElseNoOtherwise _pos) = "missing else clause"
    errorTitle (AnnotatedError (ENotCasteable _ty1 _ty2) _pos) = "invalid cast"
    errorTitle (AnnotatedError (EInvalidParameterType _param) _pos) = "invalid parameter type"
    errorTitle (AnnotatedError (EInvalidReturnType _ty) _pos) = "invalid return type"
    errorTitle (AnnotatedError (EProcedureCallExtraArgs _def _numArgsm) _pos) = "extra arguments in procedure call"
    errorTitle (AnnotatedError (EProcedureCallMissingArgs _def _numArgsm) _pos) = "missing arguments in procedure call"
    errorTitle (AnnotatedError (EProcedureCallArgTypeMismatch _def _num _actualTy) _pos) = "argument type mismatch in procedure call"
    errorTitle (AnnotatedError (EUnknownProcedure _ident) _pos) = "unknown procedure"
    errorTitle (AnnotatedError (EResourceClassNoProvides _ident) _pos) = "resource class does not provide any interface"
    errorTitle (AnnotatedError (EResourceClassAction _def _ident) _pos) = "resource class defines an action"
    errorTitle (AnnotatedError (EResourceClassInPort _def _ident) _pos) = "resource class defines an in port"
    errorTitle (AnnotatedError (EResourceClassOutPort _def _ident) _pos) = "resource class defines an out port"
    errorTitle (AnnotatedError (EInterfaceNotFound _ident) _pos) = "interface not found"
    errorTitle (AnnotatedError (EGlobalNotInterface _ident) _pos) = "identifier not an interface"
    errorTitle (AnnotatedError (EProcedureNotFromProvidedInterfaces _def _ident) _pos) = "SE-019"
    errorTitle (AnnotatedError (EMissingProcedure _ifaceId _procId) _pos) = "SE-020"
    errorTitle (AnnotatedError (EProcedureExtraParams _def _paramNumber) _pos) = "SE-021"
    errorTitle (AnnotatedError (EProcedureMissingParams _def _paramNumber) _pos) = "SE-022"
    errorTitle (AnnotatedError (EProcedureParamTypeMismatch _def _ty) _pos) = "SE-023"
    errorTitle (AnnotatedError (ETaskClassProvides _ident) _pos) = "SE-024"
    errorTitle (AnnotatedError (ETaskClassProcedure _def _ident) _pos) = "SE-025"
    errorTitle (AnnotatedError (ETaskClassNoActions _ident) _pos) = "SE-026"
    errorTitle (AnnotatedError (EHandlerClassProvides _ident) _pos) = "SE-027"
    errorTitle (AnnotatedError (EHandlerClassProcedure _def _ident) _pos) = "SE-028"
    errorTitle (AnnotatedError (EHandlerClassNoAction _ident) _pos) = "SE-029"
    errorTitle (AnnotatedError (EHandlerClassMultipleActions _ident _loc) _pos) = "SE-030"
    errorTitle (AnnotatedError (EHandlerClassNoSinkPort _ident) _pos) = "SE-031"
    errorTitle (AnnotatedError (EHandlerClassMultipleSinkPorts _ident _loc) _pos) = "SE-032"
    errorTitle (AnnotatedError (EHandlerClassInPort _def _ident) _pos) = "SE-033"
    errorTitle (AnnotatedError (EIfElseIfCondNotBool _ty) _pos) = "SE-034"
    errorTitle (AnnotatedError (EFunctionCallExtraArgs _def _paramNumber) _pos) = "SE-035"
    errorTitle (AnnotatedError (EFunctionCallMissingArgs _def _paramNumber) _pos) = "SE-036"
    errorTitle (AnnotatedError (EFunctionCallArgTypeMismatch _def _paramNumber _ty) _pos) = "SE-037"
    errorTitle (AnnotatedError (EMemberAccessNotFunction _ident) _pos) = "SE-038"
    errorTitle (AnnotatedError EMutableReferenceToImmutable _pos) = "SE-039"
    errorTitle (AnnotatedError EMutableReferenceToPrivate _pos) = "SE-040"
    errorTitle (AnnotatedError (EBinOpExpectedTypeLeft _op _ty1 _ty2) _pos) = "SE-041"
    errorTitle (AnnotatedError (EBinOpExpectedTypeRight _op _ty1 _ty2) _pos) = "SE-042"
    errorTitle (AnnotatedError (EBinOpTypeMismatch _op _ty1 _ty2) _pos) = "SE-043"
    errorTitle (AnnotatedError (EBinOpExpectedTypeNotBool _op _ty) _pos) = "SE-044"
    errorTitle (AnnotatedError (EBinOpLeftTypeNotBool _op _ty) _pos) = "SE-045"
    errorTitle (AnnotatedError (EBinOpRightTypeNotBool _op _ty) _pos) = "SE-046"
    errorTitle (AnnotatedError (EBinOpExpectedTypeNotNum _op _ty) _pos) = "SE-047"
    errorTitle (AnnotatedError (EBinOpLeftTypeNotNum _op _ty) _pos) = "SE-048"
    errorTitle (AnnotatedError (EBinOpRightTypeNotNum _op _ty) _pos) = "SE-049"
    errorTitle (AnnotatedError (EBinOpRightTypeNotPos _op _ty) _pos) = "SE-050"
    errorTitle (AnnotatedError (EBinOpLeftTypeNotEq _op _ty) _pos) = "SE-051"
    errorTitle (AnnotatedError (EBinOpRightTypeNotEq _op _ty) _pos) = "SE-052"
    errorTitle (AnnotatedError (EAtomicAccessInvalidType _ty) _pos) = "SE-053"
    errorTitle (AnnotatedError (EAtomicArrayAccessInvalidType _ty) _pos) = "SE-054"
    errorTitle (AnnotatedError (EAtomicInvalidType _ty) _pos) = "SE-055"
    errorTitle (AnnotatedError (EAtomicArrayInvalidType _ty) _pos) = "SE-056"
    errorTitle (AnnotatedError (EAtomicConnectionTypeMismatch _ty1 _ty2) _pos) = "SE-057"
    errorTitle (AnnotatedError (EAtomicArrayConnectionTypeMismatch _ty1 _ty2) _pos) = "SE-058"
    errorTitle (AnnotatedError (EAtomicArrayConnectionSizeMismatch _size1 _size2) _pos) = "SE-059"
    errorTitle (AnnotatedError (EConstantWithoutKnownType _const) _pos) = "SE-060"
    errorTitle (AnnotatedError EStructInitializerInvalidUse _pos) = "SE-061"
    errorTitle (AnnotatedError (EStructInitializerTypeMismatch _ty1 _ty2) _pos) = "SE-062"
    errorTitle (AnnotatedError (EEnumInitializerExpectedTypeMismatch _ty1 _ty2) _pos) = "SE-063"
    errorTitle (AnnotatedError ESliceInvalidUse _pos) = "SE-064"
    errorTitle (AnnotatedError EArrayInitializerInvalidUse _pos) = "SE-065"
    errorTitle (AnnotatedError (EArrayInitializerNotArray _ty) _pos) = "SE-066"
    errorTitle (AnnotatedError EArrayExprListInitializerInvalidUse _pos) = "SE-067"
    errorTitle (AnnotatedError (EArrayExprListInitializerNotArray _ty) _pos) = "SE-068"
    errorTitle (AnnotatedError EOptionVariantInitializerInvalidUse _pos) = "SE-069"
    errorTitle (AnnotatedError (EArrayInitializerSizeMismatch _size1 _size2) _pos) = "SE-070"
    errorTitle (AnnotatedError (EArrayExprListInitializerSizeMismatch _size1 _size2) _pos) = "SE-071"
    errorTitle (AnnotatedError (EArrayExprListInitializerExprTypeMismatch _ty1 _ty2) _pos) = "SE-072"
    errorTitle (AnnotatedError (EReturnValueExpected _ty) _pos) = "SE-073"
    errorTitle (AnnotatedError EReturnValueNotUnit _pos) = "SE-074"
    errorTitle (AnnotatedError (EInvalidArrayType _ty) _pos) = "SE-075"
    errorTitle (AnnotatedError (EInvalidBoxType _ty) _pos) = "SE-076"
    errorTitle (AnnotatedError (ENoTypeFound _ident) _pos) = "SE-077"
    errorTitle (AnnotatedError (EGlobalNotType _def) _pos) = "SE-078"
    errorTitle (AnnotatedError (EInvalidAccessToGlobal _ident) _pos) = "SE-079"
    errorTitle (AnnotatedError (EConstantIsReadOnly _ident) _pos) = "SE-080"
    errorTitle (AnnotatedError (ESymbolAlreadyDefined _def) _pos) = "SE-081"
    errorTitle (AnnotatedError EContinueInvalidExpression _pos) = "SE-082"
    errorTitle (AnnotatedError (EContinueInvalidMethodOrViewerCall _ident) _pos) = "SE-083"
    errorTitle (AnnotatedError (EContinueInvalidMemberCall _ty) _pos) = "SE-084"
    errorTitle (AnnotatedError (EContinueActionExtraArgs _def _num) _pos) = "SE-085"
    errorTitle (AnnotatedError (EContinueActionMissingArgs _def) _pos) = "SE-086"
    errorTitle (AnnotatedError EEnumVariantInitializerInvalidUse _pos) = "SE-087"
    errorTitle (AnnotatedError (EEnumVariantNotFound _enum _variant) _pos) = "SE-088"
    errorTitle (AnnotatedError (EEnumVariantExtraParams _def _params _paramNumber) _pos) = "SE-089"
    errorTitle (AnnotatedError (EEnumVariantMissingParams _def _params _paramNumber) _pos) = "SE-090"
    errorTitle (AnnotatedError (EEnumVariantParamTypeMismatch _def _param _ty) _pos) = "SE-091"
    errorTitle (AnnotatedError (EFunctionNotFound _ident) _pos) = "SE-092"
    errorTitle (AnnotatedError (EGlobalNotFunction _def) _pos) = "SE-093"
    errorTitle (AnnotatedError (EUnexpectedNumericConstant _ty) _pos) = "SE-094"
    errorTitle (AnnotatedError (EInvalidAssignmentExprType _ty) _pos) = "SE-095"
    errorTitle (AnnotatedError (EInvalidMessageType _ty) _pos) = "SE-096"
    errorTitle (AnnotatedError (EInvalidOptionType _ty) _pos) = "SE-097"
    errorTitle (AnnotatedError (EInvalidReferenceType _ty) _pos) = "SE-098"
    errorTitle (AnnotatedError (EInvalidFixedLocationType _ty) _pos) = "SE-099"
    errorTitle (AnnotatedError (EInvalidAllocatorType _ty) _pos) = "SE-100"
    errorTitle (AnnotatedError (EInvalidClassFieldType _ty) _pos) = "SE-101"
    errorTitle (AnnotatedError (EInvalidStructFieldType _ty) _pos) = "SE-102"
    errorTitle (AnnotatedError (EInvalidEnumParameterType _ty) _pos) = "SE-103"
    errorTitle (AnnotatedError (EInvalidAccessPortType _ty) _pos) = "SE-104"
    errorTitle (AnnotatedError (EInvalidDeclarationType _ty) _pos) = "SE-105"
    errorTitle (AnnotatedError (EInvalidTypeSpecifier _ty) _pos) = "SE-106"
    errorTitle (AnnotatedError (EInvalidNumericConstantType _ty) _pos) = "SE-107"
    errorTitle (AnnotatedError (EInvalidActionParameterType _ty) _pos) = "SE-108"
    errorTitle (AnnotatedError (EInvalidProcedureParameterType _ty) _pos) = "SE-109"
    errorTitle (AnnotatedError (EMemberFunctionCallExtraArgs _def _num) _pos) = "SE-110"
    errorTitle (AnnotatedError (EMemberFunctionCallMissingArgs _def _num) _pos) = "SE-111"
    errorTitle (AnnotatedError (EMemberFunctionCallArgTypeMismatch _def _num _ty) _pos) = "SE-112"
    errorTitle (AnnotatedError (EArrayIndexNotUSize _ty) _pos) = "SE-113"
    errorTitle (AnnotatedError (EArraySliceLowerBoundNotUSize _ty) _pos) = "SE-114"
    errorTitle (AnnotatedError (EArraySliceUpperBoundNotUSize _ty) _pos) = "SE-115"
    errorTitle (AnnotatedError (EOutboundPortSendInvalidNumArgs _num) _pos) = "SE-116"
    errorTitle (AnnotatedError (EOutboundPortArgTypeMismatch _ty1 _ty2) _pos) = "SE-117"
    errorTitle (AnnotatedError (EAssignmentExprMismatch _ty1 _ty2) _pos) = "SE-118"
    errorTitle (AnnotatedError (EFieldValueAssignmentMissingFields _def _fields) _pos) = "SE-119"
    errorTitle (AnnotatedError (EFieldValueAssignmentUnknownFields _def _fields) _pos) = "SE-120"
    errorTitle (AnnotatedError (EFieldNotFixedLocation _ident _ty) _pos) = "SE-121"
    errorTitle (AnnotatedError (EFieldNotAccessPort _ident _ty) _pos) = "SE-122"
    errorTitle (AnnotatedError (EFieldNotSinkOrInboundPort _ident _ty) _pos) = "SE-123"
    errorTitle (AnnotatedError (EFieldNotOutboundPort _ident _ty) _pos) = "SE-124"
    errorTitle (AnnotatedError (EMemberAccessInvalidType _ty) _pos) = "SE-125"
    errorTitle (AnnotatedError (EMemberFunctionCallInvalidType _ty) _pos) = "SE-126"
    errorTitle (AnnotatedError (EMemberAccessUnknownField _def _ident) _pos) = "SE-127"
    errorTitle (AnnotatedError EInvalidProcedureCallInsideMemberFunction _pos) = "SE-128"
    errorTitle (AnnotatedError (EConstantOutRange _const) _pos) = "SE-129"
    errorTitle (AnnotatedError (EForIteratorInvalidType _ty) _pos) = "SE-130"
    errorTitle (AnnotatedError (EUsedTypeName _ident _loc) _pos) = "SE-131"
    errorTitle (AnnotatedError (EUsedGlobalName _ident _loc) _pos) = "SE-132"
    errorTitle (AnnotatedError (EUsedFunName _ident _loc) _pos) = "SE-133"
    errorTitle (AnnotatedError (EAccessPortConnectionInvalidGlobal _ident) _pos) = "SE-134"
    errorTitle (AnnotatedError (EAccessPortConnectionInterfaceNotProvided _res _iface) _pos) = "SE-135"
    errorTitle (AnnotatedError (ESinkPortConnectionInvalidGlobal _ident) _pos) = "SE-136"
    errorTitle (AnnotatedError (EInboundPortConnectionInvalidObject _ident) _pos) = "SE-137"
    errorTitle (AnnotatedError (EOutboundPortConnectionInvalidGlobal _ident) _pos) = "SE-138"
    errorTitle (AnnotatedError (EAllocatorPortConnectionInvalidGlobal _ident) _pos) = "SE-139"
    errorTitle (AnnotatedError (EAtomicAccessPortConnectionInvalidGlobal _ident) _pos) = "SE-140"
    errorTitle (AnnotatedError (EAtomicArrayAccessPortConnectionInvalidGlobal _ident) _pos) = "SE-141"
    errorTitle (AnnotatedError (EStructDefNotUniqueField _fields) _pos) = "SE-142"
    errorTitle (AnnotatedError (EEnumDefNotUniqueVariant _variants) _pos) = "SE-143"
    errorTitle (AnnotatedError (EInterfaceNotUniqueProcedure _procedures) _pos) = "SE-144"
    errorTitle (AnnotatedError (EClassLoop _members) _pos) = "SE-145"
    errorTitle (AnnotatedError (EDereferenceInvalidType _ty) _pos) = "SE-146"
    errorTitle (AnnotatedError (EMatchInvalidType _ty) _pos) = "SE-147"
    errorTitle (AnnotatedError (EMatchCaseDuplicate _ident _loc) _pos) = "SE-148"
    errorTitle (AnnotatedError (EMatchCaseUnknownVariants _variants) _pos) = "SE-149"
    errorTitle (AnnotatedError (EMatchMissingCases _variants) _pos) = "SE-150"
    errorTitle (AnnotatedError (EIsVariantInvalidType _ty) _pos) = "SE-151"
    errorTitle (AnnotatedError (EIsOptionVariantInvalidType _ty) _pos) = "SE-152"
    errorTitle (AnnotatedError (EIsVariantEnumTypeMismatch _enum _variant) _pos) = "SE-153"
    errorTitle (AnnotatedError (EOutboundPortInvalidProcedure _ident) _pos) = "SE-154"
    errorTitle (AnnotatedError EInvalidPoolInitialization _pos) = "SE-155"
    errorTitle (AnnotatedError EInvalidMsgQueueInitialization _pos) = "SE-156"
    errorTitle (AnnotatedError (EUnknownGlobal _ident) _pos) = "SE-157"
    errorTitle (AnnotatedError (EInvalidInterruptEmitterType _ty) _pos) = "SE-158"
    errorTitle (AnnotatedError (EInvalidPeriodicTimerEmitterType _ty) _pos) = "SE-159"
    errorTitle (AnnotatedError (EInvalidSystemInitEmitterType _ty) _pos) = "SE-160"
    errorTitle (AnnotatedError (EInboundPortConnectionMsgQueueTypeMismatch _ident _ty1 _ty2) _pos) = "SE-161"
    errorTitle (AnnotatedError (EOutboundPortConnectionMsgQueueTypeMismatch _ident _ty1 _ty2) _pos) = "SE-162"
    errorTitle (AnnotatedError (EAllocatorPortConnectionPoolTypeMismatch _ident _ty1 _ty2) _pos) = "SE-163"
    errorTitle (AnnotatedError (EInvalidTaskType _ty) _pos) = "SE-164"
    errorTitle (AnnotatedError (EInvalidHandlerType _ty) _pos) = "SE-165"
    errorTitle (AnnotatedError (EInvalidResourceType _ty) _pos) = "SE-166"
    errorTitle (AnnotatedError (EInvalidEmitterType _ty) _pos) = "SE-167"
    errorTitle (AnnotatedError (EInvalidChannelType _ty) _pos) = "SE-168"
    errorTitle (AnnotatedError (EEmitterClassNotInstantiable _ident) _pos) = "SE-169"
    errorTitle (AnnotatedError (ESingleExpressionTypeNotUnit _ty) _pos) = "SE-170"
    errorTitle _ = "Unknown"

    toText e@(AnnotatedError err pos@(Position start end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            case err of 
                EInvalidArrayIndexing ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("You are trying to index an object of type \x1b[31m" <> showText ty <> "\x1b[0m.")) 
                ENotNamedObject ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The variable \x1b[31m" <> T.pack ident <> "\x1b[0m has not been declared")) 
                ENotConstant ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a constant."))
                EAssignmentToImmutable ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "You are trying to assign a value to an immutable object.")
                EIfElseNoOtherwise ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("You are missing the else clause in an if-else-if statement.\n" <>
                            "You must provide an else clause if you are defining an else-if clause."))
                ENotCasteable ty1 ty2 ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("You cannot cast a value of type \x1b[31m" <> showText ty1 <> "\x1b[0m to type \x1b[31m" <> showText ty2 <> "\x1b[0m."))
                EInvalidParameterType (Parameter ident ts) ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Parameter \x1b[31m" <> T.pack ident <> "\x1b[0m has an invalid type \x1b[31m" <> showText ts <> "\x1b[0m."))
                EInvalidReturnType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid return type \x1b[31m" <> showText ty <> "\x1b[0m."))
                EProcedureCallExtraArgs (procId, params, procPos) numArgs ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Procedure \x1b[31m" <> T.pack procId <>
                    "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                    "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show numArgs) <> "\x1b[0m.")) <>
                    case procPos of 
                        Position procStart _procEnd -> 
                            let procFileName = sourceName procStart
                                procSourceLines = files M.! procFileName in
                            pprintSimpleError
                                procSourceLines "\nThe interface of the procedure is defined here:" procFileName
                                procPos Nothing
                        _ -> ""
                EProcedureCallMissingArgs (ident, params, procPos) numArgs ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Procedure \x1b[31m" <> T.pack ident <>
                            "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                            "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show numArgs) <> "\x1b[0m.")) <>
                    case procPos of 
                        Position procStart _procEnd -> 
                            let procFileName = sourceName procStart
                                procSourceLines = files M.! procFileName in
                            pprintSimpleError
                                procSourceLines ("Procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is defined here:") procFileName
                                procPos Nothing
                        _ -> ""
                EProcedureCallArgTypeMismatch (ident, expectedTy, procPos) numArgs actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Argument \x1b[31m#" <> T.pack (show numArgs) <> "\x1b[0m of procedure \x1b[31m" <> T.pack ident <>
                            "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                            "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.")) <>
                    case procPos of 
                        Position procStart _procEnd -> 
                            let procFileName = sourceName procStart
                                procSourceLines = files M.! procFileName in
                            pprintSimpleError
                                procSourceLines
                                ("Procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is defined here:")
                                procFileName procPos Nothing
                        _ -> ""
                EUnknownProcedure ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Unknown procedure \x1b[31m" <> T.pack ident <> "\x1b[0m."))
                EResourceClassNoProvides ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Resource class \x1b[31m" <> T.pack ident <> "\x1b[0m does not provide any interface.\n" <>
                            "A resource class must provide at least one interface."))
                EResourceClassAction (classId, Position startPosClass endPosClass) ident ->
                    let actionStartLine = sourceLine start
                        actionEndLine = sourceLine end
                        actionStartColumn = sourceColumn start
                        actionEndColumn = 
                            if actionStartLine == actionEndLine then 
                                sourceColumn end 
                            else 
                                T.length (T.lines sourceLines !! (actionStartLine - 1)) + 1
                        classStartLine = sourceLine startPosClass
                        classEndLine = sourceLine endPosClass
                        classStartColumn = sourceColumn startPosClass
                        classEndColumn = 
                            if classStartLine == classEndLine then 
                                sourceColumn endPosClass 
                            else 
                                T.length (T.lines sourceLines !! (classStartLine - 1)) + 1

                    in
                        TL.toStrict $ prettyErrors
                            sourceLines
                            [
                                Errata
                                    (Just title)
                                    [
                                        Errata.Block
                                            fancyRedStyle
                                            (sourceName start, classStartLine, classStartColumn)
                                            Nothing
                                            [
                                                Pointer classStartLine classStartColumn
                                                        classEndColumn
                                                        True Nothing fancyRedPointer,
                                                Pointer actionStartLine actionStartColumn actionEndColumn
                                                        True (Just " \x1b[31minvalid action definition\x1b[0m") fancyRedPointer
                                            ]
                                            Nothing
                                    ]
                                    (Just
                                        ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the action \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                                        <> "Resource classes cannot define actions."))
                            ] 
                EResourceClassInPort (classId, Position startPosClass endPosClass) ident ->
                    let portStartLine = sourceLine start
                        portEndLine = sourceLine end
                        portStartColumn = sourceColumn start
                        portEndColumn = 
                            if portStartLine == portEndLine then 
                                sourceColumn end 
                            else 
                                T.length (T.lines sourceLines !! (portStartLine - 1)) + 1
                        classStartLine = sourceLine startPosClass
                        classEndLine = sourceLine endPosClass
                        classStartColumn = sourceColumn startPosClass
                        classEndColumn = 
                            if classStartLine == classEndLine then 
                                sourceColumn endPosClass 
                            else 
                                T.length (T.lines sourceLines !! (classStartLine - 1)) + 1

                    in
                        TL.toStrict $ prettyErrors
                            sourceLines
                            [
                                Errata
                                    (Just title)
                                    [
                                        Errata.Block
                                            fancyRedStyle
                                            (sourceName start, classStartLine, classStartColumn)
                                            Nothing
                                            [
                                                Pointer classStartLine classStartColumn
                                                        classEndColumn
                                                        True Nothing fancyRedPointer,
                                                Pointer portStartLine portStartColumn portEndColumn
                                                        True (Just " \x1b[31minvalid port definition\x1b[0m") fancyRedPointer
                                            ]
                                            Nothing
                                    ]
                                    (Just
                                        ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the in port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                                        <> "Resource classes cannot define in ports."))
                            ]
                EResourceClassOutPort (classId, Position startPosClass endPosClass) ident ->
                    let portStartLine = sourceLine start
                        portEndLine = sourceLine end
                        portStartColumn = sourceColumn start
                        portEndColumn = 
                            if portStartLine == portEndLine then 
                                sourceColumn end 
                            else 
                                T.length (T.lines sourceLines !! (portStartLine - 1)) + 1
                        classStartLine = sourceLine startPosClass
                        classEndLine = sourceLine endPosClass
                        classStartColumn = sourceColumn startPosClass
                        classEndColumn = 
                            if classStartLine == classEndLine then 
                                sourceColumn endPosClass 
                            else 
                                T.length (T.lines sourceLines !! (classStartLine - 1)) + 1
                    in
                        TL.toStrict $ prettyErrors
                            sourceLines
                            [
                                Errata
                                    (Just title)
                                    [
                                        Errata.Block
                                            fancyRedStyle
                                            (sourceName start, classStartLine, classStartColumn)
                                            Nothing
                                            [
                                                Pointer classStartLine classStartColumn
                                                        classEndColumn
                                                        True Nothing fancyRedPointer,
                                                Pointer portStartLine portStartColumn portEndColumn
                                                        True (Just " \x1b[31minvalid port definition\x1b[0m") fancyRedPointer
                                            ]
                                            Nothing
                                    ]
                                    (Just
                                        ("Resource class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the out port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                                        <> "Resource classes cannot define out ports."))
                            ]
                EInterfaceNotFound ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Interface \x1b[31m" <> T.pack ident <> "\x1b[0m not found."))
                EGlobalNotInterface ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not an interface."))
                _ -> pprintSimpleError sourceLines title fileName pos Nothing
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e
    
    toDiagnostic e@(AnnotatedError (EInvalidArrayIndexing _ts) pos) _files =
        LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing
        
        where 
            text = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
    toDiagnostic _ _files = 
        LSP.Diagnostic 
            emptyRange
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing
        where 
            text = T.pack "\x1b[31mUknown\x1b[0m."