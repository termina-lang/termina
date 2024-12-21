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
    errorIdent (AnnotatedError (EBinOpExpectedTypeLeft _op _expectedTy _actualTy) _pos) = "SE-041"
    errorIdent (AnnotatedError (EBinOpExpectedTypeRight _op _expectedTy _actualTy) _pos) = "SE-042"
    errorIdent (AnnotatedError (EBinOpTypeMismatch _op _ty_le _ty_re) _pos) = "SE-043"
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
    errorIdent (AnnotatedError (EAtomicConnectionTypeMismatch _expectedTy _actualTy) _pos) = "SE-057"
    errorIdent (AnnotatedError (EAtomicArrayConnectionTypeMismatch _expectedTy _actualTy) _pos) = "SE-058"
    errorIdent (AnnotatedError (EAtomicArrayConnectionSizeMismatch _expectedSize _actualSize) _pos) = "SE-059"
    errorIdent (AnnotatedError (EConstantWithoutKnownType _c) _pos) = "SE-060"
    errorIdent (AnnotatedError EStructInitializerInvalidUse _pos) = "SE-061"
    errorIdent (AnnotatedError (EStructInitializerTypeMismatch _expectedTy _actualTy) _pos) = "SE-062"
    errorIdent (AnnotatedError (EEnumInitializerExpectedTypeMismatch _expectedTy _actualTy) _pos) = "SE-063"
    errorIdent (AnnotatedError ESliceInvalidUse _pos) = "SE-064"
    errorIdent (AnnotatedError EArrayInitializerInvalidUse _pos) = "SE-065"
    errorIdent (AnnotatedError (EArrayInitializerNotArray _ty) _pos) = "SE-066"
    errorIdent (AnnotatedError EArrayExprListInitializerInvalidUse _pos) = "SE-067"
    errorIdent (AnnotatedError (EArrayExprListInitializerNotArray _ty) _pos) = "SE-068"
    errorIdent (AnnotatedError EOptionVariantInitializerInvalidUse _pos) = "SE-069"
    errorIdent (AnnotatedError (EArrayInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "SE-070"
    errorIdent (AnnotatedError (EArrayExprListInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "SE-071"
    errorIdent (AnnotatedError (EArrayExprListInitializerExprTypeMismatch _expectedTy _actualTy) _pos) = "SE-072"
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
    errorIdent (AnnotatedError (EContinueActionExtraArgs _def _argNumber) _pos) = "SE-085"
    errorIdent (AnnotatedError (EContinueActionMissingArgs _def) _pos) = "SE-086"
    errorIdent (AnnotatedError EEnumVariantInitializerInvalidUse _pos) = "SE-087"
    errorIdent (AnnotatedError (EEnumVariantNotFound _enumId _variant) _pos) = "SE-088"
    errorIdent (AnnotatedError (EEnumVariantExtraParams _def _params _paramNumber) _pos) = "SE-089"
    errorIdent (AnnotatedError (EEnumVariantMissingParams _def _params _paramNumber) _pos) = "SE-090"
    errorIdent (AnnotatedError (EEnumVariantParamTypeMismatch _def _param _actualTy) _pos) = "SE-091"
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
    errorIdent (AnnotatedError (EMemberFunctionCallExtraArgs _def _argNumber) _pos) = "SE-110"
    errorIdent (AnnotatedError (EMemberFunctionCallMissingArgs _def _argNumber) _pos) = "SE-111"
    errorIdent (AnnotatedError (EMemberFunctionCallArgTypeMismatch _def _argNumber _actualTy) _pos) = "SE-112"
    errorIdent (AnnotatedError (EArrayIndexNotUSize _ty) _pos) = "SE-113"
    errorIdent (AnnotatedError (EArraySliceLowerBoundNotUSize _ty) _pos) = "SE-114"
    errorIdent (AnnotatedError (EArraySliceUpperBoundNotUSize _ty) _pos) = "SE-115"
    errorIdent (AnnotatedError (EOutboundPortSendInvalidNumArgs _argNumber) _pos) = "SE-116"
    errorIdent (AnnotatedError (EOutboundPortArgTypeMismatch _expectedTy _actualTy) _pos) = "SE-117"
    errorIdent (AnnotatedError (EAssignmentExprMismatch _expectedTy _actualTy) _pos) = "SE-118"
    errorIdent (AnnotatedError (EFieldValueAssignmentMissingFields _def _fields) _pos) = "SE-119"
    errorIdent (AnnotatedError (EFieldValueAssignmentUnknownFields _def _fields) _pos) = "SE-120"
    errorIdent (AnnotatedError (EFieldNotFixedLocation _fieldName _ty) _pos) = "SE-121"
    errorIdent (AnnotatedError (EFieldNotAccessPort _fieldName _ty) _pos) = "SE-122"
    errorIdent (AnnotatedError (EFieldNotSinkOrInboundPort _fieldName _ty) _pos) = "SE-123"
    errorIdent (AnnotatedError (EFieldNotOutboundPort _fieldName _ty) _pos) = "SE-124"
    errorIdent (AnnotatedError (EMemberAccessInvalidType _ty) _pos) = "SE-125"
    errorIdent (AnnotatedError (EMemberFunctionCallInvalidType _ty) _pos) = "SE-126"
    errorIdent (AnnotatedError (EMemberAccessUnknownField _def _field) _pos) = "SE-127"
    errorIdent (AnnotatedError EInvalidProcedureCallInsideMemberFunction _pos) = "SE-128"
    errorIdent (AnnotatedError (EConstantOutRange _const) _pos) = "SE-129"
    errorIdent (AnnotatedError (EForIteratorInvalidType _ty) _pos) = "SE-130"
    errorIdent (AnnotatedError (EUsedTypeName _ident _loc) _pos) = "SE-131"
    errorIdent (AnnotatedError (EUsedGlobalName _ident _loc) _pos) = "SE-132"
    errorIdent (AnnotatedError (EUsedFunName _ident _loc) _pos) = "SE-133"
    errorIdent (AnnotatedError (EAccessPortConnectionInvalidGlobal _ident) _pos) = "SE-134"
    errorIdent (AnnotatedError (EAccessPortConnectionInterfaceNotProvided _ident _iface) _pos) = "SE-135"
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
    errorIdent (AnnotatedError (EInboundPortConnectionMsgQueueTypeMismatch _msgQueueId _expectedTy _actualTy) _pos) = "SE-161"
    errorIdent (AnnotatedError (EOutboundPortConnectionMsgQueueTypeMismatch _msgQueueId _expectedTy _actualTy) _pos) = "SE-162"
    errorIdent (AnnotatedError (EAllocatorPortConnectionPoolTypeMismatch _poolId _expectedTy _actualTy) _pos) = "SE-163"
    errorIdent (AnnotatedError (EInvalidTaskType _ty) _pos) = "SE-164"
    errorIdent (AnnotatedError (EInvalidHandlerType _ty) _pos) = "SE-165"
    errorIdent (AnnotatedError (EInvalidResourceType _ty) _pos) = "SE-166"
    errorIdent (AnnotatedError (EInvalidEmitterType _ty) _pos) = "SE-167"
    errorIdent (AnnotatedError (EInvalidChannelType _ty) _pos) = "SE-168"
    errorIdent (AnnotatedError (EEmitterClassNotInstantiable _ident) _pos) = "SE-169"
    errorIdent (AnnotatedError (ESingleExpressionTypeNotUnit _ty) _pos) = "SE-170"
    errorIdent _ = "Internal"

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
    errorTitle (AnnotatedError (EProcedureNotFromProvidedInterfaces _def _ident) _pos) = "procedure not from provided interfaces"
    errorTitle (AnnotatedError (EMissingProcedure _ifaceId _procId) _pos) = "missing procedure"
    errorTitle (AnnotatedError (EProcedureExtraParams _def _paramNumber) _pos) = "extra parameters in procedure definition"
    errorTitle (AnnotatedError (EProcedureMissingParams _def _paramNumber) _pos) = "missing parameters in procedure definition"
    errorTitle (AnnotatedError (EProcedureParamTypeMismatch _def _ty) _pos) = "parameter type mismatch in procedure definition"
    errorTitle (AnnotatedError (ETaskClassProvides _ident) _pos) = "task class provides an interface"
    errorTitle (AnnotatedError (ETaskClassProcedure _def _ident) _pos) = "task class defines a procedure"
    errorTitle (AnnotatedError (ETaskClassNoActions _ident) _pos) = "task class does not define any actions"
    errorTitle (AnnotatedError (EHandlerClassProvides _ident) _pos) = "handler class provides an interface"
    errorTitle (AnnotatedError (EHandlerClassProcedure _def _ident) _pos) = "handler class defines a procedure"
    errorTitle (AnnotatedError (EHandlerClassNoAction _ident) _pos) = "handler class does not define any actions"
    errorTitle (AnnotatedError (EHandlerClassMultipleActions _ident _loc) _pos) = "handler class defines multiple actions"
    errorTitle (AnnotatedError (EHandlerClassNoSinkPort _ident) _pos) = "handler class does not define any sink port"
    errorTitle (AnnotatedError (EHandlerClassMultipleSinkPorts _ident _loc) _pos) = "handler class defines multiple sink ports"
    errorTitle (AnnotatedError (EHandlerClassInPort _def _ident) _pos) = "handler class defines an in port"
    errorTitle (AnnotatedError (EIfElseIfCondNotBool _ty) _pos) = "if-else-if condition not boolean"
    errorTitle (AnnotatedError (EFunctionCallExtraArgs _def _argNumber) _pos) = "extra arguments in function call"
    errorTitle (AnnotatedError (EFunctionCallMissingArgs _def _argNumber) _pos) = "missing arguments in function call"
    errorTitle (AnnotatedError (EFunctionCallArgTypeMismatch _def _argNumber _ty) _pos) = "argument type mismatch in function call"
    errorTitle (AnnotatedError (EMemberAccessNotFunction _ident) _pos) = "access to a member that is not a function"
    errorTitle (AnnotatedError EMutableReferenceToImmutable _pos) = "mutable reference to immutable object"
    errorTitle (AnnotatedError EMutableReferenceToPrivate _pos) = "mutable reference to private object"
    errorTitle (AnnotatedError (EBinOpExpectedTypeLeft _op _expectedTy _actualTy) _pos) = "binary operation expected type on the left"
    errorTitle (AnnotatedError (EBinOpExpectedTypeRight _op _expectedTy _actualTy) _pos) = "binary operation expected type on the right"
    errorTitle (AnnotatedError (EBinOpTypeMismatch _op _ty_le _ty_re) _pos) = "binary operation type mismatch"
    errorTitle (AnnotatedError (EBinOpExpectedTypeNotBool _op _ty) _pos) = "binary operation expected result type not boolean"
    errorTitle (AnnotatedError (EBinOpLeftTypeNotBool _op _ty) _pos) = "binary operation expected boolean type on the left"
    errorTitle (AnnotatedError (EBinOpRightTypeNotBool _op _ty) _pos) = "binary operation expected boolean type on the right"
    errorTitle (AnnotatedError (EBinOpExpectedTypeNotNum _op _ty) _pos) = "binary operation expected result type not numeric"
    errorTitle (AnnotatedError (EBinOpLeftTypeNotNum _op _ty) _pos) = "binary operation expected numeric type on the left"
    errorTitle (AnnotatedError (EBinOpRightTypeNotNum _op _ty) _pos) = "binary operation expected numeric type on the right"
    errorTitle (AnnotatedError (EBinOpRightTypeNotPos _op _ty) _pos) = "binary operation expected positive numeric type on the right"
    errorTitle (AnnotatedError (EBinOpLeftTypeNotEq _op _ty) _pos) = "binary operation expected equatable type on the left"
    errorTitle (AnnotatedError (EBinOpRightTypeNotEq _op _ty) _pos) = "binary operation expected equatable type on the right"
    errorTitle (AnnotatedError (EAtomicAccessInvalidType _ty) _pos) = "invalid type for the atomic access interface"
    errorTitle (AnnotatedError (EAtomicArrayAccessInvalidType _ty) _pos) = "invalid type for the atomic array access interface"
    errorTitle (AnnotatedError (EAtomicInvalidType _ty) _pos) = "invalid atomic type"
    errorTitle (AnnotatedError (EAtomicArrayInvalidType _ty) _pos) = "invalid atomic array type"
    errorTitle (AnnotatedError (EAtomicConnectionTypeMismatch _expectedTy _actualTy) _pos) = "atomic connection type mismatch"
    errorTitle (AnnotatedError (EAtomicArrayConnectionTypeMismatch _expectedTy _actualTy) _pos) = "atomic array connection type mismatch"
    errorTitle (AnnotatedError (EAtomicArrayConnectionSizeMismatch _expectedSize _actualSize) _pos) = "atomic array connection size mismatch"
    errorTitle (AnnotatedError (EConstantWithoutKnownType _c) _pos) = "constant without known type"
    errorTitle (AnnotatedError EStructInitializerInvalidUse _pos) = "invalid use of struct initializer"
    errorTitle (AnnotatedError (EStructInitializerTypeMismatch _expectedTy _actualTy) _pos) = "struct initializer type mismatch"
    errorTitle (AnnotatedError (EEnumInitializerExpectedTypeMismatch _expectedTy _actualTy) _pos) = "enum initializer expected type mismatch"
    errorTitle (AnnotatedError ESliceInvalidUse _pos) = "invalid use of slice"
    errorTitle (AnnotatedError EArrayInitializerInvalidUse _pos) = "invalid use of an array initializer"
    errorTitle (AnnotatedError (EArrayInitializerNotArray _ty) _pos) = "assignment of an array initializer to a non-array type"
    errorTitle (AnnotatedError EArrayExprListInitializerInvalidUse _pos) = "invalid use of an expression list array initializer"
    errorTitle (AnnotatedError (EArrayExprListInitializerNotArray _ty) _pos) = "assignment of an array expression list initializer to a non-array type"
    errorTitle (AnnotatedError EOptionVariantInitializerInvalidUse _pos) = "invalid use of an option variant initializer"
    errorTitle (AnnotatedError (EArrayInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "array initializer size mismatch"
    errorTitle (AnnotatedError (EArrayExprListInitializerSizeMismatch _expectedSize _initializerSize) _pos) = "array expression list initializer size mismatch"
    errorTitle (AnnotatedError (EArrayExprListInitializerExprTypeMismatch _expectedTy _actualTy) _pos) = "list of initializing expressions type mismatch"
    errorTitle (AnnotatedError (EReturnValueExpected _ty) _pos) = "expected return value"
    errorTitle (AnnotatedError EReturnValueNotUnit _pos) = "return value not expected"
    errorTitle (AnnotatedError (EInvalidArrayType _ty) _pos) = "invalid array type"
    errorTitle (AnnotatedError (EInvalidBoxType _ty) _pos) = "invalid box type"
    errorTitle (AnnotatedError (ENoTypeFound _ident) _pos) = "no type found"
    errorTitle (AnnotatedError (EGlobalNotType _def) _pos) = "global object but not a type"
    errorTitle (AnnotatedError (EInvalidAccessToGlobal _ident) _pos) = "invalid access to global object"
    errorTitle (AnnotatedError (EConstantIsReadOnly _ident) _pos) = "invalid write to a constant"
    errorTitle (AnnotatedError (ESymbolAlreadyDefined _def) _pos) = "symbol already defined"
    errorTitle (AnnotatedError EContinueInvalidExpression _pos) = "invalid expression in continue statement"
    errorTitle (AnnotatedError (EContinueInvalidMethodOrViewerCall _ident) _pos) = "invalid method or viewer call in continue statement"
    errorTitle (AnnotatedError (EContinueInvalidMemberCall _ty) _pos) = "invalid member call in continue statement"
    errorTitle (AnnotatedError (EContinueActionExtraArgs _def _argNumber) _pos) = "extra arguments in continuation action"
    errorTitle (AnnotatedError (EContinueActionMissingArgs _def) _pos) = "missing arguments in continuation action"
    errorTitle (AnnotatedError EEnumVariantInitializerInvalidUse _pos) = "invalid use of an enum variant initializer"
    errorTitle (AnnotatedError (EEnumVariantNotFound _enumId _variant) _pos) = "enum variant not found"
    errorTitle (AnnotatedError (EEnumVariantExtraParams _def _params _paramNumber) _pos) = "extra parameters in enum variant"
    errorTitle (AnnotatedError (EEnumVariantMissingParams _def _params _paramNumber) _pos) = "missing parameters in enum variant"
    errorTitle (AnnotatedError (EEnumVariantParamTypeMismatch _def _param _actualTy) _pos) = "enum variant parameter type mismatch"
    errorTitle (AnnotatedError (EFunctionNotFound _ident) _pos) = "function not found"
    errorTitle (AnnotatedError (EGlobalNotFunction _def) _pos) = "global object but not a function"
    errorTitle (AnnotatedError (EUnexpectedNumericConstant _ty) _pos) = "unexpected numeric constant"
    errorTitle (AnnotatedError (EInvalidAssignmentExprType _ty) _pos) = "invalid assignment expression type"
    errorTitle (AnnotatedError (EInvalidMessageType _ty) _pos) = "invalid message type"
    errorTitle (AnnotatedError (EInvalidOptionType _ty) _pos) = "invalid option type"
    errorTitle (AnnotatedError (EInvalidReferenceType _ty) _pos) = "invalid reference type"
    errorTitle (AnnotatedError (EInvalidFixedLocationType _ty) _pos) = "invalid fixed-location type"
    errorTitle (AnnotatedError (EInvalidAllocatorType _ty) _pos) = "invalid allocator type"
    errorTitle (AnnotatedError (EInvalidClassFieldType _ty) _pos) = "invalid class field type"
    errorTitle (AnnotatedError (EInvalidStructFieldType _ty) _pos) = "invalid struct field type"
    errorTitle (AnnotatedError (EInvalidEnumParameterType _ty) _pos) = "invalid enum parameter type"
    errorTitle (AnnotatedError (EInvalidAccessPortType _ty) _pos) = "invalid access port type"
    errorTitle (AnnotatedError (EInvalidDeclarationType _ty) _pos) = "invalid declaration type"
    errorTitle (AnnotatedError (EInvalidTypeSpecifier _ty) _pos) = "invalid type specifier"
    errorTitle (AnnotatedError (EInvalidNumericConstantType _ty) _pos) = "invalid numeric constant type"
    errorTitle (AnnotatedError (EInvalidActionParameterType _ty) _pos) = "invalid action parameter type"
    errorTitle (AnnotatedError (EInvalidProcedureParameterType _ty) _pos) = "invalid procedure parameter type"
    errorTitle (AnnotatedError (EMemberFunctionCallExtraArgs _def _argNumber) _pos) = "extra arguments in member function call"
    errorTitle (AnnotatedError (EMemberFunctionCallMissingArgs _def _argNumber) _pos) = "missing arguments in member function call"
    errorTitle (AnnotatedError (EMemberFunctionCallArgTypeMismatch _def _argNumber _actualTy) _pos) = "member function call argument type mismatch"
    errorTitle (AnnotatedError (EArrayIndexNotUSize _ty) _pos) = "invalid array index type"
    errorTitle (AnnotatedError (EArraySliceLowerBoundNotUSize _ty) _pos) = "invalid array slice lower bound type"
    errorTitle (AnnotatedError (EArraySliceUpperBoundNotUSize _ty) _pos) = "invalid array slice upper bound type"
    errorTitle (AnnotatedError (EOutboundPortSendInvalidNumArgs _argNumber) _pos) = "invalid number of arguments in outbound port send"
    errorTitle (AnnotatedError (EOutboundPortArgTypeMismatch _expectedTy _actualTy) _pos) = "output port argument type mismatch"
    errorTitle (AnnotatedError (EAssignmentExprMismatch _expectedTy _actualTy) _pos) = "assignment expression type mismatch"
    errorTitle (AnnotatedError (EFieldValueAssignmentMissingFields _def _fields) _pos) = "missing field/s in field assignment expression"
    errorTitle (AnnotatedError (EFieldValueAssignmentUnknownFields _def _fields) _pos) = "unknown field/s in field assignment expression"
    errorTitle (AnnotatedError (EFieldNotFixedLocation _fieldName _ty) _pos) = "field is not a fixed-location field"
    errorTitle (AnnotatedError (EFieldNotAccessPort _fieldName _ty) _pos) = "field is not an access port field"
    errorTitle (AnnotatedError (EFieldNotSinkOrInboundPort _fieldName _ty) _pos) = "field is not a sink or inbound port field"
    errorTitle (AnnotatedError (EFieldNotOutboundPort _fieldName _ty) _pos) = "field is not an outbound port field"
    errorTitle (AnnotatedError (EMemberAccessInvalidType _ty) _pos) = "invalid member access type"
    errorTitle (AnnotatedError (EMemberFunctionCallInvalidType _ty) _pos) = "invalid member function call type"
    errorTitle (AnnotatedError (EMemberAccessUnknownField _def _field) _pos) = "unknown field in member access"
    errorTitle (AnnotatedError EInvalidProcedureCallInsideMemberFunction _pos) = "invalid procedure call inside member function"
    errorTitle (AnnotatedError (EConstantOutRange _const) _pos) = "constant out of range"
    errorTitle (AnnotatedError (EForIteratorInvalidType _ty) _pos) = "invalid type for for-loop iterator"
    errorTitle (AnnotatedError (EUsedTypeName _ident _loc) _pos) = "type name already used"
    errorTitle (AnnotatedError (EUsedGlobalName _ident _loc) _pos) = "global name already used"
    errorTitle (AnnotatedError (EUsedFunName _ident _loc) _pos) = "function name already used"
    errorTitle (AnnotatedError (EAccessPortConnectionInvalidGlobal _ident) _pos) = "invalid global object in access port connection"
    errorTitle (AnnotatedError (EAccessPortConnectionInterfaceNotProvided _ident _iface) _pos) = "resource does not provide the interface"
    errorTitle (AnnotatedError (ESinkPortConnectionInvalidGlobal _ident) _pos) = "invalid sink port connection"
    errorTitle (AnnotatedError (EInboundPortConnectionInvalidObject _ident) _pos) = "invalid inbound port connection"
    errorTitle (AnnotatedError (EOutboundPortConnectionInvalidGlobal _ident) _pos) = "invalid outbound port connection"
    errorTitle (AnnotatedError (EAllocatorPortConnectionInvalidGlobal _ident) _pos) = "invalid allocator port connection"
    errorTitle (AnnotatedError (EAtomicAccessPortConnectionInvalidGlobal _ident) _pos) = "invalid atomic access port connection"
    errorTitle (AnnotatedError (EAtomicArrayAccessPortConnectionInvalidGlobal _ident) _pos) = "invalid atomic array access port connection"
    errorTitle (AnnotatedError (EStructDefNotUniqueField _fields) _pos) = "duplicate field in struct definition"
    errorTitle (AnnotatedError (EEnumDefNotUniqueVariant _variants) _pos) = "duplicate variant in enum definition"
    errorTitle (AnnotatedError (EInterfaceNotUniqueProcedure _procedures) _pos) = "duplicate procedure in interface definition"
    errorTitle (AnnotatedError (EClassLoop _members) _pos) = "loop between member function calls in class definition"
    errorTitle (AnnotatedError (EDereferenceInvalidType _ty) _pos) = "invalid type for dereference"
    errorTitle (AnnotatedError (EMatchInvalidType _ty) _pos) = "invalid type for match statement"
    errorTitle (AnnotatedError (EMatchCaseDuplicate _ident _loc) _pos) = "duplicate case in match statement"
    errorTitle (AnnotatedError (EMatchCaseUnknownVariants _variants) _pos) = "unknown variant/s in match case"
    errorTitle (AnnotatedError (EMatchMissingCases _variants) _pos) = "missing case/s in match statement"
    errorTitle (AnnotatedError (EIsVariantInvalidType _ty) _pos) = "invalid type for is-variant expression"
    errorTitle (AnnotatedError (EIsOptionVariantInvalidType _ty) _pos) = "invalid type for is-option-variant expression"
    errorTitle (AnnotatedError (EIsVariantEnumTypeMismatch _enum _variant) _pos) = "type mismatch in is-variant expression"
    errorTitle (AnnotatedError (EOutboundPortInvalidProcedure _ident) _pos) = "invalid procedure in outbound port"
    errorTitle (AnnotatedError EInvalidPoolInitialization _pos) = "invalid pool initialization"
    errorTitle (AnnotatedError EInvalidMsgQueueInitialization _pos) = "invalid message queue initialization"
    errorTitle (AnnotatedError (EUnknownGlobal _ident) _pos) = "unknown global object"
    errorTitle (AnnotatedError (EInvalidInterruptEmitterType _ty) _pos) = "invalid interrupt emitter type"
    errorTitle (AnnotatedError (EInvalidPeriodicTimerEmitterType _ty) _pos) = "invalid periodic timer emitter type"
    errorTitle (AnnotatedError (EInvalidSystemInitEmitterType _ty) _pos) = "invalid system init emitter type"
    errorTitle (AnnotatedError (EInboundPortConnectionMsgQueueTypeMismatch _msgQueueId _expectedTy _actualTy) _pos) = "message queue type mismatch"
    errorTitle (AnnotatedError (EOutboundPortConnectionMsgQueueTypeMismatch _msgQueueId _expectedTy _actualTy) _pos) = "message queue type mismatch"
    errorTitle (AnnotatedError (EAllocatorPortConnectionPoolTypeMismatch _poolId _expectedTy _actualTy) _pos) = "pool type mismatch"
    errorTitle (AnnotatedError (EInvalidTaskType _ty) _pos) = "invalid task type"
    errorTitle (AnnotatedError (EInvalidHandlerType _ty) _pos) = "invalid handler type"
    errorTitle (AnnotatedError (EInvalidResourceType _ty) _pos) = "invalid resource type"
    errorTitle (AnnotatedError (EInvalidEmitterType _ty) _pos) = "invalid emitter type"
    errorTitle (AnnotatedError (EInvalidChannelType _ty) _pos) = "invalid channel type"
    errorTitle (AnnotatedError (EEmitterClassNotInstantiable _ident) _pos) = "emitter class is not instantiable"
    errorTitle (AnnotatedError (ESingleExpressionTypeNotUnit _ty) _pos) = "single expression type is not unit"
    errorTitle (AnnotatedError _err _pos) = "internal error"

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
                EProcedureNotFromProvidedInterfaces (classId, Position startPosClass endPosClass) ident ->
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
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Procedure \x1b[31m" <> T.pack procId <> "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <> "\x1b[0m is not being provided."))
                EProcedureExtraParams (ifaceId, procId, params, procPos@(Position procStart _procEnd)) paramNumber ->
                    let procFileName = sourceName procStart
                        procSourceLines = files M.! procFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Procedure \x1b[31m" <> T.pack procId <>
                                "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <>
                                "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                                "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            procSourceLines "The interface of the procedure is defined here:" procFileName
                            procPos Nothing
                EProcedureMissingParams (ifaceId, procId, params, procPos@(Position procStart _procEnd)) paramNumber ->
                    let procFileName = sourceName procStart
                        procSourceLines = files M.! procFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Procedure \x1b[31m" <> T.pack procId <>
                                "\x1b[0m of interface \x1b[31m" <> T.pack ifaceId <>
                                "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                                "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            procSourceLines "The interface of the procedure is defined here:" procFileName
                            procPos Nothing
                EProcedureParamTypeMismatch (ifaceId, procId, expectedTy, procPos@(Position procStart _procEnd)) actualTy ->
                    let procFileName = sourceName procStart
                        procSourceLines = files M.! procFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Parameter is expected to be of type \x1b[31m" <> showText expectedTy <>
                                "\x1b[0m but you are defining it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            procSourceLines
                                ("The procedure \x1b[31m" <> T.pack procId <>
                                "\x1b[0m of the interface \x1b[31m" <> T.pack ifaceId <>
                                "\x1b[0m is defined here:")
                                procFileName procPos Nothing
                ETaskClassProvides ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Task class \x1b[31m" <> T.pack ident <> "\x1b[0m provides an interface.\n" <>
                            "Task classes must not provide any interface."))
                ETaskClassProcedure (classId, Position startPosClass endPosClass) ident ->
                    let procStartLine = sourceLine start
                        procEndLine = sourceLine end
                        procStartColumn = sourceColumn start
                        procEndColumn = 
                            if procStartLine == procEndLine then 
                                sourceColumn end 
                            else 
                                T.length (T.lines sourceLines !! (procStartLine - 1)) + 1
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
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Task class \x1b[31m" <> T.pack ident <> "\x1b[0m does not define any actions.\n" <>
                            "Task classes must define at least one action."))
                EHandlerClassProvides ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Handler class \x1b[31m" <> T.pack ident <> "\x1b[0m provides an interface.\n" <>
                            "Handler classes must not provide any interface."))
                EHandlerClassProcedure (classId, Position startPosClass endPosClass) ident ->
                    let procStartLine = sourceLine start
                        procEndLine = sourceLine end
                        procStartColumn = sourceColumn start
                        procEndColumn = 
                            if procStartLine == procEndLine then 
                                sourceColumn end 
                            else 
                                T.length (T.lines sourceLines !! (procStartLine - 1)) + 1
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
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Handler class \x1b[31m" <> T.pack ident <> "\x1b[0m does not define any actions.\n" <>
                            "Handler classes must define exactly one action."))
                EHandlerClassMultipleActions classId prevActPos@(Position actStartPos _actEndPos) ->
                    let actFileName = sourceName actStartPos
                        actSourceLines = files M.! actFileName 
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines multiple actions.\n")) <>
                        pprintSimpleError
                            actSourceLines "Another action is defined here:" actFileName
                            prevActPos Nothing
                EHandlerClassNoSinkPort classId ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m does not define any sink port.\n" <>
                            "Handler classes must define exactly one sink port."))
                EHandlerClassMultipleSinkPorts classId prevPortPos@(Position portStartPos _portEndPos) ->
                    let portFileName = sourceName portStartPos
                        portSourceLines = files M.! portFileName 
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines multiple sink ports.\n")) <>
                        pprintSimpleError
                            portSourceLines "Another sink port is defined here:" portFileName
                            prevPortPos Nothing
                EHandlerClassInPort (classId, Position startPosClass endPosClass) ident ->
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
                                        ("Handler class \x1b[31m" <> T.pack classId <> "\x1b[0m defines the in port \x1b[31m" <> T.pack ident <> "\x1b[0m.\n"
                                        <> "Handler classes cannot define in ports."))
                            ]
                EIfElseIfCondNotBool ts ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The condition in the statement is expected to be of type \x1b[31mbool\x1b[0m but it is of type \x1b[31m" <> showText ts <> "\x1b[0m."))
                EFunctionCallExtraArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
                    let funcFileName = sourceName funcStart
                        funcSourceLines = files M.! funcFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Function \x1b[31m" <> T.pack funcId <>
                                "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                                "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                            funcPos Nothing
                EFunctionCallMissingArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
                    let funcFileName = sourceName funcStart
                        funcSourceLines = files M.! funcFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Function \x1b[31m" <> T.pack funcId <>
                                "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                                "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                            funcPos Nothing
                EFunctionCallArgTypeMismatch (funcId, expectedTy, funcPos@(Position funcStart _procEnd)) argNumber actualTy ->
                    let funcFileName = sourceName funcStart
                        funcSourceLines = files M.! funcFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Argument \x1b[31m#" <> T.pack (show argNumber) <> "\x1b[0m of function \x1b[31m" <> T.pack funcId <>
                                "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                                "\x1b[0m but you are providing it of type \x1b[31m" <> showText actualTy <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            funcSourceLines ("Function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                            funcPos Nothing
                EMemberAccessNotFunction ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The identifier \x1b[31m" <> T.pack ident <> "\x1b[0m is not a valid member function."))
                EMutableReferenceToImmutable ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "You are trying to create a mutable reference to an immutable object.")
                EMutableReferenceToPrivate ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "You are trying to create a mutable reference to a private object.")
                EBinOpExpectedTypeLeft op expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The result of the binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                            "\x1b[0m but the left operand you are providing is of type \x1b[31m" <>
                            showText actualTy <> "\x1b[0m."))
                EBinOpExpectedTypeRight op expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The result of the binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                            "\x1b[0m but the right operand you are providing is of type \x1b[31m" <>
                            showText actualTy <> "\x1b[0m."))
                EBinOpTypeMismatch op ty_le ty_re ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m expects operands of the same type but the left one is of type \x1b[31m" <>
                            showText ty_le <> "\x1b[0m and the right one is of type \x1b[31m" <> showText ty_re <> "\x1b[0m."))
                EBinOpExpectedTypeNotBool op ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m will result in a value of type \x1b[31m" <> showText TBool <>
                            "\x1b[0m but it is expected to be of type \x1b[31m" <> showText ty <> "\x1b[0m."))
                EBinOpLeftTypeNotBool op ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m is of type \x1b[31m" <> showText ty <>
                            "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TBool <> "\x1b[0m."))
                EBinOpRightTypeNotBool op ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m is of type \x1b[31m" <> showText ty <>
                            "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TBool <> "\x1b[0m."))
                EBinOpExpectedTypeNotNum op ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m will result in a numeric value but the expected type is \x1b[31m" <> showText ty <> "\x1b[0m."))
                EBinOpLeftTypeNotNum op ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m is of type \x1b[31m" <> showText ty <>
                            "\x1b[0m but it is expected to be of numeric type."))
                EBinOpRightTypeNotNum op ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m is of type \x1b[31m" <> showText ty <>
                            "\x1b[0m but it is expected to be of numeric type."))
                EBinOpRightTypeNotPos op ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m is of type \x1b[31m" <> showText ty <>
                            "\x1b[0m but it is expected to be of positive numeric type."))
                EBinOpLeftTypeNotEq op ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The left operand of the binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m is of type \x1b[31m" <> showText ty <>
                            "\x1b[0m but it is expected to be of equatable type."))
                EBinOpRightTypeNotEq op ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The right operand of the binary operation \x1b[31m" <> showText op <>
                            "\x1b[0m is of type \x1b[31m" <> showText ty <>
                            "\x1b[0m but it is expected to be of equatable type."))
                EAtomicAccessInvalidType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic access, only numeric types are allowed."))
                EAtomicArrayAccessInvalidType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array access, only numeric types are allowed."))
                EAtomicInvalidType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic."))
                EAtomicArrayInvalidType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not valid for atomic array."))
                EAtomicConnectionTypeMismatch expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type of the connected atomic resource is expected to be \x1b[31m" <> showText expectedTy <>
                            "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
                EAtomicArrayConnectionTypeMismatch expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type of the elements of the connected atomic array is expected to be \x1b[31m" <> showText expectedTy <>
                            "\x1b[0m but the array is of elements of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
                EAtomicArrayConnectionSizeMismatch expectedSize actualSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the connected atomic array is expected to be \x1b[31m" <> showText expectedSize <>
                            "\x1b[0m but the array has size \x1b[31m" <> showText actualSize <> "\x1b[0m."))
                EConstantWithoutKnownType c ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type of the constant \x1b[31m" <> showText c <>
                            "\x1b[0m cannot be inferred from the environment and must be explicitly defined."))
                EStructInitializerInvalidUse ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just $ "You are trying to use a struct initializer in an invalid context.\n" <>
                                "Struct initializers can only be used to initialize struct objects.")
                EStructInitializerTypeMismatch expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The struct initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                            "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
                EEnumInitializerExpectedTypeMismatch expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The enum initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                            "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
                ESliceInvalidUse ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just $ "You are trying to use a slice in an invalid context.\n" <>
                                "Slices can only be used to create references to a part of an array.")
                EArrayInitializerInvalidUse ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just $ "You are trying to use an array initializer in an invalid context.\n" <>
                                "Array initializers can only be used to initialize array objects.")
                EArrayInitializerNotArray ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid use of an array initializer.\n" <>
                            "You are trying to assign an array initializer to an object of type \x1b[31m" <>
                            showText ty <> "\x1b[0m."))
                EArrayExprListInitializerInvalidUse ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just $ "You are trying to use an array expression list initializer in an invalid context.\n" <>
                                "TArray expression list initializers can only be used to initialize array objects.")
                EArrayExprListInitializerNotArray ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Invalid use of an array expression list initializer.\n" <>
                            "You are trying to assign an array expression list initializer to an object of type \x1b[31m" <>
                            showText ty <> "\x1b[0m."))
                EOptionVariantInitializerInvalidUse ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just $ "You are trying to use an option variant initializer in an invalid context.\n" <>
                                "Option variant initializers can only be used to initialize option objects.")
                EArrayInitializerSizeMismatch expectedSize initializerSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the array initializer is \x1b[31m" <> showText initializerSize <>
                            "\x1b[0m but the expected size is \x1b[31m" <> showText expectedSize <> "\x1b[0m."))
                EArrayExprListInitializerSizeMismatch expectedSize initializerSize ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The size of the array expression list initializer is \x1b[31m" <> T.pack (show initializerSize) <>
                            "\x1b[0m but the expected size is \x1b[31m" <> T.pack (show expectedSize) <> "\x1b[0m."))
                EArrayExprListInitializerExprTypeMismatch expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The expression in the array expression list initializer is expected to be of type \x1b[31m" <> showText expectedTy <>
                            "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
                EReturnValueExpected ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The function is expected to return a value of type \x1b[31m" <> showText ty <> "\x1b[0m."))
                EReturnValueNotUnit ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "The function is not expected to return a value.")
                EInvalidArrayType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid array type."))
                EInvalidBoxType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid box type."))
                ENoTypeFound ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> T.pack ident <> "\x1b[0m is not found."))
                EGlobalNotType (ident, globalPos@(Position globalStart _)) ->
                    let globalFileName = sourceName globalStart
                        globalSourceLines = files M.! globalFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a type.\n")) <>
                        pprintSimpleError
                            globalSourceLines "The global object is defined here:" globalFileName
                            globalPos Nothing
                EInvalidAccessToGlobal ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be accessed from within this context."))
                EConstantIsReadOnly ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The constant \x1b[31m" <> T.pack ident <> "\x1b[0m is read-only and cannot be modified."))
                ESymbolAlreadyDefined (ident, symbolPos@(Position symbolStart _symbolEnd)) ->
                    let symbolFileName = sourceName symbolStart
                        symbolSourceLines = files M.! symbolFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("The symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already defined.\n")) <>
                        pprintSimpleError
                            symbolSourceLines "The symbol was previoulsy defined here:" symbolFileName
                            symbolPos Nothing
                EContinueInvalidExpression -> 
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "The expression in a continue statement must be a call to a member action.")
                EContinueInvalidMethodOrViewerCall ident -> 
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("This statement can only be used to call a continuation action.\n" <>
                            "The member function call \x1b[31m" <> T.pack ident <> "\x1b[0m in a continue statement is invalid."))
                EContinueInvalidMemberCall ts ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("This statement can only be used to call a continuation action.\n" <>
                            "Calling a procedure of an object of type \x1b[31m" <> showText ts <> "\x1b[0m in a continue statement is invalid."))
                EContinueActionExtraArgs (ident, params, actionPos@(Position actStartPos _endPos)) argNumber ->
                    let actFileName = sourceName actStartPos
                        actSourceLines = files M.! actFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Action \x1b[31m" <> T.pack ident <>
                                "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                                "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            actSourceLines "The action is defined here:" actFileName
                            actionPos Nothing
                EContinueActionMissingArgs (ident, actionPos@(Position actStartPos _endPos)) ->
                    let actFileName = sourceName actStartPos
                        actSourceLines = files M.! actFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Action \x1b[31m" <> T.pack ident <>
                                "\x1b[0m requires \x1b[31mone\x1b[0m parameter but you are providing \x1b[31mnone\x1b[0m.\n")) <>
                        pprintSimpleError
                            actSourceLines "The action is defined here:" actFileName
                            actionPos Nothing
                EEnumVariantInitializerInvalidUse ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just $ "You are trying to use an enum variant initializer in an invalid context.\n" <>
                                "Enum variant initializers can only be used to initialize enum objects.")
                EEnumVariantNotFound enumId variant ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Enum \x1b[31m" <> T.pack enumId <> "\x1b[0m does not have a variant named \x1b[31m" <> T.pack variant <> "\x1b[0m."))
                EEnumVariantExtraParams (enumId, enumPos@(Position enumStart _end)) (variant, params) paramNumber ->
                    let enumFileName = sourceName enumStart
                        enumSourceLines = files M.! enumFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Enum variant \x1b[31m" <> T.pack variant <>
                                "\x1b[0m of enum \x1b[31m" <> T.pack enumId <>
                                "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                                "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            enumSourceLines "The enum is defined here:" enumFileName
                            enumPos Nothing
                EEnumVariantMissingParams (enumId, enumPos@(Position enumStart _end)) (variant, params) paramNumber ->
                    let enumFileName = sourceName enumStart
                        enumSourceLines = files M.! enumFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Enum variant \x1b[31m" <> T.pack variant <>
                                "\x1b[0m of enum \x1b[31m" <> T.pack enumId <>
                                "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                                "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show paramNumber) <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            enumSourceLines "The enum is defined here:" enumFileName
                            enumPos Nothing
                EEnumVariantParamTypeMismatch (enumId, enumPos@(Position enumStart _end)) (variant, paramNumber, expectedTy) actualTy ->
                    let enumFileName = sourceName enumStart
                        enumSourceLines = files M.! enumFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Parameter \x1b[31m" <> T.pack (show paramNumber) <>
                                "\x1b[0m of enum variant \x1b[31m" <> T.pack variant <>
                                "\x1b[0m of enum \x1b[31m" <> T.pack enumId <>
                                "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                                "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            enumSourceLines "The enum is defined here:" enumFileName
                            enumPos Nothing
                EFunctionNotFound ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Function \x1b[31m" <> T.pack ident <> "\x1b[0m not found."))
                EGlobalNotFunction (ident, globalPos@(Position globalStart _)) ->
                    let globalFileName = sourceName globalStart
                        globalSourceLines = files M.! globalFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not a function.\n")) <>
                        pprintSimpleError
                            globalSourceLines "The global object is defined here:" globalFileName
                            globalPos Nothing
                EUnexpectedNumericConstant ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Expected a value of type \x1b[31m" <> showText ty <> "\x1b[0m but found a numeric constant."))
                EInvalidAssignmentExprType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Objects of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be copied."))
                EInvalidMessageType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid message type."))
                EInvalidOptionType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid option type."))
                EInvalidReferenceType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("References to objects of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be created."))
                EInvalidFixedLocationType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Fixed-location fields of type \x1b[31m" <> showText ty <> "\x1b[0m cannot be defined."))
                EInvalidAllocatorType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid allocator type."))
                EInvalidClassFieldType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid class field type."))
                EInvalidStructFieldType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid struct field type."))
                EInvalidEnumParameterType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for an enum variant."))
                EInvalidAccessPortType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid access port type."))
                EInvalidDeclarationType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid object declaration type."))
                EInvalidTypeSpecifier ts ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type specifier \x1b[31m" <> showText ts <> "\x1b[0m is not valid."))
                EInvalidNumericConstantType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The expected type of this expression is \x1b[31m" <> showText ty <> "\x1b[0m but it is a numeric constant."))
                EInvalidActionParameterType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for an action."))
                EInvalidProcedureParameterType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid parameter type for a procedure."))
                EMemberFunctionCallExtraArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
                    let funcFileName = sourceName funcStart
                        funcSourceLines = files M.! funcFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Member function \x1b[31m" <> T.pack funcId <>
                                "\x1b[0m has only \x1b[31m" <> T.pack (show (length params)) <>
                                "\x1b[0m parameters but you are providing \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            funcSourceLines ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                            funcPos Nothing
                EMemberFunctionCallMissingArgs (funcId, params, funcPos@(Position funcStart _procEnd)) argNumber ->
                    let funcFileName = sourceName funcStart
                        funcSourceLines = files M.! funcFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Member function \x1b[31m" <> T.pack funcId <>
                                "\x1b[0m has \x1b[31m" <> T.pack (show (length params)) <>
                                "\x1b[0m parameters but you are providing only \x1b[31m" <> T.pack (show argNumber) <> "\x1b[0m.")) <>
                        pprintSimpleError
                            funcSourceLines ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                            funcPos Nothing
                EMemberFunctionCallArgTypeMismatch (funcId, expectedTy, funcPos@(Position funcStart _procEnd)) argNumber actualTy ->
                    let funcFileName = sourceName funcStart
                        funcSourceLines = files M.! funcFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Argument \x1b[31m#" <> T.pack (show argNumber) <>
                                "\x1b[0m of member function \x1b[31m" <> T.pack funcId <>
                                "\x1b[0m is expected to be of type \x1b[31m" <> showText expectedTy <>
                                "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            funcSourceLines ("Member function \x1b[31m" <> T.pack funcId <> "\x1b[0m is defined here:") funcFileName
                            funcPos Nothing
                EArrayIndexNotUSize ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type of the array index is \x1b[31m" <> showText ty <>
                        "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TUSize <> "\x1b[0m."))
                EArraySliceLowerBoundNotUSize ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type of the lower bound of the array slice is \x1b[31m" <> showText ty <>
                        "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TUSize <> "\x1b[0m."))
                EArraySliceUpperBoundNotUSize ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type of the upper bound of the array slice is \x1b[31m" <> showText ty <>
                        "\x1b[0m but it is expected to be of type \x1b[31m" <> showText TUSize <> "\x1b[0m."))
                EOutboundPortSendInvalidNumArgs argNumber ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The send procedure of an outbound port expects \x1b[31mone\x1b[0m argument but you are providing \x1b[31m" <>
                            T.pack (show argNumber) <> "\x1b[0m."))
                EOutboundPortArgTypeMismatch expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The output data is expected to be of type \x1b[31m" <> showText expectedTy <>
                            "\x1b[0m but you are sending data of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
                EAssignmentExprMismatch expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The expected type of the assignment is \x1b[31m" <> showText expectedTy <>
                            "\x1b[0m but it is of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
                EFieldValueAssignmentMissingFields (record, recordPos) [field] ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Field \x1b[31m" <> T.pack field <>
                            "\x1b[0m is not being assigned a value in the field assignment expression.")) <>
                    case recordPos of
                        Position recordStart _end ->
                            let recordFileName = sourceName recordStart
                                recordSourceLines = files M.! recordFileName
                            in
                            pprintSimpleError
                                recordSourceLines ("\nThe type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") recordFileName
                                recordPos Nothing
                        _ -> ""
                EFieldValueAssignmentMissingFields (record, recordPos) fields ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fields) <>
                            "\x1b[0m are not being assigned a value in the field assignment expression.")) <>
                    case recordPos of
                        Position recordStart _end ->
                            let recordFileName = sourceName recordStart
                                recordSourceLines = files M.! recordFileName
                            in
                            pprintSimpleError
                                recordSourceLines ("\nThe type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") recordFileName
                                recordPos Nothing
                        _ -> ""
                EFieldValueAssignmentUnknownFields (record, recordPos) [field] ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Field \x1b[31m" <> T.pack field <>
                            "\x1b[0m is not a field of the type \x1b[31m" <> showText record <> "\x1b[0m.")) <>
                    case recordPos of
                        Position recordStart _end ->
                            let recordFileName = sourceName recordStart
                                recordSourceLines = files M.! recordFileName
                            in
                            pprintSimpleError
                                recordSourceLines ("\nThe type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") recordFileName
                                recordPos Nothing
                        _ -> ""
                EFieldValueAssignmentUnknownFields (record, recordPos) fields ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fields) <>
                            "\x1b[0m are not fields of the type \x1b[31m" <> showText record <> "\x1b[0m.")) <>
                    case recordPos of
                        Position recordStart _end ->
                            let recordFileName = sourceName recordStart
                                recordSourceLines = files M.! recordFileName
                            in
                            pprintSimpleError
                                recordSourceLines ("\nThe type \x1b[31m" <> showText record <> "\x1b[0m is defined here:") recordFileName
                                recordPos Nothing
                        _ -> ""
                EFieldNotFixedLocation fieldName ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Field \x1b[31m" <> T.pack fieldName <>
                            "\x1b[0m of type \x1b[31m" <> showText ty <>
                            "\x1b[0m is not a fixed-location field."))
                EFieldNotAccessPort fieldName ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Field \x1b[31m" <> T.pack fieldName <>
                            "\x1b[0m of type \x1b[31m" <> showText ty <>
                            "\x1b[0m is not an access port field."))
                EFieldNotSinkOrInboundPort fieldName ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Field \x1b[31m" <> T.pack fieldName <>
                            "\x1b[0m of type \x1b[31m" <> showText ty <>
                            "\x1b[0m is not a sink or inbound port field."))
                EFieldNotOutboundPort fieldName ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Field \x1b[31m" <> T.pack fieldName <>
                            "\x1b[0m of type \x1b[31m" <> showText ty <>
                            "\x1b[0m is not an outbound port field."))
                EMemberAccessInvalidType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for member access."))
                EMemberFunctionCallInvalidType ty -> 
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for member function call."))
                EMemberAccessUnknownField (recordId, recordPos@(Position recordStart _end)) field ->
                    let recordFileName = sourceName recordStart
                        recordSourceLines = files M.! recordFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Field \x1b[31m" <> T.pack field <>
                                "\x1b[0m is not a field of the type \x1b[31m" <> T.pack recordId <> "\x1b[0m.\n")) <>
                        pprintSimpleError
                            recordSourceLines ("The type \x1b[31m" <> T.pack recordId <> "\x1b[0m is defined here:") recordFileName
                            recordPos Nothing
                EInvalidProcedureCallInsideMemberFunction -> 
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "Procedure calls are not allowed inside member functions.")
                EConstantOutRange ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The constant value \x1b[31m" <> showText ty <> "\x1b[0m is out of range for its type."))
                EForIteratorInvalidType ty -> 
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for a for-loop iterator."))
                EUsedTypeName ident prevPos@(Position prevStart _) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("The type cannot be defined because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.\n")) <>
                        pprintSimpleError
                            prevSourceLines "The symbol is previously used here:" prevFileName
                            prevPos Nothing
                EUsedGlobalName ident prevPos@(Position prevStart _) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("The global object cannot be declared because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.\n")) <>
                        pprintSimpleError
                            prevSourceLines "The symbol is previously used here:" prevFileName
                            prevPos Nothing
                EUsedFunName ident prevPos@(Position prevStart _) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("The function cannot be declared because the symbol \x1b[31m" <> T.pack ident <> "\x1b[0m is already in use.\n")) <>
                        pprintSimpleError
                            prevSourceLines "The symbol is previously used here:" prevFileName
                            prevPos Nothing
                EAccessPortConnectionInvalidGlobal ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be used in an access port connection."))
                EAccessPortConnectionInterfaceNotProvided ident iface ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Resource \x1b[31m" <> T.pack ident <>
                            "\x1b[0m does not provide the interface \x1b[31m" <> T.pack iface <> "\x1b[0m."))
                ESinkPortConnectionInvalidGlobal ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to a sink port."))
                EInboundPortConnectionInvalidObject ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an inbound port."))
                EOutboundPortConnectionInvalidGlobal ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an outbound port."))
                EAllocatorPortConnectionInvalidGlobal ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an allocator port."))
                EAtomicAccessPortConnectionInvalidGlobal ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an atomic access port."))
                EAtomicArrayAccessPortConnectionInvalidGlobal ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The global object \x1b[31m" <> T.pack ident <> "\x1b[0m cannot be connected to an atomic array access port."))
                EStructDefNotUniqueField [fieldName] ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Field \x1b[31m" <> T.pack fieldName <> "\x1b[0m is duplicated in the struct definition."))
                EStructDefNotUniqueField fieldNames ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Fields \x1b[31m" <> T.intercalate ", " (map T.pack fieldNames) <>
                            "\x1b[0m are duplicated in the struct definition."))
                EEnumDefNotUniqueVariant [variantName] ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is duplicated in the enum definition."))
                EEnumDefNotUniqueVariant variantNames ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Variants \x1b[31m" <> T.intercalate ", " (map T.pack variantNames) <>
                            "\x1b[0m are duplicated in the enum definition."))
                EInterfaceNotUniqueProcedure [procName] ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Procedure \x1b[31m" <> T.pack procName <> "\x1b[0m is duplicated in the interface definition."))
                EInterfaceNotUniqueProcedure procNames ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Procedures \x1b[31m" <> T.intercalate ", " (map T.pack procNames) <>
                            "\x1b[0m are duplicated in the interface definition."))
                EClassLoop members ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The member functions \x1b[31m" <> T.intercalate " -> " (map T.pack members) <>
                            "\x1b[0m form a recursive calling loop in the class definition."))
                EDereferenceInvalidType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m cannot be dereferenced."))
                EMatchInvalidType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for match statement."))
                EMatchCaseDuplicate variantName prevCase@(Position prevStart _) ->
                    let prevFileName = sourceName prevStart
                        prevSourceLines = files M.! prevFileName
                    in
                        pprintSimpleError
                            sourceLines title fileName pos
                            (Just ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is duplicated in the match statement.\n")) <>
                        pprintSimpleError
                            prevSourceLines "The variant is previously used here:" prevFileName
                            prevCase Nothing
                EMatchCaseUnknownVariants [variantName] ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Variant \x1b[31m" <> T.pack variantName <> "\x1b[0m is not a valid variant of the enum or option."))
                EMatchCaseUnknownVariants variantNames ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Variants \x1b[31m" <> T.intercalate ", " (map T.pack variantNames) <>
                            "\x1b[0m are not valid variants of the enum or option."))
                EMatchMissingCases [caseIdent] -> 
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Case \x1b[31m" <> T.pack caseIdent <> "\x1b[0m is missing in the match statement."))
                EMatchMissingCases caseIdents ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Cases \x1b[31m" <> T.intercalate ", " (map T.pack caseIdents) <>
                            "\x1b[0m are missing in the match statement."))
                EIsVariantInvalidType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid type for is-variant expression."))
                EIsOptionVariantInvalidType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not an option type."))
                EIsVariantEnumTypeMismatch expectedEnum actualEnum ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The expected enum type is \x1b[31m" <> T.pack expectedEnum <>
                            "\x1b[0m but the actual type is \x1b[31m" <> T.pack actualEnum <> "\x1b[0m."))
                EOutboundPortInvalidProcedure ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The procedure \x1b[31m" <> T.pack ident <> "\x1b[0m is not a valid procedure for an outbound port."))
                EInvalidPoolInitialization -> 
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "A pool object cannot be initialized with a value.")
                EInvalidMsgQueueInitialization -> 
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just "A message queue object cannot be initialized with a value.")
                EUnknownGlobal ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Global object \x1b[31m" <> T.pack ident <> "\x1b[0m is not defined."))
                EInvalidInterruptEmitterType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Interrupts emit data of type \x1b[31m" <> showText TUInt32 <> 
                            "\x1b[0m but you are expecting data of type \x1b[31m" <> showText ty <> "\x1b[0m."))
                EInvalidPeriodicTimerEmitterType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Periodic timers emit data of type \x1b[31m" <> showText (TStruct "TimeVal") <> 
                            "\x1b[0m but you are expecting data of type \x1b[31m" <> showText ty <> "\x1b[0m."))
                EInvalidSystemInitEmitterType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("System init emitters emit data of type \x1b[31m" <> showText (TStruct "TimeVal") <> 
                            "\x1b[0m but you are expecting data of type \x1b[31m" <> showText ty <> "\x1b[0m."))
                EInboundPortConnectionMsgQueueTypeMismatch msgQueueId expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The message queue \x1b[31m" <> T.pack msgQueueId <> 
                            "\x1b[0m exchanges data messages of type \x1b[31m" <> showText expectedTy <> 
                            "\x1b[0m but you are expecting data of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
                EOutboundPortConnectionMsgQueueTypeMismatch msgQueueId expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The message queue \x1b[31m" <> T.pack msgQueueId <> 
                            "\x1b[0m exchanges data messages of type \x1b[31m" <> showText expectedTy <> 
                            "\x1b[0m but you are sending data of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
                EAllocatorPortConnectionPoolTypeMismatch poolId expectedTy actualTy ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The pool \x1b[31m" <> T.pack poolId <> 
                            "\x1b[0m serves data of type \x1b[31m" <> showText expectedTy <> 
                            "\x1b[0m but you are expecting data of type \x1b[31m" <> showText actualTy <> "\x1b[0m."))
                EInvalidTaskType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid task type."))
                EInvalidHandlerType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid handler type."))
                EInvalidResourceType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid resource type."))
                EInvalidEmitterType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid emitter type."))
                EInvalidChannelType ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("The type \x1b[31m" <> showText ty <> "\x1b[0m is not a valid channel type."))
                EEmitterClassNotInstantiable ident ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Applications cannot instantiate event emitters of class \x1b[31m" <> T.pack ident <> "\x1b[0m."))
                ESingleExpressionTypeNotUnit ty ->
                    pprintSimpleError
                        sourceLines title fileName pos
                        (Just ("Expressions used in single-expression statements must have type \x1b[31m" <> showText TUnit <> 
                            "\x1b[0m but the expression has type \x1b[31m" <> showText ty <> "\x1b[0m. Return values of functions cannot be ignored."))
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