{-# LANGUAGE FlexibleInstances #-}
-- | Module Encapsulating Semantic Errors

module Semantic.Errors.Errors where

-- Termina AST
import Semantic.AST
import Utils.Annotations

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

instance Annotated (AnnotatedError Error) where
  getAnnotation (AnnotatedError _err ann) = ann

  updateAnnotation (AnnotatedError err _) = AnnotatedError err