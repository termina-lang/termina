{-# LANGUAGE FlexibleInstances #-}
-- | Module Encapsulating Semantic Errors

module Semantic.Errors.Errors where

-- Termina AST
import Semantic.AST
import Semantic.Types
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
  | EUnboxingIntConst -- ^ Error when unboxing an integer constant (Internal)
  | EExpectedArrayTy TerminaType -- ^ Expected a valid type for the elements of an array (Internal)
  | EExpectedCopyType TerminaType -- ^ Expected a copiable type (Internal)
  | EExpectedNumType TerminaType -- ^ Expected a numeric type (Internal)
  | ENotNamedGlobal Identifier -- ^ Global object not found (Internal)
  | EInvalidObjectDeclaration Identifier -- ^ Invalid object declaration (Internal)
  | EMalformedSlice -- ^ Malformed slice (Internal)
  | EMalformedClassTyping -- ^ Malformed class typing (Internal)
  | EExpressionNotConstant -- ^ Expression not constant (Internal)
  | EContinueActionNotFound -- ^ Action not found in continue statement (Internal)
  | EMissingIdentifier -- ^ Missing identifier (Internal)
  | EMatchCaseInternalError -- ^ Internal error in match case (Internal)
  | EInvalidArrayIndexing TerminaType -- ^ Invalid array indexing (SE-001)
  | ENotNamedObject Identifier -- ^ Object not found (SE-002)
  | ENotConstant Identifier -- ^ Invalid use of a non-constant object (SE-003)
  | EAssignmentToImmutable -- ^ Assignment to immutable variable (SE-004)
  | EIfElseNoOtherwise -- ^ Missing else clause (SE-005)
  | ENotCasteable TerminaType TerminaType -- ^ Casting error (SE-006)
  | EInvalidParameterType Parameter -- ^ Invalid parameter type (SE-007)
  | EInvalidReturnType TerminaType -- ^ Invalid return type (SE-008)
  | EProcedureCallExtraParams (Identifier, [TerminaType], Location) Integer -- ^ Extra parameters in procedure call (SE-009)
  | EProcedureCallMissingParams (Identifier, [TerminaType], Location) Integer -- ^ Missing parameters in procedure call (SE-010)
  | EProcedureCallParamTypeMismatch (Identifier, TerminaType, Location) Integer TerminaType -- ^ Parameter type mismatch in procedure call (SE-011)
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
  | EFunctionCallExtraParams (Identifier, [TerminaType], Location) Integer -- ^ Extra parameters in function call (SE-035)
  | EFunctionCallMissingParams (Identifier, [TerminaType], Location) Integer -- ^ Missing parameters in function call (SE-036)
  | EFunctionCallParamTypeMismatch (Identifier, TerminaType, Location) Integer TerminaType -- ^ Parameter type mismatch in function call (SE-037)
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
  | EContinueActionExtraParams (Identifier, [TerminaType], Location) Integer -- ^ Extra parameters in action call in continue statement (SE-085)
  | EContinueActionMissingParam (Identifier, Location) -- ^ Missing parameters in action call in continue statement (SE-086)
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
  | EMemberMethodExtraParams
  | EMemberMethodMissingParams
  | EMemberFunctionCallParamTypeMismatch (Identifier, TerminaType, Location) Integer TerminaType -- ^ Parameter type mismatch in member function call (SE-110)
  | EArrayIndexNotUSize TerminaType -- ^ Invalid array index type (SE-111)
  | EArraySliceLowerBoundNotUSize TerminaType -- ^ Invalid array slice lower bound type (SE-112)
  | EArraySliceUpperBoundNotUSize TerminaType -- ^ Invalid array slice upper bound type (SE-113)
  | EOutputPortParamTypeMismatch TerminaType TerminaType -- ^ Parameter type mismatch in output port (SE-114)
  | EAssignmentExprMismatch TerminaType TerminaType -- ^ Assignment expression type mismatch (SE-115)
  | EFieldMissing (Identifier, Location) [Identifier] -- ^ Missing field/s in field assignment expression (SE-116)
  | EFieldExtra (Identifier, Location) [Identifier] -- ^ Extra field/s in field assignment expression (SE-117)
  | EFieldNotFixedLocation Identifier TerminaType -- ^ Field is not a fixed-location (SE-118)
  | EFieldNotAccessPort Identifier TerminaType -- ^ Field is not an access port (SE-119)
  | EFieldNotSinkOrInPort Identifier TerminaType -- ^ Field is not a sink or in port (SE-120)
  | EFieldNotOutPort Identifier TerminaType -- ^ Field is not an out port (SE-121)
  | EMemberAccessInvalidType TerminaType -- ^ Invalid member access type (SE-122)
  | EMemberFunctionAccessInvalidType TerminaType -- ^ Invalid member function access type (SE-123)
  | EMemberAccessUnknownField (Identifier, Location) Identifier -- ^ Unknown field in member access (SE-124)
  | EMemberAccessInvalidProcedureCall Identifier -- ^ Invalid procedure call inside member function (SE-125)
  | EConstantOutRange Const -- ^ Numeric constant out of range (SE-126)
  | EForIteratorInvalidType TerminaType -- ^ Invalid for iterator type (SE-127)
  | EUsedTypeName Identifier Location -- ^ Type name already used (SE-128)
  | EUsedGlobalName Identifier Location -- ^ Global object name already used (SE-129)
  | EUsedFunName Identifier Location -- ^ Function name already used (SE-130)
  | EAccessPortFieldInvalidType TerminaType -- ^ Invalid access port field type (SE-131)
  | EAccessPortConnectionInvalidGlobal Identifier -- ^ Invalid access port connection (SE-132)
  | ESinkPortConnectionInvalidGlobal Identifier -- ^ Invalid sink port connection (SE-133)
  | EInboundPortConnectionInvalidObject Identifier -- ^ Invalid inbound port connection (SE-134)
  | EOutboundPortConnectionInvalidGlobal Identifier -- ^ Invalid outbound port connection (SE-135)
  | EAllocatorPortConnectionInvalidGlobal Identifier -- ^ Invalid allocator port connection (SE-136)
  | EAtomicAccessPortConnectionInvalidGlobal Identifier -- ^ Invalid atomic access port connection (SE-137)
  | EAtomicArrayAccessPortConnectionInvalidGlobal Identifier -- ^ Invalid atomic array access port connection (SE-138)
  | EStructDefNotUniqueField [Identifier] -- ^ Repeated field in struct definition (SE-139)
  | EStructDefEmpty Identifier -- ^ Empty struct definition (SE-140)
  | EEnumDefEmpty Identifier -- ^ Empty enum definition (SE-141)
  | EEnumDefNotUniqueVariant [Identifier] -- ^ Repeated variant in enum definition (SE-142)
  | EInterfaceEmpty Identifier -- ^ Empty interface definition (SE-143)
  | EInterfaceNotUniqueProcedure [Identifier] -- ^ Repeated procedure in interface definition (SE-144)
  -- | Class Definition
  | EClassLoop [Identifier] -- Detected loop between procs, method and viewers
  | EDereferenceInvalidType TerminaType
  -- Match
  | EMatchInvalidType TerminaType
  | EMatchOptionBadArgs
  | EMatchOptionBad
  | EMatchCaseBadName Identifier Identifier
  | EMatchExtraCases
  -- Enum variant expressions
  | ETyNotEnumFound Identifier
  | ETyNotEnum Identifier (SemanTypeDef Location)
  | EIsVariantNotEnum TerminaType
  | EIsVariantNotOption TerminaType
  | EIsVariantEnumMismatch Identifier Identifier
  | EIsVariantNotFound Identifier
  -- | MsgQueue operations errors
  | EOutputPortWrongProcedure Identifier
  | EOutputPortSendWrongArgs
  deriving Show

type SemanticErrors = AnnotatedError Error Location

instance Annotated (AnnotatedError Error) where
  getAnnotation (AnnotatedError _err ann) = ann