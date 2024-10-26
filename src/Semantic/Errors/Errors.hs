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
data Error a
  -- | Expected /similar/ types?
  = 
    EMismatch TerminaType TerminaType -- ^ Type mismatch (Internal)
  | ENoStructFound Identifier -- ^ Struct not found (Internal)
  | EUnboxingObject -- ^ Error when unboxing an annotated object to get its type (Internal)
  | EUnboxingStructType -- ^ Error when unboxing a struct type (Internal)
  | EUnboxingClassType -- ^ Error when unboxing a class type (Internal)
  | EExpectedArrayTy TerminaType -- ^ Expected a valid type for the elements of an array (Internal)
  | EExpectedCopyType TerminaType -- ^ Expected a copiable type (Internal)
  | EExpectedNumType TerminaType -- ^ Expected a numeric type (Internal)
  | ENotNamedGlobal Identifier -- ^ Global object not found (Internal)
  | EInvalidObjectDeclaration Identifier -- ^ Invalid object declaration (Internal)
  | EMalformedSlice -- ^ Malformed slice (Internal)
  | EInvalidArrayIndexing TerminaType -- ^ Invalid array indexing (SE-001)
  | ENotNamedObject Identifier -- ^ Object not found (SE-002)
  | ENotConstant Identifier -- ^ Invalid use of a non-constant object (SE-003)
  | EAssignmentToImmutable -- ^ Assignment to immutable variable (SE-004)
  | EIfElseNoOtherwise -- ^ Missing else clause (SE-005)
  | ENotCasteable TerminaType TerminaType -- ^ Casting error (SE-006)
  -- TODO: Re-enumerate the errors
  | EInvalidParameterType Parameter -- ^ Invalid parameter type (SE-007)
  | EInvalidProcedureParameterType Parameter -- ^ Invalid procedure parameter type (SE-007)
  | EInvalidReturnType TerminaType -- ^ Invalid return type (SE-008)
  | EProcedureCallExtraParams (Identifier, [TerminaType], a) Integer -- ^ Extra parameters in procedure call (SE-009)
  | EProcedureCallMissingParams (Identifier, [TerminaType], a) Integer -- ^ Missing parameters in procedure call (SE-010)
  | EProcedureCallParamTypeMismatch (Identifier, TerminaType, a) Integer TerminaType -- ^ Parameter type mismatch in procedure call (SE-011)
  | EUnknownProcedure Identifier -- ^ Unknown procedure (SE-012)
  | EResourceClassNoProvides Identifier -- ^ Resource class does not provide any interface (SE-013)
  | EResourceClassAction (Identifier, a) Identifier -- ^ Resource class defines an action (SE-014)
  | EResourceClassInPort (Identifier, a) Identifier -- ^ Resource class defines an in port (SE-015)
  | EResourceClassOutPort (Identifier, a) Identifier -- ^ Resource class defines an out port (SE-016)
  | EInterfaceNotFound Identifier -- ^ Interface not found (SE-017)
  | EGlobalNotInterface Identifier -- ^ The type is not an interface (SE-018)
  | EProcedureNotFromProvidedInterfaces (Identifier, a) Identifier -- ^ Procedure not from provided interfaces (SE-019)
  | EMissingProcedure Identifier Identifier -- ^ Missing procedure (SE-020)
  | EProcedureExtraParams (Identifier, Identifier, [TerminaType], a) Integer -- ^ Extra parameters in procedure definition (SE-021)
  | EProcedureMissingParams (Identifier, Identifier, [TerminaType], a) Integer -- ^ Missing parameters in procedure definition (SE-022)
  | EProcedureParamTypeMismatch (Identifier, Identifier, TerminaType, a) TerminaType -- ^ Parameter type mismatch in procedure definition (SE-023)
  | ETaskClassProvides Identifier -- ^ Task class provides an interface (SE-024)
  | ETaskClassProcedure (Identifier, a) Identifier -- ^ Task class defines a procedure (SE-025)
  | ETaskClassNoActions Identifier -- ^ Task class does not define any actions (SE-026)
  | EHandlerClassProvides Identifier -- ^ Handler class provides an interface (SE-027)
  | EHandlerClassProcedure (Identifier, a) Identifier -- ^ Handler class defines a procedure (SE-028)
  | EHandlerClassNoAction Identifier -- ^ Handler class does not define any actions (SE-029)
  | EHandlerClassMultipleActions Identifier a -- ^ Handler class defines multiple actions (SE-030)
  | EHandlerClassNoSinkPort Identifier -- ^ Handler class does not define a sink port (SE-031)
  | EHandlerClassMultipleSinkPorts Identifier a -- ^ Handler class defines multiple sink ports (SE-032)
  | EHandlerClassInPort (Identifier, a) Identifier -- ^ Handler class defines an in port (SE-033)
  | EIfElseIfCondNotBool TerminaType -- ^ If-else-if condition is not a boolean (SE-034)
  | EFunctionCallExtraParams (Identifier, [TerminaType], a) Integer -- ^ Extra parameters in function call (SE-035)
  | EFunctionCallMissingParams (Identifier, [TerminaType], a) Integer -- ^ Missing parameters in function call (SE-036)
  | EFunctionCallParamTypeMismatch (Identifier, TerminaType, a) TerminaType -- ^ Parameter type mismatch in function call (SE-037)
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
  | EAtomicArrayConnectionTypeMismatch TerminaType TerminaType -- ^ Atomic array connection type mismatch (SE-058)
  | EAtomicArrayConnectionSizeMismatch Size Size -- ^ Atomic array connection size mismatch (SE-059)
  | EConstantWithoutKnownType Const -- ^ Constant without known type (SE-060)
  | EStructInitializerInvalidUse -- ^ Invalid use of a struct initializer (SE-061)
  | EStructInitializerTypeMismatch TerminaType TerminaType -- ^ Struct initializer type mismatch (SE-062)
  | EStructInitializerGlobalNotStruct (SemanTypeDef a) -- ^ Struct initializer expected global type not struct (SE-063)  
  -- TODO: Re-enumerate the errors
  | EStructInitializerGlobalNotClass (SemanTypeDef a) -- ^ Struct initializer expected global type not class (SE-063)
  | EEnumInitializerExpectedTypeMismatch TerminaType TerminaType -- ^ Enum initializer expected type mismatch (SE-064)
  | EStructInitializerExpectedTypeNotStruct TerminaType -- ^ Struct initializer expected type not struct (SE-064)
  | EStructInitializerUnknownType Identifier -- ^ Struct initializer unknown type (SE-065)
  | ESliceInvalidUse -- ^ Invalid use of a slice (SE-066)
  | EArrayInitializerInvalidUse -- ^ Invalid use of an array initializer (SE-067)
  | EArrayInitializerNotArray TerminaType -- ^ Assignment of an array initializer to a non-array type (SE-068)
  | EArrayExprListInitializerInvalidUse -- ^ Invalid use of an expression list array initializer (SE-069)
  | EArrayExprListInitializerNotArray TerminaType -- ^ Assignment of an expression list array initializer to a non-array type (SE-070)
  | EOptionVariantInitializerInvalidUse -- ^ Invalid use of an option variant initializer (SE-071)
  | EArrayInitializerSizeMismatch Size Size -- ^ TArray initializer size mismatch (SE-072)
  | EArrayExprListInitializerSizeMismatch Integer Integer -- ^ TArray expression list array initializer size mismatch (SE-073)
  | EArrayExprListInitializerExprTypeMismatch TerminaType TerminaType -- ^ TArray initializing expression type mismatch (SE-074)
  | EReturnValueExpected TerminaType -- ^ Expected return value (SE-075)
  | EReturnValueNotUnit -- ^ Return value not expected (SE-076)
  | EInvalidArrayType TerminaType -- ^ Invalid array type (SE-077)
  | EInvalidBoxType TerminaType -- ^ Invalid box type (SE-078)
  | ENoTypeFound Identifier -- ^ Type not found (SE-079 & Internal)
  | EGlobalNotType Identifier -- ^ Global object but not a type (SE-080)
  | EInvalidAccessToGlobal Identifier -- ^ Invalid access to global object (SE-081)
  | EConstantIsReadOnly Identifier -- ^ Invalid write to a constant (SE-082)
  | ESymbolDefined Identifier a -- ^ Symbol already defined (SE-083)
  | EExpressionNotConstant -- ^ Expression not constant (SE-084)
  | EContinueInvalidExpression -- ^ Invalid expression in continue statement (SE-085)
  | EContinueInvalidProcedureCall Identifier -- ^ Invalid procedure call in continue statement (SE-086)
  | EContinueInvalidMethodOrViewerCall Identifier -- ^ Invalid method or viewer call in continue statement (SE-087)
  | EContinueInvalidMemberCall TerminaType -- ^ Invalid member call in continue statement (SE-088)
  | EContinueActionNotFound Identifier -- ^ Action not found in continue statement (SE-089)
  | EContinueActionExtraParams (Identifier, [TerminaType], a) Integer -- ^ Extra parameters in action call in continue statement (SE-090)
  | EContinueActionMissingParam (Identifier, a) -- ^ Missing parameters in action call in continue statement (SE-091)
  | EEnumVariantInitializerInvalidUse -- ^ Invalid use of an enum variant initializer (SE-092)
  | ENoEnumFound Identifier -- ^ Enum not found (SE-093 & Internal)
  | EGlobalNotEnum (Identifier, a) -- ^ Global object but not an enum (SE-094)
  | EEnumVariantNotFound Identifier Identifier -- ^ Enum variant not found (SE-095)
  | EEnumVariantExtraParams (Identifier, a) (Identifier, [TerminaType]) Integer -- ^ Extra parameters in enum variant (SE-096)
  | EEnumVariantMissingParams (Identifier, a) (Identifier, [TerminaType]) Integer -- ^ Missing parameters in enum variant (SE-097)
  | EEnumVariantParamTypeMismatch (Identifier, a) (Identifier, Integer, TerminaType) TerminaType -- ^ Parameter type mismatch in enum variant (SE-098)
  | EFunctionNotFound Identifier -- ^ Function not found (SE-099)
  | EGlobalNotFunction (Identifier, a) -- ^ Global object but not a function (SE-100)
  | EUnexpectedNumericConstant TerminaType -- ^ Unexpected numeric constant (SE-101)
  | EInvalidAssignmentExprType TerminaType -- ^ Invalid assignment expression type (SE-102)
  | EInvalidMessageType TerminaType -- ^ Invalid message type (SE-103)
  | EInvalidOptionType TerminaType -- ^ Invalid option type (SE-104)
  | EInvalidReferenceType TerminaType -- ^ Invalid reference type (SE-105)
  | EInvalidLocationType TerminaType -- ^ Invalid location type (SE-106)
  | EInvalidAllocatorType TerminaType -- ^ Invalid allocator type (SE-107)
  | EInvalidClassFieldType TerminaType -- ^ Invalid class field type (SE-108)
  | EInvalidStructFieldType TerminaType -- ^ Invalid struct field type (SE-109)
  | EInvalidEnumParameterType TerminaType -- ^ Invalid enum parameter type (SE-110)
  | EInvalidAccessPortType TerminaType -- ^ Invalid access port type (SE-111)
  | EInvalidTypeSpecifier TypeSpecifier -- ^ Invalid type specifier (SE-112)
  | EInvalidTypeNotAStruct Identifier -- ^ Invalid type not a struct (SE-113)
  | EInvalidNumericConstantType TerminaType -- ^ Invalid numeric constant type (SE-114)
  -- | Record missing field
  | EFieldMissing [Identifier]
  -- | Record extra fields
  | EFieldExtra [Identifier]
  -- | Field is not a fixed location
  | EFieldNotFixedLocation Identifier
  -- | Field is not a port
  | EFieldNotPort Identifier
  -- | Expecting a Enumeration when memberAccessing got
  | EMemberAccess TerminaType
  | EFunctionAccessNotResource TerminaType
  | EMemberAccessNotField Identifier -- TODO: We can return the list of identifiers.
  -- | Calling a procedure within another member function
  | EMemberAccessInvalidProcedureCall Identifier
  | EMemberAccessUDef (SemanTypeDef a)
  | EMemberFunctionUDef (SemanTypeDef a)
  | EMemberMethodType
  | EMemberMethodExtraParams
  | EMemberMethodMissingParams
  -- | Not an integer const
  | ENotIntConst Const
  | EConstantOutRange Const
  -- | ForLoop
  | EForIteratorWrongType TerminaType
  -- | Defined GEntry
  | EDefinedGEntry (GEntry a)
  -- | Impossible Cases. Internal Transpiler errors
  | EUnboxingStmtExpr -- Unboxing statement as an expression.
  | EUnboxingBlockRet -- Unboxing Blockret statement
  -- | Unique names for types.
  | EUsedTypeName Identifier a
  -- | Unique names for Global
  | EUsedGlobalName Identifier a
  -- | Access port does not have an Interface type
  | EAccessPortNotInterface TerminaType
  | EAccessPortNotResource Identifier 
  | EInboundPortNotEmitter Identifier 
  | EInboundPortNotChannel Identifier
  | EOutboundPortNotChannel Identifier
  | EAccessPortNotPool Identifier
  | EAccessPortNotAtomic Identifier
  | EAccessPortNotAtomicArray Identifier
  -- | Box (type has a Box inside) as Argument of a function
  | EConstParameterNotNum Parameter
  -- | Function Declaration error,
  | EUsedFunName Identifier Location
  -- | Struct Definition
  | EStructDefNotUniqueField [Identifier]
  | EStructDefEmptyStruct Identifier
  -- | Enums Definition 
  | EEnumDefNotUniqueField [Identifier]
  -- | Interface Definition
  | EInterfaceEmpty Identifier
  | EInterfaceNotUniqueProcedure [Identifier]
  -- | Class Definition
  | EClassEmptyMethods Identifier
  | EClassLoop [Identifier] -- Detected loop between procs, method and viewers
  | EMissingIdentifier -- Should not happen
  | ENotClassField Identifier
  | EClassTyping
  -- Dereference Object
  | ETypeNotReference TerminaType
  -- Internal Unbox
  | EUnBoxExpression
  -- Match
  | EMatchNotEnum Identifier
  | EMatchWrongType TerminaType
  | EMatchOptionBadArgs
  | EMatchOptionBadSome
  | EMatchOptionBad
  | EMatchCaseInternalError
  | EMatchCaseBadName Identifier Identifier
  | EMatchExtraCases
  -- Enum variant expressions
  | ETyNotEnumFound Identifier
  | ETyNotEnum Identifier (SemanTypeDef a)
  | EIsVariantNotEnum TerminaType
  | EIsVariantNotOption TerminaType
  | EIsVariantEnumMismatch Identifier Identifier
  | EIsVariantNotFound Identifier
  -- | Unexpected Global element unboxing.
  | EInternalNoGTY
  -- | MsgQueue operations errors
  | EMsgQueueWrongProcedure Identifier
  | ENoMsgQueueSendWrongArgs
  | ENoMsgQueueRcvWrongArgs
  | EMsgQueueSendArgNotObject
  | EMsgQueueSendArgNotRefMutResult TerminaType
  | EMsgQueueSendArgNotRefImmTimeout TerminaType
  | EMsgQueueWrongType TerminaType TerminaType
  | EMsgQueueRcvWrongArgTy TerminaType
  deriving Show

type SemanticErrors = AnnotatedError (Error Location) Location

instance Annotated (AnnotatedError (Error Location)) where
  getAnnotation (AnnotatedError _err ann) = ann