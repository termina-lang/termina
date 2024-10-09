{-# LANGUAGE FlexibleInstances #-}
-- | Module Encapsulating Semantic Errors

module Semantic.Errors.Errors where

-- Termina AST
import Parser.AST
-- import SemanAST as SAST
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
  | EExpectedSimple TerminaType -- ^ Expected a simple type (Internal)
  | ENotNamedGlobal Identifier -- ^ Global object not found (Internal)
  | EInvalidObjectDeclaration Identifier -- ^ Invalid object declaration (Internal)
  | EMalformedSlice -- ^ Malformed slice (Internal)
  | EInvalidArrayIndexing TerminaType -- ^ Invalid array indexing (SE-001)
  | ENotNamedObject Identifier -- ^ Object not found (SE-002)
  | ENotConstant Identifier -- ^ Invalid use of a non-constant object (SE-003)
  | EAssignmentToImmutable -- ^ Assignment to immutable variable (SE-004)
  | EIfElseNoOtherwise -- ^ Missing else clause (SE-005)
  | ENotCasteable TerminaType TerminaType -- ^ Casting error (SE-006)
  | EInvalidParameterType Parameter -- ^ Invalid parameter type (SE-007)
  | EInvalidReturnType TerminaType -- ^ Invalid return type (SE-008)
  | EProcedureCallExtraParams (Identifier, [TerminaType], a) Integer -- ^ Extra parameters in procedure call (SE-009)
  | EProcedureCallMissingParams (Identifier, [TerminaType], a) Integer -- ^ Missing parameters in procedure call (SE-010)
  | EProcedureCallParamTypeMismatch (Identifier, TerminaType, a) TerminaType -- ^ Parameter type mismatch in procedure call (SE-011)
  | EUnknownProcedure Identifier -- ^ Unknown procedure (SE-012)
  | EResourceClassNoProvides Identifier -- ^ Resource class does not provide any interface (SE-013)
  | EResourceClassAction (Identifier, a) Identifier -- ^ Resource class defines an action (SE-014)
  | EResourceClassInPort (Identifier, a) Identifier -- ^ Resource class defines an in port (SE-015)
  | EResourceClassOutPort (Identifier, a) Identifier -- ^ Resource class defines an out port (SE-016)
  | EInterfaceNotFound Identifier -- ^ Interface not found (SE-017)
  | EMismatchIdNotInterface Identifier -- ^ The type is not an interface (SE-018)
  | EProcedureNotFromProvidedInterfaces (Identifier, a) Identifier -- ^ Procedure not from provided interfaces (SE-019)
  | EMissingProcedure Identifier Identifier -- ^ Missing procedure (SE-020)
  | EProcedureExtraParams (Identifier, Identifier, [TerminaType], a) Integer -- ^ Extra parameters in procedure definition (SE-021)
  | EProcedureMissingParams (Identifier, Identifier, [TerminaType], a) Integer -- ^ Missing parameters in procedure definition (SE-022)
  | EProcedureParamTypeMismatch (Identifier, Identifier, TerminaType, a) TerminaType -- ^ Parameter type mismatch in procedure definition (SE-023)
  | EIfElseIfCondNotBool TerminaType -- ^ If-else-if condition is not a boolean (SE-024)
  | EFunctionCallExtraParams (Identifier, [TerminaType], a) Integer -- ^ Extra parameters in function call (SE-025)
  | EFunctionCallMissingParams (Identifier, [TerminaType], a) Integer -- ^ Missing parameters in function call (SE-026)
  | EFunctionCallParamTypeMismatch (Identifier, TerminaType, a) TerminaType -- ^ Parameter type mismatch in function call (SE-027)
  | EMemberAccessNotFunction Identifier -- ^ Access to a member that is not a function (SE-028)
  | EMutableReferenceToImmutable -- ^ Mutable reference to immutable object (SE-029)
  | EMutableReferenceToPrivate -- ^ Mutable reference to immutable object (SE-030)
  | EBinOpExpectedTypeLeft Op TerminaType TerminaType -- ^ Binary operation expected type on the left (SE-031)
  | EBinOpExpectedTypeRight Op TerminaType TerminaType -- ^ Binary operation expected type on the right (SE-032)
  | EBinOpTypeMismatch Op TerminaType TerminaType -- ^ Binary operation type mismatch (SE-033)
  | EBinOpExpectedTypeNotBool Op TerminaType -- ^ Binary operation expected result type not boolean (SE-034)
  | EBinOpLeftTypeNotBool Op TerminaType -- ^ Binary operation expected boolean type on the left (SE-035)
  | EBinOpRightTypeNotBool Op TerminaType -- ^ Binary operation expected boolean type on the right (SE-036)
  | EBinOpExpectedTypeNotNum Op TerminaType -- ^ Binary operation expected result type not numeric (SE-037)
  | EBinOpLeftTypeNotNum Op TerminaType -- ^ Binary operation expected numeric type on the left (SE-038)
  | EBinOpRightTypeNotNum Op TerminaType -- ^ Binary operation expected numeric type on the right (E39)
  | EBinOpRightTypeNotPos Op TerminaType -- ^ Binary operation expected positive numeric type on the right (E40)
  | EBinOpLeftTypeNotEquatable Op TerminaType -- ^ Binary operation expected equatable type on the left (SE-041)
  | EBinOpRightTypeNotEquatable Op TerminaType -- ^ Binary operation expected equatable type on the right (SE-042)
  | EAtomicAccessInvalidType TerminaType -- ^ Invalid type for the atomic access interface (SE-043)
  | EAtomicArrayAccessInvalidType TerminaType -- ^ Invalid type for the atomic array access interface (SE-044)
  | EAtomicInvalidType TerminaType -- ^ Invalid atomic type (SE-045)
  | EAtomicArrayInvalidType TerminaType -- ^ Invalid atomic array type (SE-046)
  | EAtomicConnectionTypeMismatch TerminaType TerminaType -- ^ Atomic connection type mismatch (SE-047)
  | EAtomicArrayConnectionTypeMismatch TerminaType TerminaType -- ^ Atomic array connection type mismatch (SE-050)
  | EAtomicArrayConnectionSizeMismatch Size Size -- ^ Atomic array connection size mismatch (E49)
  | EConstantWithoutKnownType Const -- ^ Constant without known type (E50)
  | EStructInitializerInvalidUse -- ^ Invalid use of a struct initializer (SE-051)
  | EStructInitializerTypeMismatch TerminaType TerminaType -- ^ Struct initializer type mismatch (SE-052)
  | EStructInitializerGlobalNotStruct (SemanTypeDef a) -- ^ Struct initializer expected global type not struct (SE-053)  
  | EStructInitializerExpectedTypeNotStruct TerminaType -- ^ Struct initializer expected type not struct (SE-054)
  | EStructInitializerUnknownType Identifier -- ^ Struct initializer unknown type (SE-055)
  | ESliceInvalidUse -- ^ Invalid use of a slice (SE-056)
  | EArrayInitializerInvalidUse -- ^ Invalid use of an array initializer (SE-057)
  | EArrayInitializerNotArray TerminaType -- ^ Assignment of an array initializer to a non-array type (SE-058)
  | EArrayExprListInitializerInvalidUse -- ^ Invalid use of an expression list array initializer (SE-059)
  | EArrayExprListInitializerNotArray TerminaType -- ^ Assignment of an expression list array initializer to a non-array type (SE-060)
  | EOptionVariantInitializerInvalidUse -- ^ Invalid use of an option variant initializer (SE-061)
  | EArrayInitializerSizeMismatch Size Size -- ^ Array initializer size mismatch (SE-062)
  | EArrayExprListInitializerSizeMismatch Integer Integer -- ^ Array expression list array initializer size mismatch (SE-063)
  | EArrayExprListInitializerExprTypeMismatch TerminaType TerminaType -- ^ Array initializing expression type mismatch (SE-064)
  | EReturnValueExpected TerminaType -- ^ Expected return value (SE-065)
  | EReturnValueNotUnit -- ^ Return value not expected (SE-066)
  | EInvalidArrayType TerminaType -- ^ Invalid array type (SE-067)
  | EInvalidBoxType TerminaType -- ^ Invalid box type (SE-068)
  | ENoTypeFound Identifier -- ^ Type not found (SE-069 & Internal)
  | EGlobalNotType Identifier -- ^ Global object but not a type (SE-070)
  | EInvalidAccessToGlobal Identifier -- ^ Invalid access to global object (SE-071)
  | EConstantIsReadOnly Identifier -- ^ Invalid write to a constant (SE-072)
  | ESymbolDefined Identifier a -- ^ Symbol already defined (SE-073)
  | EExpressionNotConstant -- ^ Expression not constant (SE-074)
  | EContinueInvalidExpression -- ^ Invalid expression in continue statement (SE-075)
  | EContinueInvalidProcedureCall Identifier -- ^ Invalid procedure call in continue statement (SE-076)
  | EContinueInvalidMethodOrViewerCall Identifier -- ^ Invalid method or viewer call in continue statement (SE-077)
  | EContinueInvalidMemberCall TerminaType -- ^ Invalid member call in continue statement (SE-078)
  | EContinueActionNotFound Identifier -- ^ Action not found in continue statement (SE-079)
  | EContinueActionExtraParams (Identifier, [TerminaType], a) Integer -- ^ Extra parameters in action call in continue statement (SE-080)
  | EContinueActionMissingParam (Identifier, a) -- ^ Missing parameters in action call in continue statement (SE-081)
  | EEnumVariantInitializerInvalidUse -- ^ Invalid use of an enum variant initializer (SE-082)
  | ENoEnumFound Identifier -- ^ Enum not found (SE-083 & Internal)
  | EGlobalNotEnum (Identifier, a) -- ^ Global object but not an enum (SE-084)
  | EEnumVariantNotFound Identifier Identifier -- ^ Enum variant not found (SE-085)
  | EEnumVariantExtraParams (Identifier, a) (Identifier, [TerminaType]) Integer -- ^ Extra parameters in enum variant (SE-086)
  | EEnumVariantMissingParams (Identifier, a) (Identifier, [TerminaType]) Integer -- ^ Missing parameters in enum variant (SE-087)
  | EEnumVariantParamTypeMismatch (Identifier, a) (Identifier, Integer, TerminaType) TerminaType -- ^ Parameter type mismatch in enum variant (SE-088)
  | EFunctionNotFound Identifier -- ^ Function not found (SE-089)
  | EGlobalNotFunction (Identifier, a) -- ^ Global object but not a function (SE-090)
  -- | Record missing field^
  | EFieldMissing [Identifier]
  -- | Record extra fields
  | EFieldExtra [Identifier]
  -- | Field is not a fixed location
  | EFieldNotFixedLocation Identifier
  -- | Field does not have a fixed location
  | EFieldNotPort Identifier
  -- | Expecting a Enumeration when memberAccessing got
  | EMemberAccess TerminaType
  | EFunctionAccessNotResource TerminaType
  | EMemberAccessNotMember Identifier -- TODO: We can return the list of identifiers.
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
  -- | Invalid class field type
  | EInvalidClassFieldType TerminaType
  -- | Forbidden Reference Type
  | EReferenceTy TerminaType
  -- | Nested Option
  | EOptionNested 
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