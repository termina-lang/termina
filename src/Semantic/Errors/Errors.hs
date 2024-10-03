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
  | EInvalidArrayIndexing TerminaType -- ^ Invalid array indexing (E001)
  | ENotNamedObject Identifier -- ^ Object not found (E002)
  | ENotConstant Identifier -- ^ Invalid use of a non-constant object (E003)
  | EAssignmentToImmutable -- ^ Assignment to immutable variable (E004)
  | EIfElseNoOtherwise -- ^ Missing else clause (E005)
  | ENotCasteable TerminaType TerminaType -- ^ Casting error (E006)
  | EInvalidParameterType Parameter -- ^ Invalid parameter type (E007)
  | EInvalidReturnType TerminaType -- ^ Invalid return type (E008)
  | EProcedureCallExtraParams (Identifier, [TerminaType], a) Integer -- ^ Extra parameters in procedure call (E009)
  | EProcedureCallMissingParams (Identifier, [TerminaType], a) Integer -- ^ Missing parameters in procedure call (E010)
  | EProcedureCallParamTypeMismatch (Identifier, TerminaType, a) TerminaType -- ^ Parameter type mismatch in procedure call (E011)
  | EUnknownProcedure Identifier -- ^ Unknown procedure (E012)
  | EResourceClassNoProvides Identifier -- ^ Resource class does not provide any interface (E013)
  | EResourceClassAction (Identifier, a) Identifier -- ^ Resource class defines an action (E014)
  | EResourceClassInPort (Identifier, a) Identifier -- ^ Resource class defines an in port (E015)
  | EResourceClassOutPort (Identifier, a) Identifier -- ^ Resource class defines an out port (E016)
  | EInterfaceNotFound Identifier -- ^ Interface not found (E017)
  | EMismatchIdNotInterface Identifier -- ^ The type is not an interface (E018)
  | EProcedureNotFromProvidedInterfaces (Identifier, a) Identifier -- ^ Procedure not from provided interfaces (E019)
  | EMissingProcedure Identifier Identifier -- ^ Missing procedure (E020)
  | EProcedureExtraParams (Identifier, Identifier, [TerminaType], a) Integer -- ^ Extra parameters in procedure definition (E021)
  | EProcedureMissingParams (Identifier, Identifier, [TerminaType], a) Integer -- ^ Missing parameters in procedure definition (E022)
  | EProcedureParamTypeMismatch (Identifier, Identifier, TerminaType, a) TerminaType -- ^ Parameter type mismatch in procedure definition (E023)
  | EIfElseIfCondNotBool TerminaType -- ^ If-else-if condition is not a boolean (E024)
  | EFunctionCallExtraParams (Identifier, [TerminaType], a) Integer -- ^ Extra parameters in function call (E025)
  | EFunctionCallMissingParams (Identifier, [TerminaType], a) Integer -- ^ Missing parameters in function call (E026)
  | EFunctionCallParamTypeMismatch (Identifier, TerminaType, a) TerminaType -- ^ Parameter type mismatch in function call (E027)
  | EMemberAccessNotFunction Identifier -- ^ Access to a member that is not a function (E028)
  | EMutableReferenceToImmutable -- ^ Mutable reference to immutable object (E029)
  | EMutableReferenceToPrivate -- ^ Mutable reference to immutable object (E030)
  | EBinOpExpectedTypeLeft Op TerminaType TerminaType -- ^ Binary operation expected type on the left (E031)
  | EBinOpExpectedTypeRight Op TerminaType TerminaType -- ^ Binary operation expected type on the right (E032)
  | EBinOpTypeMismatch Op TerminaType TerminaType -- ^ Binary operation type mismatch (E033)
  | EBinOpExpectedTypeNotBool Op TerminaType -- ^ Binary operation expected result type not boolean (E034)
  | EBinOpLeftTypeNotBool Op TerminaType -- ^ Binary operation expected boolean type on the left (E035)
  | EBinOpRightTypeNotBool Op TerminaType -- ^ Binary operation expected boolean type on the right (E036)
  | EBinOpExpectedTypeNotNum Op TerminaType -- ^ Binary operation expected result type not numeric (E037)
  | EBinOpLeftTypeNotNum Op TerminaType -- ^ Binary operation expected numeric type on the left (E038)
  | EBinOpRightTypeNotNum Op TerminaType -- ^ Binary operation expected numeric type on the right (E39)
  | EBinOpRightTypeNotPos Op TerminaType -- ^ Binary operation expected positive numeric type on the right (E40)
  | EBinOpLeftTypeNotEquatable Op TerminaType -- ^ Binary operation expected equatable type on the left (E041)
  | EBinOpRightTypeNotEquatable Op TerminaType -- ^ Binary operation expected equatable type on the right (E042)
  | EAtomicAccessInvalidType TerminaType -- ^ Invalid type for the atomic access interface (E043)
  | EAtomicArrayAccessInvalidType TerminaType -- ^ Invalid type for the atomic array access interface (E044)
  | EAtomicInvalidType TerminaType -- ^ Invalid atomic type (E045)
  | EAtomicArrayInvalidType TerminaType -- ^ Invalid atomic array type (E046)
  | EAtomicConnectionTypeMismatch TerminaType TerminaType -- ^ Atomic connection type mismatch (E047)
  | EAtomicArrayConnectionTypeMismatch TerminaType TerminaType -- ^ Atomic array connection type mismatch (E050)
  | EAtomicArrayConnectionSizeMismatch Size Size -- ^ Atomic array connection size mismatch (E49)
  | EConstantWithoutKnownType Const -- ^ Constant without known type (E50)
  | EStructInitializerInvalidUse -- ^ Invalid use of a struct initializer (E051)
  | EStructInitializerTypeMismatch TerminaType TerminaType -- ^ Struct initializer type mismatch (E052)
  | EStructInitializerGlobalNotStruct (SemanTypeDef a) -- ^ Struct initializer expected global type not struct (E053)  
  | EStructInitializerExpectedTypeNotStruct TerminaType -- ^ Struct initializer expected type not struct (E054)
  | EStructInitializerUnknownType Identifier -- ^ Struct initializer unknown type (E055)
  | ESliceInvalidUse -- ^ Invalid use of a slice (E056)
  | EArrayInitializerInvalidUse -- ^ Invalid use of an array initializer (E057)
  | EArrayInitializerNotArray TerminaType -- ^ Assignment of an array initializer to a non-array type (E058)
  | EArrayExprListInitializerInvalidUse -- ^ Invalid use of an expression list array initializer (E059)
  | EArrayExprListInitializerNotArray TerminaType -- ^ Assignment of an expression list array initializer to a non-array type (E060)
  | EOptionVariantInitializerInvalidUse -- ^ Invalid use of an option variant initializer (E061)
  | EArrayInitializerSizeMismatch Size Size -- ^ Array initializer size mismatch (E062)
  | EArrayExprListInitializerSizeMismatch Integer Integer -- ^ Array expression list array initializer size mismatch (E063)
  | EArrayExprListInitializerExprTypeMismatch TerminaType TerminaType -- ^ Array initializing expression type mismatch (E064)
  | EReturnValueExpected TerminaType -- ^ Expected return value (E065)
  | EReturnValueNotUnit -- ^ Return value not expected (E066)
  | EInvalidArrayType TerminaType -- ^ Invalid array type (E067)
  | EInvalidBoxType TerminaType -- ^ Invalid box type (E068)
  | ENoTypeFound Identifier -- ^ Type not found (E069 & Internal)
  | EGlobalNotType Identifier -- ^ Global object but not a type (E070)
  | EInvalidAccessToGlobal Identifier -- ^ Invalid access to global object (E071)
  | EConstantIsReadOnly Identifier -- ^ Invalid write to a constant (E072)
  | ESymbolDefined Identifier a -- ^ Symbol already defined (E073)
  | EExpressionNotConstant -- ^ Expression not constant (E074)
  | EContinueInvalidExpression -- ^ Invalid expression in continue statement (E075)
  | EContinueInvalidProcedureCall Identifier -- ^ Invalid procedure call in continue statement (E076)
  | EContinueInvalidMethodOrViewerCall Identifier -- ^ Invalid method or viewer call in continue statement (E077)
  | EContinueInvalidMemberCall TerminaType -- ^ Invalid member call in continue statement (E078)
  | EContinueActionNotFound Identifier -- ^ Action not found in continue statement (E079)
  | EContinueActionExtraParams (Identifier, [TerminaType], a) Integer -- ^ Extra parameters in action call in continue statement (E080)
  | EContinueActionMissingParam (Identifier, a) -- ^ Missing parameters in action call in continue statement (E081)
  | EEnumVariantInitializerInvalidUse -- ^ Invalid use of an enum variant initializer (E082)
  | ENoEnumFound Identifier -- ^ Enum not found (E083 & Internal)
  | EGlobalNotEnum (Identifier, a) -- ^ Global object but not an enum (E084)
  | EEnumVariantNotFound Identifier Identifier -- ^ Enum variant not found (E085)
  | EEnumVariantExtraParams (Identifier, a) (Identifier, [TerminaType]) Integer -- ^ Extra parameters in enum variant (E086)
  | EEnumVariantMissingParams (Identifier, a) (Identifier, [TerminaType]) Integer -- ^ Missing parameters in enum variant (E087)
  | EEnumVariantParamTypeMismatch (Identifier, a) (Identifier, Integer, TerminaType) TerminaType -- ^ Parameter type mismatch in enum variant (E088)
  | EFunctionNotFound Identifier -- ^ Function not found (E089)
  | EGlobalNotFunction (Identifier, a) -- ^ Global object but not a function (E090)
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