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
    EMismatch TypeSpecifier TypeSpecifier -- ^ Type mismatch (Internal)
  | ENoStructFound Identifier -- ^ Struct not found (Internal)
  | EUnboxingObject -- ^ Error when unboxing an annotated object to get its type (Internal)
  | EExpectedSimple TypeSpecifier -- ^ Expected a simple type (Internal)
  | ENotNamedGlobal Identifier -- ^ Global object not found (Internal)
  | EArray TypeSpecifier -- ^ Invalid array indexing (E001)
  | ENotNamedObject Identifier -- ^ Object not found (E002)
  | ENotConstant Identifier -- ^ Invalid use of a non-constant object (E003)
  | EAssignmentToImmutable -- ^ Assignment to immutable variable (E004)
  | EIfElseNoOtherwise -- ^ Missing else clause (E005)
  | ENotCasteable TypeSpecifier TypeSpecifier -- ^ Casting error (E006)
  | EInvalidParameterType Parameter -- ^ Invalid parameter type (E007)
  | EInvalidReturnType TypeSpecifier -- ^ Invalid return type (E008)
  | EProcedureCallExtraParams (Identifier, [Parameter], a) Integer -- ^ Extra parameters in procedure call (E009)
  | EProcedureCallMissingParams (Identifier, [Parameter], a) Integer -- ^ Missing parameters in procedure call (E010)
  | EProcedureCallParamTypeMismatch (Identifier, Parameter, a) TypeSpecifier -- ^ Parameter type mismatch in procedure call (E011)
  | EUnknownProcedure Identifier -- ^ Unknown procedure (E012)
  | EResourceClassNoProvides Identifier -- ^ Resource class does not provide any interface (E013)
  | EResourceClassAction (Identifier, a) Identifier -- ^ Resource class defines an action (E014)
  | EResourceClassInPort (Identifier, a) Identifier -- ^ Resource class defines an in port (E015)
  | EResourceClassOutPort (Identifier, a) Identifier -- ^ Resource class defines an out port (E016)
  | EInterfaceNotFound Identifier -- ^ Interface not found (E017)
  | EMismatchIdNotInterface Identifier -- ^ The type is not an interface (E018)
  | EProcedureNotFromProvidedInterfaces (Identifier, a) Identifier -- ^ Procedure not from provided interfaces (E019)
  | EMissingProcedure Identifier Identifier -- ^ Missing procedure (E020)
  | EProcedureExtraParams (Identifier, Identifier, [Parameter], a) Integer -- ^ Extra parameters in procedure definition (E021)
  | EProcedureMissingParams (Identifier, Identifier, [Parameter], a) Integer -- ^ Missing parameters in procedure definition (E022)
  | EProcedureParamTypeMismatch (Identifier, Identifier, Parameter, a) TypeSpecifier -- ^ Parameter type mismatch in procedure definition (E023)
  | EIfElseIfCondNotBool TypeSpecifier -- ^ If-else-if condition is not a boolean (E024)
  | EFunctionCallExtraParams (Identifier, [Parameter], a) Integer -- ^ Extra parameters in function call (E025)
  | EFunctionCallMissingParams (Identifier, [Parameter], a) Integer -- ^ Missing parameters in function call (E026)
  | EFunctionCallParamTypeMismatch (Identifier, Parameter, a) TypeSpecifier -- ^ Parameter type mismatch in function call (E027)
  | EMemberAccessNotFunction Identifier -- ^ Access to a member that is not a function (E028)
  | EMutableReferenceToImmutable -- ^ Mutable reference to immutable object (E029)
  | EBinOpExpectedTypeLeft Op TypeSpecifier TypeSpecifier -- ^ Binary operation expected type on the left (E030)
  | EBinOpExpectedTypeRight Op TypeSpecifier TypeSpecifier -- ^ Binary operation expected type on the right (E031)
  | EBinOpTypeMismatch Op TypeSpecifier TypeSpecifier -- ^ Binary operation type mismatch (E032)
  | EBinOpExpectedTypeNotBool Op TypeSpecifier -- ^ Binary operation expected result type not boolean (E033)
  | EBinOpLeftTypeNotBool Op TypeSpecifier -- ^ Binary operation expected boolean type on the left (E034)
  | EBinOpRightTypeNotBool Op TypeSpecifier -- ^ Binary operation expected boolean type on the right (E035)
  | EBinOpExpectedTypeNotNum Op TypeSpecifier -- ^ Binary operation expected result type not numeric (E036)
  | EBinOpLeftTypeNotNum Op TypeSpecifier -- ^ Binary operation expected numeric type on the left (E037)
  | EBinOpRightTypeNotNum Op TypeSpecifier -- ^ Binary operation expected numeric type on the right (E038)
  | EBinOpRightTypeNotPos Op TypeSpecifier -- ^ Binary operation expected positive numeric type on the right (E039)
  | EBinOpLeftTypeNotEquatable Op TypeSpecifier -- ^ Binary operation expected equatable type on the left (E040)
  | EBinOpRightTypeNotEquatable Op TypeSpecifier -- ^ Binary operation expected equatable type on the right (E041)
  | EAtomicAccessInvalidType TypeSpecifier -- ^ Invalid type for the atomic access interface (E042)
  | EAtomicArrayAccessInvalidType TypeSpecifier -- ^ Invalid type for the atomic array access interface (E043)
  | EAtomicInvalidType TypeSpecifier -- ^ Invalid atomic type (E044)
  | EAtomicArrayInvalidType TypeSpecifier -- ^ Invalid atomic array type (E045)
  | EAtomicConnectionTypeMismatch TypeSpecifier TypeSpecifier -- ^ Atomic connection type mismatch (E046)
  | EAtomicArrayConnectionTypeMismatch TypeSpecifier TypeSpecifier -- ^ Atomic array connection type mismatch (E047)
  | EAtomicArrayConnectionSizeMismatch Size Size -- ^ Atomic array connection size mismatch (E048)
  | EConstantWithoutKnownType Const -- ^ Constant without known type (E049)
  | EStructInitializerInvalidUse -- ^ Invalid use of a struct initializer (E050)
  | EStructInitializerTypeMismatch TypeSpecifier TypeSpecifier -- ^ Struct initializer type mismatch (E051)
  | EStructInitializerGlobalNotStruct (SemanTypeDef a) -- ^ Struct initializer expected global type not struct (E052)  
  | EStructInitializerExpectedTypeNotStruct TypeSpecifier -- ^ Struct initializer expected type not struct (E053)
  | EStructInitializerUnknownType Identifier -- ^ Struct initializer unknown type (E054)
  | ESliceInvalidUse -- ^ Invalid use of a slice (E055)
  | EArrayIntitalizerInvalidUse -- ^ Invalid use of an array initializer (E056)
  | EArrayExprListIntitalizerInvalidUse -- ^ Invalid use of an expression list array initializer (E057)
  | EOptionVariantInitializerInvalidUse -- ^ Invalid use of an option variant initializer (E058)
  | EArrayInitializerSizeMismatch Size Size -- ^ Array initializer size mismatch (E059)
  | EArrayExprListInitializerSizeMismatch Integer Integer -- ^ Array expression list array initializer size mismatch (E060)
  | EArrayExprListInitializerExprTypeMismatch TypeSpecifier TypeSpecifier -- ^ Array initializing expression type mismatch (E061)
  | EReturnValueExpected TypeSpecifier -- ^ Expected return value (E062)
  | EReturnValueNotUnit -- ^ Return value not expected (E063)
  | EInvalidArrayType TypeSpecifier -- ^ Invalid array type (E064)
  | EInvalidBoxType TypeSpecifier -- ^ Invalid box type (E065)
  | ENoTypeFound Identifier -- ^ Type not found (E066 & Internal)
  | EGlobalNoType Identifier -- ^ Global object but not a type (E067)
  | EInvalidAccessToGlobal Identifier -- ^ Invalid access to global object (E068)
  | EConstantIsReadOnly Identifier -- ^ Invalid write to a constant (E069)
  | ESymbolDefined Identifier a -- ^ Symbol already defined (E070)
  | EExpressionNotConstant -- ^ Expression not constant (E071)
  -- | Not Function found
  | EFunctionNotFound Identifier
  -- | Record missing field^
  | EFieldMissing [Identifier]
  -- | Record extra fields
  | EFieldExtra [Identifier]
  -- | Field is not a fixed location
  | EFieldNotFixedLocation Identifier
  -- | Field does not have a fixed location
  | EFieldNotPort Identifier
  -- | Expecting a Enumeration when memberAccessing got
  | EMemberAccess TypeSpecifier
  | EFunctionAccessNotResource TypeSpecifier
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
  | EForIteratorWrongType TypeSpecifier
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
  | EAccessPortNotInterface TypeSpecifier
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
  | EInvalidClassFieldType TypeSpecifier
  -- | Forbidden Reference Type
  | EReferenceTy TypeSpecifier
  -- | Nested Option
  | EOptionNested 
  -- | Struct Definition
  | EStructDefNotUniqueField [Identifier]
  | EStructDefEmptyStruct Identifier
  -- | Enums Definition 
  | EEnumDefEmpty Identifier
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
  | ETypeNotReference TypeSpecifier
  -- Internal Unbox
  | EUnBoxExpression
  -- Match
  | EMatchNotEnum Identifier
  | EMatchWrongType TypeSpecifier
  | EMatchOptionBadArgs
  | EMatchOptionBadSome
  | EMatchOptionBad
  | EMatchCaseInternalError
  | EMatchCaseBadName Identifier Identifier
  | EMatchExtraCases
  -- Enum variant expressions
  | ETyNotEnumFound Identifier
  | EEnumVariantNotFound Identifier
  | EEnumVariantExtraParams
  | EEnumVariantMissingParams
  | ETyNotEnum Identifier (SemanTypeDef a)
  | EIsVariantNotEnum TypeSpecifier
  | EIsVariantNotOption TypeSpecifier
  | EIsVariantEnumMismatch Identifier Identifier
  | EIsVariantNotFound Identifier
  -- | Unexpected Global element unboxing.
  | EInternalNoGTY
  -- | MsgQueue operations errors
  | EMsgQueueWrongProcedure Identifier
  | ENoMsgQueueSendWrongArgs
  | ENoMsgQueueRcvWrongArgs
  | EMsgQueueSendArgNotObject
  | EMsgQueueSendArgNotRefMutResult TypeSpecifier
  | EMsgQueueSendArgNotRefImmTimeout TypeSpecifier
  | EMsgQueueWrongType TypeSpecifier TypeSpecifier
  | EMsgQueueRcvWrongArgTy TypeSpecifier
  deriving Show

type SemanticErrors = AnnotatedError (Error Location) Location

instance Annotated (AnnotatedError (Error Location)) where
  getAnnotation (AnnotatedError _err ann) = ann