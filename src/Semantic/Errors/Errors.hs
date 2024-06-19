-- | Module Encapsulating Semantic Errors

module Semantic.Errors.Errors where

-- Termina AST
import AST.Parser
-- import SemanAST as SAST
import Semantic.Types
import Parser.Parsing (Annotation)

import           Control.Monad.Except       (MonadError (..))

----------------------------------------
-- Error Handling
----------------------------------------
data Errors a
  -- | Expected /similar/ types?
  = 
    EMismatch TypeSpecifier TypeSpecifier -- ^ Type mismatch (Internal)
  | ENoTyFound Identifier -- ^ Type not found (Internal)
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
  | EResourceClassNoProvides Identifier -- ^ Resource class does not provide any interface (E011)
  | EResourceClassAction (Identifier, a) Identifier -- ^ Resource class defines an action (E012)
  | EResourceClassInPort (Identifier, a) Identifier -- ^ Resource class defines an in port (E013)
  | EResourceClassOutPort (Identifier, a) Identifier -- ^ Resource class defines an out port (E014)
  | EInterfaceNotFound Identifier -- ^ Interface not found (E015)
  | EMismatchIdNotInterface Identifier -- ^ The type is not an interface (E016)
  | EProcedureNotFromProvidedInterfaces (Identifier, a) Identifier -- ^ Procedure not from provided interfaces (E017)
  | EMissingProcedure Identifier Identifier -- ^ Missing procedure (E018)
  | EProcedureExtraParams (Identifier, Identifier, [Parameter], a) Integer -- ^ Extra parameters in procedure definition (E019)
  | EProcedureMissingParams (Identifier, Identifier, [Parameter], a) Integer -- ^ Missing parameters in procedure definition (E020)
  | EProcedureParamTypeMismatch (Identifier, Identifier, Parameter, a) TypeSpecifier -- ^ Parameter type mismatch in procedure definition (E021)
  | EIfElseIfCondNotBool TypeSpecifier -- ^ If-else-if condition is not a boolean (E022)
  | EFunctionCallExtraParams (Identifier, [Parameter], a) Integer -- ^ Extra parameters in function call (E023)
  | EFunctionCallMissingParams (Identifier, [Parameter], a) Integer -- ^ Missing parameters in function call (E024)
  | EFunctionCallParamTypeMismatch (Identifier, Parameter, a) TypeSpecifier -- ^ Parameter type mismatch in function call (E025)
  | EMemberAccessNotFunction Identifier -- ^ Access to a member that is not a function (E026)
  | EMutableReferenceToImmutable -- ^ Mutable reference to immutable object (E027)
  | EBinOpExpectedTypeLeft Op TypeSpecifier TypeSpecifier -- ^ Binary operation expected type on the left (E028)
  | EBinOpExpectedTypeRight Op TypeSpecifier TypeSpecifier -- ^ Binary operation expected type on the right (E029)
  | EBinOpTypeMismatch Op TypeSpecifier TypeSpecifier -- ^ Binary operation type mismatch (E030)
  | EBinOpExpectedTypeNotBool Op TypeSpecifier -- ^ Binary operation expected result type not boolean (E031)
  | EBinOpLeftTypeNotBool Op TypeSpecifier -- ^ Binary operation expected boolean type on the left (E032)
  | EBinOpRightTypeNotBool Op TypeSpecifier -- ^ Binary operation expected boolean type on the right (E033)
  | EBinOpExpectedTypeNotNum Op TypeSpecifier -- ^ Binary operation expected result type not numeric (E034)
  | EBinOpLeftTypeNotNum Op TypeSpecifier -- ^ Binary operation expected numeric type on the left (E035)
  | EBinOpRightTypeNotNum Op TypeSpecifier -- ^ Binary operation expected numeric type on the right (E036)
  | EBinOpRightTypeNotPos Op TypeSpecifier -- ^ Binary operation expected positive type on the right (E037)
  | EBinOpLeftTypeNotEquatable Op TypeSpecifier -- ^ Binary operation expected equatable type on the left (E038)
  | EBinOpRightTypeNotEquatable Op TypeSpecifier -- ^ Binary operation expected equatable type on the right (E039)
  | EMismatchIdNotEnum Identifier (SemanTypeDef a)
  | EMismatchDyn TypeSpecifier TypeSpecifier
  | EConstantWithoutKnownType Const
  | EReturnValueExpected TypeSpecifier
  | EReturnValueNotVoid 
  | EExpectedType
  -- | Expected Numeric Types
  | ENumTs [TypeSpecifier]
  | EUSizeTs TypeSpecifier
  -- | Reference of a global type?
  | EReferenceGlobal Identifier
  -- | Not Variable found
  | ENotVar
  -- | Invalid access to global object
  | EInvalidAccessToGlobal Identifier
  -- | Invalid writing access to read only object
  | EObjectIsReadOnly Identifier
  -- | Not Global Variable found
  | ENotNamedGlobal Identifier
  | EGlobalOtherType Identifier
  -- | Some globals cannot be assigned?
  | EGlobalNotLHS Identifier
  -- | No shadow binding
  | EVarDefined Identifier
  -- | Not Function found
  | EFunctionNotFound Identifier
  -- | TypeSpecifier Identifier is not Union/Struct
  -- Not struct type found with identifier
  | ETyNotStructFound Identifier
  -- Something was found but it is not an identifier
  | ETyNotStruct Identifier (SemanTypeDef a)
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
  | EMemberAccessNotProcedure Identifier
  | EMemberAccessUDef (SemanTypeDef a)
  | EMemberFunctionUDef (SemanTypeDef a)
  | EMemberMethodType
  | EMemberMethodExtraConstParams
  | EMemberMethodMissingConstParams
  | EMemberMethodExtraParams
  | EMemberMethodMissingParams
  -- | Pattern Matching Missing cases
  | EPMMissingOption0 -- Missing None
  | EPMMissingOption1 -- Missing Some
  | EPMMoreOptions -- More cases??
  | EPMMoreOptionsVariables -- More Variable in enum Some(a,b,c,d..)
  -- | Global Object but not a type
  | EGlobalNoType Identifier
  -- | Array with malformed size 
  | EArrayConst Size
  -- | Not an integer const
  | ENotIntConst Const
  | EConstantOutRange Const
  -- | PM Enum errors
  | EMCMissingEnum Identifier
  | EMCMoreArgs [Identifier]
  | EMCMissingArgs [TypeSpecifier]
  | EMCEmpty
  -- | ForLoop
  | EForIteratorWrongType TypeSpecifier
  | EForWhileTy TypeSpecifier -- ^ Type of while is not Bool
  -- | Defined GEntry
  | EDefinedGEntry (GEntry a)
  -- | Impossible Cases. Internal Transpiler errors
  | EUnboxingStmtExpr -- Unboxing statement as an expression.
  | EUnboxingBlockRet -- Unboxing Blockret statement
  -- | Unique names for types.
  | EUsedTypeName Identifier a
  -- | Unique names for Global
  | EUsedGlobalName Identifier a
  -- | Unique names Handler
  | EUsedHandlerName Identifier
  -- | Unique names Taks
  | EUsedTaskName Identifier
  -- | Array Type Primitive
  -- | ENoPrimitiveType TypeSpecifier
  -- | Only option Dyn
  | EOptionDyn TypeSpecifier
  -- | Access port does not have an Interface type
  | EAccessPortNotInterface TypeSpecifier
  | EAccessPortNotResource Identifier 
  | EInboundPortNotEmitter Identifier 
  | EInboundPortNotChannel Identifier
  | EOutboundPortNotChannel Identifier
  | EAccessPortNotPool Identifier
  -- | Dynamic a non primitive type
  | EDynPrim TypeSpecifier
  -- | Dynamic (type has a Dynamic inside) as Argument of a function
  | EConstParameterNotNum Parameter
  -- | Function Declaration error,
  | EUsedFunName Identifier a
  -- | Error getting type of expressions of objects.
  | EUnboxingObjectExpr
  | EUnboxingConstExpr
  | EUnboxingLocalEnvKind
  -- | Expected Simple Type
  | EExpectedSimple TypeSpecifier
  -- | Invalid class field type
  | EInvalidClassFieldType TypeSpecifier
  -- | Forbidden Reference Type
  | EReferenceTy TypeSpecifier
  -- | Nested Option
  | EOptionNested 
  -- | Complex expression on LHS
  | ELHSComplex
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
  -- Error while forcing Undyn
  | EUndynForcingError
  | EInvalidUseOfSlice
  -- Error using a method different than alloc on a pool
  | EPoolsAllocArgs
  | EPoolsMethods Identifier
  | EPoolsWrongProcedure Identifier
  | EPoolsWrongNumArgs
  | EPoolsWrongArgType TypeSpecifier
  | EPoolsWrongArgTypeW TypeSpecifier
  -- Error when constructing an option variant expression with a non-dynamic type
  | EOptionVariantNotDynamic TypeSpecifier
  -- Internal Undyn
  | EUnDynExpression
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
  | EEnumVariantType
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
  -- | Array slicing
  | EBoundsTypeMismatch TypeSpecifier TypeSpecifier -- | Lower and upper bounds are not of the same type
  | EBoundsTypeNotUSize TypeSpecifier TypeSpecifier -- | Bounds are not of of type usize
  | EBoundsLowerGTUpper Integer Integer -- | Lower bound is greater than upper bound
  | EUpperBoundGTSize Integer Integer -- | Upper bound is greater than the size of the vector
  deriving Show

instance Eq (Errors a) where
  (ENotNamedObject idls) == (ENotNamedObject idrs) = idls == idrs
  _ == _ = False


withError :: MonadError e m => (e -> e) -> m a -> m a
withError = flip catchError . (throwError .)
----------------------------------------

data AnnotatedErrors a = AnnError {semError :: Errors a , annError :: a }
  deriving Show

annotateError :: a -> Errors a -> AnnotatedErrors a
annotateError = flip AnnError

type SemanticErrors = AnnotatedErrors Annotation
