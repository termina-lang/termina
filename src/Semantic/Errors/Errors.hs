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
  = EMismatch TypeSpecifier TypeSpecifier
  | EOpMismatch Op TypeSpecifier TypeSpecifier
  | EMismatchIdNotEnum Identifier (SemanTypeDef a)
  | EMismatchDyn TypeSpecifier TypeSpecifier
  | EExpectedNumType TypeSpecifier
  | EExpectedPosType TypeSpecifier
  | ECasteable TypeSpecifier TypeSpecifier
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
  | ENotNamedObject Identifier
  -- | Not Global Variable found
  | ENotNamedGlobal Identifier
  | EGlobalOtherType Identifier
  -- | Some globals cannot be assigned?
  | EGlobalNotLHS Identifier
  -- | No shadow binding
  | EVarDefined Identifier
  -- | Not Function found
  | ENotFoundFun Identifier (GEntry a)
  -- | Type Id not found
  | ENoTyFound Identifier
  -- | Not a function
  | ENotAFun Identifier
  -- | Parameter and argument type mismatch
  | EParam TypeSpecifier TypeSpecifier
  -- | Wrong number of params
  | EFunParams
  -- | TypeSpecifier Identifier is not Union/Struct
  -- Not struct type found with identifier
  | ETyNotStructFound Identifier
  -- Something was found but it is not an identifier
  | ETyNotStruct Identifier (SemanTypeDef a)
  -- | Record missing fields
  | EFieldMissing [Identifier]
  -- | Record extra fields
  | EFieldExtra [Identifier]
  -- | Field is not a fixed location
  | EFieldNotFixedLocation Identifier
  -- | Field does not have a fixed location
  | EFieldNotPort Identifier
  -- | Expecting a Vecotor got
  | EVector TypeSpecifier
  -- | Expecting a Enumeration when memberAccessing got
  | EMemberAccess TypeSpecifier
  | EFunctionAccessNotResource TypeSpecifier
  | EMemberAccessNotMember Identifier -- TODO: We can return the list of identifiers.
  -- | Calling a procedure within another member function
  | EMemberAccessInvalidProcedureCall Identifier
  | EMemberAccessNotProcedure Identifier
  | EMemberAccessNotFunction Identifier
  | EMemberAccessUDef (SemanTypeDef a)
  | EMemberFunctionUDef (SemanTypeDef a)
  | EMemberMethodType
  | EMemberMethodExtraParams
  | EMemberMethodMissingParams
  -- | Pattern Matching Missing cases
  | EPMMissingOption0 -- Missing None
  | EPMMissingOption1 -- Missing Some
  | EPMMoreOptions -- More cases??
  | EPMMoreOptionsVariables -- More Variable in enum Some(a,b,c,d..)
  -- | Global Object but not a type
  | EGlobalNoType Identifier
  -- | Vector with malformed size 
  | EVectorConst Size
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
  | EBadRange -- ^ Range conditions are not met
  | EForWhileTy TypeSpecifier -- ^ Type of while is not Bool
  -- | Static not literal constant
  | EStaticK
  -- | Defined GEntry
  | EDefinedGEntry (GEntry a)
  -- | Impossible Cases. Internal Transpiler errors
  | ERHSCatch
  | ELookupVar
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
  -- | Vector Type Primitive
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
  | EArgHasDyn Parameter
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
  -- | Vector slicing
  | ELowerBoundConst (ConstExpression a) -- | Lower bound is not a numeric constant expression
  | EUpperBoundConst (ConstExpression a) -- | Upper bound is not a numeric constant expression
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
