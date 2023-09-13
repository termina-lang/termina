-- | Module Encapsulating Semantic Errors

module Semantic.Errors where

-- Termina AST
import AST
-- import SemanAST as SAST
import Semantic.Types
-- import qualified Parsing as Parser (Annotation)

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
  | ECasteable TypeSpecifier TypeSpecifier
  -- | Expected Numeric Types
  | ENumTs [TypeSpecifier]
  -- | Reference of a global type?
  | EReferenceGlobal Identifier
  -- | Not Variable found
  | ENotVar
  | ENotNamedVar Identifier
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
  -- | Expecting a Vecotor got
  | EVector TypeSpecifier
  -- | Expecting a Enumeration when memberAccessing got
  | EMemberAccess TypeSpecifier
  | EMethodAccessNotClass TypeSpecifier
  | EMemberAccessNotMember Identifier -- TODO: We can return the list of identifiers.
  | EMemberAccessNotMethod Identifier
  | EMemberAccessUDef (SemanTypeDef a)
  | EMemberMethodUDef (SemanTypeDef a)
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
  -- | Vectors with dynamic length
  | EVectorConst ConstExpression
  -- | Not an integer const
  | ENotIntConst Const
  | EConstantOutRange Const
  -- | PM Enum errors
  | EMCMissingEnum Identifier
  | EMCMoreArgs [Identifier]
  | EMCMissingArgs [TypeSpecifier]
  | EMCEmpty
  -- | ForLoop
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
  | EUsedTypeName Identifier
  -- | Unique names for Global
  | EUsedGlobalName Identifier
  -- | Unique names Handler
  | EUsedHandlerName Identifier
  -- | Unique names Taks
  | EUsedTaskName Identifier
  -- | Vector Type Primitive
  -- | ENoPrimitiveType TypeSpecifier
  -- | Only option Dyn
  | EOptionDyn TypeSpecifier
  -- | Dynamic a non primitive type
  | EDynPrim TypeSpecifier
  -- | Function Declaration error,
  | EUsedFunName Identifier
  -- | Error getting type of expressions of objects.
  | EUnboxingObjectExpr
  -- | Expected Simple Type
  | EExpectedSimple TypeSpecifier
  -- | Forbidden Reference Type
  | EReferenceTy TypeSpecifier
  -- | Complex expression on LHS
  | ELHSComplex
  -- | Struct Definition
  | EStructDefNotUniqueField [Identifier]
  | EStructDefEmptyStruct Identifier
  -- | Union Definition
  | EUnionDefEmptyUnion Identifier
  | EUnionDefNotUniqueField [Identifier]
  -- | Enums Definition
  | EEnumDefEmpty Identifier
  | EEnumDefNotUniqueField [Identifier]
  -- | Class Definition
  | EClassEmptyMethods Identifier
  | ENotClassField Identifier
  | ClassSelfNoSelf
  -- Dereference Object
  | ETypeNotReference TypeSpecifier
  -- Error while forcing Undyn
  | EUndynForcingError
  -- Error using a method different than alloc on a pool
  | EPoolsMethods Identifier
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
  -- | Unexpected Global element unboxing.
  | EInternalNoGTY

  deriving Show

instance Eq (Errors a) where
  (ENotNamedVar idls) == (ENotNamedVar idrs) = idls == idrs
  _ == _ = False


withError :: MonadError e m => (e -> e) -> m a -> m a
withError = flip catchError . (throwError .)
----------------------------------------

data AnnotatedErrors a = AnnError {semError :: Errors a , annError :: a }
  deriving Show

annotateError :: a -> Errors a -> AnnotatedErrors a
annotateError = flip AnnError
