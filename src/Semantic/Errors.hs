-- | Module Encapsulating Semantic Errors

module Semantic.Errors where

-- Termina AST
import AST
import Semantic.Types
-- import qualified Parsing as Parser (Annotation)

import           Control.Monad.Except       (MonadError (..))

----------------------------------------
-- Error Handling
----------------------------------------
data Errors a
  -- | Expected /similar/ types?
  = EMismatch TypeSpecifier TypeSpecifier
  | EMismatchIdNotEnum Identifier (TypeDef a)
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
  | ENoTyFound Identifier a
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
  | ETyNotStruct Identifier (TypeDef a)
  -- | Record missing fields
  | EFieldMissing [Identifier]
  -- | Record extra fields
  | EFieldExtra [Identifier]
  -- | Expecting a Vecotor got
  | EVector TypeSpecifier
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
  -- | Vector Type Primitive
  | ENoPrimitiveType TypeSpecifier
  -- | Only option Dyn
  | EOptionDyn TypeSpecifier
  -- | Dynamic a non primitive type
  | EDynPrim TypeSpecifier
  -- | Function Declaration error,
  | EUsedFunName Identifier
  deriving Show

withError :: MonadError e m => (e -> e) -> m a -> m a
withError = flip catchError . (throwError .)
----------------------------------------

data AnnotatedErrors a = AnnError {semError :: Errors a , annError :: a }

annotateError :: a -> Errors a -> AnnotatedErrors a
annotateError = flip AnnError
