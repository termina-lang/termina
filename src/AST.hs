{-# Language DeriveFunctor #-}
-- | Module where ASTs are defined.

module AST where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

type ReturnDef a = (Maybe (Expression a), [a])

-- | |BlockRet| represent a body block with its return statement
data BlockRet a
  = BlockRet
  { blockBody :: [Statement a]
  , blockRet :: ReturnDef a
  }
  deriving (Show, Functor)

-- | Annotated AST
data AnnASTElement a
  -- | A task takes an `Identifier`,  a type and its body plus returnt value
  = Task Identifier [Parameter a] (TypeSpecifier a) (BlockRet a) [ a ]
  | Function Identifier [Parameter a] (Maybe (TypeSpecifier a)) (BlockRet a) [ a ]
  | Handler Identifier [Parameter a] (TypeSpecifier a) (BlockRet a) [ a ]
  | GlobalDeclaration (Global a)
  | TypeDefinition (TypeDef a)
  | ModuleInclusion Identifier [ a ]
  deriving (Show,Functor)

-- | Identifiers as `String`
type Identifier = String
-- | Addresses as `String`
type Address = Integer
-- | General type specifier
data TypeSpecifier a
  = UInt8 | UInt16 | UInt32 | UInt64
  | Int8 | Int16 | Int32 | Int64
  | Bool | Char | DefinedType Identifier
  | Vector (TypeSpecifier a) VectorSize -- (Expression a)
  | MsgQueue (TypeSpecifier a) (Expression a)
  | Pool (TypeSpecifier a) (Expression a)
  | Option (TypeSpecifier a)
  -- enum Option<T> {None | Some (a) }
  | Reference (TypeSpecifier a)
  | DynamicSubtype (TypeSpecifier a)
  deriving (Show, Functor)

data VectorSize = K Int | V Identifier
 deriving Show

-- Ground Type equiality?
groundTyEq :: TypeSpecifier a -> TypeSpecifier a -> Bool
groundTyEq  UInt8  UInt8 = True
groundTyEq  UInt16  UInt16 = True
groundTyEq  UInt32  UInt32 = True
groundTyEq  UInt64  UInt64 = True
groundTyEq  Int8  Int8 = True
groundTyEq  Int16  Int16 = True
groundTyEq  Int32  Int32 = True
groundTyEq  Int64  Int64 = True
groundTyEq  Bool  Bool = True
groundTyEq  (Option tyspecl) (Option tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  (Reference tyspecl) (Reference tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq (DynamicSubtype tyspecl) (DynamicSubtype tyspecr) = groundTyEq tyspecl tyspecr
groundTyEq  a  b = False

data Op
  = MemberAccess
  | Multiplication
  | Division
  | Addition
  | Substraction
  | BitwiseLeftShift
  | BitwiseRightShift
  | RelationalLT
  | RelationalLTE
  | RelationalGT
  | RelationalGTE
  | RelationalEqual
  | RelationalNotEqual
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | LogicalAnd
  | LogicalOr
  deriving Show

data Expression a
  = Variable Identifier
  | Constant (Const a)
  | BinOp Op (Expression a) (Expression a)
  | ReferenceExpression (Expression a)
  | Casting (Expression a) (TypeSpecifier a)
  | FunctionExpression Identifier [ Expression a ]
  | FieldValuesAssignmentsExpression Identifier [FieldValueAssignment a]
  | VectorIndexExpression (Expression a) (Expression a) -- Binary operation : array indexing
  | VectorInitExpression (Expression a) (Expression a) -- Vector initializer
  | MatchExpression (Expression a) [ MatchCase a ]
  deriving (Show, Functor)

----------------------------------------
-- | Datatype representing Global Declarations. 
-- There are three types of global declarations:
-- - volatile
-- - static
-- - protected
data Global a
  = Volatile Identifier (TypeSpecifier a) Address [ a ]
  | Static Identifier (TypeSpecifier a) (Maybe (Expression a)) [ a ]
  | Protected Identifier (TypeSpecifier a)  (Maybe (Expression a)) [ a ]
  | Const Identifier (TypeSpecifier a)  (Expression a) [ a ]
  deriving (Show, Functor)

data TypeDef a
  = Struct Identifier [FieldDefinition a]  [ a ]
  | Union Identifier [FieldDefinition a] [ a ]
  | Enum Identifier [EnumVariant a] [ a ]
  | Class Identifier [ClassMember a] [ a ]
  deriving (Show, Functor)

data ClassMember a
  = ClassField Identifier (TypeSpecifier a) (Maybe (Expression a)) [ a ]
  | ClassMethod Identifier [Parameter a] (Maybe (TypeSpecifier a)) (BlockRet a) [ a ]
  deriving (Show, Functor)

----------------------------------------
data Parameter a = Parameter {
  paramIdentifier :: Identifier
  , paramTypeSpecifier :: TypeSpecifier a
} deriving (Show, Functor)

data FieldValueAssignment a = FieldValueAssignment {
  fieldAssigIdentifier :: Identifier
  , fieldAssigExpression :: Expression a
} deriving (Show, Functor)

data FieldDefinition a = FieldDefinition {
  fieldIdentifier :: Identifier
  , fieldTypeSpecifier :: TypeSpecifier a
  , fieldDefaultValue :: Maybe (Expression a)
} deriving (Show, Functor)

data EnumVariant a = EnumVariant {
  variantIdentifier :: Identifier
  , assocData :: [ TypeSpecifier a ]
} deriving (Show, Functor)

data MatchCase a =
  MatchCase Identifier [Identifier] (BlockRet a) [ a ]
  deriving (Show,Functor)

data ElseIf a =
  ElseIf (Expression a) [ Statement a ] [ a ]
  deriving (Show, Functor)

data Statement a =
  Declaration Identifier (TypeSpecifier a) (Maybe (Expression a)) [ a ]
  | AssignmentStmt (Expression a) (Expression a) [ a ]
  | IfElseStmt (Expression a) [ Statement a ] [ ElseIf a ] [ Statement a ] [ a ]
  -- | For loop
  | ForLoopStmt Identifier (Expression a) (Expression a) [ Statement a ] [ a ]
  | SingleExpStmt (Expression a) [ a ]
  | ReturnStmt (ReturnDef a) [ a ]
  | Break [ a ]
  deriving (Show, Functor)

-- | Constant values:
-- - Booleans
-- - Decimal integers
-- - Characters
-- - String literals
data Const a = B Bool | I (TypeSpecifier a) Int | C Char | S String
  deriving (Show, Functor)

type AnnotatedProgram a = [AnnASTElement a]
type Block a = [Statement a]

-- When annotations are just `()` we get a normal ASTs and Programs
type AST = AnnASTElement ()
type Program = AnnotatedProgram ()

forgetAnnotations :: AnnotatedProgram a -> Program
forgetAnnotations = map (fmap (const ()))
