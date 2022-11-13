-- | Module where ASTs are defined.

module AST where

-- | Annotated AST
data AnnASTElement a
  -- | A task takes an `Identifier`, at least one parameter, a type and its body
  = Task Identifier [Parameter a] (TypeSpecifier a) [Statement a] (Statement a) [ a ]
  | Function Identifier [Parameter a] (Maybe (TypeSpecifier a)) [Statement a] (Statement a) [ a ]
  | Handler Identifier [Parameter a] (TypeSpecifier a) [Statement a] (Statement a) [ a ]
  | GlobalDeclaration (Global a)
  | TypeDefinition (TypeDef a)
  | ModuleInclusion Identifier [ a ]
  deriving Show

-- | Identifiers as `String`
type Identifier = String
-- | Addresses as `String`
type Address = Integer
-- | General type specifier
data TypeSpecifier a
  = UInt8 | UInt16 | UInt32 | UInt64
  | Int8 | Int16 | Int32 | Int64
  | Bool | Char | DefinedType Identifier
  | Vector (TypeSpecifier a) (Expression a)
  | MsgQueue (TypeSpecifier a) (Expression a)
  | Pool (TypeSpecifier a) (Expression a)
  | Option (TypeSpecifier a)
  | Reference (TypeSpecifier a)
  | DynamicSubtype (TypeSpecifier a)
  deriving Show

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
  | Constant Const
  | BinOp Op (Expression a) (Expression a)
  | ReferenceExpression (Expression a)
  | Casting (Expression a) (TypeSpecifier a)
  | FunctionExpression (Expression a) [ Expression a ]
  | FieldValuesAssignmentsExpression [FieldValueAssignment a]
  | VectorIndexExpression (Expression a) (Expression a) -- Binary operation : array indexing
  | VectorInitExpression (Expression a) (Expression a) -- Vector initializer
  | MatchExpression (Expression a) [ MatchCase a ]
  deriving Show

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
  deriving Show

data TypeDef a
  = Struct Identifier [FieldDefinition a]  [ a ]
  | Union Identifier [FieldDefinition a] [ a ]
  | Enum Identifier [EnumVariant a] [ a ]
  | Class Identifier [ClassMember a] [ a ]
  deriving Show

data ClassMember a
  = ClassField Identifier (TypeSpecifier a) (Maybe (Expression a)) [ a ]
  | ClassMethod Identifier [Parameter a] (Maybe (TypeSpecifier a)) [Statement a] (Statement a) [ a ]
  deriving Show

----------------------------------------
data Parameter a = Parameter {
  paramIdentifier :: Identifier
  , paramTypeSpecifier :: TypeSpecifier a
} deriving Show

data FieldValueAssignment a = FieldValueAssignment {
  fieldAssigIdentifier :: Identifier
  , fieldAssigExpression :: Expression a
} deriving Show

data FieldDefinition a = FieldDefinition {
  fieldIdentifier :: Identifier
  , fieldTypeSpecifier :: TypeSpecifier a
  , fieldDefaultValue :: Maybe (Expression a)
} deriving Show

data EnumVariant a = EnumVariant {
  variantIdentifier :: Identifier
  , assocData :: [ TypeSpecifier a ]
} deriving Show

data MatchCase a =
  MatchCase (Expression a) [ Statement a ] (Statement a) [ a ]
  deriving Show

data ElseIf a =
  ElseIf (Expression a) [ Statement a ] [ a ]
  deriving Show

data Statement a =
  Declaration Identifier (TypeSpecifier a) (Maybe (Expression a)) [ a ]
  | AssignmentStmt (Expression a) (Expression a) [ a ]
  | IfElseStmt (Expression a) [ Statement a ] [ ElseIf a ] [ Statement a ] [ a ]
  -- | For loop
  | ForLoopStmt Identifier (Expression a) (Expression a) [ Statement a ] [ a ]
  | SingleExpStmt (Expression a) [ a ]
  | ReturnStmt (Maybe (Expression a)) [ a ]
  deriving Show

-- | Constant values:
-- - Booleans
-- - Decimal integers
-- - Characters
-- - String literals
data Const = B Bool | I Int | C Char | S String
  deriving Show

type AnnotatedProgram a = [AnnASTElement a]
type Block a = [Statement a]

-- When annotations are just `()` we get a normal ASTs and Programs
type AST = AnnASTElement ()
type Program = AnnotatedProgram ()
