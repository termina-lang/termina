-- | Module where ASTs are defined.

module AST where

-- | Annotated AST
data AnnASTElement a
  -- | A task takes an `Identifier`, at least one parameter, a type and its body
  = Task Identifier [Parameter] TypeSpecifier [Statement a] (Statement a) [ a ]
  | Function Identifier [Parameter] TypeSpecifier [Statement a] (Statement a) [ a ]
  | Handler Identifier [Parameter] [Statement a] (Statement a) [ a ]
  | GlobalDeclaration (Global a)
  | ModuleInclusion Identifier [ a ]
  deriving Show

-- | Identifiers as `String`
type Identifier = String
-- | Addresses as `String`
type Address = Integer
-- | General type specifier
data TypeSpecifier
  = UInt8 | UInt16 | UInt32 | UInt64
  | Int8 | Int16 | Int32 | Int64
  | Bool | Char | DefinedType Identifier 
  | Vector TypeSpecifier Expression
  | MsgQueue TypeSpecifier Expression
  | Pool TypeSpecifier Expression
  deriving Show

data Op
  = Reference
  | MemberAccess
  | Multiplication
  | Division
  | Addition
  | Substraction
  | BitwiseLeftShift
  | BitwiseRightShift
  | RelationalLT
  | RelationalLTE
  | RelationalGT
  | RelationalGTE
  | RelationalEqual
  | RelationalNotEqual
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | LogicalAnd
  | LogicalOr
  deriving Show

data Expression
  = Variable Identifier
  | Constant Const
  | BinOp Op Expression Expression
  | ReferenceExpression Expression
  | FunctionExpression Expression [Expression]
  | FieldValuesAssignmentsExpression [FieldValueAssignment]
  | VectorIndexExpression Expression Expression
  | VectorInitExpression Expression Expression
  deriving Show

----------------------------------------
-- | Datatype representing Global Declarations. 
-- There are three types of global declarations:
-- - volatile
-- - static
-- - protected
data Global a
  = Volatile Identifier TypeSpecifier Address [ a ]
  | Static Identifier TypeSpecifier (Maybe Expression) [ a ]
  | Protected Identifier TypeSpecifier (Maybe Expression) [ a ]
  | Const Identifier TypeSpecifier Expression [ a ]
  deriving Show

----------------------------------------
data Parameter = Parameter {
  paramIdentifier :: Identifier
  , paramTypeSpecifier :: TypeSpecifier
} deriving Show

data FieldValueAssignment = FieldValueAssignment {
  fieldAssigIdentifier :: Identifier
  , fieldAssigExpression :: Expression
} deriving Show

data MatchCase a =
  MatchCase Expression [ Statement a ] [ a ]
  deriving Show

data ElseIf a =
  ElseIf Expression [ Statement a ] [ a ]
  deriving Show

data Statement a =
  Declaration Identifier TypeSpecifier (Maybe Expression) [ a ]
  | AssignmentStmt Expression Expression [ a ]
  | IfElseStmt Expression [ Statement a ] [ ElseIf a ] [ Statement a ] [ a ]
  | MatchStatement Expression [ MatchCase a ] [ a ]
  -- | For loop
  | ForLoopStmt Expression Expression [ Statement a ] [ a ]
  | SingleExpStmt Expression [ a ]
  | ReturnStmt (Maybe Expression) [ a ]
  deriving Show

-- | Constant values:
-- - Booleans
-- - Decimal integers
-- - Characters
-- - String literals
data Const = B Bool | I Int | C Char | S String
  deriving Show

type AnnotatedProgram a = [AnnASTElement a]
type Block a = [Statement a]

-- When annotations are just `()` we get a normal ASTs and Programs
type AST = AnnASTElement ()
type Program = AnnotatedProgram ()
