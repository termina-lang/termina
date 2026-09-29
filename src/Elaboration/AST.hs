{-# LANGUAGE DeriveFunctor  #-}

-- | The elaborated AST: the lowered AST with the run-time checks decided.
--
-- Array accesses, array slices and binary operations come in a checked form,
-- which the generator emits with the check of the OSAL, and an unchecked form,
-- which it emits as the plain C operation. The other constructs and the types
-- are those of the semantic AST.
module Elaboration.AST (
  module Elaboration.AST,
  module Core.AST,
  module BasicBlocks,
  TerminaType
) where

import Core.AST
import Core.Tree
import BasicBlocks
import Semantic.AST (TerminaType)
import Utils.Annotations

data Object a
  = Variable Identifier a
  -- | An array access whose index goes through the bounds check.
  | CheckedArrayIndex (Object a) (Expression a) a
  -- | An array access whose index is known to fall inside the array.
  | UncheckedArrayIndex (Object a) (Expression a) a
  | MemberAccess (Object a) Identifier a
  | Dereference (Object a) a
  | DereferenceMemberAccess (Object a) Identifier a
  | Unbox (Object a) a
  deriving (Show, Functor)

data Expression a
  = AccessObject (Object a)
  | Constant (Const' TerminaType a) a
  -- | A binary operation emitted with the check of its operator: the amount of
  -- a shift, the result of a signed operation or the divisor of an unsigned
  -- division or remainder.
  | CheckedBinOp Op (Expression a) (Expression a) a
  -- | A binary operation emitted as the C operator alone.
  | UncheckedBinOp Op (Expression a) (Expression a) a
  | ReferenceExpression AccessKind (Object a) a
  | Casting (Expression a) (TerminaType a) a
  | FunctionCall Identifier [Expression a] a
  | MemberFunctionCall (Object a) Identifier [Expression a] a
  | DerefMemberFunctionCall (Object a) Identifier [Expression a] a
  | ArrayInitializer (Expression a) (Expression a) a
  | ArrayExprListInitializer [Expression a] a
  | StructInitializer [FieldAssignment' Expression a] a
  | EnumVariantInitializer Identifier Identifier [Expression a] a
  | MonadicVariantInitializer (MonadicVariant' Expression a) a
  | StringInitializer String a
  | IsEnumVariantExpression (Object a) Identifier Identifier a
  | IsMonadicVariantExpression (Object a) MonadicVariantLabel a
  -- | An array slice whose bounds go through the slice check.
  | CheckedArraySlice AccessKind (Object a) (Expression a) (Expression a) a
  -- | An array slice whose bounds are known to fall inside the array.
  | UncheckedArraySlice AccessKind (Object a) (Expression a) (Expression a) a
  deriving (Show, Functor)

-- | How the passes read the expressions and the objects of this AST.
elaboratedTree :: Tree Expression Object a
elaboratedTree = Tree children node

  where

    children e = case e of
      AccessObject obj -> [ChildObject obj]
      Constant {} -> []
      CheckedBinOp _ left right _ -> [ChildExpr left, ChildExpr right]
      UncheckedBinOp _ left right _ -> [ChildExpr left, ChildExpr right]
      ReferenceExpression ak obj _ -> [ChildReference ak obj]
      Casting inner _ _ -> [ChildExpr inner]
      IsEnumVariantExpression obj _ _ _ -> [ChildObject obj]
      IsMonadicVariantExpression obj _ _ -> [ChildObject obj]
      CheckedArraySlice ak obj lower upper _ ->
        [ChildReference ak obj, ChildExpr lower, ChildExpr upper]
      UncheckedArraySlice ak obj lower upper _ ->
        [ChildReference ak obj, ChildExpr lower, ChildExpr upper]
      MemberFunctionCall obj _ args _ -> ChildObject obj : map ChildArg args
      DerefMemberFunctionCall obj _ args _ -> ChildObject obj : map ChildArg args
      FunctionCall _ args _ -> map ChildArg args
      ArrayInitializer inner size _ -> [ChildExpr inner, ChildConstExpr size]
      ArrayExprListInitializer exprs _ -> map ChildExpr exprs
      StructInitializer fields _ -> concatMap fieldAssignmentChildren fields
      EnumVariantInitializer _ _ args _ -> map ChildExpr args
      MonadicVariantInitializer variant _ -> map ChildExpr (monadicVariantExprs variant)
      StringInitializer {} -> []

    node obj = case obj of
      Variable ident ann -> RootNode ident ann
      CheckedArrayIndex inner index _ -> IndexNode inner index
      UncheckedArrayIndex inner index _ -> IndexNode inner index
      MemberAccess inner ident _ -> FieldNode inner ident
      DereferenceMemberAccess inner ident _ -> DerefFieldNode inner ident
      Dereference inner _ -> DerefNode inner
      Unbox inner _ -> UnboxNode inner

-- | The operator and the operands of a binary operation, checked or not.
binaryOperation :: Expression a -> Maybe (Op, Expression a, Expression a)
binaryOperation (CheckedBinOp op left right _) = Just (op, left, right)
binaryOperation (UncheckedBinOp op left right _) = Just (op, left, right)
binaryOperation _ = Nothing

instance Annotated Object where
  getAnnotation (Variable _ a)                  = a
  getAnnotation (CheckedArrayIndex _ _ a)       = a
  getAnnotation (UncheckedArrayIndex _ _ a)     = a
  getAnnotation (MemberAccess _ _ a)            = a
  getAnnotation (Dereference _ a)               = a
  getAnnotation (DereferenceMemberAccess _ _ a) = a
  getAnnotation (Unbox _ a)                     = a

  updateAnnotation (Variable n _) = Variable n
  updateAnnotation (CheckedArrayIndex obj e _) = CheckedArrayIndex obj e
  updateAnnotation (UncheckedArrayIndex obj e _) = UncheckedArrayIndex obj e
  updateAnnotation (MemberAccess obj n _) = MemberAccess obj n
  updateAnnotation (Dereference obj _) = Dereference obj
  updateAnnotation (DereferenceMemberAccess obj n _) = DereferenceMemberAccess obj n
  updateAnnotation (Unbox obj _) = Unbox obj

instance Annotated Expression where
  getAnnotation (AccessObject obj)                = getAnnotation obj
  getAnnotation (Constant _ a)                    = a
  getAnnotation (CheckedBinOp _ _ _ a)            = a
  getAnnotation (UncheckedBinOp _ _ _ a)          = a
  getAnnotation (ReferenceExpression _ _ a)       = a
  getAnnotation (Casting _ _ a)                   = a
  getAnnotation (FunctionCall _ _ a)              = a
  getAnnotation (StructInitializer _ a)           = a
  getAnnotation (EnumVariantInitializer _ _ _ a)  = a
  getAnnotation (ArrayInitializer _ _ a)          = a
  getAnnotation (ArrayExprListInitializer _ a)    = a
  getAnnotation (MonadicVariantInitializer _ a)   = a
  getAnnotation (MemberFunctionCall _ _ _ a)      = a
  getAnnotation (DerefMemberFunctionCall _ _ _ a) = a
  getAnnotation (IsEnumVariantExpression _ _ _ a) = a
  getAnnotation (IsMonadicVariantExpression _ _ a) = a
  getAnnotation (CheckedArraySlice _ _ _ _ a)     = a
  getAnnotation (UncheckedArraySlice _ _ _ _ a)   = a
  getAnnotation (StringInitializer _ a)           = a

  updateAnnotation (AccessObject obj) = AccessObject . updateAnnotation obj
  updateAnnotation (Constant c _) = Constant c
  updateAnnotation (CheckedBinOp op e1 e2 _) = CheckedBinOp op e1 e2
  updateAnnotation (UncheckedBinOp op e1 e2 _) = UncheckedBinOp op e1 e2
  updateAnnotation (ReferenceExpression ak obj _) = ReferenceExpression ak obj
  updateAnnotation (Casting e ty _) = Casting e ty
  updateAnnotation (FunctionCall f es _) = FunctionCall f es
  updateAnnotation (StructInitializer fs _) = StructInitializer fs
  updateAnnotation (EnumVariantInitializer id1 id2 es _) = EnumVariantInitializer id1 id2 es
  updateAnnotation (ArrayInitializer e s _) = ArrayInitializer e s
  updateAnnotation (ArrayExprListInitializer es _) = ArrayExprListInitializer es
  updateAnnotation (MonadicVariantInitializer ov _) = MonadicVariantInitializer ov
  updateAnnotation (MemberFunctionCall obj f es _) = MemberFunctionCall obj f es
  updateAnnotation (DerefMemberFunctionCall obj f es _) = DerefMemberFunctionCall obj f es
  updateAnnotation (IsEnumVariantExpression obj id1 id2 _) = IsEnumVariantExpression obj id1 id2
  updateAnnotation (IsMonadicVariantExpression obj v _) = IsMonadicVariantExpression obj v
  updateAnnotation (CheckedArraySlice ak obj e1 e2 _) = CheckedArraySlice ak obj e1 e2
  updateAnnotation (UncheckedArraySlice ak obj e1 e2 _) = UncheckedArraySlice ak obj e1 e2
  updateAnnotation (StringInitializer s _) = StringInitializer s

type Const = Const' TerminaType
type Parameter = Parameter' TerminaType
type Modifier = Modifier' Expression
type FieldDefinition = FieldDefinition' TerminaType
type EnumVariant = EnumVariant' TerminaType
type InterfaceMember = InterfaceMember' TerminaType Expression
type FieldAssignment = FieldAssignment' Expression
type MonadicVariant = MonadicVariant' Expression

type MatchCase = MatchCase' TerminaType Expression Object
type DefaultCase = DefaultCase' TerminaType Expression Object
type CondIf = CondIf' TerminaType Expression Object
type CondElse = CondElse' TerminaType Expression Object
type CondElseIf = CondElseIf' TerminaType Expression Object
type Statement = Statement' TerminaType Expression Object
type BasicBlock = BasicBlock' TerminaType Expression Object
type Block = Block' TerminaType Expression Object

type AnnASTElement = AnnASTElement' TerminaType Expression Block
type Global = Global' TerminaType Expression
type TypeDef = TypeDef' TerminaType Expression Block
type ClassMember = ClassMember' TerminaType Block
type AnnotatedProgram a = [AnnASTElement' TerminaType Expression Block a]
