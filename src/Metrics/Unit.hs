-- | The structural measures of the units of a module, taken on the typed AST:
-- statements, logical size, cyclomatic complexity, nesting and parameters. A
-- unit is a function or a method, procedure, viewer, action or case of a
-- class.
module Metrics.Unit
  ( UnitKind(..)
  , Unit(..)
  , unitsOf
  , declarationsOf
  , moduleLsloc
  ) where

import Text.Parsec.Pos (sourceColumn, sourceLine)

import Core.Tree (childExpressionsOf, indexExpressionsOf)
import Metrics.Lines (CodeStarts, codeStarts, lastCodeLine)
import Parser.Types (ParserAnn(..))
import Semantic.AST
import Semantic.Types (SemanticAnn(..))
import Utils.Annotations

data UnitKind = FunctionUnit | MethodUnit | ProcedureUnit | ViewerUnit | ActionUnit | CaseUnit
  deriving (Show, Eq)

-- | The measures of a unit. The lines are those of its declaration, extended
-- upwards over the documentation comment attached to it.
data Unit = Unit
  { unitClass :: Maybe Identifier
  , unitName :: Identifier
  , unitKind :: UnitKind
  , unitFirstLine :: Int
  , unitLastLine :: Int
  , unitStatements :: Int
  , unitLsloc :: Int
  , unitStatementComplexity :: Int
  , unitExpressionComplexity :: Int
  , unitCyclomatic :: Int
  , unitNesting :: Int
  , unitParameters :: Int
  } deriving (Show, Eq)

-- | The units of a module, given its text, in source order.
unitsOf :: String -> AnnotatedProgram SemanticAnn -> [Unit]
unitsOf text = concatMap topLevel

  where

    mk = unit (codeStarts text)

    topLevel el =
      case el of
        Function name params _ body _ ann -> [mk Nothing name FunctionUnit (length params) body ann]
        TypeDefinition (Class _ cls members _ _) _ -> concatMap (member cls) members
        _ -> []

    member cls m =
      case m of
        ClassField _ -> []
        ClassMethod _ name params _ body ann -> [mk (Just cls) name MethodUnit (length params) body ann]
        ClassProcedure _ name params body ann -> [mk (Just cls) name ProcedureUnit (length params) body ann]
        ClassViewer name params _ body ann -> [mk (Just cls) name ViewerUnit (length params) body ann]
        ClassAction _ name param body ann -> [mk (Just cls) name ActionUnit (maybe 0 (const 1) param) body ann]
        ClassCase _ name params body ann -> [mk (Just cls) name CaseUnit (length params) body ann]

unit :: CodeStarts -> Maybe Identifier -> Identifier -> UnitKind -> Int -> Block a -> SemanticAnn -> Unit
unit starts cls name kind params body ann =
  Unit
    { unitClass = cls
    , unitName = name
    , unitKind = kind
    , unitFirstLine = firstLine
    , unitLastLine = lastLine
    , unitStatements = statements
    , unitLsloc = 1 + statements
    , unitStatementComplexity = decisions
    , unitExpressionComplexity = conditions
    , unitCyclomatic = 1 + decisions + conditions
    , unitNesting = blockNesting body
    , unitParameters = params
    }

  where

    (firstLine, lastLine) = unitLines starts ann
    statements = walk (const 1) body
    decisions = walk decisionsOf body
    conditions = walk (sum . map logicalOperators . expressionsOf) body

-- | The first and last lines of an element, the first moved up to its
-- documentation comment, if any, and the last brought back from the end that
-- the parser records to the closing brace.
unitLines :: CodeStarts -> SemanticAnn -> (Int, Int)
unitLines starts (SemanticAnn _ (ParserAnn loc doc)) =
  case loc of
    Position _ start end ->
      (minimum (sourceLine start : docLines), lastCodeLine starts (sourceLine end) (sourceColumn end))
    _ -> (0, 0)

  where

    docLines =
      case fmap docLocation doc of
        Just (Position _ start _) -> [sourceLine start]
        _ -> []

-- | The sum of a measure over every statement of a block, nested ones
-- included.
walk :: (Statement a -> Int) -> Block a -> Int
walk measure = sum . map statement . blockBody

  where

    statement stmt = measure stmt + sum (map (walk measure) (blocksOf stmt))

-- | The blocks directly nested in a statement.
blocksOf :: Statement a -> [Block a]
blocksOf stmt =
  case stmt of
    IfElseStmt ifc elifs els _ ->
      condIfBody ifc : map condElseIfBody elifs ++ maybe [] ((: []) . condElseBody) els
    ForLoopStmt _ _ _ _ _ body _ -> [body]
    MatchStmt _ cases dflt _ ->
      map matchBody cases ++ maybe [] (\(DefaultCase body _) -> [body]) dflt
    _ -> []

-- | The decisions a statement adds by itself, without those of its blocks: one
-- per condition of an @if@ chain, one for a @for@ and one for its @while@
-- clause, and one per case of a @match@ but the last, the @default@ counted as
-- a case.
decisionsOf :: Statement a -> Int
decisionsOf stmt =
  case stmt of
    IfElseStmt _ elifs _ _ -> 1 + length elifs
    ForLoopStmt _ _ _ _ cond _ _ -> 1 + maybe 0 (const 1) cond
    MatchStmt _ cases dflt _ -> length cases - 1 + maybe 0 (const 1) dflt
    _ -> 0

-- | The expressions a statement evaluates by itself, without those of its
-- blocks.
expressionsOf :: Statement a -> [Expression a]
expressionsOf stmt =
  case stmt of
    Declaration _ _ _ initial _ -> maybe [] (: []) initial
    AssignmentStmt obj expr _ -> indexExpressionsOf semanticTree obj ++ [expr]
    IfElseStmt ifc elifs _ _ -> condIfCond ifc : map condElseIfCond elifs
    ForLoopStmt _ _ from to cond _ _ -> from : to : maybe [] (: []) cond
    MatchStmt expr _ _ _ -> [expr]
    SingleExpStmt expr _ -> [expr]
    DiscardStmt expr _ -> [expr]
    ReturnStmt expr _ -> maybe [] (: []) expr
    ContinueStmt expr _ -> [expr]
    RebootStmt _ -> []

-- | The @&&@ and @||@ operators of an expression.
logicalOperators :: Expression a -> Int
logicalOperators expr = here + sum (map logicalOperators (childExpressionsOf semanticTree expr))

  where

    here =
      case expr of
        BinOp LogicalAnd _ _ _ -> 1
        BinOp LogicalOr _ _ _ -> 1
        _ -> 0

-- | The deepest nesting of @if@, @for@ and @match@ in a block. The branches of
-- an @if@ chain are at the same level.
blockNesting :: Block a -> Int
blockNesting = maximum . (0 :) . map statement . blockBody

  where

    statement stmt =
      case stmt of
        IfElseStmt {} -> nested stmt
        ForLoopStmt {} -> nested stmt
        MatchStmt {} -> nested stmt
        _ -> 0

    nested stmt = 1 + maximum (0 : map blockNesting (blocksOf stmt))

-- | The named declarations of a module outside its units, each counting one
-- logical line: a function declared in C, a global or constant, a struct and
-- each field, an enum, an interface and each of its procedures, and a class
-- and each of its fields and ports. The variants of an enum count zero.
declarationsOf :: AnnotatedProgram a -> Int
declarationsOf = sum . map topLevel

  where

    topLevel el =
      case el of
        Function {} -> 0
        ExternFunction {} -> 1
        GlobalDeclaration _ -> 1
        TypeDefinition def _ -> typeDef def

    typeDef def =
      case def of
        Struct _ fields _ -> 1 + length fields
        Enum {} -> 1
        Interface _ _ _ members _ -> 1 + length members
        Class _ _ members _ _ -> 1 + length [ () | ClassField _ <- members ]

-- | The logical size of a module: its declarations and the logical size of
-- each of its units.
moduleLsloc :: AnnotatedProgram a -> [Unit] -> Int
moduleLsloc program units = declarationsOf program + sum (map unitLsloc units)
