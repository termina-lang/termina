-- | Value analysis over the basic-block AST.
--
-- The pass follows what the local scalar variables of a body may hold and
-- reports a condition whose value it already knows (VAE-001), since one of the
-- paths such a condition guards is then never taken. A condition may be
-- decided by a constant of the module as much as by an assignment or by the
-- branch it sits in, so the pass is seeded with the constant environment the
-- folding builds and runs after it, apart from the rest of the basic-block
-- checks.
--
-- The walk and the finding are separate. The walk records, for every condition
-- it meets, the state of the path there, and reports nothing; the diagnostic
-- then reads those records and decides.
--
-- What a variable holds is a set of at most 'valueLimit' values or, once the
-- set has outgrown that, the interval its declared type allows; a variable
-- with no entry in the map is unknown. Where paths meet the two sets are
-- joined, and a variable falls from a set to the interval of its type and from
-- there to unknown, so the chain of a variable has a height of three and the
-- walk of a loop settles after a bounded number of turns, with no widening
-- sequence to tune.
--
-- A condition is evaluated with the evaluator of
-- 'ControlFlow.ConstFolding', which is what keeps the arithmetic of this pass
-- and the arithmetic of the folding the same one. That evaluator reads a
-- single value, so a variable known to hold one answers a lookup and a
-- variable known to hold several answers nothing. An evaluation that does not
-- succeed leaves the pass silent, which is the only direction an error of the
-- transpiler may be wrong in.
module ControlFlow.ValueAnalysis
  (
    runValueAnalysisCheck
    -- * For the tests
    --
    -- | The verdict of a comparison has two tables of six operators that no
    -- source program can reach yet, and the bound a branch puts on a variable
    -- is the one operation that makes the pass claim more, so their tests
    -- drive them from here.
  , Integers(..)
  , compareValues
  , Bound(..)
  , Values(..)
  , Value(..)
  , boundOf
  , narrow
  , without
  ) where

import qualified Data.Map.Strict as M
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S

import Configuration.Platform (Platform)
import ControlFlow.BasicBlocks.AST
import ControlFlow.BasicBlocks.Traversal
    (Child(..), expressionChildren, simpleBlockChildren, rootIdent)
import ControlFlow.ConstFolding (evalConstExpression, runConstFolding)
import ControlFlow.ConstFolding.Monad (ConstFoldEnv(..))
import ControlFlow.ValueAnalysis.Errors
import ControlFlow.Dataflow
import Core.Utils (intRange)
import Semantic.Types (SemanticAnn, getObjectSAnns)
import Utils.Annotations

-- | How many values a set may hold before it gives way to the interval of the
-- type. Raising it costs turns of the fixed point of a loop, since an
-- @x = x + 1@ adds one value per turn before it reaches the limit.
valueLimit :: Int
valueLimit = 8

-- | One of the values a variable may hold: a boolean or an integer, which is
-- what 'scalar' keeps, or one of the variants of an enumeration.
--
-- Two of them are compared by the value alone: two paths that reach the same
-- number agree whether or not it was written the same way, and the type a
-- constant carries is not part of what they agree on. A variant is held by its
-- name alone, since the type of the variable already fixes which enumeration
-- the name belongs to.
data Value =
    Scalar (Const SemanticAnn)
  | Variant Identifier
  deriving Show

instance Eq Value where
  left == right = compare left right == EQ

instance Ord Value where
  compare = comparing rank

    where

      rank (Scalar (B b)) = (0 :: Int, if b then 1 else 0 :: Integer, "")
      rank (Scalar (I (TInteger v _) _)) = (1, v, "")
      rank (Scalar _) = (2, 0, "")
      rank (Variant name) = (3, 0, name)

variantOf :: Value -> Maybe Identifier
variantOf (Variant name) = Just name
variantOf _ = Nothing

integerOf :: Value -> Maybe Integer
integerOf (Scalar (I (TInteger v _) _)) = Just v
integerOf _ = Nothing

-- | What is known of a variable at a point of a path: the values it may hold,
-- written out one by one while they are few, and as an interval with both ends
-- included once they are not.
data Values =
    Discrete (S.Set Value)
  | Interval Integer Integer
  deriving (Eq, Show)

-- | The values of a variable together with the interval its declared type
-- allows, which is where a set that outgrows 'valueLimit' lands. Taking the
-- convex hull of the set instead would let a loop that adds one at a time
-- climb to the top of the type one value per turn; going to the interval of
-- the type at once bounds the chain at three. A boolean has no interval and
-- never needs one, since two values do not reach the limit.
data Known = Known
  {
    typeRange :: Maybe (Integer, Integer)
  , varValues :: Values
    -- | The places that gave the variable what it holds, which the message
    -- points at. The walk carries them along and never reads them, so the
    -- equality below leaves them out and two states that agree on the values
    -- count as the same state; comparing them as well would spend turns of the
    -- fixed point on information nobody iterates over.
  , valueOrigins :: S.Set Origin
  }

instance Eq Known where
  left == right =
    typeRange left == typeRange right && varValues left == varValues right

-- | What belongs to the path being walked: the local variables whose value the
-- path constrains. A variable that is not here is unknown, so joining two paths
-- keeps the entries they both constrain and drops the rest.
newtype ValueAnalysisPath = ValueAnalysisPath { known :: M.Map Identifier Known }
  deriving Eq

instance Lattice ValueAnalysisPath where
  joinPath left right =
    ValueAnalysisPath (M.mapMaybe id (M.intersectionWith agree (known left) (known right)))

    where

      agree x y = do
        values <- joinValues (typeRange x) (varValues x) (varValues y)
        return (Known (typeRange x) values
                  (S.union (valueOrigins x) (valueOrigins y)))

-- | Two sets whose union stays under the limit join exactly. Past the limit
-- the union gives way to the interval of the type, and so does a join in which
-- an interval takes part and neither side contains the other; a variable whose
-- type has no interval falls to unknown instead.
joinValues :: Maybe (Integer, Integer) -> Values -> Values -> Maybe Values
joinValues range (Discrete left) (Discrete right) =
  if S.size both <= valueLimit then Just (Discrete both) else wholeType range

  where

    both = S.union left right

joinValues range left right =
  case (left `covers` right, right `covers` left) of
    (True, _) -> Just left
    (_, True) -> Just right
    _ -> wholeType range

wholeType :: Maybe (Integer, Integer) -> Maybe Values
wholeType = fmap (uncurry Interval)

covers :: Values -> Values -> Bool
covers (Interval lo hi) (Interval lo' hi') = lo <= lo' && hi' <= hi
covers (Interval lo hi) (Discrete vs) = all inside (S.toList vs)

  where

    inside value = maybe False (\v -> lo <= v && v <= hi) (integerOf value)

covers (Discrete left) (Discrete right) = right `S.isSubsetOf` left
covers (Discrete _) (Interval _ _) = False

-- | A place the walk meets that decides which path is taken, which is what
-- the diagnostic reads once the walk is over.
data Met =
    -- | A condition, which is invariant when the state determines its value.
    MetCondition (Expression SemanticAnn)
    -- | A case of a match, with what the match discriminates on and the
    -- variant of the case, which is never taken when the state rules the
    -- variant out.
  | MetCase (Expression SemanticAnn) Identifier

-- | What each function and member gives back, for a call site to read. One
-- with no entry here gives back whatever its type allows.
type ReturnedValues = M.Map Identifier Values

-- | What belongs to the program, and no branch takes back.
data ValueAnalysisGlobal = ValueAnalysisGlobal
  {
    -- | The constants the module sees, as the folding left them. They are what
    -- makes this pass answer for the case a condition is constant outright.
    moduleConsts :: M.Map Identifier (Const SemanticAnn),
    platform :: Platform,
    -- | Every place the walk has met that decides a path, with the state of
    -- the path where it met it, under its position. A loop meets the same
    -- place once per turn and each turn overwrites the one before, so what is
    -- left is what the last turn saw, which is the turn that starts from the
    -- settled state of the head.
    observed :: M.Map Location (Met, ValueAnalysisPath),
    -- | What each function and member walked so far gives back, including
    -- those of the modules this one imports.
    returnedValues :: ReturnedValues
  }

type ValueAnalysisMonad = DataflowM ValueAnalysisPath ValueAnalysisGlobal ValueAnalysisError

-- | The one value a variable holds, when the path pins it to one. A set of
-- several values and an interval answer nothing, which leaves the pass where
-- it would be with the variable unknown.
singleValue :: Known -> Maybe (Const SemanticAnn)
singleValue entry = case varValues entry of
  Discrete values | [Scalar value] <- S.toList values -> Just value
  _ -> Nothing

-- | The value of an expression, when the constants of the module and what a
-- path knows of the local variables determine it.
--
-- The evaluation is the one of the folding, seeded with the local variables
-- pinned to a single value: such a variable answers a lookup exactly as a
-- module constant does, and one that is unknown is absent, which makes the
-- evaluation fail and the caller keep quiet. The two names cannot be confused,
-- since a local may not shadow a global (SE-081).
--
-- It takes what it reads instead of getting it from the state, since the
-- diagnostic evaluates the recorded conditions once the walk is over.
evaluate ::
  Platform
  -> M.Map Identifier (Const SemanticAnn)
  -> ValueAnalysisPath
  -> Expression SemanticAnn
  -> Maybe (Const SemanticAnn)
evaluate plt consts locals expr =
  case runConstFolding env (evalConstExpression expr) of
    Left _ -> Nothing
    Right (value, _) -> Just value

  where

    env = ConstFoldEnv
      (M.union (M.mapMaybe singleValue (known locals)) consts)
      plt

valueOf :: Expression SemanticAnn -> ValueAnalysisMonad (Maybe (Const SemanticAnn))
valueOf expr = do
  global <- getGlobal
  locals <- getPath
  return (evaluate (platform global) (moduleConsts global) locals expr)

-- | The interval the type of a variable allows, which is where its set of
-- values goes once the set outgrows the limit. A type that is not an integer
-- one has none.
rangeOfType :: TerminaType SemanticAnn -> ValueAnalysisMonad (Maybe (Integer, Integer))
rangeOfType ty = do
  plt <- platform <$> getGlobal
  return (intRange plt ty)

-- | The same, for a variable whose type the walk reads from the annotation the
-- type checker left on it.
rangeOfVariable :: Object SemanticAnn -> ValueAnalysisMonad (Maybe (Integer, Integer))
rangeOfVariable obj =
  maybe (return Nothing) (rangeOfType . snd) (getObjectSAnns (getAnnotation obj))

-- | Records what a variable holds from here on. Only a boolean or an integer
-- is followed; every other value, and every expression the evaluation could
-- not determine, leaves the variable unknown.
setValue ::
  Identifier
  -> Maybe (Integer, Integer)
  -> Origin
  -> Maybe (Const SemanticAnn)
  -> ValueAnalysisMonad ()
setValue ident range origin mValue =
  setValues ident range origin
    (Discrete . S.singleton <$> (mValue >>= scalar))

-- | Records what a variable may hold from here on, which is what a call to a
-- function the walk has summarised gives back as well as what an expression
-- the folding evaluator works out does. An expression that says nothing leaves
-- the variable unknown.
setValues ::
  Identifier
  -> Maybe (Integer, Integer)
  -> Origin
  -> Maybe Values
  -> ValueAnalysisMonad ()
setValues ident range origin mValues = modifyPath $ \p -> ValueAnalysisPath $
  case mValues of
    Just values ->
      M.insert ident (Known range values (S.singleton origin)) (known p)
    Nothing -> M.delete ident (known p)

-- | The values the lattice follows, which are the booleans and the integers.
-- Every other value leaves the variable unknown.
scalar :: Const SemanticAnn -> Maybe Value
scalar value@(B _) = Just (Scalar value)
scalar value@(I _ _) = Just (Scalar value)
scalar _ = Nothing

forget :: Identifier -> ValueAnalysisMonad ()
forget ident = modifyPath (\p -> ValueAnalysisPath (M.delete ident (known p)))

-- | A variable whose address is handed out mutably is written behind the back
-- of this pass, so what was known of it no longer holds. An immutable
-- reference is not an escape, since the receiver cannot write through it.
noteEscapes :: Expression SemanticAnn -> ValueAnalysisMonad ()
noteEscapes = mapM_ escapesIn . expressionChildren

escapesIn :: Child SemanticAnn -> ValueAnalysisMonad ()
escapesIn (ChildReference Mutable obj) = forget (rootIdent obj)
escapesIn (ChildReference _ _) = return ()
escapesIn (ChildObject _) = return ()
escapesIn (ChildExpr expr) = noteEscapes expr
escapesIn (ChildArg expr) = noteEscapes expr
escapesIn (ChildConstExpr _) = return ()

-- | Takes down a condition and the state of the path that reaches it, for the
-- diagnostic to read once the walk is over. It goes to the global state, which
-- a branch does not put back, so a record outlives the branch that made it.
observeCondition :: Expression SemanticAnn -> ValueAnalysisMonad ()
observeCondition cond = do
  noteEscapes cond
  locals <- getPath
  let loc = getLocation . getAnnotation $ cond
  modifyGlobal (\g ->
    g { observed = M.insert loc (MetCondition cond, locals) (observed g) })

-- | A bound a branch puts on a variable, both ends included.
data Bound = AtLeast Integer | AtMost Integer
  deriving (Eq, Show)

satisfies :: Bound -> Integer -> Bool
satisfies (AtLeast low) value = value >= low
satisfies (AtMost high) value = value <= high

-- | What is left of the values of a variable once a bound applies to them.
-- Nothing when the bound rules out no value, and also when it would rule out
-- every value: a variable with no value left says the branch is never taken,
-- a finding this pass does not make.
narrow :: Bound -> Values -> Maybe Values
narrow bound (Discrete values) =
  if S.null kept || kept == values then Nothing else Just (Discrete kept)

  where

    kept = S.filter (maybe False (satisfies bound) . integerOf) values

narrow (AtLeast low) (Interval lo hi) =
  if raised > hi || raised <= lo then Nothing else Just (Interval raised hi)

  where

    raised = max lo low

narrow (AtMost high) (Interval lo hi) =
  if lowered < lo || lowered >= hi then Nothing else Just (Interval lo lowered)

  where

    lowered = min hi high

-- | What is left of the values of a variable once one of them is ruled out.
-- Nothing when the value was not among them, and also when it was the only
-- one: a variable with no value left says the branch is never taken, a finding
-- this pass does not make.
without :: Value -> Values -> Maybe Values
without value (Discrete values) =
  if S.notMember value values || S.size values == 1
    then Nothing
    else Just (Discrete (S.delete value values))
without _ (Interval _ _) = Nothing

-- | The bound a comparison of a variable against a value puts on the variable,
-- on the side of the branch that takes it and on the side that does not. The
-- variable is the left operand here; 'mirrored' puts it there.
boundOf :: Bool -> Op -> Integer -> Maybe Bound
boundOf True RelationalLT limit = Just (AtMost (limit - 1))
boundOf False RelationalLT limit = Just (AtLeast limit)
boundOf True RelationalLTE limit = Just (AtMost limit)
boundOf False RelationalLTE limit = Just (AtLeast (limit + 1))
boundOf True RelationalGT limit = Just (AtLeast (limit + 1))
boundOf False RelationalGT limit = Just (AtMost limit)
boundOf True RelationalGTE limit = Just (AtLeast limit)
boundOf False RelationalGTE limit = Just (AtMost (limit - 1))
boundOf _ _ _ = Nothing

-- | The comparison written the other way round, which is what a source that
-- puts the value first asks for.
mirrored :: Op -> Op
mirrored RelationalLT = RelationalGT
mirrored RelationalLTE = RelationalGTE
mirrored RelationalGT = RelationalLT
mirrored RelationalGTE = RelationalLTE
mirrored op = op

-- | What a case of a @match@ says inside its body: the object the match
-- discriminates on holds the variant of the case, which is what lets a state
-- machine be followed from one turn of its loop to the next.
enterCase ::
  Expression SemanticAnn -> MatchCase SemanticAnn -> ValueAnalysisMonad ()
enterCase discriminant (MatchCase variant bvars _ ann) = do
  -- | Taken down before the refinement below, since what says whether the case
  -- is ever taken is what the match knows on the way in, not what the case
  -- itself says.
  locals <- getPath
  modifyGlobal (\g ->
    g { observed =
          M.insert (getLocation ann) (MetCase discriminant variant, locals)
            (observed g) })
  -- | The variables a case binds are declared by the case, with a value that
  -- comes from the variant it matched.
  mapM_ forget bvars
  case discriminant of
    AccessObject obj@(Variable ident _) -> do
      range <- rangeOfVariable obj
      setValues ident range (Matched (getLocation ann))
        (Just (Discrete (S.singleton (Variant variant))))
    _ -> return ()

-- | What a condition says about the variables in it inside the branch it
-- guards. A boolean variable holds the value that took the path there; a
-- comparison against a determined value fixes the variable at that value, or
-- bounds it when the comparison is an order; and a conjunction that holds says
-- both of its halves hold.
--
-- From a conjunction that fails, and from a disjunction that holds, all that
-- follows is that one of the two halves does, and a state that keeps one value
-- per variable has no way to say "one of these two things", so those two sides
-- teach nothing.
refine :: Bool -> Expression SemanticAnn -> ValueAnalysisMonad ()
refine holds cond@(AccessObject obj@(Variable ident _)) = do
  range <- rangeOfVariable obj
  setValue ident range (Refined (getLocation . getAnnotation $ cond)) (Just (B holds))
refine True (BinOp RelationalEqual left right ann) =
  refineEquality (getLocation ann) left right
refine False (BinOp RelationalNotEqual left right ann) =
  refineEquality (getLocation ann) left right
refine holds cond@(IsEnumVariantExpression obj@(Variable ident _) _enum variant _) = do
  range <- rangeOfVariable obj
  let loc = getLocation . getAnnotation $ cond
  if holds
    then setValues ident range (Refined loc)
           (Just (Discrete (S.singleton (Variant variant))))
    else ruleOut loc obj ident (Variant variant)
refine True (BinOp LogicalAnd left right _) = do
  refine True left
  refine True right
refine False (BinOp LogicalOr left right _) = do
  refine False left
  refine False right
refine False (BinOp RelationalEqual left right ann) =
  refineExclusion (getLocation ann) left right
refine True (BinOp RelationalNotEqual left right ann) =
  refineExclusion (getLocation ann) left right
refine holds (BinOp op left right ann) =
  refineOrder holds (getLocation ann) op left right
refine _ _ = return ()

-- | What a comparison that fails teaches about the variable in it: the value
-- it was compared against is one the variable does not hold. Which operand is
-- the variable is not fixed by the syntax, so both orders are tried.
refineExclusion ::
  Location -> Expression SemanticAnn -> Expression SemanticAnn
  -> ValueAnalysisMonad ()
refineExclusion loc left right = do
  against left right
  against right left

  where

    against (AccessObject obj@(Variable ident _)) other = do
      mValue <- valueOf other
      case mValue >>= scalar of
        Nothing -> return ()
        Just value -> ruleOut loc obj ident value
    against _ _ = return ()

-- | Takes a value out of what the path holds for a variable. A variable the
-- path says nothing about stays that way: the values it does not hold are of
-- no use without the ones it does.
ruleOut ::
  Location -> Object SemanticAnn -> Identifier -> Value -> ValueAnalysisMonad ()
ruleOut loc obj ident value = do
  current <- M.lookup ident . known <$> getPath
  range <- rangeOfVariable obj
  case current >>= excluded of
    Nothing -> return ()
    Just values -> setValues ident range (Refined loc) (Just values)

  where

    excluded entry = without value (varValues entry)

-- | What an order comparison teaches about the variable in it, which is a
-- bound and not a value. Unlike an equality, it teaches on both sides of the
-- branch, since the values it rules out on one side are the ones it leaves on
-- the other.
refineOrder ::
  Bool -> Location -> Op
  -> Expression SemanticAnn -> Expression SemanticAnn
  -> ValueAnalysisMonad ()
refineOrder holds loc op left right =
  case (asVariable left, asVariable right) of
    (Just variable, Nothing) -> against variable op right
    (Nothing, Just variable) -> against variable (mirrored op) left
    -- | Two variables bound each other, which this does not follow yet, and
    -- two values are not a refinement at all.
    _ -> return ()

  where

    asVariable (AccessObject obj@(Variable ident _)) = Just (obj, ident)
    asVariable _ = Nothing

    against (obj, ident) direction other = do
      mValue <- valueOf other
      case mValue >>= integerOfConst >>= boundOf holds direction of
        Nothing -> return ()
        Just bound -> bindBound loc obj ident bound

-- | Applies a bound to what the path knows of a variable. A variable the path
-- says nothing about starts from the interval its declared type allows, so a
-- guard also bounds a parameter the body never assigns.
bindBound ::
  Location -> Object SemanticAnn -> Identifier -> Bound -> ValueAnalysisMonad ()
bindBound loc obj ident bound = do
  current <- M.lookup ident . known <$> getPath
  range <- rangeOfVariable obj
  let start = case current of
        Just entry -> Just entry
        Nothing -> (\(lo, hi) -> Known range (Interval lo hi) S.empty) <$> range
  case start >>= bounded of
    Nothing -> return ()
    Just entry ->
      modifyPath (\p -> ValueAnalysisPath (M.insert ident entry (known p)))

  where

    bounded entry = do
      values <- narrow bound (varValues entry)
      return entry
        {
          varValues = values
        , valueOrigins = S.insert (Bounded loc) (valueOrigins entry)
        }

-- | Which operand of the comparison is the variable and which the value is not
-- fixed by the syntax, so both orders are tried. A refinement only adds what
-- is known: an evaluation that fails leaves the variable as it was, instead of
-- forgetting it.
refineEquality ::
  Location -> Expression SemanticAnn -> Expression SemanticAnn -> ValueAnalysisMonad ()
refineEquality loc left right = do
  refineAgainst left right
  refineAgainst right left

  where

    refineAgainst (AccessObject obj@(Variable ident _)) other = do
      range <- rangeOfVariable obj
      valueOf other
        >>= maybe (return ()) (setValue ident range (Refined loc) . Just)
    refineAgainst _ _ = return ()

-- | What a loop gives its iterator. The generated @for (i = init; i < end;
-- i = i + 1)@ runs it over every value between the two bounds, the second one
-- excluded.
--
-- The increment lives only in the generated C, so the walk never meets it and
-- the fixed point never grows the iterator past what it was seeded with.
-- Seeding it with the value it starts at would therefore have the pass hold
-- that the iterator keeps that value on every turn and report @if (i == 0)@,
-- so the whole range goes in at once.
--
-- A range of few enough values goes in written out, which decides an equality
-- that the ends alone cannot; a longer one goes in as an interval.
seedIterator ::
  Identifier -> TerminaType SemanticAnn
  -> Expression SemanticAnn -> Expression SemanticAnn
  -> ValueAnalysisMonad ()
seedIterator ident ty initE endE = do
  mFrom <- (>>= integerOfConst) <$> valueOf initE
  mTo <- (>>= integerOfConst) <$> valueOf endE
  range <- rangeOfType ty
  case turns mFrom mTo of
    -- | Bounds the folding could not work out, and a loop of no turns at all,
    -- which the folding rejects before this pass runs (CFE-008, CFE-009).
    Nothing -> forget ident
    Just (from, to) ->
      let loc = getLocation . getAnnotation $ initE
          values =
            if to - from < fromIntegral valueLimit
              then Discrete (S.fromList (map value [from .. to]))
              else Interval from to
      in modifyPath $ \p -> ValueAnalysisPath $
           M.insert ident (Known range values (S.singleton (Iterated loc))) (known p)

  where

    turns mFrom mTo = do
      from <- mFrom
      end <- mTo
      if from <= end - 1 then Just (from, end - 1) else Nothing

    value v = Scalar (I (TInteger v DecRepr) Nothing)

integerOfConst :: Const SemanticAnn -> Maybe Integer
integerOfConst (I (TInteger value _) _) = Just value
integerOfConst _ = Nothing

checkStatement :: Statement SemanticAnn -> ValueAnalysisMonad ()
checkStatement (Declaration ident _ ty mInitExpr ann) = do
  mapM_ noteEscapes mInitExpr
  range <- rangeOfType ty
  values <- maybe (return Nothing) valuesOf mInitExpr
  setValues ident range (Assigned (getLocation ann)) values
checkStatement (AssignmentStmt obj expr ann) = do
  noteEscapes expr
  case obj of
    -- | The whole variable takes the values of the expression.
    Variable ident _ -> do
      range <- rangeOfVariable obj
      valuesOf expr >>= setValues ident range (Assigned (getLocation ann))
    -- | A write into a field or an element of an object, which is not a scalar
    -- and is therefore outside the lattice.
    _ -> return ()
checkStatement (SingleExpStmt expr _) = noteEscapes expr


-- | What each node means to this pass. Only the expressions that decide a path
-- are recorded, which is the condition of an @if@, of each of its @else if@ and
-- the break condition of a @for@; the object a @match@ inspects is not, since
-- following it means following the variants of an enumeration and not the
-- value of a scalar.
transfer :: Transfer ValueAnalysisPath ValueAnalysisGlobal ValueAnalysisError
transfer = Transfer
  {
    onStatement = checkStatement
  , onSimpleBlock = mapM_ (mapM_ escapesIn) . simpleBlockChildren
  , onExpression = noteEscapes
  , onCondition = observeCondition
  , onCaseEntry = enterCase
  , refineTrue = refine True
  , refineFalse = refine False
  , onLoopEntry = seedIterator
  }

-- | Runs the body of a member or of a function, which knows nothing of its
-- parameters: a scalar parameter holds whatever the caller passed.
checkBody :: Block SemanticAnn -> ValueAnalysisMonad ()
checkBody body = do
  putPath (ValueAnalysisPath M.empty)
  walkForward transfer body

-- | The name a member is filed under, which carries its class as well, with a
-- separator no identifier can hold so that it cannot clash with the name of a
-- function.
memberName :: Identifier -> Identifier -> Identifier
memberName className member = className ++ "::" ++ member

-- | The class a member call goes to, read from the type the receiver carries.
--
-- A method is reachable only through @self@, since what a class exposes is an
-- interface and an interface holds procedures, so this always names the class
-- the walk is inside of. It is read from the receiver rather than carried
-- along because the diagnostic asks the same question once the walk is over,
-- when there is no class it is inside of any more, while the annotation
-- travels with the expression.
--
-- A receiver that is an access port names an interface instead of a class, and
-- which class answers for it is decided by the wiring of the program, which
-- this pass does not see: the architecture is built after it runs.
classOfReceiver :: Object SemanticAnn -> Maybe Identifier
classOfReceiver obj = snd <$> getObjectSAnns (getAnnotation obj) >>= named

  where

    named (TGlobal _ className) = Just className
    named (TReference _ inner) = named inner
    named _ = Nothing

checkClassMember :: Identifier -> ClassMember SemanticAnn -> ValueAnalysisMonad ()
checkClassMember className (ClassMethod _ak member _ps _mTy body _ann) =
  checkReturning (memberName className member) body
checkClassMember className (ClassViewer member _ps _mTy body _ann) =
  checkReturning (memberName className member) body
-- | An action answers its task or handler and a procedure answers a port, so
-- neither is called by a name this pass can resolve. Their bodies are walked
-- for the conditions in them, and whatever summary they leave nobody reads.
checkClassMember className (ClassAction _ak member _mp _tyret body _ann) =
  checkReturning (memberName className member) body
checkClassMember className (ClassProcedure _ak member _ps body _ann) =
  checkReturning (memberName className member) body
checkClassMember _ (ClassField {}) = return ()

checkTypeDef :: TypeDef SemanticAnn -> ValueAnalysisMonad ()
checkTypeDef (Class _kind className members _provides _mods) =
  mapM_ (checkClassMember className) members
checkTypeDef _ = return ()

-- | The expression a body gives back. A body has at most one return and it is
-- its last statement (EE-001), so it is the last block of the body and the
-- state the walk leaves behind is the state at it. A language that allowed a
-- return anywhere else would have to gather them all and join them, since a
-- summary that missed one of the values a body gives back would have a call
-- site claim more than the body does.
returnExpression :: Block SemanticAnn -> Maybe (Expression SemanticAnn)
returnExpression body =
  case reverse (blockBody body) of
    (ReturnBlock (Just expr) _ : _) -> Just expr
    _ -> Nothing

-- | Walks the body of a function or of a member and files what it gives back
-- under the given name.
checkReturning ::
  Identifier -> Block SemanticAnn -> ValueAnalysisMonad ()
checkReturning name body = do
  checkBody body
  case returnExpression body of
    Nothing -> return ()
    Just expr -> do
      mValues <- valuesOf expr
      case mValues of
        Nothing -> return ()
        Just values ->
          modifyGlobal (\g ->
            g { returnedValues = M.insert name values (returnedValues g) })

checkElement :: AnnASTElement SemanticAnn -> ValueAnalysisMonad ()
checkElement (Function ident _ps _mTy body _mods _ann) = checkReturning ident body
checkElement (TypeDefinition tyDef _ann) = checkTypeDef tyDef
checkElement (GlobalDeclaration {}) = return ()

-- | The integers an operand of a comparison may be. It is what the abstract
-- evaluator works on, and it comes either from the lattice or from a value the
-- folding evaluator pinned down.
data Integers =
    Listed (S.Set Integer)
  | Spanning Integer Integer

ends :: Integers -> (Integer, Integer)
ends (Listed values) = (S.findMin values, S.findMax values)
ends (Spanning lo hi) = (lo, hi)

integersOf :: Values -> Maybe Integers
integersOf (Discrete values) =
  case mapMaybe integerOf (S.toList values) of
    [] -> Nothing
    integers -> Just (Listed (S.fromList integers))
integersOf (Interval lo hi) = Just (Spanning lo hi)

-- | What an operand of a comparison may be: the value the folding evaluator
-- gives it, which covers a literal, a constant of the module and a variable
-- the path pins, or else what the lattice holds for a plain variable. An
-- operand of any other shape leaves the comparison undecided.
operandOf ::
  ValueAnalysisGlobal
  -> ValueAnalysisPath
  -> Expression SemanticAnn
  -> Maybe Integers
operandOf global locals expr = valuesIn global locals expr >>= integersOf

-- | Whether a comparison holds for every pair of values its operands may take,
-- for no pair at all, or neither, which is when the pass has nothing to say.
--
-- Two listed operands are compared pair by pair, which is exact and cheap
-- since neither list outgrows 'valueLimit'. Everything else is decided from
-- the ends, which loses the gaps of a list but never claims more than the ends
-- support.
compareValues :: Op -> Integers -> Integers -> Maybe Bool
compareValues op left right = do
  decide <- comparison op
  case (left, right) of
    (Listed values, Listed others) -> pairwise decide values others
    _ -> fromEnds op left right

-- | Every value the left operand may take, against every value the right one
-- may take. Exact, and cheap because neither list outgrows 'valueLimit'.
pairwise ::
  (Integer -> Integer -> Bool) -> S.Set Integer -> S.Set Integer -> Maybe Bool
pairwise decide left right =
  case nub outcomes of
    -- | Every pair answered the same, so the comparison answers that
    -- everywhere.
    [verdict] -> Just verdict
    -- | Either the pairs disagreed, or there were no pairs at all.
    _ -> Nothing

  where

    outcomes = [decide a b | a <- S.toList left, b <- S.toList right]

-- | The verdict the ends of the two operands support, which is all there is to
-- go on once an interval takes part. It loses the gaps of a list and never
-- claims more than the ends allow.
fromEnds :: Op -> Integers -> Integers -> Maybe Bool
fromEnds op left right =
  case (always, never) of
    (True, _) -> Just True
    (_, True) -> Just False
    _ -> Nothing

  where

    (leftLow, leftHigh) = ends left
    (rightLow, rightHigh) = ends right

    apart = leftHigh < rightLow || rightHigh < leftLow

    same =
      leftLow == leftHigh && rightLow == rightHigh && leftLow == rightLow

    always = case op of
      RelationalLT -> leftHigh < rightLow
      RelationalLTE -> leftHigh <= rightLow
      RelationalGT -> leftLow > rightHigh
      RelationalGTE -> leftLow >= rightHigh
      RelationalEqual -> same
      RelationalNotEqual -> apart
      _ -> False

    never = case op of
      RelationalLT -> leftLow >= rightHigh
      RelationalLTE -> leftLow > rightHigh
      RelationalGT -> leftHigh <= rightLow
      RelationalGTE -> leftHigh < rightLow
      RelationalEqual -> apart
      RelationalNotEqual -> same
      _ -> False

-- | How a pair of integers is compared, for the operators this evaluator
-- answers. An operator that is not a comparison gets no answer at all, so no
-- verdict comes out of one.
comparison :: Op -> Maybe (Integer -> Integer -> Bool)
comparison RelationalLT = Just (<)
comparison RelationalLTE = Just (<=)
comparison RelationalGT = Just (>)
comparison RelationalGTE = Just (>=)
comparison RelationalEqual = Just (==)
comparison RelationalNotEqual = Just (/=)
comparison _ = Nothing

-- | The name of a variant of an option, a status or a result. They are a
-- different shape in the AST from the variants of an enumeration, and the same
-- thing for this pass: both hold one name out of a closed set, and both lower
-- to the same tag in the generated C.
monadicName :: MonadicVariant' Expression SemanticAnn -> Identifier
monadicName None = "None"
monadicName (Some _) = "Some"
monadicName (Ok _) = "Ok"
monadicName (Error _) = "Error"
monadicName Success = "Success"
monadicName (Failure _) = "Failure"

labelName :: MonadicVariantLabel -> Identifier
labelName NoneLabel = "None"
labelName SomeLabel = "Some"
labelName OkLabel = "Ok"
labelName ErrorLabel = "Error"
labelName SuccessLabel = "Success"
labelName FailureLabel = "Failure"

-- | Whether every value an object may hold is this variant, whether none of
-- them is, or neither. An interval holds numbers, so it is never a variant.
isVariant :: Identifier -> Values -> Maybe Bool
isVariant variant (Discrete values) =
  case nub (map (== Variant variant) (S.toList values)) of
    [verdict] -> Just verdict
    _ -> Nothing
isVariant _ (Interval _ _) = Nothing

-- | What a condition is worth when the folding evaluator cannot pin it down,
-- which is where a variable that holds several values or a range still decides
-- a comparison. Only a comparison of two integer operands is answered; every
-- other shape leaves the pass quiet.
abstractValue ::
  ValueAnalysisGlobal
  -> ValueAnalysisPath
  -> Expression SemanticAnn
  -> Maybe (Const SemanticAnn)
abstractValue global locals (BinOp op left right _) = do
  leftValues <- operandOf global locals left
  rightValues <- operandOf global locals right
  B <$> compareValues op leftValues rightValues
abstractValue global locals (IsEnumVariantExpression obj _enum variant _) = do
  values <- valuesIn global locals (AccessObject obj)
  B <$> isVariant variant values
abstractValue global locals (IsMonadicVariantExpression obj label _) = do
  values <- valuesIn global locals (AccessObject obj)
  B <$> isVariant (labelName label) values
abstractValue _ _ _ = Nothing

-- | What the walk can say an expression may be: the value the folding
-- evaluator works out, what the path holds for a plain variable, or what the
-- walk of a function said it gives back. Every other shape says nothing.
--
-- It takes what it reads instead of getting it from the state, since the
-- diagnostic asks the same question once the walk is over.
valuesIn ::
  ValueAnalysisGlobal -> ValueAnalysisPath -> Expression SemanticAnn -> Maybe Values
valuesIn global locals expr =
  case evaluate (platform global) (moduleConsts global) locals expr >>= scalar of
    Just value -> Just (Discrete (S.singleton value))
    Nothing ->
      case expr of
        AccessObject (Variable ident _) -> varValues <$> M.lookup ident (known locals)
        -- | The body of the callee was walked knowing nothing of its
        -- parameters, so what it gives back holds for any call.
        -- | A variant written out, with or without data attached, says which
        -- variant it is.
        EnumVariantInitializer _enum variant _args _ ->
          Just (Discrete (S.singleton (Variant variant)))
        MonadicVariantInitializer monadic _ ->
          Just (Discrete (S.singleton (Variant (monadicName monadic))))
        FunctionCall ident _args _ -> M.lookup ident (returnedValues global)
        MemberFunctionCall obj member _args _ -> memberSummary obj member
        DerefMemberFunctionCall obj member _args _ -> memberSummary obj member
        _ -> Nothing

  where

    memberSummary obj member = do
      className <- classOfReceiver obj
      M.lookup (memberName className member) (returnedValues global)

valuesOf :: Expression SemanticAnn -> ValueAnalysisMonad (Maybe Values)
valuesOf expr = do
  global <- getGlobal
  locals <- getPath
  return (valuesIn global locals expr)

-- | The plain variables an expression reads, in the order it reads them. A
-- field or an element of an array is left out, since the pass follows neither
-- and so has nothing to say about them.
variablesIn :: Expression SemanticAnn -> [Identifier]
variablesIn = concatMap inChild . expressionChildren

  where

    inChild (ChildObject (Variable ident _)) = [ident]
    inChild (ChildObject _) = []
    inChild (ChildReference _ _) = []
    inChild (ChildExpr expr) = variablesIn expr
    inChild (ChildArg expr) = variablesIn expr
    inChild (ChildConstExpr _) = []

-- | What the message says about each name of a condition: the value it holds
-- and where it got it. A name the path pins answers with the places that gave
-- it the value; a constant of the module answers with no place, since it has
-- none inside the body.
reasonsFor ::
  ValueAnalysisGlobal -> ValueAnalysisPath -> Expression SemanticAnn -> [Reason]
reasonsFor global locals cond =
  mapMaybe reasonOf (nub (variablesIn cond))

  where

    reasonOf ident =
      case M.lookup ident (known locals) of
        Just entry ->
          Just (Reason ident (holdsOf entry) (S.toList (valueOrigins entry)))
        Nothing ->
          (\value -> Reason ident (OneValue value) [])
            <$> M.lookup ident (moduleConsts global)

    holdsOf entry = case varValues entry of
      Interval lo hi -> Between lo hi
      Discrete values ->
        case S.toList values of
          [Scalar value] -> OneValue value
          listedValues ->
            case mapMaybe variantOf listedValues of
              [] -> OneOf (mapMaybe integerOf listedValues)
              names -> OneOfVariants names

-- | The diagnostic: of the places the walk recorded, the conditions whose
-- value the state at them determines and the cases the state rules out, in the
-- order the source has them, which is the order of their positions.
findings :: ValueAnalysisGlobal -> [ValueAnalysisError]
findings global = mapMaybe finding (M.toAscList (observed global))

  where

    finding (loc, (MetCondition cond, locals)) =
      case verdict locals cond of
        Just value@(B _) ->
          Just (annotateError loc
                  (EInvariantCondition value (reasonsFor global locals cond)))
        _ -> Nothing
    finding (loc, (MetCase discriminant variant, locals)) =
      case valuesIn global locals discriminant >>= isVariant variant of
        Just False ->
          Just (annotateError loc
                  (EUnreachableCase variant (reasonsFor global locals discriminant)))
        _ -> Nothing

    -- | The folding evaluator answers first, which keeps the arithmetic of a
    -- condition it can work out whole, with its overflow and its division by
    -- zero; the abstract one only gets what it leaves undecided.
    verdict locals cond =
      case evaluate (platform global) (moduleConsts global) locals cond of
        Just value -> Just value
        Nothing -> abstractValue global locals cond

-- | Runs the check over a whole module, with the constants it sees and the
-- returned of the modules it imports, returning the first error and the
-- returned this module adds to them.
--
-- The elements are walked in the order the source has them, and the global
-- state travels from one to the next so that a call reads the summary of the
-- function it calls. No ordering work is needed for that: the language admits
-- neither recursion nor a reference to something declared later (SE-092), so
-- the order of the source is already a topological order of the call graph.
runValueAnalysisCheck ::
  Platform
  -> M.Map Identifier (Const SemanticAnn)
  -> ReturnedValues
  -> AnnotatedProgram SemanticAnn
  -> (Maybe ValueAnalysisError, ReturnedValues)
runValueAnalysisCheck plt consts imported program =
  (listToMaybe (findings final), returnedValues final)

  where

    -- | The conditions the walk records are keyed by their position, so the
    -- state left by the last element holds the whole module in source order
    -- and the diagnostic reads it once.
    final = foldl walk start program

    start = ValueAnalysisGlobal consts plt M.empty imported

    walk global element =
      globalState . snd $
        runDataflow (DFState (ValueAnalysisPath M.empty) global) (checkElement element)
