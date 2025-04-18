{-# LANGUAGE DeriveFunctor #-}
-- | Semantic interpretation of types

module Semantic.Types where

import Semantic.AST
import Core.Utils
import Utils.Annotations
import Data.Maybe
import qualified Data.Map as M

----------------------------------------
-- Semantic interpretation of types.
-- Termina types are the same through out all the transpilation process.
-- We only have weird ints going around.

----------------------------------------

data FieldSeman a
  = SimpleField 
  | AccessPortField (M.Map Identifier (InterfaceMember a))
  deriving (Show, Functor)

-- | Semantic type information
data ExprSeman a
  = SimpleType (TerminaType a)
  | ObjectType AccessKind (TerminaType a)
  | AccessPortObjType (M.Map Identifier (InterfaceMember a)) (TerminaType a)
  | AppType [Parameter a] (TerminaType a)
  deriving (Show, Functor)

data StmtSeman a
  = SimpleStmtType -- ^ Statement with no type
    | MatchCaseStmtType [TerminaType a] -- ^ Match case with types
    | PortConnection (ConnectionSeman a)
  deriving (Show, Functor)

data ProcedureSeman a = ProcedureSeman Identifier [Parameter a] [Modifier a]
  deriving (Show, Functor)

data FunctionSeman a = FunctionSeman [Parameter a] (TerminaType a)
  deriving (Show, Functor)

data ConnectionSeman a =
    -- | Access port connection
  APConnTy
  -- | type specifier of the ports interface
    (TerminaType a)
  -- | type specifier of the connected resource
    (TerminaType a)
    -- | List of procedures that can be called on the connected resource
    [ProcedureSeman a]
  | APAtomicConnTy
    -- | Type specifier of the connected atomic
    (TerminaType a)
  | APAtomicArrayConnTy
    -- | type specifier of the connected atomic array
    (TerminaType a)
    -- | Size of the port to which the atomic array is connected
    (Expression a)
    -- | Size of the connected atomic array
    (Expression a)
  | APPoolConnTy
    -- | Type specifier of the connected pool
    (TerminaType a)
    -- | Size of the connected pool
    (Expression a)
  -- | Sink port connection
  | SPConnTy
    -- | Type specifier of the connected event emitter
    (TerminaType a)
    -- | Name of the action that will be triggered when the event emitter emits an event 
    Identifier 
  -- | In port connection
  | InPConnTy
    -- | Type specifier of the connected channel
    (TerminaType a)
    -- | Name of the action that will be triggered when the channel receives a message
    Identifier
  | OutPConnTy
    -- | Type specifier of the connected channel
    (TerminaType a)
  deriving (Show, Functor)

-- | Semantic elements
-- we have three different semantic elements:
data SemanticElems a
  =
  -- | Expressions with their types
  ETy (ExprSeman a)
  -- | Field types
  | FTy (FieldSeman a)
  -- | Statements 
  | STy (StmtSeman a)
  -- | Global objects
  | GTy (TerminaType a)
  -- | Type definitions 
  | TTy 
  -- | Function type
  | FnTy (FunctionSeman a)
  deriving Show

-- | Expression Semantic Annotations
data SemanticAnn = 
  SemanticAnn (SemanticElems SemanticAnn) Location
  deriving Show

instance Located SemanticAnn where
  getLocation (SemanticAnn _ loc) = loc
  updateLocation (SemanticAnn ann _) = SemanticAnn ann 

----------------------------------------

-- | General global entities
data GEntry a
  = GFun (FunctionSeman a)
  -- ^ Functions
  | GGlob (TerminaType a)
  -- ^ Globals
  | GConst (TerminaType a) (Expression a)
  -- ^ Constants
  | GConstExpr (TerminaType a) (Expression a)
  -- ^ Constant Expressions
  | GType (SemanTypeDef a)
  -- ^ Types
  deriving (Show, Functor)

-- Simple TypeDef
-- It only has type information.
-- Aux constant type type-constructor
data EmptyBlock a = EmptyBlock
  deriving (Show, Functor)
type SemanTypeDef a = TypeDef' TerminaType EmptyBlock a

-- Forgetfull Class member map
kClassMember :: ClassMember' ty blk a -> ClassMember' ty EmptyBlock a
kClassMember (ClassField fld) = ClassField fld
kClassMember (ClassMethod idx ps _blk ann) =
  ClassMethod idx ps EmptyBlock ann
kClassMember (ClassProcedure idx ps _blk ann) =
  ClassProcedure idx ps EmptyBlock ann
kClassMember (ClassViewer idx ps ty _blk ann) =
  ClassViewer idx ps ty EmptyBlock ann
kClassMember (ClassAction idx ps ty _blk ann) =
  ClassAction idx ps ty EmptyBlock ann

----------------------------------------
-- Subtyping.
-- This fuction says what types can be casted into others.
casteableTys :: TerminaType a -> TerminaType a -> Bool
casteableTys source@(TConstSubtype _) target@(TConstSubtype _) = numTy source && numTy target
casteableTys _ (TConstSubtype _) = False
casteableTys source target = numTy source && numTy target

-- Relation between types
-- we use to define (box A \subseteq A)
subTypes :: TerminaType a -> TerminaType a -> Bool
subTypes (TBoxSubtype a) (TBoxSubtype b) = sameTy a b
subTypes (TBoxSubtype a) b              = sameTy a b
subTypes a (TBoxSubtype b)              = sameTy a b
-- Id \subseteq Subtypes
subTypes a b                           = sameTy a b


getResultingType :: SemanticElems a -> Maybe (TerminaType a)
getResultingType (ETy ty) = 
  Just (
    case ty of {
      SimpleType t -> t; 
      ObjectType _ t -> t; 
      AppType _ t -> t;
      AccessPortObjType _ t -> t;})
getResultingType _        = Nothing

getObjectSAnns :: SemanticAnn -> Maybe (AccessKind, TerminaType SemanticAnn)
getObjectSAnns (SemanticAnn (ETy (ObjectType ak ty)) _) = Just (ak, ty)
getObjectSAnns (SemanticAnn (ETy (AccessPortObjType _ ty)) _) = Just (Mutable, ty)
getObjectSAnns _                                    = Nothing

getArgumentsType :: SemanticElems a -> Maybe [Parameter a]
getArgumentsType (ETy (AppType ts _)) = Just ts
getArgumentsType _                    = Nothing

getMatchCaseTypes :: SemanticElems a -> Maybe [TerminaType a]
getMatchCaseTypes (STy (MatchCaseStmtType ts)) = Just ts
getMatchCaseTypes _                           = Nothing

isResultFromApp :: SemanticElems a -> Bool
isResultFromApp = isJust . getArgumentsType

buildFieldAnn :: Location -> SemanticAnn
buildFieldAnn = SemanticAnn (FTy SimpleField)

buildAccessPortFieldAnn :: Location -> M.Map Identifier (InterfaceMember SemanticAnn) -> SemanticAnn
buildAccessPortFieldAnn = flip $ SemanticAnn . FTy . AccessPortField

buildExpAnn :: Location -> TerminaType SemanticAnn -> SemanticAnn
buildExpAnn = flip $ SemanticAnn . ETy . SimpleType

buildExpAnnObj :: Location -> AccessKind -> TerminaType SemanticAnn -> SemanticAnn
buildExpAnnObj loc ak ty = SemanticAnn (ETy (ObjectType ak ty)) loc

buildExpAnnAccessPortObj :: Location -> M.Map Identifier (InterfaceMember SemanticAnn) -> TerminaType SemanticAnn -> SemanticAnn
buildExpAnnAccessPortObj loc ifaces pty = SemanticAnn (ETy (AccessPortObjType ifaces pty)) loc

buildExpAnnApp :: Location -> [Parameter SemanticAnn] -> TerminaType SemanticAnn -> SemanticAnn
buildExpAnnApp loc params rty = SemanticAnn (ETy (AppType params rty)) loc

-- | Build annotations for global objects (tasks, handlers, resources, channels or emitters)
buildGlobalAnn :: Location -> TerminaType SemanticAnn -> SemanticAnn
buildGlobalAnn = flip $ SemanticAnn . GTy 

buildTypeAnn :: Location -> SemanticAnn
buildTypeAnn = SemanticAnn TTy

buildStmtAnn :: Location -> SemanticAnn
buildStmtAnn = SemanticAnn (STy SimpleStmtType)

buildStmtMatchCaseAnn :: Location -> [TerminaType SemanticAnn] -> SemanticAnn
buildStmtMatchCaseAnn loc ts = SemanticAnn (STy (MatchCaseStmtType ts)) loc

buildOutPortConnAnn :: Location -> TerminaType SemanticAnn -> SemanticAnn
buildOutPortConnAnn loc ts = SemanticAnn (STy (PortConnection (OutPConnTy ts))) loc

buildAccessPortConnAnn :: Location -> TerminaType SemanticAnn -> TerminaType SemanticAnn -> [ProcedureSeman SemanticAnn] -> SemanticAnn
buildAccessPortConnAnn loc ifaceTy ts procs = SemanticAnn (STy (PortConnection (APConnTy ifaceTy ts procs))) loc

buildPoolConnAnn :: Location -> TerminaType SemanticAnn -> Expression SemanticAnn -> SemanticAnn
buildPoolConnAnn loc ts s = SemanticAnn (STy (PortConnection (APPoolConnTy ts s))) loc

buildAtomicConnAnn :: Location -> TerminaType SemanticAnn -> SemanticAnn
buildAtomicConnAnn loc ts = SemanticAnn (STy (PortConnection (APAtomicConnTy ts))) loc

buildAtomicArrayConnAnn :: Location 
  -> TerminaType SemanticAnn 
  -> Expression SemanticAnn 
  -> Expression SemanticAnn 
  -> SemanticAnn
buildAtomicArrayConnAnn loc ts portSize glbSize = SemanticAnn (STy (PortConnection (APAtomicArrayConnTy ts portSize glbSize))) loc

buildSinkPortConnAnn :: Location -> TerminaType SemanticAnn -> Identifier -> SemanticAnn
buildSinkPortConnAnn loc ts action = SemanticAnn (STy (PortConnection (SPConnTy ts action))) loc

buildInPortConnAnn :: Location -> TerminaType SemanticAnn -> Identifier -> SemanticAnn
buildInPortConnAnn loc ts action = SemanticAnn (STy (PortConnection (InPConnTy ts action))) loc

getSemanticAnn :: SemanticAnn -> SemanticElems SemanticAnn
getSemanticAnn (SemanticAnn ann _loc) = ann

forgetSemAnn :: SemanticAnn -> Location
forgetSemAnn (SemanticAnn _ann loc) = loc

getTypeSemAnn :: SemanticAnn -> Maybe (TerminaType SemanticAnn)
getTypeSemAnn  = getResultingType . getSemanticAnn

unboxExpType :: ExprSeman SemanticAnn -> ExprSeman SemanticAnn
unboxExpType (SimpleType (TBoxSubtype ty)) = SimpleType ty
unboxExpType (ObjectType ak (TBoxSubtype ty)) = ObjectType ak ty
unboxExpType (AppType ts (TBoxSubtype ty)) = AppType ts ty
unboxExpType _ = error "impossible 888+1"

unboxTypeAnn :: SemanticAnn -> SemanticAnn
unboxTypeAnn (SemanticAnn (ETy en) p) = SemanticAnn (ETy (unboxExpType en)) p
unboxTypeAnn _                    = error "impossible 888"

unboxConnectionAnn :: SemanticAnn -> ConnectionSeman SemanticAnn
unboxConnectionAnn (SemanticAnn (STy (PortConnection connAnn)) _) = connAnn
unboxConnectionAnn _ = error "Invalid annotation"
