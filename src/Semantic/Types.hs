{-# LANGUAGE DeriveFunctor #-}
-- | Semantic interpretation of types

module Semantic.Types where

import Semantic.AST
import Core.Utils
import Utils.Annotations
import Data.Maybe

----------------------------------------
-- Semantic interpretation of types.
-- Termina types are the same through out all the transpilation process.
-- We only have weird ints going around.

----------------------------------------

data FieldSeman
  = SimpleField 
  | AccessPortField [InterfaceMember SemanticAnn]
  deriving Show

-- | Semantic type information
data ExprSeman
  = SimpleType TerminaType
  | ObjectType AccessKind TerminaType
  | AccessPortObjType [InterfaceMember SemanticAnn] TerminaType
  | AppType [TerminaType] TerminaType
  deriving Show

data StmtSeman
  = SimpleStmtType -- ^ Statement with no type
    | MatchCaseStmtType [TerminaType] -- ^ Match case with types
    | PortConnection ConnectionSeman
  deriving Show

data ProcedureSeman = ProcedureSeman Identifier [TerminaType]
  deriving (Show)

data FunctionSeman = FunctionSeman [TerminaType] TerminaType
  deriving (Show)

data ConnectionSeman =
    -- | Access port connection
  APConnTy
  -- | type specifier of the ports interface
    TerminaType
  -- | type specifier of the connected resource
    TerminaType
    -- | List of procedures that can be called on the connected resource
    [ProcedureSeman]
  | APAtomicConnTy
    -- | Type specifier of the connected atomic
    TerminaType
  | APAtomicArrayConnTy
    -- | type specifier of the connected atomic array
    TerminaType
    -- | Size of the connected atomic array
    Size
  | APPoolConnTy
    -- | Type specifier of the connected pool
    TerminaType
    -- | Size of the connected pool
    Size
  -- | Sink port connection
  | SPConnTy
    -- | Type specifier of the connected event emitter
    TerminaType
    -- | Name of the action that will be triggered when the event emitter emits an event 
    Identifier 
  -- | In port connection
  | InPConnTy
    -- | Type specifier of the connected channel
    TerminaType
    -- | Name of the action that will be triggered when the channel receives a message
    Identifier
  | OutPConnTy
    -- | Type specifier of the connected channel
    TerminaType
  deriving Show

data TypeDefSeman =
  StructTy
  | EnumTy
  | ClsTy ClassKind
  | InterfaceTy InterfaceKind [ProcedureSeman]
  deriving Show

-- | Semantic elements
-- we have three different semantic elements:
data SemanticElems
  =
  -- | Expressions with their types
  ETy ExprSeman
  -- | Field types
  | FTy FieldSeman
  -- | Statements 
  | STy StmtSeman
  -- | Global objects
  | GTy TerminaType
  -- | Type definitions 
  | TTy TypeDefSeman
  -- | Function type
  | FnTy FunctionSeman
  deriving Show

-- | Expression Semantic Annotations
type SemanticAnn = LocatedElement SemanticElems

----------------------------------------

-- | General global entities
data GEntry a
  = GFun FunctionSeman
  -- ^ Functions
  | GGlob TerminaType
  -- ^ Globals
  | GConst TerminaType Const
  -- ^ Constants
  | GType (SemanTypeDef a)
  -- ^ Types
  deriving (Functor,Show)

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
casteableTys :: TerminaType -> TerminaType -> Bool
casteableTys a b = numTy a && numTy b

-- Relation between types
-- we use to define (box A \subseteq A)
subTypes :: TerminaType -> TerminaType -> Bool
subTypes (TBoxSubtype a) (TBoxSubtype b) = sameTy a b
subTypes (TBoxSubtype a) b              = sameTy a b
subTypes a (TBoxSubtype b)              = sameTy a b
-- Id \subseteq Subtypes
subTypes a b                           = sameTy a b


getResultingType :: SemanticElems -> Maybe TerminaType
getResultingType (ETy ty) = 
  Just (
    case ty of {
      SimpleType t -> t; 
      ObjectType _ t -> t; 
      AppType _ t -> t;
      AccessPortObjType _ t -> t;})
getResultingType _        = Nothing

getObjectSAnns :: SemanticAnn -> Maybe (AccessKind, TerminaType)
getObjectSAnns (LocatedElement (ETy (ObjectType ak ty)) _) = Just (ak, ty)
getObjectSAnns (LocatedElement (ETy (AccessPortObjType _ ty)) _) = Just (Mutable, ty)
getObjectSAnns _                                    = Nothing

getArgumentsType :: SemanticElems -> Maybe [TerminaType]
getArgumentsType (ETy (AppType ts _)) = Just ts
getArgumentsType _                    = Nothing

getMatchCaseTypes :: SemanticElems -> Maybe [TerminaType]
getMatchCaseTypes (STy (MatchCaseStmtType ts)) = Just ts
getMatchCaseTypes _                           = Nothing

isResultFromApp :: SemanticElems -> Bool
isResultFromApp = isJust . getArgumentsType

buildFieldAnn :: Location -> SemanticAnn
buildFieldAnn loc = locate loc $ FTy SimpleField

buildAccessPortFieldAnn :: Location -> [InterfaceMember SemanticAnn] -> SemanticAnn
buildAccessPortFieldAnn loc = locate loc . FTy . AccessPortField

buildExpAnn :: Location -> TerminaType -> SemanticAnn
buildExpAnn loc = locate loc . ETy . SimpleType

buildExpAnnObj :: Location -> AccessKind -> TerminaType -> SemanticAnn
buildExpAnnObj loc ak = locate loc . ETy . ObjectType ak

buildExpAnnAccessPortObj :: Location -> [InterfaceMember SemanticAnn] -> TerminaType -> SemanticAnn
buildExpAnnAccessPortObj loc ifaces = locate loc . ETy . AccessPortObjType ifaces

buildExpAnnApp :: Location -> [TerminaType] -> TerminaType -> SemanticAnn
buildExpAnnApp loc tys = locate loc . ETy . AppType tys

-- | Build annotations for global objects (tasks, handlers, resources, channels or emitters)
buildGlobalAnn :: Location -> TerminaType -> SemanticAnn
buildGlobalAnn loc = locate loc . GTy 

buildStructTypeAnn :: Location -> SemanticAnn
buildStructTypeAnn = LocatedElement (TTy StructTy)

buildEnumTypeAnn :: Location -> SemanticAnn
buildEnumTypeAnn = LocatedElement (TTy EnumTy)

buildClassTypeAnn :: Location -> ClassKind -> SemanticAnn
buildClassTypeAnn loc clsKind = LocatedElement (TTy (ClsTy clsKind)) loc

buildInterfaceTypeAnn :: Location -> InterfaceKind -> [ProcedureSeman] -> SemanticAnn
buildInterfaceTypeAnn loc iKind procs = LocatedElement (TTy (InterfaceTy iKind procs)) loc

buildStmtAnn :: Location -> SemanticAnn
buildStmtAnn = LocatedElement (STy SimpleStmtType)

buildStmtMatchCaseAnn :: Location -> [TerminaType] -> SemanticAnn
buildStmtMatchCaseAnn loc ts = locate loc (STy (MatchCaseStmtType ts))

buildOutPortConnAnn :: Location -> TerminaType -> SemanticAnn
buildOutPortConnAnn loc ts = locate loc (STy (PortConnection (OutPConnTy ts)))

buildAccessPortConnAnn :: Location -> TerminaType -> TerminaType -> [ProcedureSeman] -> SemanticAnn
buildAccessPortConnAnn loc ifaceTy ts procs = locate loc (STy (PortConnection (APConnTy ifaceTy ts procs)))

buildPoolConnAnn :: Location -> TerminaType -> Size -> SemanticAnn
buildPoolConnAnn loc ts s = locate loc (STy (PortConnection (APPoolConnTy ts s)))

buildAtomicConnAnn :: Location -> TerminaType -> SemanticAnn
buildAtomicConnAnn loc ts = locate loc (STy (PortConnection (APAtomicConnTy ts)))

buildAtomicArrayConnAnn :: Location -> TerminaType -> Size -> SemanticAnn
buildAtomicArrayConnAnn loc ts s = locate loc (STy (PortConnection (APAtomicArrayConnTy ts s)))

buildSinkPortConnAnn :: Location -> TerminaType -> Identifier -> SemanticAnn
buildSinkPortConnAnn loc ts action = locate loc (STy (PortConnection (SPConnTy ts action)))

buildInPortConnAnn :: Location -> TerminaType -> Identifier -> SemanticAnn
buildInPortConnAnn loc ts action = locate loc (STy (PortConnection (InPConnTy ts action)))

getSemanticAnn :: SemanticAnn -> SemanticElems
getSemanticAnn = element

forgetSemAnn :: SemanticAnn -> Location
forgetSemAnn = location

getTypeSemAnn :: SemanticAnn -> Maybe TerminaType
getTypeSemAnn  = getResultingType . getSemanticAnn

unboxExpType :: ExprSeman -> ExprSeman
unboxExpType (SimpleType (TBoxSubtype ty)) = SimpleType ty
unboxExpType (ObjectType ak (TBoxSubtype ty)) = ObjectType ak ty
unboxExpType (AppType ts (TBoxSubtype ty)) = AppType ts ty
unboxExpType _ = error "impossible 888+1"

unboxTypeAnn :: SemanticAnn -> SemanticAnn
unboxTypeAnn (LocatedElement (ETy en) p) = LocatedElement (ETy (unboxExpType en)) p
unboxTypeAnn _                    = error "impossible 888"

unboxConnectionAnn :: SemanticAnn -> ConnectionSeman
unboxConnectionAnn (LocatedElement (STy (PortConnection connAnn)) _) = connAnn
unboxConnectionAnn _ = error "Invalid annotation"
