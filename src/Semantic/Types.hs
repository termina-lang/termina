{-# LANGUAGE DeriveFunctor #-}
-- | Semantic interpretation of types

module Semantic.Types where

import Semantic.AST
import Core.Utils
import Utils.Annotations
import Parser.Types (ParserAnn(..), undocumented)
import Data.Maybe
import qualified Data.Map.Strict as M

----------------------------------------
-- Semantic interpretation of types.
-- Termina types are the same through out all the transpilation process.
-- We only have weird ints going around.

----------------------------------------

data FieldSeman a
  = SimpleField
  | AccessPortField (M.Map Identifier (InterfaceMember a))
  deriving (Show, Functor)

-- | Semantic information of a type definition
data TypeDefSeman a
  = SimpleTypeDef
  -- | A resource class, with the declarations of the interfaces it provides
  -- and of the ones they extend, each one once
  | ResourceClassTypeDef [GlobalDecl a]
  deriving (Show, Functor)

-- | Semantic type information
data ExprSeman a
  = SimpleType (TerminaType a)
  | ObjectType AccessKind (TerminaType a)
  | AccessPortObjType AccessKind (M.Map Identifier (InterfaceMember a)) (TerminaType a)
  | AppType [Parameter a] (TerminaType a)
  deriving (Show, Functor)

data StmtSeman a
  = SimpleStmtType -- ^ Statement with no type
    | NoReturnCallStmtType -- ^ Call to an extern noreturn function
    | MatchCaseStmtType [TerminaType a] -- ^ Match case with types
    | PortConnection (ConnectionSeman a)
  deriving (Show, Functor)

data ProcedureSeman a = ProcedureSeman Identifier [Parameter a] [Modifier a]
  deriving (Show, Functor)

data FunctionSeman a = FunctionSeman [Parameter a] (TerminaType a) ReturnMode
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
    -- | Type specifier of the blocks of the connected pool
    (TerminaType a)
  | APTestPoolConnTy
    -- | Type specifier of the blocks of the connected pool of a unit test
    (TerminaType a)
  | APDequeConnTy
    -- | Type specifier of the boxes of the connected deque
    (TerminaType a)
  -- | Sink port connection
  | SPConnTy
    -- | Type specifier of the connected event emitter
    (TerminaType a)
    -- | Name of the action that will be triggered when the event emitter emits an event 
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
  -- | Type definitions 
  | TTy (TypeDefSeman a)
  -- | Variants of an enum
  | VTy
  -- | Function type
  | FnTy (FunctionSeman a)
  deriving Show

-- | Semantic annotations: what the type checker knows of an element, and the
-- annotation of the parser, with its location and its documentation comment.
data SemanticAnn =
  SemanticAnn (SemanticElems SemanticAnn) ParserAnn
  deriving Show

instance Located SemanticAnn where
  getLocation (SemanticAnn _ src) = getLocation src
  updateLocation (SemanticAnn ann src) loc = SemanticAnn ann (updateLocation src loc)

----------------------------------------

-- | A declaration of the program as the global environment keeps it: as in the
-- AST, without the bodies of its functions and of the members of its classes,
-- and with its annotation, which holds its location and its documentation
-- comment.
type GlobalDecl a = AnnASTElement' TerminaType Expression EmptyBlock a

-- | The declaration that the global environment keeps of an element of the
-- typed AST.
globalDecl :: AnnASTElement a -> GlobalDecl a
globalDecl (Function ident ps ty _ mods ann) = Function ident ps ty EmptyBlock mods ann
globalDecl (ExternFunction ident ps ty mode mods ann) = ExternFunction ident ps ty mode mods ann
globalDecl (GlobalDeclaration glb) = GlobalDeclaration glb
globalDecl (TypeDefinition tydef ann) = TypeDefinition (semanTypeDef tydef) ann

-- | The name that a declaration takes in the global environment.
globalDeclName :: AnnASTElement' ty expr blk a -> Identifier
globalDeclName (Function ident _ _ _ _ _) = ident
globalDeclName (ExternFunction ident _ _ _ _ _) = ident
globalDeclName (GlobalDeclaration glb) = getGlobalIdentifier glb
globalDeclName (TypeDefinition tydef _) = getTypeIdentifier tydef

-- | The type of a global object of the environment, which is a task, a
-- handler, a resource, a channel, an emitter or a test; nothing for a
-- constant, a function or a type.
globalObjectType :: GlobalDecl a -> Maybe (TerminaType a)
globalObjectType (GlobalDeclaration glb) = case glb of
  Const {} -> Nothing
  ConstExpr {} -> Nothing
  _ -> Just (getGlobalType glb)
globalObjectType _ = Nothing

-- | The signature of a function of the environment, which its annotation
-- holds; nothing for any other declaration.
globalFunctionSeman :: GlobalDecl SemanticAnn -> Maybe (FunctionSeman SemanticAnn)
globalFunctionSeman (Function _ _ _ _ _ (SemanticAnn (FnTy fs) _)) = Just fs
globalFunctionSeman (ExternFunction _ _ _ _ _ (SemanticAnn (FnTy fs) _)) = Just fs
globalFunctionSeman _ = Nothing

-- Simple TypeDef
-- It only has type information.
-- Aux constant type type-constructor
data EmptyBlock a = EmptyBlock
  deriving (Show, Functor)
type SemanTypeDef a = TypeDef' TerminaType Expression EmptyBlock a

-- | A type definition without the bodies of the members of its classes.
semanTypeDef :: TypeDef a -> SemanTypeDef a
semanTypeDef (Struct i f m) = Struct i f m
semanTypeDef (Enum i e m) = Enum i e m
semanTypeDef (Class kind i cls ps m) = Class kind i (map kClassMember cls) ps m
semanTypeDef (Interface kind i extends cls m) = Interface kind i extends cls m

-- Forgetfull Class member map
kClassMember :: ClassMember' ty blk a -> ClassMember' ty EmptyBlock a
kClassMember (ClassField fld) = ClassField fld
kClassMember (ClassMethod ak idx ps mty _blk ann) =
  ClassMethod ak idx ps mty EmptyBlock ann
kClassMember (ClassProcedure ak idx ps _blk ann) =
  ClassProcedure ak idx ps EmptyBlock ann
kClassMember (ClassViewer idx ps mty _blk ann) =
  ClassViewer idx ps mty EmptyBlock ann
kClassMember (ClassAction ak idx ps _blk ann) =
  ClassAction ak idx ps EmptyBlock ann
kClassMember (ClassCase ak idx ps _blk ann) =
  ClassCase ak idx ps EmptyBlock ann

----------------------------------------
-- Subtyping.
-- This fuction says what types can be casted into others.
casteableTys :: TerminaType a -> TerminaType a -> Bool
casteableTys source@(TConstSubtype _) target@(TConstSubtype _) = arithTy source && arithTy target
casteableTys _ (TConstSubtype _) = False
casteableTys source target = arithTy source && arithTy target

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
      AccessPortObjType _ _ t -> t;})
getResultingType _        = Nothing

getObjectSAnns :: SemanticAnn -> Maybe (AccessKind, TerminaType SemanticAnn)
getObjectSAnns (SemanticAnn (ETy (ObjectType ak ty)) _) = Just (ak, ty)
getObjectSAnns (SemanticAnn (ETy (AccessPortObjType ak _ ty)) _) = Just (ak, ty)
getObjectSAnns _                                    = Nothing

getArgumentsType :: SemanticElems a -> Maybe [Parameter a]
getArgumentsType (ETy (AppType ts _)) = Just ts
getArgumentsType _                    = Nothing

getMatchCaseTypes :: SemanticElems a -> Maybe [TerminaType a]
getMatchCaseTypes (STy (MatchCaseStmtType ts)) = Just ts
getMatchCaseTypes _                           = Nothing

isResultFromApp :: SemanticElems a -> Bool
isResultFromApp = isJust . getArgumentsType

-- The semantic annotation of an element is built from the annotation of the
-- parser, which it keeps whole.

buildFieldAnn :: ParserAnn -> SemanticAnn
buildFieldAnn = SemanticAnn (FTy SimpleField)

buildAccessPortFieldAnn :: ParserAnn -> M.Map Identifier (InterfaceMember SemanticAnn) -> SemanticAnn
buildAccessPortFieldAnn src procs = SemanticAnn (FTy (AccessPortField procs)) src

buildExpAnn :: ParserAnn -> TerminaType SemanticAnn -> SemanticAnn
buildExpAnn = flip $ SemanticAnn . ETy . SimpleType

buildExpAnnObj :: ParserAnn -> AccessKind -> TerminaType SemanticAnn -> SemanticAnn
buildExpAnnObj src ak ty = SemanticAnn (ETy (ObjectType ak ty)) src

buildExpAnnAccessPortObj :: ParserAnn -> AccessKind -> M.Map Identifier (InterfaceMember SemanticAnn) -> TerminaType SemanticAnn -> SemanticAnn
buildExpAnnAccessPortObj src ak ifaces pty = SemanticAnn (ETy (AccessPortObjType ak ifaces pty)) src

buildExpAnnApp :: ParserAnn -> [Parameter SemanticAnn] -> TerminaType SemanticAnn -> SemanticAnn
buildExpAnnApp src params rty = SemanticAnn (ETy (AppType params rty)) src

-- | Build annotations for global objects (tasks, handlers, resources, channels or emitters)
buildGlobalAnn :: ParserAnn -> TerminaType SemanticAnn -> SemanticAnn
buildGlobalAnn src ty = SemanticAnn (GTy ty) src

buildTypeAnn :: ParserAnn -> SemanticAnn
buildTypeAnn = SemanticAnn (TTy SimpleTypeDef)

buildResourceClassAnn :: ParserAnn -> [GlobalDecl SemanticAnn] -> SemanticAnn
buildResourceClassAnn ann ifaces = SemanticAnn (TTy (ResourceClassTypeDef ifaces)) ann

buildEnumVariantAnn :: ParserAnn -> SemanticAnn
buildEnumVariantAnn = SemanticAnn VTy

-- Elements that no source file declares, such as the variants of the enums of
-- the environment and of Option, Status and Result, or the elements that a
-- test builds by hand.

internalVariant :: Identifier -> [TerminaType SemanticAnn] -> EnumVariant SemanticAnn
internalVariant ident params = EnumVariant ident params (buildEnumVariantAnn (undocumented Internal))

-- | A type definition with the annotation that the type checker gives it. A
-- resource class built by hand is taken to provide one interface, the first
-- it names, that declares exactly its procedures, without comments.
internalTypeDefinition :: TypeDef SemanticAnn -> AnnASTElement SemanticAnn
internalTypeDefinition tydef@(Class ResourceClass _ members provides _) =
  TypeDefinition tydef (buildResourceClassAnn (undocumented Internal) [ internalInterface iface | iface <- take 1 provides ])

  where

    internalInterface iface = TypeDefinition (Interface RegularInterface iface [] procs []) (buildTypeAnn (undocumented Internal))
    procs = [ InterfaceProcedure ak name params [] (buildExpAnn (undocumented Internal) TUnit)
            | ClassProcedure ak name params _ _ <- members ]
internalTypeDefinition tydef = TypeDefinition tydef (buildTypeAnn (undocumented Internal))

internalField :: Identifier -> TerminaType SemanticAnn -> FieldDefinition SemanticAnn
internalField ident ty = FieldDefinition ident ty (buildFieldAnn (undocumented Internal))

internalAccessPortField :: Identifier -> TerminaType SemanticAnn -> M.Map Identifier (InterfaceMember SemanticAnn) -> FieldDefinition SemanticAnn
internalAccessPortField ident ty procs = FieldDefinition ident ty (buildAccessPortFieldAnn (undocumented Internal) procs)

-- | The annotation of a procedure of a class, as the type checker gives it.
internalProcedureAnn :: SemanticAnn
internalProcedureAnn = buildExpAnn (undocumented Internal) TUnit

buildStmtAnn :: ParserAnn -> SemanticAnn
buildStmtAnn = SemanticAnn (STy SimpleStmtType)

buildNoReturnCallStmtAnn :: ParserAnn -> SemanticAnn
buildNoReturnCallStmtAnn = SemanticAnn (STy NoReturnCallStmtType)

buildStmtMatchCaseAnn :: ParserAnn -> [TerminaType SemanticAnn] -> SemanticAnn
buildStmtMatchCaseAnn src ts = SemanticAnn (STy (MatchCaseStmtType ts)) src

buildOutPortConnAnn :: ParserAnn -> TerminaType SemanticAnn -> SemanticAnn
buildOutPortConnAnn src ts = SemanticAnn (STy (PortConnection (OutPConnTy ts))) src

buildAccessPortConnAnn :: ParserAnn -> TerminaType SemanticAnn -> TerminaType SemanticAnn -> [ProcedureSeman SemanticAnn] -> SemanticAnn
buildAccessPortConnAnn src ifaceTy ts procs = SemanticAnn (STy (PortConnection (APConnTy ifaceTy ts procs))) src

buildPoolConnAnn :: ParserAnn -> TerminaType SemanticAnn -> SemanticAnn
buildPoolConnAnn src ts = SemanticAnn (STy (PortConnection (APPoolConnTy ts))) src

buildTestPoolConnAnn :: ParserAnn -> TerminaType SemanticAnn -> SemanticAnn
buildTestPoolConnAnn src ts = SemanticAnn (STy (PortConnection (APTestPoolConnTy ts))) src

buildDequeConnAnn :: ParserAnn -> TerminaType SemanticAnn -> SemanticAnn
buildDequeConnAnn src ts = SemanticAnn (STy (PortConnection (APDequeConnTy ts))) src

buildAtomicConnAnn :: ParserAnn -> TerminaType SemanticAnn -> SemanticAnn
buildAtomicConnAnn src ts = SemanticAnn (STy (PortConnection (APAtomicConnTy ts))) src

buildAtomicArrayConnAnn :: ParserAnn
  -> TerminaType SemanticAnn
  -> Expression SemanticAnn
  -> Expression SemanticAnn
  -> SemanticAnn
buildAtomicArrayConnAnn src ts portSize glbSize = SemanticAnn (STy (PortConnection (APAtomicArrayConnTy ts portSize glbSize))) src

buildSinkPortConnAnn :: ParserAnn -> TerminaType SemanticAnn -> Identifier -> SemanticAnn
buildSinkPortConnAnn src ts action = SemanticAnn (STy (PortConnection (SPConnTy ts action))) src

buildInPortConnAnn :: ParserAnn -> TerminaType SemanticAnn -> Identifier -> SemanticAnn
buildInPortConnAnn src ts action = SemanticAnn (STy (PortConnection (InPConnTy ts action))) src

getSemanticAnn :: SemanticAnn -> SemanticElems SemanticAnn
getSemanticAnn (SemanticAnn ann _src) = ann

-- | The annotation of the parser that a semantic annotation keeps.
sourceOf :: SemanticAnn -> ParserAnn
sourceOf (SemanticAnn _ src) = src

-- | An annotation with the documentation comment of another one: the one of
-- a call with the comment of the statement that makes it.
withDocOf :: SemanticAnn -> SemanticAnn -> SemanticAnn
withDocOf from (SemanticAnn elems src) = SemanticAnn elems src { parsedDoc = parsedDoc (sourceOf from) }

forgetSemAnn :: SemanticAnn -> Location
forgetSemAnn = getLocation

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

getConnectionAnn :: SemanticAnn -> ConnectionSeman SemanticAnn
getConnectionAnn (SemanticAnn (STy (PortConnection connAnn)) _) = connAnn
getConnectionAnn _ = error "Invalid annotation"
