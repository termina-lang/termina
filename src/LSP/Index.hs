{-# LANGUAGE OverloadedStrings #-}

-- | Where each name of a module is defined and what each use of a name points
-- at, which is what answers "go to the definition" and, later, "find every
-- reference". The index is built from the typed AST, so a name that the type
-- checker rejected has no entry and nothing to jump to.
--
-- The name of a type inside a declaration carries no annotation, so a use of it
-- reaches nothing through the walk. That is what 'wordAt' is for: the server
-- falls back to the word under the cursor and looks it up among the definitions,
-- which is enough because a type is named at the top level.
module LSP.Index (
    Target(..),
    ModuleIndex(..),
    emptyIndex,
    indexModule,
    referenceAt,
    typeAt,
    wordAt
  ) where

import qualified Data.Map.Strict as M
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Char (isAlphaNum)
import Data.Ord (comparing)
import qualified Data.Text as T
import Text.Parsec.Pos (sourceLine, sourceColumn)

import Core.AST (Identifier)
import qualified Semantic.AST as SAST
import Semantic.Types
import Utils.Annotations
import Utils.Printer (ShowText(showText))

-- | What a use of a name refers to. A local is resolved while the body is
-- walked, so it carries the position it was declared at; everything else is
-- resolved by name, which the server looks up in this module first and in the
-- rest of the project afterwards.
data Target
  = TopLevel Identifier
  | Member Identifier Identifier
  | Local Location
  deriving (Eq, Show)

-- | What one step of the walk produces: a use of a name, or the type the
-- checker gave to a piece of source, which is what a hover shows.
data Entry
  = Ref Location Target
  | Ty Location T.Text
  deriving (Eq, Show)

-- | The names a module defines, the uses it makes of a name, and the type of
-- each object it mentions.
data ModuleIndex = ModuleIndex
  {
    indexTopLevel :: M.Map Identifier Location
  , indexMembers :: M.Map (Identifier, Identifier) Location
  , indexRefs :: [(Location, Target)]
  , indexTypes :: [(Location, T.Text)]
  } deriving Show

emptyIndex :: ModuleIndex
emptyIndex = ModuleIndex M.empty M.empty [] []

-- | The environment of a body: the locals in scope with the position each one
-- was declared at.
type Locals = M.Map Identifier Location

-- | The class a member call is made on, read from the type the checker gave the
-- object. A call on anything else, such as a port, resolves to no class.
classOf :: SAST.Object SemanticAnn -> Maybe Identifier
classOf obj =
    case getObjectSAnns (getAnnotation obj) of
        Just (_, ty) -> named ty
        Nothing -> Nothing

    where

        named (SAST.TGlobal _ ident) = Just ident
        named (SAST.TStruct ident) = Just ident
        named (SAST.TInterface _ ident) = Just ident
        named (SAST.TReference _ inner) = named inner
        named (SAST.TBoxSubtype inner) = named inner
        named (SAST.TAccessPort inner) = named inner
        named (SAST.TSinkPort inner _) = named inner
        named (SAST.TOutPort inner) = named inner
        named (SAST.TInPort inner _) = named inner
        named _ = Nothing

-- | The index of a typed module.
indexModule :: SAST.AnnotatedProgram SemanticAnn -> ModuleIndex
indexModule program =
    ModuleIndex
      (M.fromList (concatMap topLevelOf program))
      (M.fromList (concatMap membersOf program))
      [ (loc, target) | Ref loc target <- entries ]
      [ (loc, rendered) | Ty loc rendered <- entries ]

    where

        entries = concatMap elemRefs program

        topLevelOf :: SAST.AnnASTElement SemanticAnn -> [(Identifier, Location)]
        topLevelOf (SAST.Function ident _ _ _ _ ann) = [(ident, getLocation ann)]
        topLevelOf (SAST.GlobalDeclaration glb) = [(globalName glb, getLocation (getAnnotation glb))]
        topLevelOf (SAST.TypeDefinition tydef ann) = [(typeName tydef, getLocation ann)]

        globalName :: SAST.Global SemanticAnn -> Identifier
        globalName (SAST.Task ident _ _ _ _) = ident
        globalName (SAST.Resource ident _ _ _ _) = ident
        globalName (SAST.Channel ident _ _ _ _) = ident
        globalName (SAST.Emitter ident _ _ _ _) = ident
        globalName (SAST.Handler ident _ _ _ _) = ident
        globalName (SAST.Const ident _ _ _ _) = ident
        globalName (SAST.ConstExpr ident _ _ _ _) = ident

        typeName :: SAST.TypeDef SemanticAnn -> Identifier
        typeName (SAST.Struct ident _ _) = ident
        typeName (SAST.Enum ident _ _) = ident
        typeName (SAST.Class _ ident _ _ _) = ident
        typeName (SAST.Interface _ ident _ _ _) = ident

        membersOf :: SAST.AnnASTElement SemanticAnn -> [((Identifier, Identifier), Location)]
        membersOf (SAST.TypeDefinition (SAST.Class _ ident members _ _) _) =
            concatMap (memberOf ident) members
        membersOf (SAST.TypeDefinition (SAST.Struct ident fields _) _) =
            [ ((ident, SAST.fieldIdentifier field), getLocation (SAST.fieldAnnotation field))
            | field <- fields ]
        membersOf (SAST.TypeDefinition (SAST.Interface _ ident _ members _) _) =
            concatMap (interfaceMemberOf ident) members
        membersOf _ = []

        -- | A call through a port reaches the interface, not the class that
        -- implements it, so the members of an interface are definitions too.
        interfaceMemberOf :: Identifier -> SAST.InterfaceMember SemanticAnn
            -> [((Identifier, Identifier), Location)]
        interfaceMemberOf owner (SAST.InterfaceProcedure _ ident _ _ ann) =
            [((owner, ident), getLocation ann)]

        memberOf :: Identifier -> SAST.ClassMember SemanticAnn -> [((Identifier, Identifier), Location)]
        memberOf owner (SAST.ClassField field) =
            [((owner, SAST.fieldIdentifier field), getLocation (SAST.fieldAnnotation field))]
        memberOf owner (SAST.ClassMethod _ ident _ _ _ ann) = [((owner, ident), getLocation ann)]
        memberOf owner (SAST.ClassProcedure _ ident _ _ ann) = [((owner, ident), getLocation ann)]
        memberOf owner (SAST.ClassViewer ident _ _ _ ann) = [((owner, ident), getLocation ann)]
        memberOf owner (SAST.ClassAction _ ident _ _ _ ann) = [((owner, ident), getLocation ann)]

        elemRefs :: SAST.AnnASTElement SemanticAnn -> [Entry]
        elemRefs (SAST.Function _ params _ body _ _) = blockRefs (paramsEnv params) body
        elemRefs (SAST.GlobalDeclaration glb) = globalRefs glb
        elemRefs (SAST.TypeDefinition (SAST.Class _ _ members _ _) _) =
            concatMap classMemberRefs members
        elemRefs (SAST.TypeDefinition _ _) = []

        globalRefs :: SAST.Global SemanticAnn -> [Entry]
        globalRefs (SAST.Task _ _ (Just e) _ _) = exprRefs M.empty e
        globalRefs (SAST.Resource _ _ (Just e) _ _) = exprRefs M.empty e
        globalRefs (SAST.Channel _ _ (Just e) _ _) = exprRefs M.empty e
        globalRefs (SAST.Emitter _ _ (Just e) _ _) = exprRefs M.empty e
        globalRefs (SAST.Handler _ _ (Just e) _ _) = exprRefs M.empty e
        globalRefs (SAST.ConstExpr _ _ e _ _) = exprRefs M.empty e
        globalRefs _ = []

        classMemberRefs :: SAST.ClassMember SemanticAnn -> [Entry]
        classMemberRefs (SAST.ClassField _) = []
        classMemberRefs (SAST.ClassMethod _ _ params _ body ann) =
            blockRefs (memberEnv ann params) body
        classMemberRefs (SAST.ClassProcedure _ _ params body ann) =
            blockRefs (memberEnv ann params) body
        classMemberRefs (SAST.ClassViewer _ params _ body ann) =
            blockRefs (memberEnv ann params) body
        classMemberRefs (SAST.ClassAction _ _ mparam _ body ann) =
            blockRefs (memberEnv ann (maybe [] (: []) mparam)) body

-- | The parameters of a body, which are in scope from its first statement.
paramsEnv :: [SAST.Parameter SemanticAnn] -> Locals
paramsEnv params =
    M.fromList
      [ (SAST.paramIdentifier param, getLocation (SAST.paramAnnotation param))
      | param <- params ]

-- | The environment of a member of a class, which is its parameters plus self.
-- The declaration of self is the signature of the member: the AST keeps the
-- access kind of the member and no position for the word, and the signature is
-- where a reader of self looks anyway.
memberEnv :: SemanticAnn -> [SAST.Parameter SemanticAnn] -> Locals
memberEnv ann params = M.insert "self" (getLocation ann) (paramsEnv params)

-- | The uses a block makes, with the locals its statements declare added to the
-- environment as the walk goes forward, so a name resolves to the declaration
-- above it and not to the one of another branch.
blockRefs :: Locals -> SAST.Block SemanticAnn -> [Entry]
blockRefs locals = snd . foldl step (locals, []) . SAST.blockBody

    where

        step (env, acc) stmt =
            let (env', refs) = stmtRefs env stmt in (env', acc ++ refs)

stmtRefs :: Locals -> SAST.Statement SemanticAnn -> (Locals, [Entry])
stmtRefs locals (SAST.Declaration ident _ _ minit ann) =
    (M.insert ident (getLocation ann) locals, maybe [] (exprRefs locals) minit)
stmtRefs locals (SAST.AssignmentStmt obj e _) =
    (locals, objRefs locals obj ++ exprRefs locals e)
stmtRefs locals (SAST.IfElseStmt condIf elseIfs melse _) =
    (locals, ifRefs ++ elseIfRefs ++ elseRefs)

    where

        ifRefs = exprRefs locals (SAST.condIfCond condIf)
            ++ blockRefs locals (SAST.condIfBody condIf)
        elseIfRefs = concat
            [ exprRefs locals (SAST.condElseIfCond c)
                ++ blockRefs locals (SAST.condElseIfBody c)
            | c <- elseIfs ]
        elseRefs = maybe [] (blockRefs locals . SAST.condElseBody) melse
stmtRefs locals (SAST.ForLoopStmt ident _ from to mbreak body ann) =
    (locals, exprRefs locals from ++ exprRefs locals to
        ++ maybe [] (exprRefs iterator) mbreak
        ++ blockRefs iterator body)

    where

        iterator = M.insert ident (getLocation ann) locals
stmtRefs locals (SAST.MatchStmt e cases mdefault _) =
    (locals, exprRefs locals e ++ concatMap caseRefs cases
        ++ maybe [] defaultRefs mdefault)

    where

        caseRefs c = blockRefs (bound c) (SAST.matchBody c)

        -- | A case binds the values of its variant, each one declared where the
        -- case writes it.
        bound c =
            foldr
              (\(ident, bann) env -> M.insert ident (getLocation bann) env)
              locals
              (SAST.matchBVars c)

        defaultRefs (SAST.DefaultCase body _) = blockRefs locals body
stmtRefs locals (SAST.SingleExpStmt e _) = (locals, exprRefs locals e)
stmtRefs locals (SAST.ReturnStmt me _) = (locals, maybe [] (exprRefs locals) me)
stmtRefs locals (SAST.ContinueStmt e _) = (locals, exprRefs locals e)
stmtRefs locals (SAST.RebootStmt _) = (locals, [])

exprRefs :: Locals -> SAST.Expression SemanticAnn -> [Entry]
exprRefs locals (SAST.AccessObject obj) = objRefs locals obj
exprRefs _ (SAST.Constant _ _) = []
exprRefs locals (SAST.BinOp _ lhs rhs _) = exprRefs locals lhs ++ exprRefs locals rhs
exprRefs locals (SAST.ReferenceExpression _ obj _) = objRefs locals obj
exprRefs locals (SAST.Casting e _ _) = exprRefs locals e
exprRefs locals (SAST.FunctionCall ident args ann) =
    Ref (getLocation ann) (TopLevel ident) : concatMap (exprRefs locals) args
exprRefs locals (SAST.MemberFunctionCall obj ident args ann) =
    memberCall locals obj ident args ann
exprRefs locals (SAST.DerefMemberFunctionCall obj ident args ann) =
    memberCall locals obj ident args ann
exprRefs locals (SAST.ArrayInitializer e size _) =
    exprRefs locals e ++ exprRefs locals size
exprRefs locals (SAST.ArrayExprListInitializer es _) = concatMap (exprRefs locals) es
exprRefs locals (SAST.StructInitializer assignments _) =
    concatMap (fieldRefs locals) assignments
exprRefs locals (SAST.EnumVariantInitializer ident _ args ann) =
    Ref (getLocation ann) (TopLevel ident) : concatMap (exprRefs locals) args
exprRefs locals (SAST.MonadicVariantInitializer variant _) = variantRefs locals variant
exprRefs _ (SAST.StringInitializer _ _) = []
exprRefs locals (SAST.IsEnumVariantExpression obj ident _ ann) =
    Ref (getLocation ann) (TopLevel ident) : objRefs locals obj
exprRefs locals (SAST.IsMonadicVariantExpression obj _ _) = objRefs locals obj
exprRefs locals (SAST.ArraySliceExpression _ obj from to _) =
    objRefs locals obj ++ exprRefs locals from ++ exprRefs locals to

-- | A call on a member resolves to the class of the object it is called on.
memberCall :: Locals -> SAST.Object SemanticAnn -> Identifier
    -> [SAST.Expression SemanticAnn] -> SemanticAnn -> [Entry]
memberCall locals obj ident args ann =
    thisCall ++ objRefs locals obj ++ concatMap (exprRefs locals) args

    where

        thisCall =
            case classOf obj of
                Just owner -> [Ref (getLocation ann) (Member owner ident)]
                Nothing -> []

fieldRefs :: Locals -> SAST.FieldAssignment SemanticAnn -> [Entry]
fieldRefs locals (SAST.FieldValueAssignment _ e _) = exprRefs locals e
fieldRefs locals (SAST.FieldAddressAssignment _ e _) = exprRefs locals e
fieldRefs _ (SAST.FieldPortConnection _ _ _ _) = []

variantRefs :: Locals -> SAST.MonadicVariant SemanticAnn -> [Entry]
variantRefs locals (SAST.Some e) = exprRefs locals e
variantRefs locals (SAST.Ok e) = exprRefs locals e
variantRefs locals (SAST.Error e) = exprRefs locals e
variantRefs locals (SAST.Failure e) = exprRefs locals e
variantRefs _ SAST.None = []
variantRefs _ SAST.Success = []

objRefs :: Locals -> SAST.Object SemanticAnn -> [Entry]
objRefs locals (SAST.Variable ident ann) =
    case M.lookup ident locals of
        Just declared -> Ref (getLocation ann) (Local declared) : typeOf ann
        Nothing -> Ref (getLocation ann) (TopLevel ident) : typeOf ann
objRefs locals (SAST.ArrayIndexExpression obj idx _) =
    objRefs locals obj ++ exprRefs locals idx
objRefs locals (SAST.MemberAccess obj ident ann) = memberAccess locals obj ident ann
objRefs locals (SAST.Dereference obj _) = objRefs locals obj
objRefs locals (SAST.DereferenceMemberAccess obj ident ann) = memberAccess locals obj ident ann
objRefs locals (SAST.Unbox obj _) = objRefs locals obj

memberAccess :: Locals -> SAST.Object SemanticAnn -> Identifier -> SemanticAnn
    -> [Entry]
memberAccess locals obj ident ann =
    thisAccess ++ objRefs locals obj

    where

        thisAccess =
            case classOf obj of
                Just owner -> [Ref (getLocation ann) (Member owner ident)]
                Nothing -> []

-- | The type the checker gave an object, rendered the way the language writes
-- it, which is what a hover over it shows.
typeOf :: SemanticAnn -> [Entry]
typeOf ann =
    case getObjectSAnns ann of
        Just (_, ty) -> [Ty (getLocation ann) (showText ty)]
        Nothing -> []

-- | What the name at a position refers to, which is the use with the smallest
-- span that covers it: the argument of a call wins over the call itself, so the
-- cursor lands where the reader is looking.
referenceAt :: ModuleIndex -> (Int, Int) -> Maybe Target
referenceAt index position =
    case mapMaybe covering (indexRefs index) of
        [] -> Nothing
        candidates -> Just (snd (minimumBy (comparing fst) candidates))

    where

        covering (loc, target) =
            case spanOf loc of
                Just (start, end, size) | start <= position && position <= end ->
                    Just (size, target)
                _ -> Nothing

        spanOf (Position _ start end) =
            let from = (sourceLine start, sourceColumn start)
                to = (sourceLine end, sourceColumn end)
            in Just (from, to, (fst to - fst from, snd to - snd from))
        spanOf _ = Nothing

-- | The identifier written at a position of a source file, which answers for
-- what the walk does not reach, such as the name of a type inside a
-- declaration. The characters of an identifier are taken to both sides of the
-- cursor, and a position that is not on one gives nothing.
wordAt :: T.Text -> (Int, Int) -> Maybe Identifier
wordAt source (line, col) =
    case drop (line - 1) (T.lines source) of
        [] -> Nothing
        (text:_) ->
            let (before, after) = T.splitAt (col - 1) text
                start = T.takeWhileEnd isIdentChar before
                end = T.takeWhile isIdentChar after
                word = start <> end
            in if T.null word then Nothing else Just (T.unpack word)

    where

        isIdentChar c = isAlphaNum c || c == '_'

-- | The type written at a position, which is the one of the smallest object
-- that covers it, so the hover over a field shows the field and not the whole
-- expression it belongs to.
typeAt :: ModuleIndex -> (Int, Int) -> Maybe T.Text
typeAt index position =
    case mapMaybe covering (indexTypes index) of
        [] -> Nothing
        candidates -> Just (snd (minimumBy (comparing fst) candidates))

    where

        covering (loc, rendered) =
            case spanOf loc of
                Just (start, end, size) | start <= position && position <= end ->
                    Just (size, rendered)
                _ -> Nothing

        spanOf (Position _ start end) =
            let from = (sourceLine start, sourceColumn start)
                to = (sourceLine end, sourceColumn end)
            in Just (from, to, (fst to - fst from, snd to - snd from))
        spanOf _ = Nothing
