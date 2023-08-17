{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

-- | Semantic Analysis Module i.e. Type checking
-- In particular this module, describes a mapping from |AST Parser.Annotation|
-- to ~|AST SemanticAnnotations|~ | SemanAST {ParserInfo , TypeInfo}|

module Semantic.TypeChecking where

-- Termina Ast and Utils
import           AST                  as PAST
import Annotations
import           CoreAST
import           Utils.AST
import           Utils.TypeSpecifier
import Utils.SemanAST

-- Termina Semantic AST
import qualified SemanAST             as SAST

-- Just annotations from Parser
import qualified Parsing              as Parser (Annotation (..))

----------------------------------------
-- Internal modules to the semantic phase.
-- Interpretation of types
import           Semantic.Types
-- Error module
import           Control.Monad.Except (MonadError (..))
import           Semantic.Errors
-- Semantic Monad
import           Semantic.Monad

----------------------------------------
-- Libaries and stuff

import           Data.List            (find, foldl', map, nub, sort, sortOn,
                                       (\\))
import           Data.Maybe

-- import Control.Monad.State as ST
import           Data.Map             as M

import           Control.Arrow
import           Control.Monad

import qualified Data.Graph as Graph
import Data.Graph (graphFromEdges)

type SemanticPass t = t Parser.Annotation -> SemanticMonad (t SemanticAnns)

----------------------------------------
-- Function type-checking binary operations.
-- It returns resulting type of using an operation.
typeOfOps :: Locations -> PAST.Op -> PAST.TypeSpecifier -> PAST.TypeSpecifier -> SemanticMonad SemanticAnns
typeOfOps locs op lty rty =
  either
  -- | Check if there was an error, if not, returns the first type.
  (return . SemAnn locs . ETy)
  (throwError . annotateError locs . uncurry EMismatch)
  $ typeOfOps' op lty rty
  where
    typeOfOps' :: Op -> TypeSpecifier -> TypeSpecifier -> Either TypeSpecifier (TypeSpecifier,TypeSpecifier)
    -- Alg ops Same numeric type
    typeOfOps' Multiplication tyl tyr     = cmpNumTy tyl tyl tyr
    typeOfOps' Division tyl tyr           = cmpNumTy tyl tyl tyr
    typeOfOps' Addition tyl tyr           = cmpNumTy tyl tyl tyr
    typeOfOps' Subtraction tyl tyr        = cmpNumTy tyl tyl tyr
    -- shifts both numeric but may not be the same
    -- Q2
    typeOfOps' BitwiseLeftShift tyl tyr   = justNumTy tyl tyl tyr
    typeOfOps' BitwiseRightShift tyl tyr  = justNumTy tyl tyl tyr
    -- >, =>, <, <= : some numeric type.
    typeOfOps' RelationalLT tyl tyr       = cmpNumTy Bool tyl tyr
    typeOfOps' RelationalLTE tyl tyr      = cmpNumTy Bool tyl tyr
    typeOfOps' RelationalGT tyl tyr       = cmpNumTy Bool tyl tyr
    typeOfOps' RelationalGTE tyl tyr      = cmpNumTy Bool tyl tyr
    -- Equiality: TODO I think we said structural equality, but not sure.
    typeOfOps' RelationalEqual tyl tyr    = sameTyOne tyl tyr
    typeOfOps' RelationalNotEqual tyl tyr = sameTyOne tyl tyr
    -- Bitwise. I guess this is like C so nums?
    typeOfOps' BitwiseAnd tyl tyr         = cmpNumTy Bool tyl tyr
    typeOfOps' BitwiseOr  tyl tyr         = cmpNumTy Bool tyl tyr
    typeOfOps' BitwiseXor tyl tyr         = cmpNumTy Bool tyl tyr
    -- Logical And/Or bool
    typeOfOps' LogicalAnd tyl tyr         = justBoolTy tyl tyr
    typeOfOps' LogicalOr  tyl tyr         = justBoolTy tyl tyr
    sameTyOne t t' =
      if groundTyEq t t'
      then Left t
      else Right (t,t')
    cmpNumTy tres t t' =
      if groundTyEq t t'
      then Left tres
      else Right (t,t')
    justNumTy tres t t' =
      if numTy t && numTy t'
      then Left tres
      else Right (t,t')
    justBoolTy t t' =
      if boolTy t && boolTy t'
      then Left Bool
      else Right (t,t')

-- | Type assignment list of param expressions.
paramTy :: Parser.Annotation -> [Parameter] -> [Expression Parser.Annotation] -> SemanticMonad [SAST.Expression SemanticAnns]
paramTy _ann [] [] = return []
paramTy ann (p : ps) (a : as) =
  checkParamTy (paramTypeSpecifier p) a
  >>= \tyed_exp -> (tyed_exp :) <$> paramTy ann ps as
  where checkParamTy pTy exp = mustByTy pTy =<< expressionType exp
paramTy ann (p : _) [] = throwError $ annotateError ann EFunParams
paramTy ann [] (a : _) = throwError $ annotateError ann EFunParams


rhsObject
  :: RHSObject Parser.Annotation
  -> SemanticMonad (SAST.RHSObject SemanticAnns, TypeSpecifier)
rhsObject = (first SAST.RHS <$>) . typeObject getRHSVarTy (const typeExpression) . unRHS

lhsObject
  :: LHSObject Parser.Annotation
  -> SemanticMonad (SAST.LHSObject SemanticAnns, TypeSpecifier)
lhsObject =  (first SAST.LHS <$>) . typeObject getLHSVarTy failing . unLHS
  where
    failing ann _ = throwError $ annotateError ann ELHSComplex

objectType
  :: (Parser.Annotation -> Identifier -> SemanticMonad TypeSpecifier)
  -> (Parser.Annotation -> exprIdent Parser.Annotation -> SemanticMonad (exprIdentS SemanticAnns, TypeSpecifier))
  -> Object' exprIdent Parser.Annotation
  -> SemanticMonad (SAST.Object' exprIdentS SemanticAnns)
objectType getVarTy _ (Variable ident ann) =
  SAST.Variable ident . buildExpAnn ann <$> getVarTy ann ident
objectType  _ exprIdent (IdentifierExpression e ann) =
  exprIdent ann e >>= \(e_typed, e_ty) -> return (SAST.IdentifierExpression e_typed (buildExpAnn ann e_ty))
objectType  getVarTy typeI (VectorIndexExpression obj idx ann) =
  typeObject getVarTy typeI obj >>= \(obj_typed , obj_ty) ->
  case obj_ty of
    Vector ty_elems _vexp ->
      typeExpression idx >>= \(idx_typed , idx_ty) ->
        if numTy idx_ty
        then return $ SAST.VectorIndexExpression obj_typed idx_typed $ buildExpAnn ann ty_elems
        else throwError $ annotateError ann (ENumTs [idx_ty])
    ty -> throwError $ annotateError ann (EVector ty)
objectType getVarTy typeI (MemberAccess obj ident ann) =
  typeObject getVarTy typeI obj  >>= \(obj_typed , obj_ty) ->
  case obj_ty of
    DefinedType dident -> getGlobalTy ann dident >>=
      \case{
        Struct _identTy fields _mods ->
            let mfield = find ((ident ==) . fieldIdentifier) fields in
              maybe
              (throwError $ annotateError ann (EMemberAccessNotMember ident))
              (return . SAST.MemberAccess obj_typed ident . buildExpAnn ann . fieldTypeSpecifier)
              mfield
        ;
        Class _identTy cls _mods ->
          -- TODO Class access?
          _ClassAccess
        ;
        ty -> throwError $ annotateError ann (EMemberAccessUDef (fmap (fmap forgetSemAnn) ty))
      }
    ty -> throwError $ annotateError ann (EMemberAccess ty)
objectType getVarTy typeI (MemberMethodAccess obj ident args ann) =
  _ClassMethodAccess
objectType getVarTy typeI (Dereference obj ann) =
  _Dereference

typeObject
  :: (Parser.Annotation -> Identifier -> SemanticMonad TypeSpecifier)
  -> (Parser.Annotation -> exprIdent Parser.Annotation -> SemanticMonad (exprIdentS SemanticAnns, TypeSpecifier))
  -> Object' exprIdent Parser.Annotation
  -> SemanticMonad (SAST.Object' exprIdentS SemanticAnns, TypeSpecifier)
typeObject identTy eidentTy =
  (\typed_o -> (typed_o , ) <$> getObjType typed_o) <=< objectType identTy eidentTy
  where
    getObjType = maybe (throwError $ annotateError internalErrorSeman EUnboxingObjectExpr) return . getTySpec . ty_ann . getAnnotation

-- | Function |expressionType| takes an expression from the parser, traverse it
-- annotating each node with its type.
-- Since we are also creating new nodes (|Undyn| annotations), instead of just
-- traversing, we are actually /creating/ a new tree with implicit
-- constructions.
expressionType
  :: Expression Parser.Annotation
  -> SemanticMonad (SAST.Expression SemanticAnns)
-- expressionType (Variable vident pann) =
--   -- | Assign type to a variable found in the wild (RHS variables).
--   -- The type of a variable is whatever the environment says.
--   SAST.Variable vident . buildExpAnn pann <$> getRHSVarTy pann vident
expressionType (AccessObject obj) = _TypeAccess
expressionType (Constant c pann) =
  -- | Constants
  SAST.Constant c . buildExpAnn pann <$>
  case c of
    -- Rules *(eConstTrue)* and *(eConstFalse)*
    B b -> return Bool
    -- Typing constant integers
    I tyI i ->
      -- DONE Q8
      -- Source expression | z : tyI|
      if numTy tyI -- First we check if |tyI| is a numeric type.
      then -- Then we check that |z \subseteq [[ tyI ]]|
        checkIntConstant pann tyI i >>
        return tyI
      else throwError (annotateError pann $ ENumTs [tyI])
    -- Rule *(eConstChar)*
    C c -> return Char
expressionType (Casting e nty pann) = do
  -- | Casting Expressions.
  typed_exp <- expressionType e
  type_exp <- getExpType typed_exp
  -- expressionType e >>= getExpType >>= \ety ->
  if casteableTys type_exp nty -- ety \subseteq nty
  then return (SAST.Casting typed_exp nty (buildExpAnn pann nty))
  else throwError (annotateError pann $ ECasteable type_exp nty)
expressionType (BinOp op le re pann) = do
  -- | Binary operation typings
  tyle <- expressionType le
  tyre <- expressionType re
  type_le <- getExpType tyle
  type_re <- getExpType tyre
  SAST.BinOp op tyle tyre <$> typeOfOps pann op type_le type_re
expressionType (ReferenceExpression rhs_e pann) =
  -- | Reference Expression
  -- TODO [Q15]
  rhsObject rhs_e >>= \(obj_typed, obj_type) ->
  return (SAST.ReferenceExpression obj_typed (buildExpAnn pann (Reference obj_type)))
-- Function call?
expressionType (FunctionExpression fun_name args pann) =
  -- | Function Expression.  A tradicional function call
  getFunctionTy pann fun_name >>= \(params, retty) ->
  flip (SAST.FunctionExpression fun_name) (buildExpAnn pann retty) <$> paramTy pann params args
expressionType (FieldValuesAssignmentsExpression id_ty fs pann) =
  -- | Field Type
  catchError
    (getGlobalTy pann id_ty )
    (\_ -> throwError $ annotateError pann (ETyNotStructFound id_ty))
  >>= \case{
   Struct _ ty_fs _mods  ->
       flip (SAST.FieldValuesAssignmentsExpression id_ty)
            (buildExpAnn pann (DefinedType id_ty))
       <$>
       checkFieldValues pann ty_fs fs
       ;
   Union _ ty_fs _mods ->
       flip (SAST.FieldValuesAssignmentsExpression id_ty) (buildExpAnn pann (DefinedType id_ty))
       <$> checkFieldValues pann ty_fs fs;
   x -> throwError $ annotateError pann (ETyNotStruct id_ty (fmap (fmap forgetSemAnn) x));
  }
-- IDEA Q4
expressionType (VectorInitExpression iexp kexp@(KC const) pann) = do
-- | Vector Initialization
  (typed_init , type_init) <- typeExpression iexp
  -- Check that the type is correct
  _ <- checkConstant pann const
  return (SAST.VectorInitExpression typed_init kexp (buildExpAnn pann (Vector type_init kexp)))
expressionType (VectorInitExpression _ lexp pann) = throwError $ annotateError pann (EVectorConst lexp)
-- DONE [Q5]
-- TODO [Q17]

-- Zipping list of same length
zipSameLength ::  ([b] -> e) -> ([a] -> e) -> (a -> b -> c) -> [a] -> [b] -> Either e [c]
zipSameLength ea eb f as bs = zipSameLength' ea eb f as bs []
  where
    -- Tail recursive version
    zipSameLength' :: ([b] -> e) -> ([a] -> e) -> (a -> b -> c) -> [a] -> [b] -> [c] -> Either e [c]
    zipSameLength' _ _ _ [] [] acc = Right acc
    zipSameLength' ea eb f (a : as) (b : bs) acc = zipSameLength' ea eb f as bs (f a b : acc)
    zipSameLength' ea _ _ [] bs _ = Left (ea bs)
    zipSameLength' _ eb _ as [] _ = Left (eb as)
--

checkFieldValue
  :: Parser.Annotation
  -> FieldDefinition
  -> FieldValueAssignment Parser.Annotation
  -> SemanticMonad (SAST.FieldValueAssignment SemanticAnns)
checkFieldValue loc (FieldDefinition fid fty) (FieldValueAssignment faid faexp) =
  if fid == faid
  then
    SAST.FieldValueAssignment faid <$> (expressionType faexp >>= mustByTy fty)
  else throwError $ annotateError loc (EFieldMissing [fid])

checkFieldValues
  :: Parser.Annotation
  -> [FieldDefinition]
  -> [FieldValueAssignment Parser.Annotation]
  -> SemanticMonad [SAST.FieldValueAssignment SemanticAnns]
checkFieldValues loc fds fas = checkSortedFields sorted_fds sorted_fas []
  where
    tError = throwError . annotateError loc
    sorted_fds = sortOn fieldIdentifier fds
    sorted_fas = sortOn fieldAssigIdentifier fas
    -- Same length monadic Zipwith
    checkSortedFields [] [] xs = return $ reverse xs
    checkSortedFields [] es _ = tError (EFieldExtra (fmap fieldAssigIdentifier es))
    checkSortedFields ms [] _ = tError (EFieldMissing (fmap fieldIdentifier ms))
    checkSortedFields (d:ds) (a:as) acc =
      checkFieldValue loc d a >>= checkSortedFields ds as . (:acc)

retStmt :: ReturnStmt Parser.Annotation -> SemanticMonad (SAST.ReturnStmt SemanticAnns)
retStmt (ReturnStmt Nothing anns) = return $ SAST.ReturnStmt Nothing (buildExpAnn anns Unit)
retStmt (ReturnStmt (Just e) anns)
  = typeExpression e >>= \(typed_e, e_type) ->
  -- ReturnStmt (Just ety) . buildExpAnn anns <$> getExpType ety
  return $ ReturnStmt (Just typed_e) (buildExpAnn anns e_type)

retblockType :: BlockRet Parser.Annotation -> SemanticMonad (SAST.BlockRet SemanticAnns)
retblockType (BlockRet bbody rete) = BlockRet <$> blockType bbody <*> retStmt rete

blockType :: Block Parser.Annotation -> SemanticMonad (SAST.Block SemanticAnns)
blockType = mapM statementTySimple

-- | Type checking statements. We should do something about Break
-- Rules here are just environment control.
statementTySimple :: Statement Parser.Annotation -> SemanticMonad (SAST.Statement SemanticAnns)
-- Declaration semantic analysis
statementTySimple (Declaration lhs_id lhs_type expr anns) =
  -- Check type is alright
  checkTypeDefinition anns lhs_type >>
  -- Expression and type must match
  expressionType expr >>= mustByTy lhs_type >>= \ety ->
  -- Insert variables in the local environment
  insertLocalVar anns lhs_id lhs_type >>
  -- Return annotated declaration
  return (Declaration lhs_id lhs_type ety (buildStmtAnn anns))
statementTySimple (AssignmentStmt lhs_o rhs_expr anns) =
{- TODO Q19 && Q20 -}
  lhsObject lhs_o >>= \(lhs_o_typed, lhs_o_type) ->
  -- getLHSVarTy anns lhs_id >>= \lhs_ty ->
  expressionType rhs_expr >>= mustByTy lhs_o_type >>= \ety ->
  return $ AssignmentStmt lhs_o_typed ety $ buildStmtAnn anns
statementTySimple (IfElseStmt cond_expr tt_branch elifs otherwise_branch anns) =
  -- let (cs, bds) = unzip (Prelude.map (\c -> (elseIfCond c, elseIfBody c)) elifs) in
  IfElseStmt
    <$> (mustByTy Bool =<< expressionType cond_expr)
    <*> localScope (blockType tt_branch)
    <*> mapM (\case {
                 ElseIf eCond eBd ann ->
                   ElseIf <$> (mustByTy Bool =<< expressionType eCond)
                          <*> localScope (blockType eBd)
                          <*> return (buildStmtAnn ann)
                    }) elifs
    <*> localScope (blockType otherwise_branch)
    <*> return (buildStmtAnn anns)
-- Here we could implement some abstract interpretation analysis
statementTySimple (ForLoopStmt it_id from_expr to_expr mWhile body_stmt ann) = do
  (typed_fromexpr, from_ty) <- typeExpression from_expr
  (typed_toexpr, to_ty) <- typeExpression to_expr
  -- Both boundaries should have the same numeric type
  if sameNumTy from_ty to_ty
  then
    ForLoopStmt it_id typed_fromexpr typed_toexpr
      <$> (case mWhile of
                Nothing -> return Nothing
                Just whileC -> typeExpression whileC >>= \(typed_whileC , type_whileC) ->
                    if groundTyEq Bool type_whileC
                    then return (Just typed_whileC)
                    else throwError $ annotateError ann (EForWhileTy type_whileC)
          )
      <*> addTempVars ann [(it_id, from_ty)] (blockType body_stmt)
      <*> return (buildStmtAnn ann)
  else
    throwError $ annotateError ann EBadRange
statementTySimple (SingleExpStmt expr anns) =
  flip SingleExpStmt (buildStmtAnn anns) <$> expressionType expr

----------------------------------------
-- Programs Semantic Analyzer
-- For now all are kinda the same thing but eventually they should not :shrug:
----------------------------------------

-- Keeping only type information
-- TODO Check ident is not defined?
globalCheck :: Global Parser.Annotation -> SemanticMonad (SAST.Global SemanticAnns)
globalCheck (Volatile ident ty addr mods anns) =
  checkTypeDefinition anns ty >>
  -- Check TypeSpecifier is correct
  return (Volatile ident ty addr mods (buildGlobalAnn anns (SVolatile ty)))
-- DONE [Q13]
globalCheck (Static ident ty (Just sexp@(Constant address a)) mods anns) =
  checkTypeDefinition anns ty >>
  expressionType sexp >>= \sexp_ty ->
  return (Static ident ty (Just sexp_ty) mods (buildGlobalAnn anns (SStatic ty)))
globalCheck (Static _ _ _ _ anns) = throwError $ annotateError anns EStaticK
--
globalCheck (Shared ident ty mexpr mods anns) = do
  checkTypeDefinition anns ty
  exprty <- case mexpr of
              -- If it has an initial value great
              Just expr -> Just <$> (mustByTy ty =<< expressionType expr)
              -- If it has not, we need to check for defaults.
              Nothing   -> return Nothing
  return (Shared ident ty exprty mods (buildGlobalAnn anns (SShared ty)))
-- TODO [Q14]
globalCheck (Const ident ty expr mods anns) =
  checkTypeDefinition anns ty >>
  Const ident ty
  <$> (mustByTy ty =<< expressionType expr)
  <*> pure mods
  <*> pure (buildGlobalAnn anns (SConst ty))

-- Here we actually only need Global
programSeman :: AnnASTElement Parser.Annotation -> SemanticMonad (SAST.AnnASTElement SemanticAnns)
programSeman (Task ident ps ty bret mods anns) =
  checkTypeDefinition anns ty >>
  (Task ident ps ty
  <$> -- | Add
    (addTempVars anns
    -- | list of variables
      (fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) ps)
    -- | analyze the body and check the returining type
      (retblockType bret >>= \bret' ->
        blockRetTy ty bret' >> return bret'
         ))
  <*> pure mods
  <*> pure (buildGlobal anns (GTask ps ty)))
programSeman (Function ident ps mty bret mods anns) =
  maybe (return ()) (checkTypeDefinition anns) mty >>
  (Function ident ps mty
  <$> (addTempVars anns
          (fmap (\ p -> (paramIdentifier p , paramTypeSpecifier p)) ps)
          (retblockType bret) >>= \ typed_bret ->
    (maybe
      -- | Procedure
      (blockRetTy Unit)
      -- | Function
      blockRetTy
      mty) typed_bret >> return typed_bret
      )
  <*> pure mods
  <*> pure (buildGlobal anns (GFun ps (fromMaybe Unit mty))))
programSeman (Handler ident ps ty bret mods anns) =
  checkTypeDefinition anns ty >>
  Handler ident ps ty
  <$> (addTempVars anns (fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) ps)
                  (retblockType bret) >>= \ typed_bret ->
            blockRetTy ty typed_bret >>
            return typed_bret
      )
  <*> pure mods
  <*> pure (buildGlobal anns (GHand ps ty))
programSeman (GlobalDeclaration gbl) =
  -- TODO Add global declarations
  GlobalDeclaration <$> globalCheck gbl
programSeman (TypeDefinition tydef ann) = _addType
programSeman (ModuleInclusion ident _mods anns) = undefined

typeExpression :: Expression Locations -> SemanticMonad (SAST.Expression SemanticAnns , TypeSpecifier)
typeExpression e = expressionType e >>= \typed_e -> (typed_e, ) <$> getExpType typed_e

-- | Function checking that constant expressions are correct.
-- Here we have that syntact constant expressions are quite right, but we want
-- to check that integers are correct.
checkConstant :: Locations -> Const -> SemanticMonad Const
checkConstant loc t@(I type_c c) =
  -- |type_c| is correct
  checkTypeDefinition loc type_c >>
  checkIntConstant loc type_c c >>
  return t
checkConstant _ t = pure t

checkIntConstant :: Locations -> TypeSpecifier -> Integer -> SemanticMonad ()
checkIntConstant loc tyI i =
  if memberIntCons i tyI
  then return ()
  else throwError $ annotateError loc (EConstantOutRange (I tyI i))

-- Type definition
-- Here I am traversing lists serveral times, I prefer to be clear than
-- proficient for the time being.
typeDefCheck :: Locations -> TypeDef Locations -> SemanticMonad (SAST.TypeDef SemanticAnns)
-- Check Type definitions https://hackmd.io/@termina-lang/SkglB0mq3#Struct-definitions
typeDefCheck ann (Struct ident fs mds)
  -- Check every type is well-defined
  = when (Prelude.null fs) (throwError $ annotateError ann (EStructDefEmptyStruct ident))
  >> mapM_ (fieldDefinitionTy ann) fs
  -- TODO mods?
  -- Check names are unique
  >> checkUniqueNames ann EStructDefNotUniqueField (Data.List.map fieldIdentifier fs)
  -- Return same struct
  >> return (Struct ident fs mds)
typeDefCheck ann (Union ident fs mds )
  = when (Prelude.null fs) (throwError $ annotateError ann (EUnionDefEmptyUnion ident))
  >> mapM_ (fieldDefinitionTy ann) fs
  -- TODO mods?
  -- Check names are unique
  >> checkUniqueNames ann EUnionDefNotUniqueField (Data.List.map fieldIdentifier fs)
  -- Return same struct
  >> return (Union ident fs mds)
typeDefCheck ann (Enum ident evs mds)
  -- See https://hackmd.io/@termina-lang/SkglB0mq3#Enumeration-definitions
  = when (Prelude.null evs) (throwError $ annotateError ann (EEnumDefEmpty ident))
  -- Type checking
  >> mapM_ (enumDefinitionTy ann) evs
  -- Check names are unique.
  >> checkUniqueNames ann EEnumDefNotUniqueField (Data.List.map variantIdentifier evs)
  -- Return the same definition.
  >> return (Enum ident evs mds)
typeDefCheck ann (Class ident cls mds)
  -- See https://hackmd.io/@termina-lang/SkglB0mq3#Classes
  -- check that it defines at least one method.
  = when (emptyClass cls) (throwError $ annotateError ann (EClassEmptyMethods ident))
  -- TODO loop free
  -- Split definitions in tree, fields, no self reference and self reference.
  -- plus check that all types are well define (see |classTypeChecking|)
  >> foldM
    (\(fs, nsl, sl) cl ->
       case cl of
         ClassField fs_id fs_ty ann
           -> checkTypeDefinition ann fs_ty
           >> simpleTyorFail ann fs_ty
           >> let checkFs = ClassField fs_id fs_ty (buildExpAnn ann fs_ty)
              in return (checkFs : fs, nsl ,sl )
         nslm@( ClassMethod fm_id fm_tys NoSelf body ann )
           -> mapM_ (checkTypeDefinition ann . paramTypeSpecifier) fm_tys
           >> return (fs, nslm : nsl, sl)
         slm@(ClassMethod fm_id fm_tys Self body ann)
           ->  mapM_ (checkTypeDefinition ann . paramTypeSpecifier) fm_tys
           >> return (fs, nsl, slm : sl )
        )
    ([],[],[]) cls
  >>= \(fls  -- Fields do not need type checking :shrug:
       , nsl -- NoSelf methods do not depend on the other ones.
       , sl  -- Self methods can induce undefined behaviour
       -- introduce a semi-well formed type.
       )->
  do
  -- Now we can go method by method checking everything is well typed.
  -- No Self
    nslChecked <- mapM (\case
             -- Filtered cases
             ClassField {} -> throwError (annotateError internalErrorSeman ClassSelfNoSelf)
             ClassMethod _ _ Self _ _ -> throwError (annotateError internalErrorSeman ClassSelfNoSelf)
             -- Interesting case
             ClassMethod ident ps NoSelf body ann ->
               localScope (
               -- Insert params types
                 insertLocalVariables ann (Data.List.map (\p -> (paramIdentifier p, paramTypeSpecifier p)) ps)
                 >>
              -- Type check body
                 flip (ClassMethod ident ps NoSelf) (buildExpAnn ann Unit) <$> blockType body
                 )
         ) nsl
  -- Methos with Self references.
  -- Get depndencies
    let dependencies = Data.List.map (\case{
                                         -- This shouldn't happen here but I doesn't add anything
                                         l@( ClassField ident _ _ ) -> (l, ident, []);
                                         -- This is the interesting one.
                                         l@(ClassMethod ident ps _self bs _ann) ->
                                           (l, ident
                                           -- This is weird, can we have "self.f1(53).f2"
                                           , concatMap (\case{ Just ("self", [ ids ]) -> [ ids ];
                                                               a -> error ("In case the impossible happens >>> " ++ show a);
                                                              }
                                                         . depToList
                                                       )
                                             (getDepBlock bs) )
                                         ;
                                     }) sl
  -- Build dependency graph and functions from internal Node representations to ClassMembers
    let (selfMethodDepGraph, vertexF , _KeyF) = Graph.graphFromEdges dependencies
    let vertexToIdent = (\(n,_,_) -> n) . vertexF
  -- Generate a solution with possible loops.
    let topSortOrder = Data.List.map vertexToIdent $ Graph.topSort selfMethodDepGraph
  -- Type check in order, if a method is missing is because there is a loop.
    slChecked <-
      foldM (\prevMembers newMember ->
          -- Intermediate Class type only containing fields, no self methods and
          -- previous (following the topsort order) to the current method.
          let clsType = Class ident
                              -- Function |kClassMember| /erases/ body of methods.
                              -- When typing, we do not need them
                              (Data.List.map kClassMember (fls ++ nslChecked ++ prevMembers))
                              mds  in
          localScope $
          insertGlobalTy ann  clsType >>
          insertLocalVar ann "self" (Reference (DefinedType ident)) >>
          -- Now analyze new member.
          case newMember of
            -- Filtered Cases
             ClassField {} -> throwError (annotateError internalErrorSeman ClassSelfNoSelf)
             ClassMethod _ _ NoSelf _ _ -> throwError (annotateError internalErrorSeman ClassSelfNoSelf)
            -- Interesting case
             ClassMethod mIdent mps Self mbody mann ->
               -- Insert method arguments as local variables.
               insertLocalVariables
                    mann
                    (Data.List.map (\p -> (paramIdentifier p, paramTypeSpecifier p)) mps)
               -- Type check body and build the class method back.
               >>
              (:prevMembers) . flip (ClassMethod mIdent mps Self) (buildExpAnn mann Unit) <$> blockType mbody
      ) [] topSortOrder
    return (Class ident (fls ++ nslChecked ++ slChecked) mds)

----------------------------------------
-- Field definition helpers.
fieldDefinitionTy :: Locations -> FieldDefinition -> SemanticMonad ()
fieldDefinitionTy ann f
  -- First we check its type is well-defined
  = checkTypeDefinition ann tyFD
  -- and that it is simply (see simple types).
  >> simpleTyorFail ann tyFD
  -- we just return it.
  where
    tyFD = fieldTypeSpecifier f

-- Enum Variant definition helpers.
enumDefinitionTy :: Locations -> EnumVariant -> SemanticMonad ()
enumDefinitionTy ann ev
  = mapM_ (\ty -> checkTypeDefinition ann ty >> simpleTyorFail ann ty) ev_tys
  where
    ev_tys = assocData ev

----------------------------------------
-- Class Definition Helpers

-- Empty Class Methods
emptyClass :: [ ClassMember' expr lha a ] -> Bool
emptyClass = Data.List.foldl' (\ b x -> case x of { ClassField {} -> b; ClassMethod {} -> False}) True

classDependencies
  :: PAST.ClassMember a
  -> (Identifier, [ClassDep])
classDependencies (ClassField fs_id _ty _ann)
  -- Definition does not depends on anything.
  = (fs_id , [])
classDependencies (ClassMethod fm_id _ NoSelf _body _ann)
  -- No Self methods do not generate dependencies within the same class
  = (fm_id , [])
classDependencies (ClassMethod fm_id _ Self bs _ann)
  -- We need to get all invocations to other methods in self.
  = (fm_id, getDepBlock bs)

----------------------------------------
checkUniqueNames :: Locations -> ([Identifier] -> Errors Locations) -> [Identifier] -> SemanticMonad ()
checkUniqueNames ann err is =
  if allUnique is then return () else throwError $ annotateError ann (err (repeated is))
-----------------------------------------
-- TODO Improve this two functions.
-- nub is O(n^2)
allUnique :: Eq a => [a] -> Bool
allUnique xs = nub xs == xs

repeated :: Eq a => [a] -> [a]
repeated xs = nub $ xs Data.List.\\ nub xs
-----------------------------------------
