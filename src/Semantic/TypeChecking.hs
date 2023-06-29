{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

-- | Semantic Analysis Module i.e. Type checking
-- In particular this module, describes a mapping from |AST Parser.Annotation|
-- to |AST SemanticAnnotations|

module Semantic.TypeChecking where

-- Termina Stuff
import           AST
import           Utils.AST
import           Utils.TypeSpecifier

-- Just annotations from Parser
import qualified Parsing as Parser (Annotation(..))

-- Interpretation of types
import Semantic.Types
-- Error module
import Semantic.Errors
import           Control.Monad.Except       (MonadError (..))
-- Semantic Monad
import Semantic.Monad

----------------------------------------
-- Libaries and stuff

import           Data.List                  (foldl', sort, sortOn)
import Data.Maybe

-- import Control.Monad.State as ST
import           Data.Map                   as M

import           Control.Arrow
-- import Parser (Equation(lhs))

type SemanticPass t = t Parser.Annotation -> SemanticMonad (t SemanticAnns)

-------------
-- More helers, incomplete errors
-- Function to type check types of binary operations.
-- It returns resulting type of using an operation.
typeOfOps :: Locations -> Op -> TypeSpecifier -> TypeSpecifier -> SemanticMonad SemanticAnns
typeOfOps locs op lty rty =
  either
  -- | Check if there was an error, if not, returns the first type.
  (return . SemAnn locs . ETy)
  (throwError . annotateError locs . uncurry EMismatch)
  $ typeOfOps' op lty rty
  where
    typeOfOps' :: Op -> TypeSpecifier -> TypeSpecifier -> Either TypeSpecifier (TypeSpecifier,TypeSpecifier)
    -- Alg ops Same numeric type
    typeOfOps' Multiplication tyl tyr = cmpNumTy tyl tyl tyr
    typeOfOps' Division tyl tyr = cmpNumTy tyl tyl tyr
    typeOfOps' Addition tyl tyr = cmpNumTy tyl tyl tyr
    typeOfOps' Subtraction tyl tyr = cmpNumTy tyl tyl tyr
    -- shifts both numeric but may not be the same
    -- Q2
    typeOfOps' BitwiseLeftShift tyl tyr = justNumTy tyl tyl tyr
    typeOfOps' BitwiseRightShift tyl tyr = justNumTy tyl tyl tyr
    -- >, =>, <, <= : some numeric type.
    typeOfOps' RelationalLT tyl tyr  = cmpNumTy Bool tyl tyr
    typeOfOps' RelationalLTE tyl tyr = cmpNumTy Bool tyl tyr
    typeOfOps' RelationalGT tyl tyr  = cmpNumTy Bool tyl tyr
    typeOfOps' RelationalGTE tyl tyr = cmpNumTy Bool tyl tyr
    -- Equiality: TODO I think we said structural equality, but not sure.
    typeOfOps' RelationalEqual tyl tyr = sameTyOne tyl tyr
    typeOfOps' RelationalNotEqual tyl tyr = sameTyOne tyl tyr
    -- Bitwise. I guess this is like C so nums?
    typeOfOps' BitwiseAnd tyl tyr = cmpNumTy Bool tyl tyr
    typeOfOps' BitwiseOr  tyl tyr  = cmpNumTy Bool tyl tyr
    typeOfOps' BitwiseXor tyl tyr = cmpNumTy Bool tyl tyr
    -- Logical And/Or bool
    typeOfOps' LogicalAnd tyl tyr = justBoolTy tyl tyr
    typeOfOps' LogicalOr  tyl tyr  = justBoolTy tyl tyr
    sameTyOne t t' =
      if sameTy t t'
      then Left t
      else Right (t,t')
    cmpNumTy tres t t' =
      if sameNumTy t t'
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
paramTy :: Parser.Annotation -> [Parameter] -> [Expression Parser.Annotation] -> SemanticMonad [Expression SemanticAnns]
paramTy _ann [] [] = return []
paramTy ann (p : ps) (a : as) =
  checkParamTy (paramTypeSpecifier p) a
  >>= \tyed_exp -> (tyed_exp :) <$> paramTy ann ps as
  where checkParamTy pTy exp = mustByTy pTy =<< expressionType exp
paramTy ann (p : _) [] = throwError $ annotateError ann EFunParams
paramTy ann [] (a : _) = throwError $ annotateError ann EFunParams

expressionType :: Expression Parser.Annotation -> SemanticMonad (Expression SemanticAnns)
expressionType (Variable vident pann) =
  -- | Assign type to a variable found in the wild (RHS variables).
  Variable vident . buildExpAnn pann <$> getRHSVarTy pann vident
expressionType (Constant c pann) =
  -- | Constants
  Constant c . buildExpAnn pann <$>
  case c of
    B b -> return Bool
    I tyI i ->
      -- DONE Q8
      if numTy tyI then return tyI else throwError (annotateError pann $ ENumTs [tyI])
    C c -> return Char
expressionType (Casting e nty pann) = do
  -- | Casting Expressions.
  typed_exp <- expressionType e
  type_exp <- getExpType typed_exp
  -- expressionType e >>= getExpType >>= \ety ->
  if casteableTys type_exp nty -- ety \subseteq nty
  then return (Casting typed_exp nty (buildExpAnn pann nty)) else throwError (annotateError pann $ ECasteable type_exp nty)
expressionType (BinOp op le re pann) = do
  -- | Binary operation typings
  tyle <- expressionType le
  tyre <- expressionType re
  type_le <- getExpType tyle
  type_re <- getExpType tyre
  BinOp op tyle tyre <$> typeOfOps pann op type_le type_re
expressionType (ReferenceExpression e pann) =
  -- | Reference Expression
  -- TODO [Q15]
  expressionType e >>= \ety ->
  ReferenceExpression ety . buildExpAnn pann . Reference <$> getExpType ety
-- Function call?
expressionType (FunctionExpression fun_name args pann) =
  -- | Function Expression.  A tradicional function call
  getFunctionTy pann fun_name >>= \(params, retty) ->
  flip (FunctionExpression fun_name) (buildExpAnn pann retty) <$> paramTy pann params args
expressionType (FieldValuesAssignmentsExpression id_ty fs pann) =
  -- | Field Type
  catchError (getGlobalTy pann id_ty ) (\_ -> throwError $ annotateError pann (ETyNotStructFound id_ty))
  >>= \case{
   Struct _ ty_fs _mods _ann ->
       flip (FieldValuesAssignmentsExpression id_ty) (buildExpAnn pann (DefinedType id_ty))
       <$> checkFieldValues pann ty_fs fs;
   Union _ ty_fs _mods _ann ->
       flip (FieldValuesAssignmentsExpression id_ty) (buildExpAnn pann (DefinedType id_ty))
       <$> checkFieldValues pann ty_fs fs;
   x -> throwError $ annotateError pann (ETyNotStruct id_ty (fmap location x));
  }
expressionType (VectorIndexExpression vec_exp index_exp pann) =
  -- | VectorIndex.
  expressionType vec_exp >>= \ety ->
  getExpType ety >>= \case {
    Vector ty_elems _vexp ->
        expressionType index_exp >>= \ity ->
        getExpType ity >>= \indexType ->
        if numTy indexType
        then return $ VectorIndexExpression ety ity $ buildExpAnn pann ty_elems
        else throwError $ annotateError pann (ENumTs [indexType])
    ; ty -> throwError $ annotateError pann (EVector ty)
    }
-- IDEA Q4
expressionType (VectorInitExpression iexp kexp@(KC const) pann) = do
-- | Vector Initialization
  (typed_init , type_init) <- typeExpression iexp
  -- Check that the type is correct
  _ <- checkConstant pann const
  return (VectorInitExpression typed_init kexp (buildExpAnn pann (Vector type_init kexp)))
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
  -> SemanticMonad (FieldValueAssignment SemanticAnns)
checkFieldValue loc (FieldDefinition fid fty) (FieldValueAssignment faid faexp) =
  if fid == faid
  then
    FieldValueAssignment faid <$> (expressionType faexp >>= mustByTy fty)
  else throwError $ annotateError loc (EFieldMissing [fid])

checkFieldValues
  :: Parser.Annotation
  -> [FieldDefinition ]
  -> [FieldValueAssignment Parser.Annotation]
  -> SemanticMonad [FieldValueAssignment SemanticAnns]
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

retStmt :: ReturnStmt Parser.Annotation -> SemanticMonad (ReturnStmt SemanticAnns)
retStmt (ReturnStmt Nothing anns) = return $ ReturnStmt Nothing (buildExpAnn anns Unit)
retStmt (ReturnStmt (Just e) anns)
  = typeExpression e >>= \(typed_e, e_type) ->
  -- ReturnStmt (Just ety) . buildExpAnn anns <$> getExpType ety
  return $ ReturnStmt (Just typed_e) (buildExpAnn anns e_type)

retblockType :: BlockRet Parser.Annotation -> SemanticMonad (BlockRet SemanticAnns)
retblockType (BlockRet bbody rete) = BlockRet <$> blockType bbody <*> retStmt rete

blockType :: Block Parser.Annotation -> SemanticMonad (Block SemanticAnns)
blockType = mapM statementTySimple

-- | Type checking statements. We should do something about Break
-- Rules here are just environment control.
statementTySimple :: Statement Parser.Annotation -> SemanticMonad (Statement SemanticAnns)
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
statementTySimple (AssignmentStmt lhs_id rhs_expr anns) =
{- TODO Q19 && Q20 -}
  getLHSVarTy anns lhs_id >>= \lhs_ty ->
  expressionType rhs_expr >>= mustByTy lhs_ty >>= \ety ->
  return $ AssignmentStmt lhs_id ety $ buildStmtAnn anns
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
                    if sameTy Bool type_whileC
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

-- Task Semantic Analyzer
-- taskSemAnalyzer
--   :: [Parameter]
--   -> TypeSpecifier
--   -> BlockRet Parser.Annotation
--   -> SemanticAnns
--   -> SemanticMonad (BlockRet SemanticAnns, GEntry SemanticAnns)
-- taskSemAnalyzer taskPs taskRetType taskBody anns =
--   checkTypeDefinition anns taskRetType >>
--   ( , GTask taskPs taskRetType) <$>
--   -- | Add
--   addTempVars
--     -- | list of variables
--     (fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) taskPs)
--     -- | analyze the body and check the returining type
--     mustByTy taskRetType =<< retblockType taskBody

-- Function Semantic Analyzer
-- functionSemAnalyzer
--   :: [Parameter]
--   -> Maybe TypeSpecifier
--   -> BlockRet Parser.Annotation
--   -> SemanticAnns
--   -> SemanticMonad (BlockRet SemanticAnns, SemFunction)
-- functionSemAnalyzer funPS mbFunRetTy funBody anns =
--   maybe (return ()) (checkTypeDefinition anns) mbFunRetTy >>
--   maybe
--     (fmap (,SFunction funPS ty (buildExpAnn anns Unit)) . mustByTy Unit)
--     (\retTy ->fmap (,SFunction funPS ty (buildExpAnn anns retTy)) . mustByTy retTy)
--     mbFunRetTy
--   =<< addTempVars (fmap (\ p -> (paramIdentifier p , paramTypeSpecifier p)) funPS) (retblockType funBody)

-- Handler Semantic Analyzer (Template)
-- handlerSemAnalyzer
--   :: [Parameter]
--   -> TypeSpecifier
--   -> BlockRet Parser.Annotation
--   -> SemanticAnns
--   -> SemanticMonad (BlockRet SemanticAnns , SemHandler)
-- handlerSemAnalyzer hanPS hanType hanBody anns =
--   checkTypeDefinition anns hanType >>
--   (, SemHandler hanPS hanType anns)
--   <$>
--   addTempVars (fmap (\p -> (paramIdentifier p, paramTypeSpecifier p)) hanPS)
--               (mustByTy hanType) =<< retblockType hanBody

-- Keeping only type information
-- TODO Check ident is not defined?
globalCheck :: Global Parser.Annotation -> SemanticMonad (Global SemanticAnns)
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
              Nothing -> return Nothing
  return (Shared ident ty exprty mods (buildGlobalAnn anns (SShared ty)))
-- TODO [Q14]
globalCheck (Const ident ty expr mods anns) =
  checkTypeDefinition anns ty >>
  Const ident ty
  <$> (mustByTy ty =<< expressionType expr)
  <*> pure mods
  <*> pure (buildGlobalAnn anns (SConst ty))

-- Global Helpers
-- | Add global member if it doesn't exists
-- addGlobal :: SemMonad m => Identifier -> GEntry -> m ()
-- addGlobal ident gentry =
--   -- | Lookup if |ident| is already in global map
--   ((M.lookup ident) <$> (gets global)) >>=
--   maybe
--     -- | If it is not par of the global map
--     (modify (\s -> s{global = M.insert ident gentry (global s)}))
--     -- | If it is defined
--     (const $ throwError $ EDefinedGEntry gentry)

-- Here we actually only need Global
programSeman :: AnnASTElement Parser.Annotation -> SemanticMonad (AnnASTElement SemanticAnns)
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
programSeman (TypeDefinition tydef) = _addType
programSeman (ModuleInclusion ident _mods anns) = undefined

typeExpression :: Expression Locations -> SemanticMonad (Expression SemanticAnns , TypeSpecifier)
typeExpression e = expressionType e >>= \typed_e -> (typed_e, ) <$> getExpType typed_e

-- | Function checking that constant expressions are correct.
-- Here we have that syntact constant expressions are quite right, but we want
-- to check that integers are correct.
checkConstant :: Locations -> Const -> SemanticMonad Const
checkConstant loc t@(I type_c c) =
  -- |type_c| is correct
  checkTypeDefinition loc type_c >>
  if memberIntCons c type_c
  then pure t
  else throwError $ annotateError loc (EConstantOutRange t)
checkConstant _ t = pure t

typeDefCheck :: TypeDef Locations -> SemanticMonad (TypeDef SemanticAnns)
typeDefCheck _ = _TODO
