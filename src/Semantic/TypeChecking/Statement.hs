module Semantic.TypeChecking.Statement where

import Utils.Annotations
import Parser.AST as PAST
import Core.Utils

-- Top Sort

-- Termina Semantic AST
import qualified Semantic.AST as SAST

----------------------------------------
-- Internal modules to the semantic phase.
-- Interpretation of types
import Semantic.Types
import Control.Monad
import Control.Monad.Except
import Semantic.Errors
-- Semantic Monad
import Semantic.Monad
import Data.Maybe
import Parser.Types
import Utils.Monad
import Semantic.TypeChecking.Expression
import qualified Data.Map as M

typeBlock :: Maybe (SAST.TerminaType SemanticAnn) -> Block ParserAnn -> SemanticMonad (SAST.Block SemanticAnn)
typeBlock rTy (Block stmts loc) = do
  compound <- mapM (typeStatement rTy) stmts
  return $ SAST.Block compound (buildStmtAnn loc)

-- | Type checking statements. We should do something about Break
-- Rules here are just environment control.
typeStatement :: Maybe (SAST.TerminaType SemanticAnn) -> Statement ParserAnn -> SemanticMonad (SAST.Statement SemanticAnn)
-- Declaration semantic analysis
typeStatement _retTy (Declaration lhs_id lhs_ak lhs_ts expr anns) = do
  lhs_type <- typeTypeSpecifier anns typeRHSObject lhs_ts
  -- Check type is alright
  checkTerminaType anns lhs_type
  -- Check if the type is a valid declaration type
  declTyOrFail anns lhs_type
  -- Expression and type must match
  ety <- typeAssignmentExpression lhs_type typeRHSObject expr
  -- Insert object in the corresponding environment
  -- If the object is mutable, then we insert it in the local mutable environment
  -- otherwise we insert it in the read-only environment
  case lhs_ak of
    Mutable -> insertLocalMutObj anns lhs_id lhs_type
    Immutable -> insertLocalImmutObj anns lhs_id lhs_type
    Private -> throwError $ annotateError Internal (EInvalidObjectDeclaration lhs_id)
  -- Return annotated declaration
  return (SAST.Declaration lhs_id lhs_ak lhs_type ety (buildStmtAnn anns))
typeStatement _retTy (AssignmentStmt lhs_o rhs_expr anns) = do
  lhs_o_typed' <- typeLHSObject lhs_o
  (lhs_o_ak', lhs_o_type') <- getObjType lhs_o_typed'
  let (lhs_o_typed, lhs_o_ak, lhs_o_type) =
        maybe (lhs_o_typed', lhs_o_ak', lhs_o_type') (unBox lhs_o_typed', Mutable, ) (isBox lhs_o_type')
  unless (lhs_o_ak /= Immutable) (throwError $ annotateError anns EAssignmentToImmutable)
  rhs_expr_typed' <- typeAssignmentExpression lhs_o_type typeRHSObject rhs_expr
  type_rhs' <- getExprType rhs_expr_typed'
  rhs_expr_typed <- maybe (return rhs_expr_typed') (\_ -> unBoxExp rhs_expr_typed') (isBox type_rhs')
  ety <- mustBeTy lhs_o_type rhs_expr_typed
  return $ SAST.AssignmentStmt lhs_o_typed ety $ buildStmtAnn anns
typeStatement retTy (IfElseStmt cond_expr tt_branch elifs otherwise_branch anns) = do
  -- | Check that if the statement defines an else-if branch, then it must have an otherwise branch
  when (not (null elifs) && isNothing otherwise_branch) (throwError $ annotateError anns EIfElseNoOtherwise)
  SAST.IfElseStmt
    -- | Check that the condition is a boolean expression
    <$> typeCondExpr cond_expr
    <*> localScope (typeBlock retTy tt_branch)
    <*> mapM (\case {
                ElseIf eCond eBd ann ->
                  SAST.ElseIf <$> typeCondExpr eCond
                         <*> localScope (typeBlock retTy eBd)
                         <*> return (buildStmtAnn ann)
                  }) elifs
    <*> maybe (return Nothing) (fmap Just . localScope . typeBlock retTy) otherwise_branch
    <*> return (buildStmtAnn anns)
  where
    typeCondExpr :: Expression ParserAnn -> SemanticMonad (SAST.Expression SemanticAnn)
    typeCondExpr bExpr = catchError (typeExpression (Just TBool) typeRHSObject bExpr)
      (\err -> case getError err of
        EMismatch TBool ty -> throwError $ annotateError (getAnnotation err) $ EIfElseIfCondNotBool ty
        _ -> throwError err
      )
-- Here we could implement some abstract interpretation analysis
typeStatement retTy (ForLoopStmt it_id it_ts from_expr to_expr mWhile body_stmt anns) = do
  it_ty <- typeTypeSpecifier anns typeRHSObject it_ts
  -- Check if the type of the iterator is a valid declaration type
  declTyOrFail anns it_ty
  -- Check the iterator is of numeric type
  unless (numTy it_ty) (throwError $ annotateError anns (EForIteratorInvalidType it_ty))
  -- Both boundaries should have the same numeric type and be a constant
  typed_fromexpr <- catchMismatch (getAnnotation from_expr) (EForLoopLowerBoundTypeMismatch it_ty)
    (typeExpression (Just (TConstSubtype it_ty)) typeRHSObject from_expr)
  typed_toexpr <- catchMismatch (getAnnotation to_expr) (EForLoopUpperBoundTypeMismatch it_ty)
    (typeExpression (Just (TConstSubtype it_ty)) typeRHSObject to_expr)
  SAST.ForLoopStmt it_id it_ty typed_fromexpr typed_toexpr
    <$> (case mWhile of
              Nothing -> return Nothing
              Just whileC -> do
                  typed_whileC <- localScope $ do
                      insertLocalImmutObj anns it_id it_ty
                      typeExpression (Just TBool) typeRHSObject whileC
                  return (Just typed_whileC)
        )
    <*> localScope (insertLocalImmutObj anns it_id it_ty >> typeBlock retTy body_stmt)
    <*> return (buildStmtAnn anns)
typeStatement _retTy (SingleExpStmt expr anns) = do
  typed_expr <- catchError (typeExpression (Just TUnit) typeRHSObject expr) (
    \err -> case getError err of
      EMismatch ty _ -> throwError $ annotateError anns (ESingleExpressionTypeNotUnit ty)
      _ -> throwError err)
  return $ SAST.SingleExpStmt typed_expr (buildStmtAnn anns)
typeStatement retTy (MatchStmt matchE cases mDefaultCase ann) = do
  typed_matchE <- typeExpression Nothing typeRHSObject matchE
  type_matchE <- getExprType typed_matchE
  -- Check for duplicate cases
  checkCaseDuplicates cases
  mTypedDefCase <- case mDefaultCase of
    Nothing -> return Nothing
    Just (DefaultCase bd ann') -> do
      typed_bd <- localScope $ typeBlock retTy bd
      return $ Just (SAST.DefaultCase typed_bd (buildStmtAnn ann'))
  case type_matchE of
    TEnum t -> getGlobalTypeDef ann t >>=
        \case {
          LocatedElement (Enum _ident flsDef _mods) _ -> do
            let flsDefIdents = variantIdentifier <$> flsDef
                casesIdents = matchIdentifier <$> cases
                variantMap = M.fromList (map (\variant@(EnumVariant vId _) -> (vId, variant)) flsDef)
                caseMap = M.fromList (map (\c@(MatchCase i _ _ _) -> (i, c)) cases)
            when total (foldM (checkMissingCase caseMap) [] flsDefIdents >>= 
              \case { [] -> return (); cs -> throwError $ annotateError ann (EMatchMissingCases cs); })
            typedCases <- mapM (typeMatchCase caseMap variantMap) casesIdents
            when (not total && (length flsDefIdents == length casesIdents)) $
              throwError $ annotateError ann EInvalidDefaultCase
            return $ SAST.MatchStmt typed_matchE typedCases mTypedDefCase (buildStmtAnn ann)
          ;
          _ -> throwError $ annotateError Internal EUnboxingEnumType
        }
    TOption t -> do
      let flsDefIdents = ["None", "Some"]
          casesIdents = matchIdentifier <$> cases
          variantMap = M.fromList [("None", EnumVariant "None"[]), ("Some", EnumVariant "Some" [t])] 
          caseMap = M.fromList (map (\c@(MatchCase i _ _ _) -> (i, c)) cases)
      when total (foldM (checkMissingCase caseMap) [] flsDefIdents >>= 
        \case { [] -> return (); cs -> throwError $ annotateError ann (EMatchMissingCases cs); })
      typedCases <- mapM (typeMatchCase caseMap variantMap) casesIdents
      when (not total && (length flsDefIdents == length casesIdents)) $
        throwError $ annotateError ann EInvalidDefaultCase
      return $ SAST.MatchStmt typed_matchE typedCases mTypedDefCase (buildStmtAnn ann)
    TStatus t -> do
      let flsDefIdents = ["Success", "Failure"]
          casesIdents = matchIdentifier <$> cases
          variantMap = M.fromList [("Success", EnumVariant "Success" []), ("Failure", EnumVariant "Failure" [t])]
          caseMap = M.fromList (map (\c@(MatchCase i _ _ _) -> (i, c)) cases)
      when total (foldM (checkMissingCase caseMap) [] flsDefIdents >>= 
        \case { [] -> return (); cs -> throwError $ annotateError ann (EMatchMissingCases cs); })
      typedCases <- mapM (typeMatchCase caseMap variantMap) casesIdents
      when (not total && (length flsDefIdents == length casesIdents)) $
        throwError $ annotateError ann EInvalidDefaultCase
      return $ SAST.MatchStmt typed_matchE typedCases mTypedDefCase (buildStmtAnn ann)
    TResult okTy errorTy -> do
      let flsDefIdents = ["Ok", "Error"]
          casesIdents = matchIdentifier <$> cases
          variantMap = M.fromList [("Ok", EnumVariant "Ok" [okTy]), ("Error", EnumVariant "Error" [errorTy])]
          caseMap = M.fromList (map (\c@(MatchCase i _ _ _) -> (i, c)) cases)
      when total (foldM (checkMissingCase caseMap) [] flsDefIdents >>= 
        \case { [] -> return (); cs -> throwError $ annotateError ann (EMatchMissingCases cs); })
      typedCases <- mapM (typeMatchCase caseMap variantMap) casesIdents
      when (not total && (length flsDefIdents == length casesIdents)) $
        throwError $ annotateError ann EInvalidDefaultCase
      return $ SAST.MatchStmt typed_matchE typedCases mTypedDefCase (buildStmtAnn ann)
    _ -> throwError $  annotateError ann $ EMatchInvalidType type_matchE

    where

      total = isNothing mDefaultCase

      checkCaseDuplicates :: [MatchCase ParserAnn] -> SemanticMonad ()
      checkCaseDuplicates = foldM_ checkCaseDuplicates' M.empty

      checkCaseDuplicates' :: 
        M.Map Identifier Location 
        -> MatchCase ParserAnn 
        -> SemanticMonad (M.Map Identifier Location)
      checkCaseDuplicates' acc (MatchCase i _ _ loc) =
        case M.lookup i acc of
          Nothing -> return $ M.insert i loc acc
          Just prevLoc -> throwError $ annotateError loc (EMatchCaseDuplicate i prevLoc)
      
      checkMissingCase :: 
        M.Map Identifier a 
        -> [Identifier]
        -> Identifier -> SemanticMonad [Identifier]
      checkMissingCase m acc i =
        case M.lookup i m of
          Nothing -> return $ i : acc
          Just _ -> return acc

      typeMatchCase :: M.Map Identifier (MatchCase ParserAnn)
        -> M.Map Identifier (SAST.EnumVariant SemanticAnn)
        -> Identifier -> SemanticMonad (SAST.MatchCase SemanticAnn)
      typeMatchCase caseMap variantMap caseId = 
        case M.lookup caseId variantMap of 
          Nothing -> throwError $ annotateError ann (EMatchCaseUnknownVariant caseId)
          Just (EnumVariant _ vData) ->  typeMatchCase' c vData

        where

          c = caseMap M.! caseId
          
          typeMatchCase' (MatchCase cIdent bVars bd mcann) tVars =
              if length bVars == length tVars then
              flip (SAST.MatchCase cIdent bVars) 
                  (buildStmtMatchCaseAnn (matchAnnotation c) tVars) 
                  <$> localScope (
                        mapM_ (uncurry $ insertLocalImmutObj mcann) (zip bVars tVars) >>
                        typeBlock retTy bd
                      )
              else throwError $ annotateError Internal EMatchCaseInternalError

typeStatement rTy (ReturnStmt retExpression anns) =
  case (rTy, retExpression) of
    (Nothing, Nothing) -> return $ SAST.ReturnStmt Nothing (buildExpAnn anns TUnit)
    (Just ts, Just e) -> do
      typed_e <- typeExpression (Just ts) typeRHSObject e
      -- ReturnStmt (Just ety) . buildExpAnn anns <$> getExprType ety
      return $ SAST.ReturnStmt (Just typed_e) (buildExpAnn anns ts)
    (Just ty, Nothing) -> throwError $ annotateError anns $ EReturnValueExpected ty
    (Nothing, Just _) -> throwError $ annotateError anns EReturnValueNotUnit
typeStatement _rTy (ContinueStmt contE anns) =
  case contE of
    MemberFunctionCall obj ident args ann -> do
      obj_typed <- typeRHSObject obj
      (_, obj_ty) <- getObjType obj_typed
      ((ps, typed_args), fty) <- typeActionCall ann obj_ty ident args
      return $ SAST.ContinueStmt (SAST.MemberFunctionCall obj_typed ident typed_args (buildExpAnnApp ann ps fty)) (buildStmtAnn anns)
    DerefMemberFunctionCall obj ident args ann -> do
      obj_typed <- typeRHSObject obj
      (_, obj_ty) <- getObjType obj_typed
      case obj_ty of
        TReference _ rTy -> do
          ((ps, typed_args), fty) <- typeActionCall ann rTy ident args
          return $ SAST.ContinueStmt (SAST.DerefMemberFunctionCall obj_typed ident typed_args (buildExpAnnApp ann ps fty)) (buildStmtAnn anns)
        ty -> throwError $ annotateError anns $ EDereferenceInvalidType ty
    _ -> throwError $ annotateError anns EContinueInvalidExpression

  where

    typeActionCall ::
      ParserAnn
      -> SAST.TerminaType SemanticAnn -- ^ type of the object
      -> Identifier -- ^ type of the member function to be called
      -> [Expression ParserAnn] -- ^ arguments
      -> SemanticMonad (([SAST.Parameter SemanticAnn], [SAST.Expression SemanticAnn]), SAST.TerminaType SemanticAnn)
    typeActionCall ann obj_ty ident args =
      case obj_ty of
        TGlobal _ dident -> getGlobalTypeDef ann dident >>=
          \case{
            -- This case corresponds to a call to an inner method or viewer from the self object.
            LocatedElement (Class _ _identTy cls _provides _mods) _ ->
              case findClassViewerOrMethod ident cls of
                Just _ -> throwError $ annotateError ann (EContinueInvalidMethodOrViewerCall ident)
                Nothing ->
                  case findClassAction ident cls of
                    Just (param@(Parameter _ ty), rty, SemanticAnn _ loc) -> do
                      case ty of
                        TUnit -> case args of
                          [] -> return (([], []), ty)
                          _ -> throwError $ annotateError ann (EContinueActionExtraArgs (ident, [], loc) (fromIntegral (length args)))
                        _ -> case args of
                          [arg] -> do
                            typed_arg <- typeExpression (Just ty) typeRHSObject arg
                            return (([param], [typed_arg]), rty)
                          [] -> throwError $ annotateError ann (EContinueActionMissingArgs (ident, loc))
                          _ -> throwError $ annotateError ann (EContinueActionExtraArgs (ident, [param], loc) (fromIntegral (length args)))
                    -- This should not happen, since the expression has been 
                    -- type-checked before and the method should have been found.
                    _ -> throwError $ annotateError Internal EContinueActionNotFound
              ;
            -- Other user-defined types do not define methods (yet?)
            _ -> throwError $ annotateError Internal EUnboxingClassType
          }
        _ -> throwError $ annotateError ann (EContinueInvalidMemberCall obj_ty)
