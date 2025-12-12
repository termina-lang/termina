{-# LANGUAGE FlexibleContexts #-}
module EFP.Schedulability.WCET.TypeChecking where

import qualified Data.Map as M
import qualified Semantic.Types as STYPES
import qualified EFP.Schedulability.TransPath.Types as TTYPES
import qualified EFP.Schedulability.WCET.Types as WTYPES
import Utils.Annotations
import ControlFlow.Architecture.Types
import Control.Monad.Except
import qualified Control.Monad.State as ST
import EFP.Schedulability.WCET.Errors
import EFP.Schedulability.WCET.AST
import qualified Data.Set as S
import Control.Monad
import Utils.Monad
import Configuration.Platform

----------------------------------------
-- Termina Programs definitions


type TPGlobalConstsEnv = M.Map Identifier Location

data TransPathState = TransPathState
    {
        progArch :: TerminaProgArch STYPES.SemanticAnn
        , transPathMap :: TTYPES.TransPathMap TTYPES.SemanticAnn
        , globalConsts :: TPGlobalConstsEnv
        , localConsts :: S.Set Identifier
        , transWCETs :: WTYPES.WCETimesMap WTYPES.SemanticAnn
    } deriving Show

type TransPathMonad = ExceptT TransPathErrors (ST.State TransPathState)

-- | Insert immutable object (variable) in local scope.
insertConstParameter :: Location -> Identifier -> TransPathMonad ()
insertConstParameter loc ident = do
  prev <- ST.gets (M.lookup ident . globalConsts)
  case prev of
    Just prevLoc -> throwError $ annotateError loc $ EConstVarAlreadyDefined (ident, prevLoc)
    Nothing -> do
        prevParam <- ST.gets (S.member ident . localConsts)
        when prevParam $
            throwError $ annotateError loc $ EConstParamAlreadyDefined ident
        ST.modify (\s -> s{localConsts = S.insert ident (localConsts s)})

getTPClass :: Location -> Identifier -> TransPathMonad (TPClass STYPES.SemanticAnn)
getTPClass loc classId = do
    taskClss <- ST.gets (taskClasses . progArch)
    case M.lookup classId taskClss of
        Just taskCls -> return taskCls
        Nothing -> do
            handlerClss <- ST.gets (handlerClasses . progArch)
            case M.lookup classId handlerClss of
                Just handlerCls -> return handlerCls
                Nothing -> do
                    resourceClss <- ST.gets (resourceClasses . progArch)
                    case M.lookup classId resourceClss of
                        Just resourceCls -> return resourceCls
                        Nothing -> throwError . annotateError loc $ EUnknownClass classId

typeConstExpression :: (Located a) => ConstExpression a -> TransPathMonad (ConstExpression WTYPES.SemanticAnn)
typeConstExpression (ConstInt i ann) = 
    return $ ConstInt i (WTYPES.SemanticAnn (getLocation ann))
typeConstExpression (ConstObject ident ann) = do
    isGlobalConst <- ST.gets (M.member ident . globalConsts)
    unless isGlobalConst $ do
        isLocalConst <- ST.gets (S.member ident . localConsts)
        unless isLocalConst $
            throwError . annotateError (getLocation ann) $ EUnknownVariable ident
    return $ ConstObject ident (WTYPES.SemanticAnn (getLocation ann))
typeConstExpression (ConstBinOp op left right ann) = do
    left' <- typeConstExpression left
    right' <- typeConstExpression right
    return $ ConstBinOp op left' right' (WTYPES.SemanticAnn (getLocation ann))

typeWCET :: (Located a) => Identifier -> TransactionalWCET a -> TransPathMonad (TransactionalWCET WTYPES.SemanticAnn)
typeWCET plt (TransactionalWCET classId functionId pathName constParams wcet ann) = do
    tpClass <- getTPClass (getLocation ann) classId
    let clsLoc = getLocation . classAnns $ tpClass
    if sameSource clsLoc (getLocation ann) then
        return ()
    else
        throwError . annotateError (getLocation ann) $ EClassPathMismatch classId (clsLoc, getLocation ann)
    case M.lookup functionId (classMemberFunctions tpClass) of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownMemberFunction functionId (classIdentifier tpClass, getLocation . classAnns $ tpClass)
        Just (TPFunction _ params _ _ fann) -> do
            trPaths <- ST.gets transPathMap
            case M.lookup (classId, functionId) trPaths of
                Nothing -> throwError . annotateError (getLocation ann) $ EUnknownTransactionalPath functionId classId pathName
                Just funcPaths ->
                    unless (M.member pathName funcPaths) $
                        throwError . annotateError (getLocation ann) $ EUnknownTransactionalPath functionId classId pathName
            trWCETs <- ST.gets transWCETs
            pltMap <- case M.lookup plt trWCETs of
                Nothing -> return M.empty
                Just pm -> return pm
            functionWCETs <- case M.lookup (classId, functionId) pltMap of
                Nothing -> return M.empty
                Just fps -> return fps
            -- | Check that there is no other worst-case execution time defined for the same path and platform
            case M.lookup pathName functionWCETs of
                Nothing -> return ()
                Just (TransactionalWCET _ _ _ _ _ ann') ->
                    throwError . annotateError (getLocation ann) $ EDuplicatedWCETAssignment pathName plt (classId, functionId, getLocation ann')
            let funcConstParams = [name | Parameter name (TConstSubtype _) <- params]
            if length funcConstParams /= length constParams then
                throwError . annotateError (getLocation ann) $ 
                    EConstParamsNumMismatch classId functionId (toInteger (length funcConstParams)) (toInteger (length constParams)) (getLocation fann)
            else do
                tyWCET <- localScope $ 
                    mapM_ (insertConstParameter (getLocation ann)) constParams >>
                    typeConstExpression wcet
                return $ TransactionalWCET classId functionId pathName constParams tyWCET (WTYPES.SemanticAnn (getLocation ann))

typeWCETPlatformAssignment :: (Located a) => WCETPlatformAssignment a -> TransPathMonad ()
typeWCETPlatformAssignment (WCETPlatformAssignment plt wcets ann) = do
    case checkPlatform plt of
        Nothing -> throwError . annotateError (getLocation ann) $ EInvalidPlatform plt
        Just _ -> return ()
    ty_wcets <- mapM (typeWCET plt) wcets
    mapM_ insertPathWCET ty_wcets

    where 

    insertPathWCET path@(TransactionalWCET classId functionId pathName _ _ _) =
        ST.modify $ \s ->
            let newPath = M.singleton (classId, functionId)
                    (M.singleton pathName path) in
            s { 
                transWCETs = M.insertWith (M.unionWith M.union) plt newPath (transWCETs s) }
    
runWCETTypeChecking :: (Located a) => TerminaProgArch STYPES.SemanticAnn
    -> TTYPES.TransPathMap TTYPES.SemanticAnn
    -> WTYPES.WCETimesMap WTYPES.SemanticAnn
    -> [WCETPlatformAssignment a]
    -> Either TransPathErrors (WTYPES.WCETimesMap WTYPES.SemanticAnn)
runWCETTypeChecking arch trPathMap prevMap wcetPltAssig =
    let gConsts = getLocation . constantAnn <$> globalConstants arch
        initialState = TransPathState arch trPathMap gConsts S.empty prevMap in
    case ST.runState (runExceptT (mapM_ typeWCETPlatformAssignment wcetPltAssig)) initialState of
        (Left err, _) -> Left err
        (_, st) -> Right (transWCETs st)