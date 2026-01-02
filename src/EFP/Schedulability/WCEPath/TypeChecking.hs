{-# LANGUAGE FlexibleContexts #-}
module EFP.Schedulability.WCEPath.TypeChecking where

import qualified Data.Map as M

import Semantic.Types
import EFP.Schedulability.WCEPath.Types
import Utils.Annotations
import ControlFlow.Architecture.Types
import Control.Monad.Except
import qualified Control.Monad.State as ST
import EFP.Schedulability.WCEPath.Errors
import EFP.Schedulability.WCEPath.AST
import qualified Data.Set as S
import Control.Monad
import Utils.Monad
import ControlFlow.Architecture.Utils
import EFP.Schedulability.Core.Types
import EFP.Schedulability.WCEPath.Monad



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

getTPClass :: Location -> Identifier -> TransPathMonad (TPClass SemanticAnn)
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

typeConstExpression :: ConstExpression ParserAnn -> TransPathMonad (ConstExpression WCEPSemAnn)
typeConstExpression (ConstInt i ann) = 
    return $ ConstInt i (WCEPExprTy TConstInt (getLocation ann))
typeConstExpression (ConstDouble d ann) = 
    return $ ConstDouble d (WCEPExprTy TConstDouble (getLocation ann))
typeConstExpression (ConstObject ident ann) = do
    isGlobalConst <- ST.gets (M.member ident . globalConsts)
    unless isGlobalConst $ do
        isLocalConst <- ST.gets (S.member ident . localConsts)
        unless isLocalConst $
            throwError . annotateError (getLocation ann) $ EUnknownVariable ident
    -- | For now, all constants are of integer type: Termina does not support other constant types yet.
    return $ ConstObject ident (WCEPExprTy TConstInt (getLocation ann))
typeConstExpression (ConstBinOp op left right ann) = do
    left' <- typeConstExpression left
    right' <- typeConstExpression right
    case (getAnnotation left', getAnnotation right') of
        (WCEPExprTy t1 _, WCEPExprTy t2 _) -> do
            unless (t1 == t2) $ 
                throwError . annotateError (getLocation ann) $ EConstExpressionTypeMismatch t1 t2
            return $ ConstBinOp op left' right' (WCEPExprTy t1 (getLocation ann))
        _ -> throwError . annotateError Internal $ EInvalidConstExpressionOperandTypes

typePathBlock :: TPClass SemanticAnn -> WCEPathBlock ParserAnn -> TransPathMonad (WCEPathBlock WCEPSemAnn)
typePathBlock tpCls (WCEPathCondIf blks pos ann) = do
    tyBlks <-   mapM (typePathBlock tpCls) blks
    return $ WCEPathCondIf tyBlks pos (WCEPBlock (getLocation ann))
typePathBlock tpCls (WCEPathCondElseIf blks pos ann) = do
    tyBlks <-   mapM (typePathBlock tpCls) blks
    return $ WCEPathCondElseIf tyBlks pos (WCEPBlock (getLocation ann))
typePathBlock tpCls (WCEPathCondElse blks pos ann) = do
    tyBlks <-  mapM (typePathBlock tpCls) blks
    return $ WCEPathCondElse tyBlks pos (WCEPBlock (getLocation ann))
typePathBlock tpCls (WCEPathForLoop initExpr finalExpr blks pos _ann) = do
    initExpr' <- typeConstExpression initExpr
    finalExpr' <- typeConstExpression finalExpr
    tyBlks <- mapM (typePathBlock tpCls) blks
    return $ WCEPathForLoop initExpr' finalExpr' tyBlks pos (WCEPBlock (getLocation _ann))
typePathBlock tpCls (WCEPathMatchCase blks pos ann) = do
    tyBlks <- mapM (typePathBlock tpCls) blks
    return $ WCEPathMatchCase tyBlks pos (WCEPBlock (getLocation ann))
typePathBlock tpCls (WCEPSendMessage portName pos _ann) = do
    case M.lookup portName (outputPorts tpCls) of
        Nothing -> throwError . annotateError (getLocation _ann) $ EUnknownOutputPort portName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just _ -> return $ WCEPSendMessage portName pos (WCEPBlock (getLocation _ann))
typePathBlock tpCls (WCEPathMemberFunctionCall funcName argExprs pos ann) = do
    case M.lookup funcName (M.union (classMethods tpCls) (classViewers tpCls)) of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownMemberFunction funcName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just _ -> do
            tyArgExprs <- mapM typeConstExpression argExprs
            return $ WCEPathMemberFunctionCall funcName tyArgExprs pos (WCEPBlock (getLocation ann))
typePathBlock tpCls (WCEPProcedureInvoke portName procName argExprs _pos ann) = do
    case M.lookup portName (accessPorts tpCls) of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownAccessPort portName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just (TInterface _ iface, SemanticAnn (FTy (AccessPortField ifacesMap)) _) -> do
            unless (M.member procName ifacesMap) $ 
                throwError . annotateError (getLocation ann) $ EUnknownProcedure procName portName iface
            tyArgExprs <- mapM typeConstExpression argExprs
            return $ WCEPProcedureInvoke portName procName tyArgExprs _pos (WCEPBlock (getLocation ann))
        Just (TAllocator _, _) -> throwError . annotateError (getLocation ann) $ EInvalidAccessToAllocator procName portName
        Just _ -> throwError . annotateError Internal $ EInvalidAccessPortAnnotation
typePathBlock tpCls (WCEPAllocBox portName pos ann) = do
    case M.lookup portName (accessPorts tpCls) of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownAccessPort portName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just _ -> return $ WCEPAllocBox portName pos (WCEPBlock (getLocation ann))
typePathBlock tpCls (WCEPFreeBox portName pos ann) = do
    case M.lookup portName (accessPorts tpCls) of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownAccessPort portName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just _ -> return $ WCEPFreeBox portName pos (WCEPBlock (getLocation ann))
typePathBlock _tpCls (WCEPRegularBlock pos ann) = return $ WCEPRegularBlock pos (WCEPBlock (getLocation ann))
typePathBlock _tpCls (WCEPReturn pos ann) = return $ WCEPReturn pos (WCEPBlock (getLocation ann))
typePathBlock tpCls (WCEPContinue actionName pos ann) = do
    case M.lookup actionName (classActions tpCls) of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownMemberFunction actionName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just _ -> return $ WCEPContinue actionName pos (WCEPBlock (getLocation ann))
typePathBlock _tpCls (WCEPReboot pos ann) = return $ WCEPReboot pos (WCEPBlock (getLocation ann))
typePathBlock _tpCls (WCEPSystemCall sysCallName argExprs pos ann) = do
    tyArgExprs <- mapM typeConstExpression argExprs
    return $ WCEPSystemCall sysCallName tyArgExprs pos (WCEPBlock (getLocation ann))

typePath :: WCEPath ParserAnn -> TransPathMonad (WCEPath WCEPSemAnn)
typePath (WCEPath classId functionId pathName constParams blks ann) = do
    tpClass <- getTPClass (getLocation ann) classId
    let clsLoc = getLocation . classAnns $ tpClass
    if sameSource clsLoc (getLocation ann) then
        return ()
    else
        throwError . annotateError (getLocation ann) $ EClassPathMismatch classId (clsLoc, getLocation ann)
    case M.lookup functionId (classMemberFunctions tpClass) of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownMemberFunction functionId (classIdentifier tpClass, getLocation . classAnns $ tpClass)
        Just (TPFunction _ params _ _ fann) -> do
            trPaths <- ST.gets transPaths
            functionPaths <- case M.lookup (classId, functionId) trPaths of
                Nothing -> return M.empty
                Just fps -> return fps
            -- | Check that there is no other path with the same name for the same function
            case M.lookup pathName functionPaths of
                Nothing -> return ()
                Just (WCEPath _ _ _ _ _ ann') ->
                    throwError . annotateError (getLocation ann) $ EDuplicatedPathName pathName (classId, functionId, getLocation ann')
            let funcConstParams = [name | Parameter name (TConstSubtype _) <- params]
            if length funcConstParams /= length constParams then
                throwError . annotateError (getLocation ann) $ 
                    EConstParamsNumMismatch classId functionId (toInteger (length funcConstParams)) (toInteger (length constParams)) (getLocation fann)
            else do
                tyBlks <- localScope $ 
                    mapM_ (insertConstParameter (getLocation ann)) constParams >>
                    mapM (typePathBlock tpClass) blks
                return $ WCEPath classId functionId pathName constParams tyBlks (WCEPPathTy (getLocation ann))

typeTransPaths :: [WCEPath ParserAnn] -> TransPathMonad ()
typeTransPaths paths = do
    tyPaths <- mapM typePath paths
    mapM_ insertTypedPath tyPaths

    where 

    insertTypedPath path@(WCEPath classId functionId pathName _ _ _) =
        ST.modify $ \s -> s { 
            transPaths = M.insertWith M.union (classId, functionId) (M.singleton pathName path) (transPaths s) }
    
runTransPathTypeChecking :: TerminaProgArch SemanticAnn
    -> WCEPathMap WCEPSemAnn
    -> [WCEPath ParserAnn]
    -> Either WCEPathErrors (WCEPathMap WCEPSemAnn)
runTransPathTypeChecking arch prevMap paths =
    let gConsts = getLocation . constantAnn <$> globalConstants arch
        initialState = TransPathState arch gConsts S.empty prevMap in
    case ST.runState (runExceptT (typeTransPaths paths)) initialState of
        (Left err, _) -> Left err
        (_, st) -> Right (transPaths st)