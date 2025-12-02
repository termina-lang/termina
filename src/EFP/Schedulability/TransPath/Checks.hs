module EFP.Schedulability.TransPath.Checks where

import qualified Data.Map as M
import qualified Semantic.Types as STYPES
import qualified EFP.Schedulability.TransPath.Types as TTYPES
import Utils.Annotations
import ControlFlow.Architecture.Types
import Control.Monad.Except
import qualified Control.Monad.State as ST
import EFP.Schedulability.TransPath.Errors
import EFP.Schedulability.TransPath.AST
import qualified Data.Set as S
import Control.Monad
import Utils.Monad


type TPGlobalConstsEnv = M.Map Identifier Location
type TransPathMap a = M.Map (Identifier, Identifier) (M.Map Identifier (TransactionalWCEPath a))

data TransPathState = TransPathState
    {
        progArch :: TerminaProgArch STYPES.SemanticAnn
        , globalConsts :: TPGlobalConstsEnv
        , localConsts :: S.Set Identifier
        , transPaths :: TransPathMap TTYPES.SemanticAnn
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

getTPClass :: (Located a) => a -> Identifier -> TransPathMonad (TPClass STYPES.SemanticAnn)
getTPClass ann classId = do
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
                        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownClass classId

checkConstExpression :: (Located a) => ConstExpression a  -> TransPathMonad ()
checkConstExpression (ConstInt _ _)= return ()
checkConstExpression (ConstObject ident ann) = do
    isGlobalConst <- ST.gets (M.member ident . globalConsts)
    unless isGlobalConst $ do
        isLocalConst <- ST.gets (S.member ident . localConsts)
        unless isLocalConst $
            throwError . annotateError (getLocation ann) $ EUnknownVariable ident
checkConstExpression (ConstBinOp _ left right _) = do
    checkConstExpression left   
    checkConstExpression right

checkPathBlock :: (Located a) => TPClass STYPES.SemanticAnn -> WCEPathBlock a -> TransPathMonad ()
checkPathBlock tpCls (WCEPathCondIf blks _pos _ann) = mapM_ (checkPathBlock tpCls) blks
checkPathBlock tpCls (WCEPathCondElseIf blks _pos _ann) = mapM_ (checkPathBlock tpCls) blks
checkPathBlock tpCls (WCEPathCondElse blks _pos _ann) = mapM_ (checkPathBlock tpCls) blks
checkPathBlock tpCls (WCEPathForLoop initExpr finalExpr blks _pos _ann) = do
    checkConstExpression initExpr
    checkConstExpression finalExpr
    mapM_ (checkPathBlock tpCls) blks
checkPathBlock tpCls (WCEPathMatchCase blks _pos _ann) = mapM_ (checkPathBlock tpCls) blks
checkPathBlock tpCls (WCEPSendMessage portName _pos _ann) = do
    case M.lookup portName (outputPorts tpCls) of
        Nothing -> throwError . annotateError (getLocation _ann) $ EUnknownOutputPort portName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just _ -> return ()
checkPathBlock tpCls (WCEPathMemberFunctionCall funcName argExprs _pos _ann) = do
    case M.lookup funcName (classMemberFunctions tpCls) of
        Nothing -> throwError . annotateError (getLocation _ann) $ EUnknownMemberFunction funcName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just _ -> mapM_ checkConstExpression argExprs
checkPathBlock tpCls (WCEPProcedureInvoke portName procName argExprs _pos ann) = do
    case M.lookup portName (accessPorts tpCls) of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownAccessPort portName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just (iface, STYPES.SemanticAnn (STYPES.FTy (STYPES.AccessPortField ifacesMap)) _) -> do
            unless (M.member procName ifacesMap) $ 
                throwError . annotateError (getLocation ann) $ EUnknownProcedure procName portName iface
            mapM_ checkConstExpression argExprs
        Just _ -> throwError . annotateError Internal $ EInvalidAccessPortAnnotation
checkPathBlock tpCls (WCEPAllocBox portName _pos _ann) = do
    case M.lookup portName (accessPorts tpCls) of
        Nothing -> throwError . annotateError (getLocation _ann) $ EUnknownAccessPort portName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just _ -> return ()
checkPathBlock tpCls (WCEPFreeBox portName _pos _ann) = do
    case M.lookup portName (accessPorts tpCls) of
        Nothing -> throwError . annotateError (getLocation _ann) $ EUnknownAccessPort portName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just _ -> return ()
checkPathBlock _tpCls (WCEPRegularBlock _pos _ann) = return ()
checkPathBlock _tpCls (WCEPReturn _pos _ann) = return ()
checkPathBlock tpCls (WCEPContinue actionName _pos _ann) = do
    case M.lookup actionName (classMemberFunctions tpCls) of
        Nothing -> throwError . annotateError (getLocation _ann) $ EUnknownMemberFunction actionName (classIdentifier tpCls, getLocation . classAnns $ tpCls)
        Just _ -> return ()
checkPathBlock _tpCls (WCEPReboot _pos _ann) = return ()
checkPathBlock _tpCls (WCEPSystemCall _sysCallName argExprs _pos _ann) = mapM_ checkConstExpression argExprs

checkPath :: (Located a) => TransactionalWCEPath a -> TransPathMonad ()
checkPath (TransactionalWCEPath classId functionId pathName constParams blks ann) = do
    tpClass <- getTPClass ann classId
    case M.lookup functionId (classMemberFunctions tpClass) of
        Nothing -> throwError . annotateError (getLocation ann) $ EUnknownMemberFunction functionId (classIdentifier tpClass, getLocation . classAnns $ tpClass)
        Just (TPFunction _ params _ _ fann) -> do
            trPaths <- ST.gets transPaths
            functionPaths <- case M.lookup (classId, functionId) trPaths of
                Nothing -> return M.empty
                Just fps -> return fps
            case M.lookup pathName functionPaths of
                Nothing -> return ()
                Just (TransactionalWCEPath _ _ _ _ _ ann') ->
                    throwError . annotateError (getLocation ann) $ EDuplicatePathName pathName (classId, functionId, getLocation ann')
            let funcConstParams = [name | Parameter name (TConstSubtype _) <- params]
            if length funcConstParams /= length constParams then
                throwError . annotateError (getLocation ann) $ 
                    EConstParamsNumMismatch classId functionId (toInteger (length funcConstParams)) (toInteger (length constParams)) (getLocation fann)
            else
                localScope $ do
                    mapM_ (insertConstParameter (getLocation ann)) constParams
                    mapM_ (checkPathBlock tpClass) blks


