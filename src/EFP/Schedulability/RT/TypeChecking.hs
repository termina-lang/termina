{-# LANGUAGE FlexibleContexts #-}
module EFP.Schedulability.RT.TypeChecking where

import qualified Data.Map as M

import Semantic.Types
import EFP.Schedulability.RT.Types
import Utils.Annotations
import ControlFlow.Architecture.Types
import Control.Monad.Except
import qualified Control.Monad.State as ST
import EFP.Schedulability.RT.Errors
import EFP.Schedulability.RT.AST
import EFP.Schedulability.WCEPath.Types
import Utils.Monad
import EFP.Schedulability.Core.Types
import EFP.Schedulability.RT.Monad
import EFP.Schedulability.RT.TypeChecking.Transaction
import EFP.Schedulability.RT.TypeChecking.Situation


typeRTElement :: RTElement ParserAnn -> RTMonad ()
typeRTElement (RTTransaction transId firstStep ann) = do
    -- Check that the transaction identifier is unique
    prvTransactions <- ST.gets transactions
    prvSituations <- ST.gets situations
    case M.lookup transId prvTransactions of
        Just prev -> throwError . annotateError (getLocation ann) $ EPreviousTransactionWithSameName transId (getLocation . getAnnotation $ prev)
        Nothing -> return ()
    case M.lookup transId prvSituations of
        Just _ -> throwError . annotateError (getLocation ann) $ EPreviousSituationWithSameName transId (getLocation ann)
        Nothing -> return ()
    tyFirstStep <- localScope $ typeTransStep [] firstStep
    let trans = RTTransaction transId tyFirstStep (RTTransTy (getLocation ann))
    ST.modify $ \s -> s { 
            transactions = M.insert transId trans (transactions s) }
typeRTElement (RTSituation sitId sitInitializer ann) = do
    -- Check that the situation identifier is unique
    prvTransactions <- ST.gets transactions
    prvSituations <- ST.gets situations
    case M.lookup sitId prvSituations of
        Just (_prev, ann') -> throwError . annotateError (getLocation ann) $ EPreviousSituationWithSameName sitId (getLocation ann')
        Nothing -> return ()
    case M.lookup sitId prvTransactions of
        Just prev  -> throwError . annotateError (getLocation ann) $ EPreviousTransactionWithSameName sitId (getLocation . getAnnotation $ prev)
        Nothing -> return ()
    evMap <- typeSituationInitializer sitInitializer
    ST.modify $ \s -> s { 
            situations = M.insert sitId (evMap, RTSitTy (getLocation ann)) (situations s) }


runRTTypeChecking :: TerminaProgArch SemanticAnn
    -> WCEPathMap TRPSemAnn
    -> [RTElement ParserAnn]
    -> Either RTErrors (RTTransactionMap RTSemAnn, RTSituationMap RTSemAnn)
runRTTypeChecking arch transPath elements =
    let initialState = RTState arch transPath M.empty M.empty M.empty in
    case ST.runState (runExceptT (mapM_ typeRTElement elements)) initialState of
        (Left err, _) -> Left err
        (_, st) -> Right (transactions st, situations st)