{-# LANGUAGE FlexibleContexts #-}
module EFP.Schedulability.RT.TypeChecking where

import qualified Data.Map.Strict as M

import Semantic.Types
import EFP.Schedulability.RT.Semantic.Types
import Utils.Annotations
import ControlFlow.Architecture.Types
import Control.Monad.Except
import qualified Control.Monad.State as ST
import EFP.Schedulability.RT.Errors
import EFP.Schedulability.RT.Parser.AST
import qualified EFP.Schedulability.RT.Semantic.AST as SAST
import EFP.Schedulability.WCEPath.Types
import Utils.Monad
import EFP.Schedulability.Core.Types
import EFP.Schedulability.RT.Monad
import EFP.Schedulability.RT.TypeChecking.Transaction
import EFP.Schedulability.RT.TypeChecking.Situation
import Control.Monad


typeRTElement :: RTElement ParserAnn -> RTMonad (SAST.RTElement RTSemAnn)
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
    let trans = SAST.RTTransaction transId tyFirstStep (RTTransTy (getLocation ann))
    ST.modify $ \s -> s { 
            transactions = M.insert transId trans (transactions s) }
    return trans
typeRTElement (RTSituation sitId events ann) = do
    -- Check that the situation identifier is unique
    prvTransactions <- ST.gets transactions
    prvSituations <- ST.gets situations
    case M.lookup sitId prvSituations of
        Just prev -> throwError . annotateError (getLocation ann) $ 
            EPreviousSituationWithSameName sitId (getLocation . getAnnotation $ prev)
        Nothing -> return ()
    case M.lookup sitId prvTransactions of
        Just prev  -> throwError . annotateError (getLocation ann) $ 
            EPreviousTransactionWithSameName sitId (getLocation . getAnnotation $ prev)
        Nothing -> return ()
    evMap <- foldM typeEventDefinition M.empty events
    let typedSit = SAST.RTSituation sitId evMap (RTSitTy (getLocation ann))
    ST.modify $ \s -> s { 
            situations = M.insert sitId typedSit (situations s) }
    return typedSit


runRTTypeChecking :: TerminaProgArch SemanticAnn
    -> WCEPathMap WCEPSemAnn
    -> [RTElement ParserAnn]
    -> Either RTErrors [SAST.RTElement RTSemAnn]
runRTTypeChecking arch transPath elements =
    let initialState = RTState arch transPath M.empty M.empty M.empty in
    ST.evalState (runExceptT (mapM typeRTElement elements)) initialState