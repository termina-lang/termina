{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.TransPath.PlantUML where
import EFP.Schedulability.TransPath.AST
import qualified Data.Map.Strict as M
import EFP.Schedulability.TransPath.Types
import EFP.Schedulability.PlantUML.AST
import qualified Data.Text as T
import Control.Monad.Except
import qualified Control.Monad.State as ST
import Control.Monad
import Utils.Printer

data PlantUMLGenState = PlantUMLGenState
    {
        stepMap :: TRPStepMap TRPSemAnn
        , participants :: M.Map Identifier PlantUMLParticipant
    } deriving Show

type PlantUMLGenError = T.Text

type PlantUMLGenMonad = ExceptT PlantUMLGenError (ST.State PlantUMLGenState) 

genBlock :: Identifier -> [PlantUMLSeqDiagramStatement]  -> TransPathBlock a -> PlantUMLGenMonad [PlantUMLSeqDiagramStatement]
genBlock currentCmp acc (TPBlockCondIf blocks pos _) = do
    let startGroup = PlantUMLGroupStart (T.pack "if" <> showText pos)  Nothing
    let endGroup = PlantUMLGroupEnd
    innerStmts <- foldM (genBlock currentCmp) [] blocks
    return $ acc ++ [startGroup] ++ innerStmts ++ [endGroup]
genBlock currentCmp acc (TPBlockCondElseIf blocks pos _) = do
    let startGroup = PlantUMLGroupStart (T.pack "elif" <> showText pos)  Nothing
    let endGroup = PlantUMLGroupEnd
    innerStmts <- foldM (genBlock currentCmp) [] blocks
    return $ acc ++ [startGroup] ++ innerStmts ++ [endGroup]
genBlock currentCmp acc (TPBlockCondElse blocks pos _) = do
    let startGroup = PlantUMLGroupStart (T.pack "else" <> showText pos)  Nothing
    let endGroup = PlantUMLGroupEnd
    innerStmts <- foldM (genBlock currentCmp) [] blocks
    return $ acc ++ [startGroup] ++ innerStmts ++ [endGroup]
genBlock currentCmp acc (TPBlockForLoop iterations blocks pos _) = do
    let startGroup = PlantUMLGroupStart (T.pack "for" <> showText pos) (Just (T.pack $ show iterations))
    let endGroup = PlantUMLGroupEnd
    innerStmts <- foldM (genBlock currentCmp) [] blocks
    return $ acc ++ [startGroup] ++ innerStmts ++ [endGroup]
genBlock currentCmp acc (TPBlockMatchCase blocks pos _) = do
    let startGroup = PlantUMLGroupStart (T.pack "case" <> showText pos) Nothing
    let endGroup = PlantUMLGroupEnd
    innerStmts <- foldM (genBlock currentCmp) [] blocks
    return $ acc ++ [startGroup] ++ innerStmts ++ [endGroup]
genBlock currentCmp acc (TPBlockMemberFunctionCall args called _pos _) = do
    stepStmts <- genOperation currentCmp called args
    return $ acc ++ stepStmts
genBlock currentCmp acc (TPBlockProcedureInvoke args called _pos _) = do
    stepStmts <- genOperation currentCmp called args
    return $ acc ++ stepStmts
genBlock currentCmp acc (TPBlockAllocBox poolName _pos _) = do
    let startMsg = PlantUMLSeqMessage currentCmp poolName (Just (T.pack "alloc"))
        activate = PlantUMLSeqActivationStart currentCmp
        endMsg = PlantUMLSeqMessage poolName currentCmp Nothing
        deactivate = PlantUMLSeqActivationEnd currentCmp
    ST.modify $ \s -> s { participants = M.insert poolName (PlantUMLDatabase poolName 30) (participants s) }
    return $ acc ++ [startMsg, activate, endMsg, deactivate]
genBlock currentCmp acc (TPBlockFreeBox poolName _pos _) = do
    let startMsg = PlantUMLSeqMessage currentCmp poolName (Just (T.pack "free"))
        activate = PlantUMLSeqActivationStart currentCmp
        endMsg = PlantUMLSeqMessage poolName currentCmp Nothing
        deactivate = PlantUMLSeqActivationEnd currentCmp
    ST.modify $ \s -> s { participants = M.insert poolName (PlantUMLDatabase poolName 30) (participants s) }
    return $ acc ++ [startMsg, activate, endMsg, deactivate]
genBlock _currentCmp acc (TPBlockReturn {}) = do
    return acc 
genBlock _currentCmp acc (TPBlockReboot {}) = do
    return acc
genBlock _currentCmp acc (TPBlockSystemCall {}) = do
    return acc

genOperation :: Identifier -> TRPOperation a -> [ConstExpression a] -> PlantUMLGenMonad [PlantUMLSeqDiagramStatement]
genOperation prevCmp (TRPTaskOperation stepName taskName actionName pathName blocks nextSteps _ _) _ = do
    nextActivities <- mapM (\actId -> do
        stMap <- ST.gets stepMap
        case M.lookup actId stMap of
            Nothing -> throwError $ T.pack $ "Activity not found in step map: " ++ actId
            Just act -> genOperation taskName act []
        ) nextSteps
    let activityName = T.pack "<b>" <> T.pack stepName <> "</b> " <> T.pack actionName <> "::" <> T.pack pathName
    let startMsg = PlantUMLSeqMessage prevCmp taskName (Just activityName)
    stepStmts <- foldM (genBlock taskName) [] blocks
    ST.modify $ \s -> s { participants = M.insert taskName (PlantUMLParticipant taskName 20) (participants s) }
    return $ startMsg : stepStmts ++ concat nextActivities
genOperation prevCmp (TRPHandlerOperation stepName handlerName actionName pathName blocks nextSteps _ _) _ = do
    nextActivities <- mapM (\actId -> do
        stMap <- ST.gets stepMap
        case M.lookup actId stMap of
            Nothing -> throwError $ T.pack $ "Activity not found in step map: " ++ actId
            Just act -> genOperation handlerName act []
        ) nextSteps
    let activityName = T.pack "<b>" <> T.pack stepName <> "</b> " <> T.pack actionName <> "::" <> T.pack pathName
    let startMsg = PlantUMLSeqMessage prevCmp handlerName (Just activityName)
    stepStmts <- foldM (genBlock handlerName) [] blocks
    ST.modify $ \s -> s { participants = M.insert handlerName (PlantUMLParticipant handlerName 10) (participants s) }
    return $ startMsg : stepStmts ++ concat nextActivities
genOperation prevCmp (TRPResourceOperation currentCmp procName pathName blocks _ _) args = do
    let activityName = T.pack procName <> "::" <> T.pack pathName <> "(" <> T.intercalate ", " (map showText args) <> ")"
        startMsg = PlantUMLSeqMessage prevCmp currentCmp (Just activityName)
        activate = PlantUMLSeqActivationStart currentCmp
        endMsg = PlantUMLSeqMessage currentCmp prevCmp Nothing
        deactivate = PlantUMLSeqActivationEnd currentCmp
    stepStmts <- foldM (genBlock currentCmp) [] blocks
    ST.modify $ \s -> s { participants = M.insert currentCmp (PlantUMLDatabase currentCmp 30) (participants s) }
    return $ startMsg : activate : stepStmts ++ [endMsg, deactivate]

runPlantUMLGenerator :: Identifier -> Identifier -> TRPStepMap TRPSemAnn -> Either T.Text PlantUMLDiagram
runPlantUMLGenerator emitterId initialStep stMap =
    case M.lookup initialStep stMap of
        Nothing -> Left $ T.pack $ "Initial step not found in activity map: " ++ initialStep
        Just initialAct -> 
            let initialState = PlantUMLGenState stMap M.empty
                (result, finalState) = ST.runState (runExceptT (genOperation emitterId initialAct [])) initialState
            in case result of
                Left err -> Left err
                Right stmts -> 
                    let parts = M.elems (participants finalState) ++ [PlantUMLActor emitterId 1]
                    in Right $ PlantUMLSeqDiagram parts stmts
