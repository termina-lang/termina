module Generator.CodeGen.Application.Config where

import Generator.LanguageC.AST
import qualified Data.Map as M
import Generator.LanguageC.Embedded
import Generator.CodeGen.Common
import Control.Monad.Except (MonadError(throwError), runExceptT)
import Control.Monad.Reader (runReader)
import Data.Text (unpack)
import Generator.LanguageC.Printer
import Semantic.Types
import ControlFlow.Architecture.Types
import Generator.Utils
import ControlFlow.Architecture.Utils
import Generator.CodeGen.Application.Utils
import Configuration.Configuration
import Semantic.AST
import Generator.CodeGen.Expression
import Generator.Monadic
import Control.Monad.State
import qualified Data.Set as S
import Utils.Annotations

genConfigFile ::
    QualifiedName
    -> TerminaProgArch SemanticAnn
    -> CGenerator CFile
genConfigFile mName progArchitecture = do
    let progTasks = M.elems $ tasks progArchitecture
        taskClss = taskClasses progArchitecture
        periodicTimers = M.filter (\case { TPPeriodicTimerEmitter {} -> True; _ -> False }) (emitters progArchitecture)
        progPools = M.elems $ pools progArchitecture

    resLockingMap <- genResLockingMap progArchitecture dependenciesMap

    let mutexes = M.filter (\case{ OSALResourceLockMutex {} -> True; _ -> False }) resLockingMap

    channelMessageQueues <- getChannelsMessageQueues progArchitecture
    sinkPortMessageQueues <- getSinkPortMessageQueues progArchitecture
    taskMessageQueues <- getTasksMessageQueues progArchitecture (sinkPortMessageQueues ++ channelMessageQueues)

    cVariantsForTaskPorts <- concat <$> traverse genVariantsForTaskPorts (M.elems taskClss)
    cMutexDefines <- genDefineMutexId (M.keys mutexes)
    cTaskDefines <- genDefineTaskId (M.keys $ tasks progArchitecture)
    cHandlerDefines <- genDefineHandlerId (M.keys $ handlers progArchitecture)
    cPoolDefines <- genDefinePoolId (M.keys $ pools progArchitecture)
    cMsgQueueDefines <- genDefineMsgQueueId (taskMessageQueues ++ sinkPortMessageQueues ++ channelMessageQueues)
    cTimerDefines <- genDefineTimerId (M.keys periodicTimers)

    let msgQueues = taskMessageQueues ++ sinkPortMessageQueues ++ channelMessageQueues
    messageBufferMemory <- genMessageBufferMemory msgQueues

    return $ CHeaderFile mName $ [
            _ifndef "__CONFIG_H__",
            _define "__CONFIG_H__" Nothing
        ] ++ cVariantsForTaskPorts 
        ++ cMutexDefines 
        ++ cTaskDefines 
        ++ cHandlerDefines 
        ++ cMsgQueueDefines 
        ++ cPoolDefines 
        ++ cTimerDefines
        ++ [
            pre_cr $ _define "__TERMINA_APP_CONFIG_POOLS" (Just [show (length progPools)]),
            pre_cr $ _define "__TERMINA_APP_CONFIG_TASKS" (Just [show (length progTasks)]),
            pre_cr $ _define "__TERMINA_APP_CONFIG_PERIODIC_TIMERS" (Just [show (length (M.elems periodicTimers))]),
            pre_cr $ _define "__TERMINA_APP_CONFIG_MUTEXES" (Just [show (length mutexes)]),
            pre_cr $ _define "__TERMINA_APP_CONFIG_MESSAGE_QUEUES" (Just [show (length msgQueues)])
        ] ++ messageBufferMemory ++
        [
            pre_cr $ _define "__TERMINA_MICROSECONDS_PER_TICK" (Just [show (10000 :: Integer)])
        ] ++
        [
            pre_cr _endif
        ]

    where

        dependenciesMap = getResDependencies progArchitecture

        genMessagesForQueue :: OSALMsgQueue -> CGenerator [String]
        genMessagesForQueue (OSALTaskMsgQueue _ _ size) = do
            cSize <- genExpression size
            let cSizeOf = _sizeOfType (typeDef terminaID)
                ppSize = unpack . render $ runReader (pprint cSize) (CPrinterConfig False False)
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    __TERMINA_APP_CONFIG_MESSAGE_QUEUE_BUFFER( ",
                    "        " <> ppSize <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        -- | Message queues with unit type do not need to be accounted for when
        -- assigning memory for the message buffer.
        genMessagesForQueue (OSALChannelMsgQueue _ TUnit _ _ _) = return []
        genMessagesForQueue (OSALChannelMsgQueue _ ts size _ _) = do
            cSize <- genExpression size
            cTs <- genType noqual ts
            let cSizeOf = _sizeOfType cTs
                ppSize = unpack . render $ runReader (pprint cSize) (CPrinterConfig False False)
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    __TERMINA_APP_CONFIG_MESSAGE_QUEUE_BUFFER( ",
                    "        " <> ppSize <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        genMessagesForQueue (OSALSinkPortMsgQueue _ _ _ ts size) = do
            cSize <- genExpression size
            cTs <- genType noqual ts
            let cSizeOf = _sizeOfType cTs
                ppSize = unpack . render $ runReader (pprint cSize) (CPrinterConfig False False)
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    __TERMINA_APP_CONFIG_MESSAGE_QUEUE_BUFFER( ",
                    "        " <> ppSize <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]

        genMessagesForQueues :: [OSALMsgQueue] -> CGenerator [String]
        genMessagesForQueues [msgq] = genMessagesForQueue msgq
        genMessagesForQueues (msgq : xs) = do
            msgsForQueue <- genMessagesForQueue msgq
            msgsForQueues <- genMessagesForQueues xs
            return $ msgsForQueue ++ ["+ "] ++ msgsForQueues
        genMessagesForQueues [] = throwError $ InternalError "Invalid message queue list: empty list"

        genMessageBufferMemory :: [OSALMsgQueue] -> CGenerator [CFileItem]
        genMessageBufferMemory [] = return []
        genMessageBufferMemory msgq = do
            messagesForQueue <- genMessagesForQueues msgq
            return [
                    CPPDirective (CPPDefine "__TERMINA_APP_CONFIG_MESSAGE_BUFFER_MEMORY"
                        (Just $
                            "( " : messagesForQueue ++ [")"]
                        )) (internalAnn (CPPDirectiveAnn True))
                ]


runGenConfigFile ::
    TerminaConfig
    -> M.Map Identifier Integer
    -> QualifiedName
    -> TerminaProgArch SemanticAnn
    -> Either CGeneratorError CFile
runGenConfigFile config irqMap configFilePath progArchitecture =
    case runState (runExceptT (genConfigFile configFilePath progArchitecture))
        (CGeneratorEnv configFilePath S.empty emptyMonadicTypes config irqMap) of
    (Left err, _) -> Left err
    (Right file, _) -> Right file