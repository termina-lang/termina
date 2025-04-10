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
import Modules.Modules (QualifiedName)
import Configuration.Configuration
import Semantic.AST
import Generator.CodeGen.Expression

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

    taskMessageQueues <- getTasksMessageQueues progArchitecture
    channelMessageQueues <- getChannelsMessageQueues progArchitecture

    cVariantsForTaskPorts <- concat <$> mapM genVariantsForTaskPorts (M.elems taskClss)
    cMutexDefines <- genDefineMutexId (M.keys mutexes)
    cTaskDefines <- genDefineTaskId (M.keys $ tasks progArchitecture)
    cPoolDefines <- genDefinePoolId (M.keys $ pools progArchitecture)
    cMsgQueueDefines <- genDefineMsgQueueId (taskMessageQueues ++ channelMessageQueues)
    cTimerDefines <- genDefineTimerId (M.keys periodicTimers)

    let msgQueues = taskMessageQueues ++ channelMessageQueues
    messageBufferMemory <- genMessageBufferMemory msgQueues

    return $ CHeaderFile mName $ [
            _ifndef "__CONFIG_H__",
            _define "__CONFIG_H__" Nothing
        ] ++ cVariantsForTaskPorts ++ cMutexDefines ++ cTaskDefines ++ cMsgQueueDefines ++ cPoolDefines ++ cTimerDefines
        ++ [
            pre_cr $ _define "__TERMINA_APP_CONFIG_POOLS" (Just [show (length progPools)]),
            pre_cr $ _define "__TERMINA_APP_CONFIG_TASKS" (Just [show (length progTasks)]),
            pre_cr $ _define "__TERMINA_APP_CONFIG_PERIODIC_TIMERS" (Just [show (length (M.elems periodicTimers))]),
            pre_cr $ _define "__TERMINA_APP_CONFIG_MUTEXES" (Just [show (length mutexes)]),
            pre_cr $ _define "__TERMINA_APP_CONFIG_MESSAGE_QUEUES" (Just [show (length msgQueues)])
            -- #define CONFIGURE_MAXIMUM_TIMERS
            -- #define CONFIGURE_MAXIMUM_SEMAPHORES
        ] ++ messageBufferMemory ++
        [
            pre_cr $ _define "__TERMINA_MICROSECONDS_PER_TICK" (Just [show (10000 :: Integer)]),
            pre_cr $ _define "__TERMINA_CONFIG_SYS_PRINT_BUFFER_SIZE" (Just [show (256 :: Integer)])
        ] ++
        [
            pre_cr _endif
        ]

    where

        dependenciesMap = getResDependencies progArchitecture

        genMessagesForQueue :: OSALMsgQueue -> CGenerator [String]
        genMessagesForQueue (OSALTaskMsgQueue _ _ size) = do
            cSize <- genExpression size
            let cSizeOf = _sizeOfType uint32_t
                ppSize = unpack . render $ runReader (pprint cSize) (CPrinterConfig False False)
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    __TERMINA_APP_CONFIG_MESSAGE_QUEUE_BUFFER( ",
                    "        " <> ppSize <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
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
    runReader (runExceptT (genConfigFile configFilePath progArchitecture))
        (CGeneratorEnv M.empty config irqMap)