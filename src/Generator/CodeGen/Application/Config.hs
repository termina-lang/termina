module Generator.CodeGen.Application.Config where

import Generator.LanguageC.AST
import qualified Data.Map.Strict as M
import Generator.LanguageC.Embedded
import Generator.CodeGen.Common
import Configuration.Platform (Platform, interruptTableSize)
import Control.Monad.Except (MonadError(throwError), runExceptT)
import Control.Monad.Reader (runReader)
import Data.Text (unpack)
import Generator.LanguageC.Printer
import Semantic.Types
import ControlFlow.Architecture.Types
import ControlFlow.Architecture.Utils
import Generator.CodeGen.Application.Utils
import Configuration.Configuration
import Semantic.AST
import Generator.CodeGen.Expression
import Generator.CodeGen.Types
import Generator.Monadic
import Control.Monad.State
import qualified Data.Set as S
import Utils.Annotations
import Utils.Printer

genConfigFile ::
    QualifiedName
    -> TerminaConfig
    -> TerminaProgArch SemanticAnn
    -> CGenerator CFile
genConfigFile mName config progArchitecture = do
    let progTasks = M.elems $ tasks progArchitecture
        taskClss = taskClasses progArchitecture
        periodicTimers = M.filter (\case { TPPeriodicTimerEmitter {} -> True; _ -> False }) (emitters progArchitecture)
        progPools = M.elems $ pools progArchitecture

    let resLockMap = genResourceLockings progArchitecture
    let mutexes =
            M.filter (\case{ResourceLockMutex {} -> True; _ -> False}) resLockMap

    channelMessageQueues <- getChannelsMessageQueues progArchitecture
    sinkPortMessageQueues <- getSinkPortMessageQueues progArchitecture
    taskMessageQueues <- getTasksMessageQueues progArchitecture (sinkPortMessageQueues ++ channelMessageQueues)

    cVariantsForTaskPorts <- concat <$> traverse genVariantsForTaskPorts (M.elems taskClss)
    cEmitterDefines <- genDefineEmitterId (M.keys $ emitters progArchitecture)
    cMutexDefines <- genDefineMutexId (M.keys mutexes)
    cTaskDefines <- genDefineTaskId (M.keys $ tasks progArchitecture)
    cHandlerDefines <- genDefineHandlerId (M.keys $ handlers progArchitecture)
    cPoolDefines <- genDefinePoolId (M.keys $ pools progArchitecture)
    cMsgQueueDefines <- genDefineMsgQueueId (taskMessageQueues ++ sinkPortMessageQueues ++ channelMessageQueues)
    cTimerDefines <- genDefineTimerId (M.keys periodicTimers)

    let msgQueues = taskMessageQueues ++ sinkPortMessageQueues ++ channelMessageQueues
    messageBufferMemory <- genMessageBufferMemory msgQueues

    plt <- gets targetPlatform

    return $ CHeaderFile mName $ [
            _ifndef "CONFIG_H__",
            _define "CONFIG_H__" Nothing
        ] ++ cVariantsForTaskPorts
        ++ cEmitterDefines
        ++ cMutexDefines
        ++ cTaskDefines
        ++ cHandlerDefines
        ++ cMsgQueueDefines
        ++ cPoolDefines
        ++ cTimerDefines
        ++ [
            pre_cr $ _define "TERMINA__APP_CONFIG__POOLS" (Just [show (length progPools)]),
            pre_cr $ _define "TERMINA__APP_CONFIG__TASKS" (Just [show (length progTasks)]),
            pre_cr $ _define "TERMINA__APP_CONFIG__PERIODIC_TIMERS" (Just [show (length (M.elems periodicTimers))]),
            pre_cr $ _define "TERMINA__APP_CONFIG__MUTEXES" (Just [show (length mutexes)]),
            pre_cr $ _define "TERMINA__APP_CONFIG__MESSAGE_QUEUES" (Just [show (length msgQueues)])
        ] ++ messageBufferMemory ++
        [
            pre_cr $ _define "TERMINA__TIME__MICROSECONDS_PER_TICK" (Just [show (10000 :: Integer)]),
            -- | The size of the table the runtime indexes by interrupt vector,
            -- a property of the target. It travelled as a -D of each
            -- platform.mk until 2026-09-23, which kept the value in two places
            -- and left it invisible to make and to every tool that reads the
            -- headers.
            pre_cr $ _define "TERMINA__INTERRUPT__NUMBER_OF_INTERRUPTS" (Just [show (interruptTableSize plt)])
        ] ++
        -- | The debug profile, which the runtime reads to stop at a breakpoint
        -- before it resets. Absent means release, so a build that says nothing
        -- gets the behaviour of the target.
        ([pre_cr $ _define "TERMINA__DEBUG" Nothing | profile config == Debug]) ++
        ([pre_cr $ _define "TERMINA__SYS_PRINT__OUTPUT_BUFFER_SIZE" (Just [show $ sysPrintOutputBufferSize config]) | sysPrintOutputBufferSize config /= defaultSysPrintOutputBufferSize]) ++
        ([pre_cr $ _define "TERMINA__SYS_READ__INPUT_BUFFER_SIZE" (Just [show $ sysReadInputBufferSize config]) | sysReadInputBufferSize config /= defaultSysReadInputBufferSize]) ++
        [
            pre_cr _endif
        ]

    where

        genMessagesForQueue :: OSALMsgQueue -> CGenerator [String]
        genMessagesForQueue (OSALTaskMsgQueue _ _ size) = do
            cSize <- genExpression size
            let cSizeOf = _sizeOfType termina__event_t
                ppSize = unpack . render $ runReader (pprint cSize) (CPrinterConfig False False)
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    TERMINA__APP_CONFIG__MESSAGE_QUEUE_BUFFER( ",
                    "        " <> ppSize <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        -- | Message queues with unit type do not need to be accounted for when
        -- assigning memory for the message buffer.
        genMessagesForQueue (OSALChannelMsgQueue _ TUnit _ _ _) = return []
        genMessagesForQueue (OSALChannelMsgQueue _ ty size _ _) = do
            cSize <- genExpression size
            cTy <- genType noqual ty
            let cSizeOf = _sizeOfType cTy
                ppSize = unpack . render $ runReader (pprint cSize) (CPrinterConfig False False)
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    TERMINA__APP_CONFIG__MESSAGE_QUEUE_BUFFER( ",
                    "        " <> ppSize <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        genMessagesForQueue (OSALSinkPortMsgQueue _ _ _ ty size) = do
            cSize <- genExpression size
            cTy <- genType noqual ty
            let cSizeOf = _sizeOfType cTy
                ppSize = unpack . render $ runReader (pprint cSize) (CPrinterConfig False False)
                ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    TERMINA__APP_CONFIG__MESSAGE_QUEUE_BUFFER( ",
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
                    CPPDirective (CPPDefine "TERMINA__APP_CONFIG__MESSAGE_BUFFER_MEMORY"
                        (Just $
                            "( " : messagesForQueue ++ [")"]
                        )) (internalAnn (CPPDirectiveAnn True))
                ]


runGenConfigFile ::
    TerminaConfig
    -> Platform
    -> QualifiedName
    -> TerminaProgArch SemanticAnn
    -> Either CGeneratorError CFile
runGenConfigFile config plt configFilePath progArchitecture =
    case runState (runExceptT (genConfigFile configFilePath config progArchitecture))
        (CGeneratorEnv configFilePath S.empty emptyMonadicTypes config plt False) of
    (Left err, _) -> Left err
    (Right file, _) -> Right file