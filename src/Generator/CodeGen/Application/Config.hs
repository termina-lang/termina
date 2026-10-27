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
import Generator.CodeGen.Expression
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
    let periodicTimers = M.filter (\case { TPPeriodicTimerEmitter {} -> True; _ -> False }) (emitters progArchitecture)
        -- | A unit test adds the harness, as a task and as an emitter, and the test emitters
        harness = [testHarness | isUnitTest progArchitecture]
        taskIds = M.keys (tasks progArchitecture) ++ harness
        emitterIds = M.keys (emitters progArchitecture) ++ harness ++ M.keys (testEmitters progArchitecture)
        poolIds = M.keys (pools progArchitecture) ++ M.keys (testPools progArchitecture)

    let resLockMap = genResourceLockings progArchitecture
    let mutexes =
            M.filter (\case{ResourceLockMutex {} -> True; _ -> False}) resLockMap

    channelMessageQueues <- getChannelsMessageQueues progArchitecture
    sinkPortMessageQueues <- getSinkPortMessageQueues progArchitecture
    taskMessageQueues <- getTasksMessageQueues progArchitecture (sinkPortMessageQueues ++ channelMessageQueues)
    let testChannelMessageQueues = getTestChannelsMessageQueues progArchitecture
        msgQueues = taskMessageQueues ++ sinkPortMessageQueues ++ channelMessageQueues ++ testChannelMessageQueues

    cEmitterDefines <- genDefineEmitterId emitterIds
    cMutexDefines <- genDefineMutexId (M.keys mutexes)
    cTaskDefines <- genDefineTaskId taskIds
    cHandlerDefines <- genDefineHandlerId (M.keys $ handlers progArchitecture)
    cPoolDefines <- genDefinePoolId poolIds
    cMsgQueueDefines <- genDefineMsgQueueId msgQueues
    cTimerDefines <- genDefineTimerId (M.keys periodicTimers)

    messageBufferMemory <- genMessageBufferMemory msgQueues

    plt <- gets targetPlatform

    return $ CHeaderFile mName $ [
            _ifndef "CONFIG_H__",
            _define "CONFIG_H__" Nothing
        ] ++ cEmitterDefines
        ++ cMutexDefines
        ++ cTaskDefines
        ++ cHandlerDefines
        ++ cMsgQueueDefines
        ++ cPoolDefines
        ++ cTimerDefines
        ++ [
            pre_cr $ _define "TERMINA__APP_CONFIG__POOLS" (Just [show (length poolIds)]),
            pre_cr $ _define "TERMINA__APP_CONFIG__TASKS" (Just [show (length taskIds)]),
            pre_cr $ _define "TERMINA__APP_CONFIG__PERIODIC_TIMERS" (Just [show (length (M.elems periodicTimers))]),
            pre_cr $ _define "TERMINA__APP_CONFIG__MUTEXES" (Just [show (length mutexes)]),
            pre_cr $ _define "TERMINA__APP_CONFIG__MESSAGE_QUEUES" (Just [show (length msgQueues)])
        ] ++ messageBufferMemory ++
        [
            pre_cr $ _define "TERMINA__TIME__MICROSECONDS_PER_TICK" (Just [show (microsecondsPerTick config) ++ "U"]),
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
        ([pre_cr $ _define "TERMINA__PROFILE__DEBUG" Nothing | profile config == Debug]) ++
        [
            pre_cr _endif
        ]

    where

        -- | Message queues with unit type do not need to be accounted for when
        -- assigning memory for the message buffer.
        genMessagesForQueue :: OSALMsgQueue -> CGenerator [String]
        genMessagesForQueue msgq = do
            elements <- msgQueueElements msgq
            case elements of
                Nothing -> return []
                Just (size, cTy) -> do
                    cSize <- genConstExpression size
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
        genMessageBufferMemory [] = return [pre_cr $ _define "TERMINA__APP_CONFIG__MESSAGE_BUFFER_MEMORY" (Just ["0"])]
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