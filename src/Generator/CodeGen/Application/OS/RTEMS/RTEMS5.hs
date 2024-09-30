{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.OS.RTEMS.RTEMS5 where

import ControlFlow.AST
import Control.Monad.Except
import Generator.CodeGen.Common
import DataFlow.Architecture.Types
import Semantic.Types
import Generator.LanguageC.AST
import qualified Data.Map as M
import Generator.CodeGen.Utils
import Generator.CodeGen.Application.Types

-- | Data type used to represent a message queue in RTEMS
--  There are different types depending on the original source code element that
--  generated the message queue
data RTEMSMsgQueue =
    RTEMSTaskMsgQueue
      Identifier -- ^ task identifier
      Identifier -- ^ task class identifier
      Size -- ^ message queue size
    | RTEMSChannelMsgQueue
      Identifier -- ^ name of the channel
      TerminaType -- ^ type of the elements of the message queue
      Size -- ^ message queue size
      Identifier -- ^ name of the task that will receive the messages
      Identifier -- ^ name of the port to wich the messages will be sent
    | RTEMSSinkPortMsgQueue
      Identifier -- ^ identifier of the receiving task
      Identifier -- ^ identifier of the class of the receiving task
      Identifier -- ^ identifier of the port that will receive the messages
      TerminaType -- ^ type of the elements of the message queue
      Size -- ^ message queue size
    deriving Show

-- | Data type used to represent a resource lock in RTEMS
data RTEMSResourceLock =
    RTEMSResourceLockNone -- ^ no resource lock
    | RTEMSResourceLockIrq  -- ^ interrupt lock (disable and enable interrupts)
    | RTEMSResourceLockMutex TInteger -- ^ mutex lock with priority-ceiling protocol
    deriving Show

-- | This function generates the list of RTEMS message queues that must be created
-- from the list of tasks in the program architecture. For each task, a message queue
-- is created for the task itself and for each sink port of the task.
getTasksMessageQueues :: (MonadError CGeneratorError m) => TerminaProgArch SemanticAnn -> m [RTEMSMsgQueue]
getTasksMessageQueues progArchitecture = return $ foldr (\glb acc ->
        case glb of
            TPTask identifier classId _ sinkPorts _ _ _ _ _ -> RTEMSTaskMsgQueue identifier classId (K (TInteger 1 DecRepr)) :
                foldr (\(portId, (ts, _, _)) acc' ->
                    RTEMSSinkPortMsgQueue identifier classId portId ts (K (TInteger 1 DecRepr)) : acc'
                ) acc (M.toList sinkPorts)
    ) [] (tasks progArchitecture)

-- | This function generates the list of RTEMS message queues that must be created
-- from the list of channels in the program architecture. For each channel, a message
-- queue is created for the channel itself.
getChannelsMessageQueues :: (MonadError CGeneratorError m) => TerminaProgArch SemanticAnn -> m [RTEMSMsgQueue]
getChannelsMessageQueues progArchitecture = 
    foldM (\acc (TPMsgQueue identifier ts size _ _) ->
        case M.lookup identifier (channelTargets progArchitecture) of
            Just (task, port, _, _) -> return (RTEMSChannelMsgQueue identifier ts size task port : acc)
            Nothing -> throwError $ InternalError ("channel not connected: " ++ show identifier)
    ) [] (channels progArchitecture)

genVariantForPort ::
    -- | Name of the task class
    Identifier
    -- | Name of the port
    -> Identifier -> CSourceGenerator Identifier
genVariantForPort taskCls port = return $ namefy $ taskCls <::> port

genVariantsForTaskPorts :: TypeDef SemanticAnn -> CSourceGenerator [CFileItem]
genVariantsForTaskPorts (Class _ classId members _ _) =
    genDefineVariantsForPorts ports
    where

        ports = foldr (\field acc ->
                        case field of
                            ClassField (FieldDefinition prt (SinkPort {})) _ -> prt : acc
                            ClassField (FieldDefinition prt (InPort {})) _ -> prt : acc
                            _ -> acc ) [] members

        genDefineVariantsForPorts :: [Identifier] -> CSourceGenerator [CFileItem]
        genDefineVariantsForPorts [] = return []
        genDefineVariantsForPorts (port : xs) = do
            this_variant <- genVariantForPort classId port
            rest <- genDefineVariantsForPorts' xs 1
            return $ pre_cr (_define this_variant (Just [show (0 :: Integer)])) : rest

        genDefineVariantsForPorts' :: [Identifier] -> Integer -> CSourceGenerator [CFileItem]
        genDefineVariantsForPorts' [] _ = return []
        genDefineVariantsForPorts' (port : xs) value = do
            rest <- genDefineVariantsForPorts' xs (value + 1)
            this_variant <- genVariantForPort classId port
            return $ _define this_variant (Just [show value]) : rest

genVariantsForTaskPorts def = throwError $ InternalError $ "Definition not a class: " ++ show def

genPoolMemoryArea :: Bool -> TPPool a -> CSourceGenerator CFileItem
genPoolMemoryArea before (TPPool identifier ts size _ _) = do
    cSize <- genArraySize size
    cType <- genType noqual ts
    let poolSize = __termina_pool__size @@ [_sizeOfType cType, cSize]
    if before then 
        return $ pre_cr $ static_global (poolMemoryArea identifier) (CTArray uint8_t poolSize)
    else
        return $ static_global (poolMemoryArea identifier) (CTArray uint8_t poolSize)

genPoolMemoryAreas :: [TPPool a] -> CSourceGenerator [CFileItem]
genPoolMemoryAreas [] = return []
genPoolMemoryAreas (obj : objs) = do
    memArea <- genPoolMemoryArea True obj
    rest <- mapM (genPoolMemoryArea False) objs
    return $ memArea : rest

genAtomicDeclaration :: Bool -> TPAtomic a -> CSourceGenerator CFileItem
genAtomicDeclaration before (TPAtomic identifier ts _ _) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cType <- genType atomic ts
    return $ CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec cType) (Just identifier) Nothing)) declStmt

genAtomicDeclarations :: [TPAtomic a] -> CSourceGenerator [CFileItem]
genAtomicDeclarations [] = return []
genAtomicDeclarations (obj : objs) = do
    decl <- genAtomicDeclaration True obj
    rest <- mapM (genAtomicDeclaration False) objs
    return $ decl : rest

genAtomicArrayDeclaration :: Bool -> TPAtomicArray a -> CSourceGenerator CFileItem
genAtomicArrayDeclaration before (TPAtomicArray identifier ts size _ _) = do
    let declStmt = internalAnn (CDeclarationAnn before)
    cSize <- genArraySize size
    cType <- genType atomic ts
    return $ CExtDecl (CEDVariable Nothing (CDecl (CTypeSpec (CTArray cType cSize)) (Just identifier) Nothing)) declStmt

genAtomicArrayDeclarations :: [TPAtomicArray a] -> CSourceGenerator [CFileItem]
genAtomicArrayDeclarations [] = return []
genAtomicArrayDeclarations (obj : objs) = do
    decl <- genAtomicArrayDeclaration True obj
    rest <- mapM (genAtomicArrayDeclaration False) objs
    return $ decl : rest