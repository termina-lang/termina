{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Platform.RTEMS5NoelSpike where

import Generator.LanguageC.AST
import Parser.Parsing
import System.FilePath
import Semantic.Monad
import AST.Seman
import Modules.Modules
import Generator.CodeGen.Common
import qualified AST.Seman as SAST
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List (find)
import Semantic.Types (GEntry (GGlob), SemGlobal (SEmitter))
import Control.Monad.Except
import Generator.LanguageC.Printer
import Control.Monad.Reader
import Data.Text (unpack)
import Utils.Annotations

data RTEMSPort =
    RTEMSEventPort
        Identifier -- ^ port identifier
        Identifier -- ^ event emitter identifier
        TypeSpecifier -- ^ data type specifier
        Identifier -- ^ action to be executed
    | RTEMSAccessPort
        Identifier -- ^ port identifier
        Identifier -- ^ resource identifier
    | RTEMSInputPort
        Identifier -- ^ port identifier
        Identifier -- ^ channel identifier
        TypeSpecifier -- ^ data type specifier
        Identifier -- ^ action to be executed
    | RTEMSOutputPort
        Identifier -- ^ port identifier
        Identifier -- ^ channel identifier
    deriving Show

data RTEMSGlobal =
    -- | RTEMS Task
    RTEMSTask
      Identifier -- ^ task identifier
      Identifier -- ^ task class identifier
      (TypeDef SemanticAnns) -- ^ task class definition
      TInteger -- ^ task priority
      TInteger -- ^ task stack size
      [RTEMSPort] -- ^ task ports
    -- | RTEMS Handler
    | RTEMSHandler
      Identifier -- ^ handler identifier
      Identifier -- ^ handler class identifier
      (TypeDef SemanticAnns)  -- ^ handler class definition
      RTEMSPort -- ^ event port
      [RTEMSPort] -- ^ resource access ports
    -- | RTEMS Resource
    | RTEMSResource
      Identifier -- ^ resource identifier
      Identifier -- ^ resource class identifier
      [RTEMSPort] -- ^ resource access ports
    | RTEMSPool
      Identifier -- ^ pool identifier
      TypeSpecifier -- ^ type of the elements of the pool
      Size -- ^ pool size
    | RTEMSAtomic
      Identifier -- ^ atomic identifier
      TypeSpecifier -- ^ type of the atomic
    | RTEMSAtomicArray
      Identifier -- ^ atomic array identifier
      TypeSpecifier -- ^ type of the elements of the atomic array
      Size -- ^ atomic array size
    deriving Show

data RTEMSEmitter =
    RTEMSInterruptEmitter
      Identifier -- ^ interrupt identifier
      RTEMSGlobal -- ^ target of the interrupt (task or handler)
    | RTEMSPeriodicTimerEmitter
      Identifier -- ^ periodic timer identifier
      RTEMSGlobal -- ^ target of the timer (task or handler)
    | RTEMSSystemInitEmitter
      Identifier -- ^ initial event identifier
      RTEMSGlobal -- ^ target of the initial event (task or handler)
    deriving Show

data RTEMSMsgQueue =
    RTEMSTaskMsgQueue
      Identifier -- ^ message queue identifier
      TInteger -- ^ message queue size
    | RTEMSChannelMsgQueue
      Identifier -- ^ name of the channel
      TypeSpecifier -- ^ type of the elements of the message queue
      TInteger -- ^ message queue size
      RTEMSGlobal -- ^ task that will receive the messages
    | RTEMSSinkPortMsgQueue
      Identifier -- ^ identifier of the receiving task
      Identifier -- ^ identifier of the port that will receive the messages
      TypeSpecifier -- ^ type of the elements of the message queue
      TInteger -- ^ message queue size
    deriving Show

data RTEMSResourceLock =
    RTEMSResourceLockNone |
    RTEMSResourceLockIrq |
    RTEMSResourceLockMutex TInteger
    deriving Show

-- | Eq instance for RTEMSGlobal
instance Eq RTEMSGlobal where
    (RTEMSTask id1 _ _ _ _ _) == (RTEMSTask id2 _ _ _ _ _) = id1 == id2
    (RTEMSHandler id1 _ _ _ _) == (RTEMSHandler id2 _ _ _ _) = id1 == id2
    (RTEMSResource id1 _ _) == (RTEMSResource id2 _ _) = id1 == id2
    _ == _ = False

-- | Ord instance for RTEMSGlobal
instance Ord RTEMSGlobal where
    compare (RTEMSTask id1 _ _ _ _ _) (RTEMSTask id2 _ _ _ _ _) = compare id1 id2
    compare (RTEMSHandler id1 _ _ _ _) (RTEMSHandler id2 _ _ _ _) = compare id1 id2
    compare (RTEMSResource id1 _ _) (RTEMSResource id2 _ _) = compare id1 id2
    compare (RTEMSPool id1 _ _) (RTEMSPool id2 _ _) = compare id1 id2
    compare (RTEMSTask {}) _ = LT
    compare (RTEMSHandler {}) _ = LT
    compare (RTEMSResource {}) _ = LT
    compare (RTEMSPool {}) _ = LT
    compare (RTEMSAtomic {}) _ = LT
    compare (RTEMSAtomicArray {}) _ = LT

-- | Returns the value of the "priority" modifier, if present in the list of modifiers.
-- If not, it returns 255, which is the default value for the priority (the lowest).
getPriority :: [Modifier] -> TInteger
getPriority [] = TInteger 255 DecRepr
getPriority ((Modifier "priority" (Just (I priority _))) : _) = priority
getPriority (_ : modifiers) = getPriority modifiers

-- | Returns the value of the "stack_size" modifier, if present in the list of modifiers.
-- If not, it returns 4096, which is the default value for the stack size (RTEMS_MINIUMUM_STACK_SIZE)
getStackSize :: [Modifier] -> TInteger
getStackSize [] = TInteger 4096 DecRepr
getStackSize ((Modifier "stack_size" (Just (I stackSize _))) : _) = stackSize
getStackSize (_ : modifiers) = getStackSize modifiers

addDependency :: RTEMSGlobal -> Maybe (S.Set RTEMSGlobal) -> Maybe (S.Set RTEMSGlobal)
addDependency newGlb Nothing = Just (S.singleton newGlb)
addDependency newGlb (Just prevGlbs) = Just (S.insert newGlb prevGlbs)

-- Finds the assignment that connects a given port
findPortConnection :: Identifier -> [FieldAssignment a] -> Maybe (FieldAssignment a)
findPortConnection _ [] = Nothing
findPortConnection identifier (assignment : assignments) =
    case assignment of
        FieldPortConnection _ port _ _ | port == identifier -> Just assignment
        _ -> findPortConnection identifier assignments

buildRTEMSGlobal :: Global SemanticAnns -> M.Map Identifier (TypeDef SemanticAnns) -> RTEMSGlobal
buildRTEMSGlobal (Task identifier (DefinedType ty) (Just (StructInitializer assignments _ _)) modifiers _) classMap =
    RTEMSTask identifier clsIdentifier clsTypeDefinition (getPriority modifiers) (getStackSize modifiers) ports
    where
        -- Task class
        (clsTypeDefinition, clsIdentifier, clsMembers) = case fromJust (M.lookup ty classMap) of
            cls@(Class TaskClass clsId members _ _) -> (cls, clsId, members)
            cls -> error $ "invalid task class: " ++ show cls

        ports =
            concatMap (\case
                ClassField (FieldDefinition portIdentifier (AccessPort {})) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection AccessPortConnection _ resourceIdentifier _) ->
                            [RTEMSAccessPort portIdentifier resourceIdentifier]
                        _ -> error $ "Invalid port connections: " ++ show portIdentifier;
                ClassField (FieldDefinition portIdentifier (SinkPort dts action)) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection InboundPortConnection _ eventEmitter _) ->
                            [RTEMSEventPort portIdentifier eventEmitter dts action]
                        _ -> error $ "Invalid port connections: " ++ show portIdentifier;
                ClassField (FieldDefinition portIdentifier (InPort dts action)) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection InboundPortConnection _ channelIdentifier _) ->
                            [RTEMSInputPort portIdentifier channelIdentifier dts action]
                        _ -> error $ "Invalid port connections: " ++ show portIdentifier;
                ClassField (FieldDefinition portIdentifier (OutPort _)) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection OutboundPortConnection _ channelIdentifier _) ->
                            [RTEMSOutputPort portIdentifier channelIdentifier]
                        _ -> error $ "Invalid port connections: " ++ show portIdentifier;
                _ -> []) clsMembers
buildRTEMSGlobal (Handler identifier (DefinedType ty) (Just (StructInitializer assignments _ _)) _ _) classMap =
    RTEMSHandler identifier clsIdentifier clsTypeDefinition eventPort ports
    where
        -- Handler class
        (clsTypeDefinition, clsIdentifier, clsMembers) = case fromJust (M.lookup ty classMap) of
            cls@(Class HandlerClass clsId members _ _) -> (cls, clsId, members)
            cls -> error $ "invalid task class: " ++ show cls

        buildEventPort :: [ClassMember' a b c] -> RTEMSPort
        buildEventPort [] = error $ "handler does not have an event port: " ++ show clsIdentifier
        buildEventPort (ClassField (FieldDefinition portIdentifier (SinkPort dts action)) _ : _) =
            case findPortConnection portIdentifier assignments of
                Just (FieldPortConnection InboundPortConnection _ emitterIdentifier _) ->
                    RTEMSEventPort portIdentifier emitterIdentifier dts action
                conn -> error $ "Invalid port connection: " ++ show conn
        buildEventPort (_ : members) = buildEventPort members

        eventPort = buildEventPort clsMembers

        ports =
            concatMap (\case
                ClassField (FieldDefinition portIdentifier (AccessPort {})) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection AccessPortConnection _ resourceIdentifier _) ->
                            [RTEMSAccessPort portIdentifier resourceIdentifier]
                        _ -> error $ "Invalid port connection: " ++ show portIdentifier;
                ClassField (FieldDefinition portIdentifier (OutPort {})) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection OutboundPortConnection _ channelIdentifier _) ->
                            [RTEMSOutputPort portIdentifier channelIdentifier]
                        _ -> error $ "Invalid port connection: " ++ show portIdentifier;
                _ -> []) clsMembers
buildRTEMSGlobal (Resource identifier (DefinedType ty) (Just (StructInitializer assignments _ _)) _ _) classMap =
    RTEMSResource identifier clsIdentifier ports
    where
        -- Resource class
        (clsIdentifier, clsMembers) = case fromJust (M.lookup ty classMap) of
            (Class ResourceClass clsId members _ _) -> (clsId, members)
            cls -> error $ "invalid task class: " ++ show cls

        ports =
            concatMap (\case
                ClassField (FieldDefinition portIdentifier (AccessPort {})) _ ->
                    case findPortConnection portIdentifier assignments of
                        Just (FieldPortConnection AccessPortConnection _ resourceIdentifier _) ->
                            [RTEMSAccessPort portIdentifier resourceIdentifier]
                        _ -> error $ "Invalid port connection: " ++ show portIdentifier;
                _ -> []) clsMembers
buildRTEMSGlobal (Resource identifier (Pool ty size) _ _ _) _ = RTEMSPool identifier ty size
buildRTEMSGlobal (Resource identifier (Atomic ty) _ _ _) _ = RTEMSAtomic identifier ty
buildRTEMSGlobal (Resource identifier (AtomicArray ty size) _ _ _) _ = RTEMSAtomicArray identifier ty size
buildRTEMSGlobal obj _ = error $ "Invalid global object: " ++ show obj

buildRTEMSEmitter :: Global SemanticAnns -> M.Map Identifier RTEMSGlobal -> Maybe RTEMSEmitter
buildRTEMSEmitter (Emitter identifier (DefinedType "Interrupt") _ _ _) connectionsMap =
    case M.lookup identifier connectionsMap of
        Just glb -> Just (RTEMSInterruptEmitter identifier glb)
        Nothing -> Nothing -- Not connected
buildRTEMSEmitter (Emitter identifier (DefinedType "PeriodicTimer") _ _ _) connectionsMap =
    case M.lookup identifier connectionsMap of
        Just glb -> Just (RTEMSPeriodicTimerEmitter identifier glb)
        Nothing -> Nothing -- Not connected
buildRTEMSEmitter (Emitter identifier (DefinedType "SystemInit") _ _ _) connectionsMap =
    case M.lookup identifier connectionsMap of
        Just glb -> Just (RTEMSSystemInitEmitter identifier glb)
        Nothing -> Nothing -- Not connected
buildRTEMSEmitter emitter@(Emitter {}) _ = error $ "Unsupported emitter" ++ show emitter
buildRTEMSEmitter _ _ = Nothing

genVariantForPort ::
    -- | Name of the task class
    Identifier
    -- | Name of the port
    -> Identifier -> CSourceGenerator Identifier
genVariantForPort taskCls port = return $ namefy $ taskCls <::> port

genVariantsForTaskPorts :: TypeDef SemanticAnns -> CSourceGenerator [CFileItem]
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
            variant <- genVariantForPort classId port
            rest <- genDefineVariantsForPorts' xs 1
            return $ CPPDirective (CPPDefine variant (Just [show (0 :: Integer)]) (CAnnotations Internal (CPPDirectiveAnn True))) : rest

        genDefineVariantsForPorts' :: [Identifier] -> Integer -> CSourceGenerator [CFileItem]
        genDefineVariantsForPorts' [] _ = return []
        genDefineVariantsForPorts' (port : xs) value = do
            rest <- genDefineVariantsForPorts' xs (value + 1)
            variant <- genVariantForPort classId port
            return $ CPPDirective (CPPDefine variant (Just [show value]) (CAnnotations Internal (CPPDirectiveAnn False))) : rest

genVariantsForTaskPorts def = throwError $ InternalError $ "Definition not a class: " ++ show def

genPoolMemoryArea :: Bool -> RTEMSGlobal -> CSourceGenerator CFileItem
genPoolMemoryArea before (RTEMSPool identifier ts size) = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn before)
        cSize = case size of
            (K s) -> CConst (CIntConst (genInteger s)) cAnn
            (V v) -> CVar v cAnn
    declSpec <- genDeclSpecifiers ts
    return $ CExtDecl $ CDeclExt $
        CDeclaration [CStorageSpec CStatic, CTypeSpec CUInt8Type]
            [(Just $ CDeclarator (Just $ poolMemoryArea identifier)
                [
                    CArrDeclr [] (CArrSize False (CCall (CVar "__termina_pool__size" cAnn) [
                        CSizeofType (CDeclaration declSpec [] (CAnnotations Internal (CDeclarationAnn False))) cAnn,
                        cSize
                    ] cAnn)) cAnn
                ] [] cAnn, Nothing, Nothing)]
            declStmt
genPoolMemoryArea _ obj = error $ "Invalid global object (not a pool): " ++ show obj

genPoolMemoryAreas :: [RTEMSGlobal] -> CSourceGenerator [CFileItem]
genPoolMemoryAreas [] = return []
genPoolMemoryAreas (obj : objs) = do
    memArea <- genPoolMemoryArea True obj
    rest <- mapM (genPoolMemoryArea False) objs
    return $ memArea : rest

genAtomicDeclaration :: Bool -> RTEMSGlobal -> CSourceGenerator CFileItem
genAtomicDeclaration before (RTEMSAtomic identifier ts) = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn before)
    declSpec <- genDeclSpecifiers ts
    return $ CExtDecl $ CDeclExt $
        CDeclaration (CTypeQual CAtomicQual : declSpec)
            [(Just $ CDeclarator (Just identifier)
                [] [] cAnn, Nothing, Nothing)]
            declStmt
genAtomicDeclaration _ obj = error $ "Invalid global object (not an atomic): " ++ show obj

genAtomicDeclarations :: [RTEMSGlobal] -> CSourceGenerator [CFileItem]
genAtomicDeclarations [] = return []
genAtomicDeclarations (obj : objs) = do
    decl <- genAtomicDeclaration True obj
    rest <- mapM (genAtomicDeclaration False) objs
    return $ decl : rest

genAtomicArrayDeclaration :: Bool -> RTEMSGlobal -> CSourceGenerator CFileItem
genAtomicArrayDeclaration before (RTEMSAtomicArray identifier ts size) = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn before)
        cSize = case size of
            (K s) -> CConst (CIntConst (genInteger s)) cAnn
            (V v) -> CVar v cAnn
    declSpec <- genDeclSpecifiers ts
    return $ CExtDecl $ CDeclExt $
        CDeclaration (CTypeQual CAtomicQual : declSpec)
            [(Just $ CDeclarator (Just identifier)
                [CArrDeclr [] (CArrSize False cSize) cAnn] [] cAnn, Nothing, Nothing)]
            declStmt
genAtomicArrayDeclaration _ obj = error $ "Invalid global object (not an atomic array): " ++ show obj

genAtomicArrayDeclarations :: [RTEMSGlobal] -> CSourceGenerator [CFileItem]
genAtomicArrayDeclarations [] = return []
genAtomicArrayDeclarations (obj : objs) = do
    decl <- genAtomicArrayDeclaration True obj
    rest <- mapM (genAtomicArrayDeclaration False) objs
    return $ decl : rest

genInterruptEmitterDeclaration :: Bool -> RTEMSEmitter -> CSourceGenerator CFileItem
genInterruptEmitterDeclaration before (RTEMSInterruptEmitter identifier (RTEMSTask {})) = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn before)
    return $ CExtDecl $ CDeclExt $
        CDeclaration [CStorageSpec CStatic, CTypeSpec $ CTypeDef ("rtems" <::> "interrupt_emitter_t")]
            [(Just $ CDeclarator (Just identifier) [] [] cAnn, Nothing, Nothing)]
            declStmt
genInterruptEmitterDeclaration _ obj = error $ "Invalid global object (not an interrupt emitter): " ++ show obj

genInterruptEmitterDeclarations :: [RTEMSEmitter] -> CSourceGenerator [CFileItem]
genInterruptEmitterDeclarations [] = return []
genInterruptEmitterDeclarations (obj : objs) = do
    decl <- genInterruptEmitterDeclaration True obj
    rest <- mapM (genInterruptEmitterDeclaration False) objs
    return $ decl : rest

genTaskClassCode :: TypeDef SemanticAnns -> CSourceGenerator CFileItem
genTaskClassCode (Class TaskClass classId members _ _) = do
    let retTypeDecl = [CStorageSpec CStatic, CTypeSpec $ CTypeDef "rtems_task"]
        cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn True)
        cReturn = [CBlockStmt $ CReturn Nothing (CAnnotations Internal (CStatementAnn True False))]
        cParam = CDeclaration [CTypeSpec $ CTypeDef "rtems_task_argument"] [(Just (CDeclarator (Just "arg") [] [] cAnn), Nothing, Nothing)]
            (CAnnotations Internal (CDeclarationAnn False))
    cBody <- genBody
    return $ CExtDecl $ CFDefExt $ CFunDef retTypeDecl
        (CDeclarator (Just $ namefy "rtems_task" <::> classId) [CFunDeclr [cParam] [] cAnn] [] cAnn)
        (CCompound (cBody ++ cReturn) (CAnnotations Internal (CCompoundAnn False True)))
        declStmt

    where

        actions :: [(Identifier, TypeSpecifier, Identifier)]
        actions = foldl (\acc member ->
            case member of
                ClassField (FieldDefinition identifier (SinkPort dts action)) _ -> (identifier, dts, action) : acc
                ClassField (FieldDefinition identifier (InPort dts action)) _ -> (identifier, dts, action) : acc
                _ -> acc
            ) [] members

        -- TOOD: The current implementation does not work with vectors
        getMsgDataVariable :: Bool -> Identifier -> TypeSpecifier -> CSourceGenerator CCompoundBlockItem
        getMsgDataVariable before action dts = do
            let cAnn = CAnnotations Internal CGenericAnn
                declStmt = CAnnotations Internal (CDeclarationAnn before)
            decls <- genDeclSpecifiers dts
            return $ CBlockDecl $ CDeclaration decls
                [(Just $ CDeclarator (Just $ action <::> "msg_data") [] [] cAnn, Nothing, Nothing)] declStmt

        getMsgDataVariables :: [(Identifier, TypeSpecifier, Identifier)] -> CSourceGenerator [CCompoundBlockItem]
        getMsgDataVariables [] = return []
        getMsgDataVariables ((_identifier, dts, action) : xs) = do
            decl <- getMsgDataVariable True action dts
            rest <- mapM (uncurry (getMsgDataVariable False) . (\(_, dts', action') -> (action', dts'))) xs
            return $ decl : rest

        genCase :: (Identifier, TypeSpecifier, Identifier) -> CSourceGenerator [CCompoundBlockItem]
        genCase (port, _, action) = do
            let cAnn = CAnnotations Internal CGenericAnn
                stmt before expand = CAnnotations Internal (CStatementAnn before expand)
            variant <- genVariantForPort classId port
            classFunctionName <- genClassFunctionName classId action
            return
                [
                    -- case variant:
                    CBlockStmt $ CCase (CVar variant cAnn)
                    -- status = rtems_message_queue_receive(self->port, &action_msg_data, 
                    --                                      &size, RTEMS_WAIT, RTEMS_NO_TIMEOUT);
                        (CExpr (Just $ CAssignment (CVar "status" cAnn)
                            (CCall (CVar "rtems_message_queue_receive" cAnn)
                                [CMember (CVar "self" cAnn) port True cAnn,
                                CUnary CAdrOp (CVar (action <::> "msg_data") cAnn) cAnn,
                                CUnary CAdrOp (CVar "size" cAnn) cAnn,
                                CVar "RTEMS_NO_WAIT" cAnn,
                                CVar "RTEMS_NO_TIMEOUT" cAnn] cAnn) cAnn) (stmt True True))
                        (stmt True False),
                    -- if (RTEMS_SUCCESSFUL != status)
                    CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (stmt False False)
                        ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (stmt True True),
                    -- result = classFunctionName(self, action_msg_data);
                    CBlockStmt $ CExpr (Just $ CAssignment
                        (CVar "result" cAnn)
                        (CCall (CVar classFunctionName cAnn)
                            [CVar "self" cAnn, CVar (action <::> "msg_data") cAnn] cAnn) cAnn) (stmt True True),
                    -- if (result.__variant != Result__Ok)
                    CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                        (CVar ("Result" <::> "Ok") cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (stmt False False)
                        ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (stmt True True),
                    -- break;
                    CBlockStmt $ CBreak (stmt True True)

                ]

        genLoop :: CSourceGenerator CStatement
        genLoop = do
            let cAnn = CAnnotations Internal CGenericAnn
                stmt before = CAnnotations Internal (CStatementAnn before False)
                compoundAnn = CAnnotations Internal (CCompoundAnn False True)
            cases <- concat <$> mapM genCase actions
            return $ CCompound [
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                        (CCall (CVar "rtems_message_queue_receive" cAnn)
                            [CMember (CMember (CVar "self" cAnn) "__task" True cAnn) "msgq_id" False cAnn,
                            CUnary CAdrOp (CVar "next_msg" cAnn) cAnn,
                            CUnary CAdrOp (CVar "size" cAnn) cAnn,
                            CVar "RTEMS_WAIT" cAnn,
                            CVar "RTEMS_NO_TIMEOUT" cAnn] cAnn) cAnn) (stmt True),
                    CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                        (CCompound [CBlockStmt $ CBreak (stmt False)] (CAnnotations Internal (CCompoundAnn False False))) Nothing (stmt True),
                    CBlockStmt $ CSwitch (CVar "next_msg" cAnn)
                        (CCompound (cases ++
                            [
                                -- default:
                                CBlockStmt $ CDefault
                                    -- rtems_shutdown_executive(1);
                                    (CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                    [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn True True))) (stmt True),
                                    -- break;
                                CBlockStmt $ CBreak (CAnnotations Internal (CStatementAnn True True))
                            ])
                        compoundAnn) (stmt True)
                ] compoundAnn

        genBody :: CSourceGenerator [CCompoundBlockItem]
        genBody = do
            let declStmt before = CAnnotations Internal (CDeclarationAnn before)
                stmt before = CAnnotations Internal (CStatementAnn before False)
                cAnn = CAnnotations Internal CGenericAnn
            msgDataVars <- getMsgDataVariables actions
            loop <- genLoop
            return $ [
                    -- ClassIdentifier self = (ClassIdentifier *)&arg;
                    CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef classId]
                        [(Just $ CDeclarator (Just "self") [CPtrDeclr [] cAnn] [] cAnn,
                          Just (CInitExpr (CCast
                                (CDeclaration [CTypeSpec $ CTypeDef classId]
                                    [(Just $ CDeclarator Nothing [CPtrDeclr [] cAnn] [] cAnn, Nothing, Nothing)]
                                        (CAnnotations Internal (CDeclarationAnn False)))
                                    (CVar "arg" cAnn) cAnn) cAnn), Nothing)] (declStmt True),
                    -- rtems_status_code status = RTEMS_SUCCESSFUL;
                    CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "rtems_status_code"]
                        [(Just $ CDeclarator (Just "status") [] [] cAnn,
                          Just (CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn), Nothing)] (declStmt False),
                    -- uint32_t next_msg = 0;
                    CBlockDecl $ CDeclaration [CTypeSpec CUInt32Type]
                        [(Just $ CDeclarator (Just "next_msg") [] [] cAnn,
                          Just (CInitExpr (CConst (CIntConst (CInteger 0 CDecRepr)) cAnn) cAnn), Nothing)] (declStmt False),
                    -- size_t size = 0;
                    CBlockDecl $ CDeclaration [CTypeSpec CSizeTType]
                        [(Just $ CDeclarator (Just "size") [] [] cAnn,
                          Just (CInitExpr (CConst (CIntConst (CInteger 0 CDecRepr)) cAnn) cAnn), Nothing)] (declStmt False),
                    -- Result result;
                    CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "Result"]
                        [(Just $ CDeclarator (Just "result") [] [] cAnn,
                          Nothing, Nothing)] (declStmt True),
                    -- result.__variant = Result__Ok;
                    CBlockStmt $ CExpr (Just $ CAssignment
                        (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                        (CVar ("Result" <::> "Ok") cAnn) cAnn) (stmt False)

                ] ++ msgDataVars ++
                [
                    CBlockStmt $ CFor (Left Nothing) Nothing Nothing loop (stmt True),
                    CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                        [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (stmt True)
                ]
genTaskClassCode obj = throwError $ InternalError $ "Invalid global object (not a task): " ++ show obj

emitterToArrayMap :: M.Map Identifier Integer
emitterToArrayMap = M.fromList [("irq_0", 0), ("irq_1", 1), ("irq_2", 2), ("irq_3", 3), ("irq_4", 4)]

genArmTimer :: CExpression -> Identifier -> CSourceGenerator [CCompoundBlockItem]
genArmTimer cExpr identifier = do
    let cAnn = CAnnotations Internal CGenericAnn
        stmtAnn = CAnnotations Internal (CStatementAnn True False)
    return [
            -- __termina__add_timeval(&timer.__timer.current, timer.period);
            CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "termina" <::> "add_timeval") cAnn)
                [
                    CUnary CAdrOp (CMember (CMember cExpr (namefy "timer") False cAnn) "current" False cAnn) cAnn,
                    CUnary CAdrOp (CMember cExpr "period" False cAnn) cAnn
                ] cAnn) stmtAnn,
            -- status = rtems_timer_fire_after(timer.__timer.timer_id, &timer.__timer.current, timer.period);
            CBlockStmt $ CExpr (Just $ CAssignment
                (CVar "status" cAnn)
                    (CCall (CVar (namefy $ "rtems" <::> "timer_delay_at") cAnn)
                        [
                            CMember (CMember cExpr (namefy "timer") False cAnn) "timer_id" False cAnn,
                            CUnary CAdrOp (CMember (CMember cExpr (namefy "timer") False cAnn) "current" False cAnn) cAnn,
                            CVar (namefy "rtems_periodic_timer" <::> identifier) cAnn
                        ] cAnn) cAnn) stmtAnn
        ]

genEmitter :: RTEMSEmitter -> CSourceGenerator CFileItem
genEmitter (RTEMSInterruptEmitter interrupt (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn True)
        irqArray = emitterToArrayMap M.! interrupt
    classFunctionName <- genClassFunctionName classId action
    return $ CExtDecl $ CFDefExt $
        -- void * rtems_isr_interrupt(void * ignored) {
        CFunDef [CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_isr" <::> interrupt) [CFunDeclr
                [CDeclaration
                    [CTypeSpec CVoidType] [(Just (CDeclarator (Just "_ignored") [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False))] [] cAnn] [] cAnn)
            (CCompound [
                -- classId * self = &identifier;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef classId]
                    [(Just $ CDeclarator (Just "self") [CPtrDeclr [] cAnn] [] cAnn, Just $ CInitExpr (CUnary CAdrOp (CVar identifier cAnn) cAnn) cAnn, Nothing)] declStmt,
                -- Result result;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "Result"]
                    [(Just $ CDeclarator (Just "result") [] [] cAnn, Nothing, Nothing)] declStmt,
                -- result.__variant = Result__Ok;
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                -- result = classFunctionName(self, interrupt);
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "result" cAnn)
                    (CCall (CVar classFunctionName cAnn)
                        [CVar "self" cAnn, CConst (CIntConst (CInteger irqArray CDecRepr)) cAnn] cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn)
                    (CCompound [
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False)),
                CBlockStmt $ CReturn Nothing (CAnnotations Internal (CStatementAnn True False))
            ] (CAnnotations Internal (CCompoundAnn False True))) declStmt
genEmitter (RTEMSInterruptEmitter interrupt (RTEMSTask {})) = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn True)
        irqArray = emitterToArrayMap M.! interrupt
    return $ CExtDecl $ CFDefExt $
        -- void * rtems_isr_interrupt(void * ignored) {
        CFunDef [CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_isr" <::> interrupt) [CFunDeclr
                [CDeclaration
                    [CTypeSpec CVoidType] [(Just (CDeclarator (Just "_ignored") [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False))] [] cAnn] [] cAnn)
            (CCompound [
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "rtems_status_code"]
                    [(Just $ CDeclarator (Just "status") [] [] cAnn, Just $ CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn, Nothing)] declStmt,
                -- uint32_t vector = interrupt;
                CBlockDecl $ CDeclaration [CTypeSpec CUInt32Type]
                    [(Just $ CDeclarator (Just "vector") [] [] cAnn, Just $ CInitExpr (CConst (CIntConst (CInteger irqArray CDecRepr)) cAnn) cAnn, Nothing)] declStmt,
                -- status = rtems_message_queue_send(interrupt.sink_msgq_id, &interrupt.task_port, sizeof(uint32_t));
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "status" cAnn)
                    (CCall (CVar "rtems_message_queue_send" cAnn)
                        [CMember (CVar interrupt cAnn) "sink_msgq_id" False cAnn,
                        CUnary CAdrOp (CMember (CVar interrupt cAnn) "vector" False cAnn) cAnn,
                        CSizeofType (CDeclaration [CTypeSpec CUInt32Type] [] (CAnnotations Internal (CDeclarationAnn False))) cAnn] cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                -- if (RTEMS_SUCCESSFUL == status)
                CBlockStmt $ CIf (CBinary CEqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- status = rtems_message_queue_send(interrupt.task_msgq_id, &vector, sizeof(uint32_t));
                        CBlockStmt $ CExpr (Just $ CAssignment
                            (CVar "status" cAnn)
                            (CCall (CVar "rtems_message_queue_send" cAnn)
                                [CMember (CVar interrupt cAnn) "task_msgq_id" False cAnn,
                                CUnary CAdrOp (CMember (CVar interrupt cAnn) "task_port" False cAnn) cAnn,
                                CSizeofType (CDeclaration [CTypeSpec CUInt32Type] [] (CAnnotations Internal (CDeclarationAnn False))) cAnn] cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False)),
                -- if (RTEMS_SUCCESSFUL != status)
                CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False)),
                -- return;
                CBlockStmt $ CReturn Nothing (CAnnotations Internal (CStatementAnn True False))
            ] (CAnnotations Internal (CCompoundAnn False True))) declStmt
genEmitter (RTEMSInterruptEmitter _ glb) = throwError $ InternalError $ "Invalid connection for interrupt: " ++ show glb
genEmitter (RTEMSPeriodicTimerEmitter timer (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn True)
    classFunctionName <- genClassFunctionName classId action
    armTimer <- genArmTimer (CVar timer cAnn) timer
    return $ CExtDecl $ CFDefExt $
        -- void * rtems_periodic_timer(void * _timer_id, void * _argument) {
        CFunDef [CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_periodic_timer" <::> timer) [CFunDeclr
                [CDeclaration
                    [CTypeSpec $ CTypeDef "rtems_id"] [(Just (CDeclarator (Just "_timer_id") [] [] cAnn), Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False)),
                 CDeclaration
                    [CTypeSpec CVoidType] [(Just (CDeclarator (Just "_ignored") [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False))] [] cAnn] [] cAnn)
            (CCompound ([
                -- classId * self = &identifier;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef classId]
                    [(Just $ CDeclarator (Just "self") [CPtrDeclr [] cAnn] [] cAnn, Just $ CInitExpr (CUnary CAdrOp (CVar identifier cAnn) cAnn) cAnn, Nothing)] declStmt,
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "rtems_status_code"]
                    [(Just $ CDeclarator (Just "status") [] [] cAnn, Just $ CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn, Nothing)] declStmt,
                -- Result result;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "Result"]
                    [(Just $ CDeclarator (Just "result") [] [] cAnn, Nothing, Nothing)] declStmt,
                -- result.__variant = Result__Ok;
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                -- result = classFunctionName(self, timer.current);
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "result" cAnn)
                    (CCall (CVar classFunctionName cAnn)
                        [CVar "self" cAnn, CMember (CVar timer cAnn) "current" False cAnn] cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                -- if (result.__variant != Result__Ok)
                CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False))
            ] ++ armTimer ++ [
                -- if (RTEMS_SUCCESSFUL != status)
                CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False)),
                -- return;
                CBlockStmt $ CReturn Nothing (CAnnotations Internal (CStatementAnn True False))
            ]) (CAnnotations Internal (CCompoundAnn False True))) declStmt
genEmitter (RTEMSPeriodicTimerEmitter timer (RTEMSTask {})) = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn True)
    armTimer <- genArmTimer (CVar timer cAnn) timer
    return $ CExtDecl $ CFDefExt $
        -- void * rtems_periodic_timer(void * _timer_id, void * _argument) {
        CFunDef [CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_periodic_timer" <::> timer) [CFunDeclr
                [CDeclaration
                    [CTypeSpec $ CTypeDef "rtems_id"] [(Just (CDeclarator (Just "_timer_id") [] [] cAnn), Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False)),
                 CDeclaration
                    [CTypeSpec CVoidType] [(Just (CDeclarator (Just "_ignored") [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False))] [] cAnn] [] cAnn)
            (CCompound ([
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "rtems_status_code"]
                    [(Just $ CDeclarator (Just "status") [] [] cAnn, Just $ CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn, Nothing)] declStmt,
                -- status = rtems_message_queue_send(interrupt.sink_msgq_id, &interrupt.task_port, sizeof(uint32_t));
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "status" cAnn)
                    (CCall (CVar "rtems_message_queue_send" cAnn)
                        [CMember (CMember (CVar timer cAnn) (namefy "timer") False cAnn) "sink_msgq_id" False cAnn,
                        CUnary CAdrOp (CMember (CMember (CVar timer cAnn) (namefy "timer") False cAnn) "current" False cAnn) cAnn,
                        CSizeofType (CDeclaration [CTypeSpec $ CTypeDef "TimeVal"] [] (CAnnotations Internal (CDeclarationAnn False))) cAnn] cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                -- if (RTEMS_SUCCESSFUL == status)
                CBlockStmt $ CIf (CBinary CEqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- status = rtems_message_queue_send(interrupt.task_msgq_id, &vector, sizeof(uint32_t));
                        CBlockStmt $ CExpr (Just $ CAssignment
                            (CVar "status" cAnn)
                            (CCall (CVar "rtems_message_queue_send" cAnn)
                                [CMember (CMember (CVar timer cAnn) (namefy "timer") False cAnn) "task_msgq_id" False cAnn,
                                CUnary CAdrOp (CMember (CMember (CVar timer cAnn) (namefy "timer") False cAnn) "task_port" False cAnn) cAnn,
                                CSizeofType (CDeclaration [CTypeSpec CUInt32Type] [] (CAnnotations Internal (CDeclarationAnn False))) cAnn] cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False)),
                -- if (RTEMS_SUCCESSFUL != status)
                CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False))
            ] ++ armTimer ++ [
                -- if (RTEMS_SUCCESSFUL != status)
                CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False)),
                CBlockStmt $ CReturn Nothing (CAnnotations Internal (CStatementAnn True False))
            ]) (CAnnotations Internal (CCompoundAnn False True))) declStmt
genEmitter (RTEMSPeriodicTimerEmitter _ glb) = throwError $ InternalError $ "Invalid connection for timer: " ++ show glb
genEmitter (RTEMSSystemInitEmitter _ (RTEMSHandler identifier classId _ (RTEMSEventPort _ _ _ action) _)) = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn True)
    classFunctionName <- genClassFunctionName classId action
    return $ CExtDecl $ CFDefExt $
        -- void * rtems_periodic_timer(void * _timer_id, void * _argument) {
        CFunDef [CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "inital_event") [CFunDeclr
                [CDeclaration
                    [CTypeSpec $ CTypeDef "TimeVal"] [(Just (CDeclarator (Just "current") [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False))] [] cAnn] [] cAnn)
            (CCompound [
                -- classId * self = &identifier;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef classId]
                    [(Just $ CDeclarator (Just "self") [CPtrDeclr [] cAnn] [] cAnn, Just $ CInitExpr (CUnary CAdrOp (CVar identifier cAnn) cAnn) cAnn, Nothing)] declStmt,
                -- Result result;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "Result"]
                    [(Just $ CDeclarator (Just "result") [] [] cAnn, Nothing, Nothing)] declStmt,
                -- result.__variant = Result__Ok;
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                -- result = classFunctionName(self, *current);
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "result" cAnn)
                    (CCall (CVar classFunctionName cAnn)
                        [CVar "self" cAnn, CUnary CIndOp (CVar "current" cAnn) cAnn] cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                -- if (result.__variant != Result__Ok)
                CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False)),
                CBlockStmt $ CReturn Nothing (CAnnotations Internal (CStatementAnn True False))
            ] (CAnnotations Internal (CCompoundAnn False True))) declStmt
genEmitter (RTEMSSystemInitEmitter event (RTEMSTask identifier classId _ _ _ ports)) = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn True)
    action <- case find (\case { RTEMSEventPort _ emitter _ _ -> event == emitter; _ -> False }) ports of
        Just (RTEMSEventPort _ _ _ actionId) -> return actionId
        _ -> throwError $ InternalError $ "Invalid port connection for interrupt: " ++ show event
    classFunctionName <- genClassFunctionName classId action
    return $ CExtDecl $ CFDefExt $
        -- void * rtems_periodic_timer(void * _timer_id, void * _argument) {
        CFunDef [CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "inital_event") [CFunDeclr
                [CDeclaration
                    [CTypeSpec $ CTypeDef "TimeVal"] [(Just (CDeclarator (Just "current") [] [] cAnn), Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False))] [] cAnn] [] cAnn)
            (CCompound [
                -- classId * self = &identifier;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef classId]
                    [(Just $ CDeclarator (Just "self") [CPtrDeclr [] cAnn] [] cAnn, Just $ CInitExpr (CUnary CAdrOp (CVar identifier cAnn) cAnn) cAnn, Nothing)] declStmt,
                -- Result result;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "Result"]
                    [(Just $ CDeclarator (Just "result") [] [] cAnn, Nothing, Nothing)] declStmt,
                -- result.__variant = Result__Ok;
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                -- result = classFunctionName(self, timer.current);
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "result" cAnn)
                    (CCall (CVar classFunctionName cAnn)
                        [CVar "self" cAnn, CVar "current" cAnn] cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                -- if (result.__variant != Result__Ok)
                CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False)),
                CBlockStmt $ CReturn Nothing (CAnnotations Internal (CStatementAnn True False))
            ] (CAnnotations Internal (CCompoundAnn False True))) declStmt
genEmitter _ = error "Invalid emitter"

-- | Function __rtems_app__enable_protection. This function is called from the Init task.
-- It enables the protection of the shared resources when needed. In case the resource uses a mutex,
-- it also initializes the mutex. The function is called AFTER the initialization of the tasks and handlers.
genEnableProtection :: M.Map Identifier RTEMSResourceLock -> CSourceGenerator CFileItem
genEnableProtection resLockingMap = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt = CAnnotations Internal (CDeclarationAnn True)
    initResourcesProt <- concat <$> mapM genInitResourceProt (M.toList resLockingMap)
    return $ CExtDecl $ CFDefExt $
        -- void * rtems_periodic_timer(void * _timer_id, void * _argument) {
        CFunDef [CStorageSpec CStatic, CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "enable_protection") [CFunDeclr [] [] cAnn] [] cAnn)
            (CCompound ([
                -- Result result;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "Result"]
                    [(Just $ CDeclarator (Just "result") [] [] cAnn, Nothing, Nothing)] declStmt,
                -- result.__variant = Result__Ok;
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False))
            ] ++ initResourcesProt) (CAnnotations Internal (CCompoundAnn False True))) declStmt
    where

        genInitResourceProt :: (Identifier, RTEMSResourceLock) -> CSourceGenerator [CCompoundBlockItem]
        genInitResourceProt (identifier, RTEMSResourceLockNone) = do
            let cAnn = CAnnotations Internal CGenericAnn
                stmt before expand = CAnnotations Internal (CStatementAnn before expand)
            return [
                CBlockStmt $ CExpr (Just $
                    CAssignment (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "lock" False cAnn)
                        (CVar (namefy "RTEMSResourceLock__None") cAnn) cAnn) (stmt True False)
                ]
        genInitResourceProt (identifier, RTEMSResourceLockIrq) = do
            let cAnn = CAnnotations Internal CGenericAnn
                stmt before expand = CAnnotations Internal (CStatementAnn before expand)
            return [
                CBlockStmt $ CExpr (Just $
                    CAssignment (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "lock" False cAnn)
                        (CVar (namefy "RTEMSResourceLock__Irq") cAnn) cAnn) (stmt True False)
                ]
        genInitResourceProt (identifier, RTEMSResourceLockMutex ceilPrio) = do
            let cAnn = CAnnotations Internal CGenericAnn
                stmt before expand = CAnnotations Internal (CStatementAnn before expand)
                cCeilPrio = genInteger ceilPrio
            return [
                CBlockStmt $ CExpr (Just $
                    CAssignment (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "lock" False cAnn)
                        (CVar (namefy "RTEMSResourceLock__Mutex") cAnn) cAnn) (stmt True False),
                CBlockStmt $ CExpr (Just $
                    CAssignment (CMember (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "mutex" False cAnn) "policy" False cAnn)
                        (CVar (namefy "RTEMSMutexPolicy__Ceiling") cAnn) cAnn) (stmt False False),
                CBlockStmt $ CExpr (Just $
                    CAssignment (CMember (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "mutex" False cAnn) "prio_ceiling" False cAnn)
                        (CConst (CIntConst cCeilPrio) cAnn) cAnn) (stmt False False),
               -- result = classFunctionName(self, timer.current);
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CVar "result" cAnn)
                    (CCall (CVar (namefy $ "termina_resource" <::> "init") cAnn)
                        [CUnary CAdrOp (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) cAnn] cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False)),
                -- if (result.__variant != Result__Ok)
                CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (CAnnotations Internal (CStatementAnn True False))
                ]

-- | Function __rtems_app__init_globals. This function is called from the Init task.
-- The function is called BEFORE the initialization of the tasks and handlers. The function disables
-- the protection of the global resources, since it is not needed when running in the Init task. It also
-- executes the init() method of the resources if defined.
genInitGlobals ::
    -- | Resources 
    [RTEMSGlobal]
    -- | Pools
    -> [RTEMSGlobal]
    -- | Task Message Queues
    -> [RTEMSMsgQueue]
    -- | Channel Message Queues  
    -> [RTEMSMsgQueue]
    -- | Interrupt emitters connected to tasks
    -> [RTEMSEmitter]
    -- | Timers connected to tasks
    -> [RTEMSEmitter]
    -- | Complete list of tasks
    -> [RTEMSGlobal]
    -- | Complete list of timers
    -> [RTEMSEmitter]
    -> CSourceGenerator CFileItem
genInitGlobals resources pools tasksMessageQueues channelMessageQueues interruptEmittersToTasks timersToTasks tasks timers = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt before = CAnnotations Internal (CDeclarationAnn before)
    initResources <- mapM genInitResource resources
    initPools <- concat <$> mapM genInitPool pools
    cTaskMessageQueues <- concat <$> mapM genRTEMSCreateMsgQueue tasksMessageQueues
    cChannelMessageQueues <- concat <$> mapM genRTEMSCreateMsgQueue channelMessageQueues
    cInterruptEmittersToTasks <- concat <$> mapM genInitInterruptEmitterToTask interruptEmittersToTasks
    cTimersToTasks <- concat <$> mapM genInitTimerToTask timersToTasks
    cTaskInitialization <- concat <$> mapM genTaskInitialization tasks
    cCreateTimers <- concat <$> mapM genRTEMSCreateTimer timers
    return $ CExtDecl $ CFDefExt $
        -- static void __rtems_app__init_globals() {
        CFunDef [CStorageSpec CStatic, CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "init_globals") [CFunDeclr [] [] cAnn] [] cAnn)
            (CCompound ([
                -- Result result;
                CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "Result"]
                    [(Just $ CDeclarator (Just "result") [] [] cAnn, Nothing, Nothing)] (declStmt True),
                -- result.__variant = Result__Ok;
                CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                    (CVar ("Result" <::> "Ok") cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False))
            ] ++ initResources ++ initPools
            ++ (if not (null tasksMessageQueues) || not (null channelMessageQueues) || not (null timers) then
                [
                    -- rtems_status_code status = RTEMS_SUCCESSFUL;
                    CBlockDecl $ CDeclaration [CTypeSpec $ CTypeDef "rtems_status_code"]
                        [(Just $ CDeclarator (Just "status") [] [] cAnn,
                          Just (CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn), Nothing)] (declStmt True)
                ] else []) ++ cTaskMessageQueues ++ cChannelMessageQueues
                ++ cInterruptEmittersToTasks ++ cTimersToTasks ++ cTaskInitialization ++ cCreateTimers
            ) (CAnnotations Internal (CCompoundAnn False True))) (declStmt True)

    where

        genInitResource :: RTEMSGlobal -> CSourceGenerator CCompoundBlockItem
        genInitResource (RTEMSResource identifier _ _) = do
            let cAnn = CAnnotations Internal CGenericAnn
            return $ CBlockStmt $ CExpr (Just $ CAssignment
                    (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "lock" False cAnn)
                    (CVar "__RTEMSResourceLock__None" cAnn) cAnn) (CAnnotations Internal (CStatementAnn True False))
        genInitResource obj = throwError $ InternalError $ "Invalid global object (not a resource): " ++ show obj

        genInitPool :: RTEMSGlobal -> CSourceGenerator [CCompoundBlockItem]
        genInitPool (RTEMSPool identifier ts _) = do
            let cAnn = CAnnotations Internal CGenericAnn
                cStmtAnn = CAnnotations Internal (CStatementAnn True False)
            declSpec <- genDeclSpecifiers ts
            return
                [
                    -- identifier.resource.lock = __RTEMSResourceLock__None;
                    CBlockStmt $ CExpr (Just $ CAssignment
                        (CMember (CMember (CVar identifier cAnn) resourceClassIDField False cAnn) "lock" False cAnn)
                        (CVar "__RTEMSResourceLock__None" cAnn) cAnn) cStmtAnn,
                    -- result = __termina_pool__init(&identifier, (void *)memory_area_identifier)
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "result" cAnn)
                            (CCall (CVar (namefy "termina_pool" <::> "init") cAnn)
                                [
                                    CUnary CAdrOp (CVar identifier cAnn) cAnn,
                                    CCast (CDeclaration [CTypeSpec CVoidType]
                                        [(Just $ CDeclarator Nothing [CPtrDeclr [] cAnn] [] cAnn, Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False)))
                                        (CVar (poolMemoryArea identifier) cAnn) cAnn,
                                    CSizeofExpr (CVar (poolMemoryArea identifier) cAnn) cAnn,
                                    CSizeofType (CDeclaration declSpec [] (CAnnotations Internal (CDeclarationAnn False))) cAnn
                                    ] cAnn) cAnn) cStmtAnn,
                    -- if (result.__variant != Result__Ok)
                    CBlockStmt $ CIf (CBinary CNeqOp (CMember (CVar "result" cAnn) enumVariantsField False cAnn)
                        (CVar ("Result" <::> "Ok") cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                        ] (CAnnotations Internal (CCompoundAnn False False))) Nothing cStmtAnn
                ]
        genInitPool obj = throwError $ InternalError $ "Invalid global object (not a pool): " ++ show obj

        -- | Prints the code to initialize a message queue. The function is called to generate the code for the
        -- message queues corresponding to the channels declared by the user plus the ones that belong to each
        -- of the tasks that is used to notify the inclusion of a given message on a specific queue.
        genRTEMSCreateMsgQueue :: RTEMSMsgQueue -> CSourceGenerator [CCompoundBlockItem]
        genRTEMSCreateMsgQueue (RTEMSChannelMsgQueue identifier ts size (RTEMSTask taskId classId _ _ _ ports)) = do
            let cAnn = CAnnotations Internal CGenericAnn
                cStmtAnn before = CAnnotations Internal (CStatementAnn before False)
                cSize = genInteger size
            declSpec <- genDeclSpecifiers ts
            variantForPort <- genVariantForPort classId port
            return
                [
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "task_msgq_id" False cAnn)
                            (CMember (CMember (CVar taskId cAnn) taskClassIDField False cAnn) "msgq_id" False cAnn) cAnn) (cStmtAnn True),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "task_port" False cAnn)
                            (CVar variantForPort cAnn) cAnn) (cStmtAnn False),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "message_size" False cAnn)
                            (CSizeofType (CDeclaration declSpec [] (CAnnotations Internal (CDeclarationAnn False))) cAnn) cAnn) (cStmtAnn False),
                    -- statuss = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                            (CCall (CVar (namefy "rtems" <::> "create_msg_queue") cAnn)
                                [
                                    CConst (CIntConst cSize) cAnn,
                                    CSizeofType (CDeclaration declSpec [] (CAnnotations Internal (CDeclarationAnn False))) cAnn,
                                    CUnary CAdrOp (CMember (CVar identifier cAnn) "msgq_id" False cAnn) cAnn
                                ] cAnn) cAnn) (cStmtAnn True),
                    CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                        ] (CAnnotations Internal (CCompoundAnn False False))) Nothing (cStmtAnn True)
                ]
            where

                port = case find (\case {
                    RTEMSInputPort _ chid _ _ -> chid == identifier;
                    _ -> False }) ports of
                        Just (RTEMSInputPort prt _ _ _) -> prt
                        _ -> error $ "Invalid port connection for channel: " ++ show identifier

        genRTEMSCreateMsgQueue obj@(RTEMSChannelMsgQueue {}) = throwError $ InternalError $ "Invalid channel objet: " ++ show obj
        genRTEMSCreateMsgQueue (RTEMSTaskMsgQueue identifier size) = do
            let cAnn = CAnnotations Internal CGenericAnn
                cStmtAnn = CAnnotations Internal (CStatementAnn True False)
                cSize = genInteger size
            return
                [
                    -- statuss = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                            (CCall (CVar (namefy "rtems" <::> "create_msg_queue") cAnn)
                                [
                                    CConst (CIntConst cSize) cAnn,
                                    CSizeofType (CDeclaration [CTypeSpec CUInt32Type] [] (CAnnotations Internal (CDeclarationAnn False))) cAnn,
                                    CUnary CAdrOp (CMember (CMember (CVar identifier cAnn) taskClassIDField False cAnn) "msgq_id" False cAnn) cAnn
                                ] cAnn) cAnn) cStmtAnn,
                    CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                        ] (CAnnotations Internal (CCompoundAnn False False))) Nothing cStmtAnn
                ]
        genRTEMSCreateMsgQueue (RTEMSSinkPortMsgQueue taskId portId ts size) = do
            let cAnn = CAnnotations Internal CGenericAnn
                cStmtAnn = CAnnotations Internal (CStatementAnn True False)
                cSize = genInteger size
            declSpec <- genDeclSpecifiers ts
            return
                [
                    -- status = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                            (CCall (CVar (namefy "rtems" <::> "create_msg_queue") cAnn)
                                [
                                    CConst (CIntConst cSize) cAnn,
                                    CSizeofType (CDeclaration declSpec [] (CAnnotations Internal (CDeclarationAnn False))) cAnn,
                                    CUnary CAdrOp (CMember (CVar taskId cAnn) portId False cAnn) cAnn
                                ] cAnn) cAnn) cStmtAnn,
                    CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                        ] (CAnnotations Internal (CCompoundAnn False False))) Nothing cStmtAnn
                ]

        genInitInterruptEmitterToTask :: RTEMSEmitter -> CSourceGenerator [CCompoundBlockItem]
        genInitInterruptEmitterToTask (RTEMSInterruptEmitter identifier (RTEMSTask taskId classId _ _ _ ports)) = do
            let cAnn = CAnnotations Internal CGenericAnn
                cStmtAnn before = CAnnotations Internal (CStatementAnn before False)
            variantForPort <- genVariantForPort classId port
            return
                [
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "task_msgq_id" False cAnn)
                            (CMember (CMember (CVar taskId cAnn) taskClassIDField False cAnn) "msgq_id" False cAnn) cAnn) (cStmtAnn True),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "sink_msgq_id" False cAnn)
                            (CMember (CVar taskId cAnn) port False cAnn) cAnn) (cStmtAnn False),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) "task_port" False cAnn)
                            (CVar variantForPort cAnn) cAnn) (cStmtAnn False)
                ]

            where
                port = case find (\case {
                    RTEMSEventPort _ chid _ _ -> chid == identifier;
                    _ -> False }) ports of
                        Just (RTEMSEventPort prt _ _ _) -> prt
                        _ -> error $ "Invalid port connection for channel: " ++ show identifier
        genInitInterruptEmitterToTask obj = throwError $ InternalError $ "Invalid global object (not an interrupt emitter connected to a task): " ++ show obj

        genInitTimerToTask :: RTEMSEmitter -> CSourceGenerator [CCompoundBlockItem]
        genInitTimerToTask (RTEMSPeriodicTimerEmitter identifier (RTEMSTask taskId classId _ _ _ ports)) = do
            let cAnn = CAnnotations Internal CGenericAnn
                cStmtAnn before = CAnnotations Internal (CStatementAnn before False)
            variantForPort <- genVariantForPort classId port
            return
                [
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CMember (CVar identifier cAnn) "__timer" False cAnn) "task_msgq_id" False cAnn)
                            (CMember (CMember (CVar taskId cAnn) taskClassIDField False cAnn) "msgq_id" False cAnn) cAnn) (cStmtAnn True),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CMember (CVar identifier cAnn) "__timer" False cAnn) "sink_msgq_id" False cAnn)
                            (CMember (CVar taskId cAnn) port False cAnn) cAnn) (cStmtAnn False),
                    CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CMember (CVar identifier cAnn) "__timer" False cAnn) "task_port" False cAnn)
                            (CVar variantForPort cAnn) cAnn) (cStmtAnn False)
                ]

            where

                port = case find (\case {
                    RTEMSEventPort _ chid _ _ -> chid == identifier;
                    _ -> False }) ports of
                        Just (RTEMSEventPort prt _ _ _) -> prt
                        _ -> error $ "Invalid port connection for channel: " ++ show identifier
        genInitTimerToTask obj = throwError $ InternalError $ "Invalid global object (not a timer connected to a task): " ++ show obj

        genTaskInitialization :: RTEMSGlobal -> CSourceGenerator [CCompoundBlockItem]
        genTaskInitialization (RTEMSTask identifier _ _ _ _ ports) = do
            mapM genInputPortInitialization inputPorts

            where

                inputPorts = filter (\case {
                    RTEMSInputPort {} -> True;
                    _ -> False }) ports

                genInputPortInitialization :: RTEMSPort -> CSourceGenerator CCompoundBlockItem
                genInputPortInitialization (RTEMSInputPort portId channelId _ _) = do
                    let cAnn = CAnnotations Internal CGenericAnn
                        cStmtAnn = CAnnotations Internal (CStatementAnn True False)
                    return $ CBlockStmt $ CExpr (Just $
                        CAssignment (CMember (CVar identifier cAnn) portId False cAnn)
                            (CMember (CVar channelId cAnn) "msgq_id" False cAnn) cAnn) cStmtAnn
                genInputPortInitialization obj = throwError $ InternalError $ "Invalid port object: " ++ show obj
        genTaskInitialization obj = throwError $ InternalError $ "Invalid global object (not a task): " ++ show obj

        genRTEMSCreateTimer :: RTEMSEmitter -> CSourceGenerator [CCompoundBlockItem]
        genRTEMSCreateTimer (RTEMSPeriodicTimerEmitter identifier _) = do
            let cAnn = CAnnotations Internal CGenericAnn
                cStmtAnn = CAnnotations Internal (CStatementAnn True False)
            return
                [
                    -- statuss = __rtems__create_msg_queue(&identifier, (void *)memory_area_identifier)
                    CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                            (CCall (CVar (namefy "rtems" <::> "create_timer") cAnn)
                                [
                                    CUnary CAdrOp (CMember (CMember (CVar identifier cAnn) "__timer" False cAnn) "timer_id" False cAnn) cAnn
                                ] cAnn) cAnn) cStmtAnn,
                    CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                        (CCompound [
                            -- rtems_shutdown_executive(1);
                            CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                                [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                        ] (CAnnotations Internal (CCompoundAnn False False))) Nothing cStmtAnn
                ]
        genRTEMSCreateTimer obj = throwError $ InternalError $ "Invalid global object (not a timer): " ++ show obj

-- | Function __rtems_app__install_emitters. This function is called from the Init task.
-- The function installs the ISRs and the periodic timers. The function is called AFTER the initialization
-- of the tasks and handlers.
genInstallEmitters :: [RTEMSEmitter] -> CSourceGenerator CFileItem
genInstallEmitters emitters = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt before = CAnnotations Internal (CDeclarationAnn before)
    installEmitters <- mapM genRTEMSInstallEmitter $ filter (\case { RTEMSSystemInitEmitter {} -> False; _ -> True }) emitters
    return $ CExtDecl $ CFDefExt $
        -- static void __rtems_app__init_globals() {
        CFunDef [CStorageSpec CStatic, CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "install_emitters") [CFunDeclr
                [CDeclaration
                    [CTypeSpec $ CTypeDef "TimeVal"] [(Just (CDeclarator (Just "current") [CPtrDeclr [] cAnn] [] cAnn), Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False))]
                [] cAnn] [] cAnn)
            (CCompound (
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl (CDeclaration [CTypeSpec $ CTypeDef "rtems_status_code"]
                    [(Just $ CDeclarator (Just "status") [] [] cAnn, Just $ CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn, Nothing)] (declStmt True))
             : installEmitters) (CAnnotations Internal (CCompoundAnn False True))) (declStmt True)

    where

        genRTEMSInstallEmitter :: RTEMSEmitter -> CSourceGenerator CCompoundBlockItem
        genRTEMSInstallEmitter (RTEMSInterruptEmitter interrupt _) = do
            let cAnn = CAnnotations Internal CGenericAnn
                stmtAnn = CAnnotations Internal (CStatementAnn True False)
            return $
                CBlockStmt $ CIf (CBinary CEqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                                (CCall (CVar (namefy "rtems" <::> "install_isr") cAnn)
                                    [CVar (show $ emitterToArrayMap M.! interrupt) cAnn,
                                    CVar (namefy $ "rtems_isr" <::> interrupt) cAnn] cAnn) cAnn)
                            stmtAnn
                    ] (CAnnotations Internal (CCompoundAnn False True))) Nothing stmtAnn
        genRTEMSInstallEmitter (RTEMSPeriodicTimerEmitter timer _) = do
            let cAnn = CAnnotations Internal CGenericAnn
                stmtAnn = CAnnotations Internal (CStatementAnn True False)
                cExpr = CVar timer cAnn
            armTimer <- genArmTimer cExpr timer
            return $
                CBlockStmt $ CIf (CBinary CEqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound
                        (CBlockStmt (CExpr (Just $ CAssignment
                            (CMember (CMember cExpr (namefy "timer") False cAnn) "current" False cAnn)
                            (CUnary CIndOp (CVar "current" cAnn) cAnn) cAnn) stmtAnn) :
                        armTimer)
                    (CAnnotations Internal (CCompoundAnn False False))) Nothing stmtAnn
        genRTEMSInstallEmitter (RTEMSSystemInitEmitter {}) = throwError $ InternalError $ "Initial event does not have to be installed"

genCreateTasks :: [RTEMSGlobal] -> CSourceGenerator CFileItem
genCreateTasks tasks = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt before = CAnnotations Internal (CDeclarationAnn before)
    createTasks <- concat <$> mapM genRTEMSCreateTask tasks
    return $ CExtDecl $ CFDefExt $
        -- static void __rtems_app__create_tasks() {
        CFunDef [CStorageSpec CStatic, CTypeSpec CVoidType]
            (CDeclarator (Just $ namefy "rtems_app" <::> "create_tasks") [CFunDeclr
                [] [] cAnn] [] cAnn)
            (CCompound (
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl (CDeclaration [CTypeSpec $ CTypeDef "rtems_status_code"]
                    [(Just $ CDeclarator (Just "status") [] [] cAnn, Just $ CInitExpr (CVar "RTEMS_SUCCESSFUL" cAnn) cAnn, Nothing)] (declStmt True))
             : createTasks) (CAnnotations Internal (CCompoundAnn False True))) (declStmt True)
    where

        genRTEMSCreateTask :: RTEMSGlobal -> CSourceGenerator [CCompoundBlockItem]
        genRTEMSCreateTask (RTEMSTask identifier classId _ priority stackSize _) = do
            let cAnn = CAnnotations Internal CGenericAnn
                stmtAnn = CAnnotations Internal (CStatementAnn True False)
                cPriority = genInteger priority
                cStackSize = genInteger stackSize
            return [
                CBlockStmt $ CExpr (Just $ CAssignment (CVar "status" cAnn)
                    (CCall (CVar (namefy "rtems" <::> "create_task") cAnn)
                        [CConst (CIntConst cPriority) cAnn,
                        CConst (CIntConst cStackSize) cAnn,
                        CVar (namefy $ "rtems_task" <::> classId) cAnn,
                        CCast
                            (CDeclaration [CTypeSpec $ CTypeDef "rtems_task_argument"]
                                [] (CAnnotations Internal (CDeclarationAnn False)))
                                (CUnary CAdrOp (CVar identifier cAnn) cAnn) cAnn,
                        CUnary CAdrOp
                            (CMember (CMember (CVar identifier cAnn) "__task" False cAnn) "task_id" False cAnn) cAnn
                        ] cAnn) cAnn)
                    stmtAnn,
                CBlockStmt $ CIf (CBinary CNeqOp (CVar "RTEMS_SUCCESSFUL" cAnn) (CVar "status" cAnn) cAnn)
                    (CCompound [
                        -- rtems_shutdown_executive(1);
                        CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_shutdown_executive" cAnn)
                            [CConst (CIntConst (CInteger 1 CDecRepr)) cAnn] cAnn) (CAnnotations Internal (CStatementAnn False False))
                    ] (CAnnotations Internal (CCompoundAnn False False))) Nothing stmtAnn
                ]
        genRTEMSCreateTask obj = throwError $ InternalError $ "Invalid global object (not a task): " ++ show obj

genInitTask :: [RTEMSEmitter] -> CSourceGenerator CFileItem
genInitTask emitters = do
    let cAnn = CAnnotations Internal CGenericAnn
        declStmt before = CAnnotations Internal (CDeclarationAnn before)
        cStmtAnn = CAnnotations Internal (CStatementAnn True False)
    return $ CExtDecl $ CFDefExt $
        CFunDef [CTypeSpec $ CTypeDef "rtems_task"]
            (CDeclarator (Just $ "Init") [CFunDeclr
                [CDeclaration
                    [CTypeSpec $ CTypeDef "rtems_task_argument"] [(Just (CDeclarator (Just "_ignored") [] [] cAnn), Nothing, Nothing)] (CAnnotations Internal (CDeclarationAnn False))]
                [] cAnn] [] cAnn)
            (CCompound ([
                -- rtems_status_code status = RTEMS_SUCCESSFUL;
                CBlockDecl (CDeclaration [CTypeSpec $ CTypeDef "TimeVal"]
                    [(Just $ CDeclarator (Just "current") [] [] cAnn, Nothing, Nothing)] (declStmt True)),
                CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "termina" <::> "clock_get_uptime") cAnn)
                                [CUnary CAdrOp (CVar "current" cAnn) cAnn] cAnn) cStmtAnn,
                CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "termina_app" <::> "init_globals") cAnn) [] cAnn) cStmtAnn,
                CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "rtems_app" <::> "init_globals") cAnn) [] cAnn) cStmtAnn
             ] ++
                (case find (\case { RTEMSSystemInitEmitter {} -> True; _ -> False }) emitters of
                    Just (RTEMSSystemInitEmitter {}) -> [
                            CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "rtems_app" <::> "inital_event") cAnn)
                                [CUnary CAdrOp (CVar "current" cAnn) cAnn] cAnn) cStmtAnn
                        ]
                    _ -> []) ++
                [
                    CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "rtems_app" <::> "enable_protection") cAnn) [] cAnn) cStmtAnn,
                    CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "rtems_app" <::> "install_emitters") cAnn)
                        [CUnary CAdrOp (CVar "current" cAnn) cAnn] cAnn) cStmtAnn,
                    CBlockStmt $ CExpr (Just $ CCall (CVar (namefy $ "rtems_app" <::> "create_tasks") cAnn) [] cAnn) cStmtAnn,
                    CBlockStmt $ CExpr (Just $ CCall (CVar "rtems_task_delete" cAnn) [CVar "RTEMS_SELF" cAnn] cAnn) cStmtAnn
                ]
             ) (CAnnotations Internal (CCompoundAnn False True))) (declStmt True)

genAppConfig ::
    [RTEMSGlobal]
    -> [RTEMSMsgQueue]
    -> [RTEMSEmitter]
    -> [RTEMSResourceLock]
    -> CSourceGenerator [CFileItem]
genAppConfig tasks msgQueues timers mutexes = do
    let cppAnn before = CAnnotations Internal (CPPDirectiveAnn before)
    messageBufferMemory <- genMessageBufferMemory msgQueues
    return $ [
            -- #define CONFIGURE_MAXIMUM_TASKS
            CPPDirective $ CPPDefine "CONFIGURE_MAXIMUM_TASKS" (Just [show (length tasks + 1)]) (cppAnn True),
            -- #define CONFIGURE_MAXIMUM_MESSAGE_QUEUES
            CPPDirective $ CPPDefine "CONFIGURE_MAXIMUM_MESSAGE_QUEUES" (Just [show (length msgQueues)]) (cppAnn False),
            -- #define CONFIGURE_MAXIMUM_TIMERS
            CPPDirective $ CPPDefine "CONFIGURE_MAXIMUM_TIMERS" (Just [show (length timers)]) (cppAnn False),
            -- #define CONFIGURE_MAXIMUM_SEMAPHORES
            CPPDirective $ CPPDefine "CONFIGURE_MAXIMUM_SEMAPHORES" (Just [show (length mutexes)]) (cppAnn False)
        ] ++ messageBufferMemory ++
        [
            CPPDirective $ CPPDefine "CONFIGURE_APPLICATION_DOES_NOT_NEED_CONSOLE_DRIVER" Nothing (cppAnn True),
            CPPDirective $ CPPDefine "CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER" Nothing (cppAnn False),
            CPPDirective $ CPPDefine "CONFIGURE_MICROSECONDS_PER_TICK" (Just [show (10000 :: Integer)]) (cppAnn False),
            CPPDirective $ CPPDefine "CONFIGURE_RTEMS_INIT_TASKS_TABLE" Nothing (cppAnn True),
            CPPDirective $ CPPDefine "CONFIGURE_INIT" Nothing (cppAnn True),
            CPPDirective $ CPPInclude True "rtems/confdefs.h" (cppAnn True)
        ]

    where

        genSizeOf :: TypeSpecifier -> CSourceGenerator CExpression
        genSizeOf ts = do
            let cAnn = CAnnotations Internal CGenericAnn
            declSpec <- genDeclSpecifiers ts
            return $ CSizeofType (CDeclaration declSpec [] (CAnnotations Internal (CDeclarationAnn False))) cAnn


        genMessagesForQueue :: RTEMSMsgQueue -> CSourceGenerator [String]
        genMessagesForQueue (RTEMSTaskMsgQueue _ (TInteger size _)) = do
            cSizeOf <- genSizeOf UInt32
            let ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> show size <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        genMessagesForQueue (RTEMSChannelMsgQueue _ ts (TInteger size _) _) = do
            cSizeOf <- genSizeOf ts
            let ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> show size <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]
        genMessagesForQueue (RTEMSSinkPortMsgQueue _ _ ts (TInteger size _)) = do
            cSizeOf <- genSizeOf ts
            let ppSizeOf = unpack . render $ runReader (pprint cSizeOf) (CPrinterConfig False False)
            return [
                    "    CONFIGURE_MESSAGE_BUFFERS_FOR_QUEUE( ",
                    "        " <> show size <> ", ",
                    "        " <> ppSizeOf <> " ",
                    "    ) "
                ]

        genMessagesForQueues :: [RTEMSMsgQueue] -> CSourceGenerator [String]
        genMessagesForQueues [msgq] = genMessagesForQueue msgq
        genMessagesForQueues (msgq : xs) = do
            msgsForQueue <- genMessagesForQueue msgq
            msgsForQueues <- genMessagesForQueues xs
            return $ msgsForQueue ++ ["+ "] ++ msgsForQueues
        genMessagesForQueues [] = throwError $ InternalError "Invalid message queue list: empty list"

        genMessageBufferMemory :: [RTEMSMsgQueue] -> CSourceGenerator [CFileItem]
        genMessageBufferMemory [] = return []
        genMessageBufferMemory msgq = do
            messagesForQueue <- genMessagesForQueues msgq
            return [
                    CPPDirective $ CPPDefine "CONFIGURE_MESSAGE_BUFFER_MEMORY"
                        (Just $
                            "( " : messagesForQueue ++ [")"]
                        ) (CAnnotations Internal (CPPDirectiveAnn True))
                ]


genMainFile :: QualifiedName ->  [(QualifiedName, SAST.AnnotatedProgram SemanticAnns)] -> CSourceGenerator CFile
genMainFile mName prjprogs = do
    let cAnn = CAnnotations Internal CGenericAnn
        includeRTEMS = CPPDirective $ CPPInclude True "rtems.h" (CAnnotations Internal (CPPDirectiveAnn True))
        includeTermina = CPPDirective $ CPPInclude True "termina.h" (CAnnotations Internal (CPPDirectiveAnn True))
        externInitGlobals = CExtDecl $ CDeclExt $ CDeclaration
            [CStorageSpec CExtern, CTypeSpec CVoidType]
            [(Just $ CDeclarator (Just $ namefy $ "termina_app" <::> "init_globals") [CFunDeclr [] [] cAnn] [] cAnn, Nothing, Nothing)]
            (CAnnotations Internal (CDeclarationAnn True))
    cVariantsForTaskPorts <- concat <$> mapM genVariantsForTaskPorts (M.elems taskClss)
    cPoolMemoryAreas <- genPoolMemoryAreas pools
    cAtomicDeclarations <- genAtomicDeclarations atomics
    cAtomicArrayDeclarations <- genAtomicArrayDeclarations atomicArrays
    cInterruptEmitterDeclarations <- genInterruptEmitterDeclarations interruptEmittersToTasks
    cTaskClassesCode <- mapM genTaskClassCode (M.elems taskClss)
    cEmitters <- mapM genEmitter emitters
    enableProtection <- genEnableProtection resLockingMap
    initGlobals <- genInitGlobals (M.elems resources) pools tasksMessageQueues channelMessageQueues interruptEmittersToTasks timersToTasks tasks timers
    installEmitters <- genInstallEmitters emitters
    createTasks <- genCreateTasks tasks
    initTask <- genInitTask emitters
    appConfig <- genAppConfig tasks msgQueues timers mutexes
    return $ CSourceFile mName $ [
            -- #include <rtems.h>
            includeRTEMS,
            -- #include <termina.h>
            includeTermina
        ] ++ includes
        ++ [
            externInitGlobals
        ] ++ cVariantsForTaskPorts ++ cAtomicDeclarations ++ cAtomicArrayDeclarations 
        ++ cPoolMemoryAreas ++ cInterruptEmitterDeclarations
        ++ cTaskClassesCode ++ cEmitters ++ [enableProtection, initGlobals, installEmitters, createTasks, initTask]
        ++ appConfig

    where
        -- | Original program list filtered to only include the global declaration
        globals = map (\(mn, elems) -> (mn, [g | (SAST.GlobalDeclaration g) <- elems])) prjprogs
        -- | Map between the class identifiers and the class definitions
        classMap = foldr
                (\(_, objs) accMap ->
                    foldr (\obj currMap ->
                        case obj of
                            SAST.TypeDefinition cls@(Class _ classId _ _ _) _ -> M.insert classId cls currMap
                            _ -> currMap
                        ) accMap objs
                ) M.empty prjprogs
        -- | List of modules that actually contain the global declarations and are the only ones that must
        -- be included
        glbs = filter (\(_, objs) -> not (null objs)) globals
        -- | List of modules that must be included
        incs = map fst glbs
        -- | List of include directives
        includes = map (\nm -> CPPDirective $ CPPInclude False (nm <.> "h") (CAnnotations Internal (CPPDirectiveAnn True))) incs
        -- List of RTEMS global declarations (tasks, handlers, resources and channels)
        rtemsGlbs = concatMap (\(_, objs) ->
            map (`buildRTEMSGlobal` classMap)
                (filter (\case { Task {} -> True; Resource {} -> True; Handler {} -> True; _ -> False}) objs)
            ) glbs

        -- List of used task classes
        taskClss = foldr (\glb acc -> case glb of
                RTEMSTask _ _ cls@(Class _ classId _ _ _) _ _ _ -> M.insert classId cls acc
                _ -> acc
            ) M.empty rtemsGlbs

        tasks = [t | t@(RTEMSTask {}) <- rtemsGlbs]
        pools = [p | p@(RTEMSPool {}) <- rtemsGlbs]
        resources = M.fromList [(ident, r) | r@(RTEMSResource ident _ _) <- rtemsGlbs]
        atomics = [a | a@(RTEMSAtomic {}) <- rtemsGlbs]
        atomicArrays = [a | a@(RTEMSAtomicArray {}) <- rtemsGlbs]

        targetChannelConnections = foldr
                (\glb accMap ->
                    case glb of
                        RTEMSTask _ _ _ _ _ ports  ->
                            foldr (\port currMap ->
                                case port of
                                    RTEMSInputPort _ channelIdentifier _ _ -> M.insert channelIdentifier glb currMap
                                    _ -> currMap
                            ) accMap ports
                        _ -> accMap
                ) M.empty rtemsGlbs

        tasksMessageQueues = foldr (\glb acc ->
                case glb of
                    RTEMSTask identifier _ _ _ _ ports -> RTEMSTaskMsgQueue identifier (TInteger 1 DecRepr) :
                        foldr (\port acc' ->
                            case port of
                                RTEMSEventPort portId _ ts _ -> RTEMSSinkPortMsgQueue identifier portId ts (TInteger 1 DecRepr) : acc'
                                _ -> acc'
                        ) acc ports
                    _ -> acc
            ) [] tasks

        channelMessageQueues = concatMap (\(_, objs) ->
            map (\case {
                    (Channel identifier (MsgQueue ts (K size)) _ _ _) ->
                        case M.lookup identifier targetChannelConnections of
                            Just task@(RTEMSTask {}) ->
                                RTEMSChannelMsgQueue identifier ts size task
                            _ -> error $ "channel not connected: " ++ show identifier
                        ;
                    _ -> error "Invalid global object (not a channel)"})
                (filter (\case { Channel {} -> True; _ -> False}) objs)
            ) glbs

        msgQueues = tasksMessageQueues ++ channelMessageQueues

        -- Map between the resources and the task and handlers that access them
        dependenciesMap = foldr
                (\glb accMap ->
                    case glb of
                    -- TODO: We are not considering the case of a resource being accessed by another resources
                    -- We need to change the way we are building the dependencies map to consider this case
                    RTEMSResource {} -> accMap
                    RTEMSTask _ _ _ _ _ ports ->
                        foldr (\port currMap ->
                                case port of
                                    RTEMSAccessPort _ identifier -> M.alter (addDependency glb) identifier currMap
                                    _ -> currMap
                            ) accMap ports
                    RTEMSHandler _ _ _ _ ports ->
                        foldr (\port currMap ->
                                case port of
                                    RTEMSAccessPort _ identifier -> M.alter (addDependency glb) identifier currMap
                                    _ -> currMap
                            ) accMap ports
                    _ -> accMap
                ) M.empty rtemsGlbs

        emitterConnectionsMap = foldr
                (\glb accMap ->
                    case glb of
                        RTEMSTask _ _ _ _ _ ports  ->
                            foldr (\port currMap ->
                                case port of
                                    RTEMSEventPort _ identifier _ _ -> M.insert identifier glb currMap
                                    _ -> currMap
                            ) accMap ports
                        RTEMSHandler _ _ _ eventPort _ ->
                            case eventPort of
                                RTEMSEventPort _ identifier _ _ -> M.insert identifier glb accMap
                                _ -> error $ "invalid event port for handler: " ++ show glb
                        _ -> accMap
                ) M.empty rtemsGlbs

        emitters = catMaybes $ concatMap (\(_, objs) ->
                map (`buildRTEMSEmitter` emitterConnectionsMap) objs) glbs ++
                map (`buildRTEMSEmitter` emitterConnectionsMap) [
                        Emitter "irq_1" (DefinedType "Interrupt") Nothing [] (internalErrorSeman `SemAnn` GTy (GGlob (SEmitter (DefinedType "Interrupt")))),
                        Emitter "irq_2" (DefinedType "Interrupt") Nothing [] (internalErrorSeman `SemAnn` GTy (GGlob (SEmitter (DefinedType "Interrupt")))),
                        Emitter "irq_3" (DefinedType "Interrupt") Nothing [] (internalErrorSeman `SemAnn` GTy (GGlob (SEmitter (DefinedType "Interrupt")))),
                        Emitter "irq_4" (DefinedType "Interrupt") Nothing [] (internalErrorSeman `SemAnn` GTy (GGlob (SEmitter (DefinedType "Interrupt")))),
                        Emitter "system_init" (DefinedType "SystemInit") Nothing [] (internalErrorSeman `SemAnn` GTy (GGlob (SEmitter (DefinedType "SystemInit"))))
                    ]

        timers = [t | t <- emitters, case t of { RTEMSPeriodicTimerEmitter {} -> True; _ -> False }]
        interruptEmittersToTasks = [e | e <- emitters, case e of { RTEMSInterruptEmitter _ (RTEMSTask{}) -> True; _ -> False }]
        timersToTasks = [e | e <- emitters, case e of { RTEMSPeriodicTimerEmitter _ (RTEMSTask{}) -> True; _ -> False }]

        -- | Map between the resources and the locking mechanism that must be used
        resLockingMap = getResLocking . S.elems <$> M.filterWithKey (\k _ -> M.member k resources) dependenciesMap
        -- | Obtains the locking mechanism that must be used for a resource
        getResLocking :: [RTEMSGlobal] -> RTEMSResourceLock
        getResLocking [] = RTEMSResourceLockNone
        getResLocking [_] = RTEMSResourceLockNone
        getResLocking ((RTEMSHandler {}) : _) = RTEMSResourceLockIrq
        getResLocking ((RTEMSTask _ _ _ priority _ _) : gs) = getResLocking' priority gs
            where
                getResLocking' :: TInteger -> [RTEMSGlobal] -> RTEMSResourceLock
                -- | If we have reach the end of the list, it means that there are at least two different tasks that
                -- access the resource. We are going to force the use of the priority ceiling algorithm. In the
                -- (hopefully near) future, we will support algorithm selection via the configuration file.
                getResLocking' ceilPrio [] = RTEMSResourceLockMutex ceilPrio
                getResLocking' _ ((RTEMSHandler {}) : _) = RTEMSResourceLockIrq
                getResLocking' ceilPrio ((RTEMSTask _ _ _ prio _ _) : gs') = getResLocking' (min prio ceilPrio) gs'
                getResLocking' _ _ = error "Internal error when obtaining the resource dependencies."
        getResLocking _ = error "Internal error when obtaining the resource dependencies."

        mutexes = [m | m <- M.elems resLockingMap, (\case{ RTEMSResourceLockMutex {} -> True; _ -> False }) m]

runGenMainFile :: QualifiedName -> [(QualifiedName, SAST.AnnotatedProgram SemanticAnns)] -> Either CGeneratorError CFile
runGenMainFile mainFilePath prjprogs = runReaderT (genMainFile mainFilePath prjprogs) M.empty