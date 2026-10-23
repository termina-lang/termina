module ControlFlow.Architecture.BoxInOut where

import Lowering.AST
import ControlFlow.Architecture.Types

import Control.Monad
import qualified Control.Monad.State.Strict as ST
import Control.Monad.Except

import qualified Data.Map.Strict as M
import ControlFlow.Architecture.Errors
import Semantic.Types
import ControlFlow.Architecture.Utils
import Utils.Annotations
import Data.Maybe

type BoxInOutMonad = ExceptT ArchitectureError (ST.State (BoxInOutState SemanticAnn))

localInputScope :: BoxInOutMonad a -> BoxInOutMonad a
localInputScope comp = do
  prevst <- ST.get
  res <- comp
  currst <- ST.get
  ST.put (prevst { outputInputMaps = outputInputMaps currst })
  return res

clearInputScope :: BoxInOutMonad ()
clearInputScope = ST.modify (\s -> s { inBoxMap = M.empty, inOptionBoxMap = M.empty, outParamMap = M.empty })

addOptionBox :: Identifier -> InOptionBox SemanticAnn -> BoxInOutMonad ()
addOptionBox inPt optionBox = do
    ST.modify (\s -> s { inOptionBoxMap = M.insert inPt optionBox (inOptionBoxMap s) })

-- | Fills an option-box. When it is an option-box parameter of the procedure,
-- the box leaves the class through it.
fillOptionBox :: Identifier -> SemanticAnn -> InOptionBox SemanticAnn -> BoxInOutMonad ()
fillOptionBox optionBox ann source = do
    outParam <- ST.gets (M.lookup optionBox . outParamMap)
    case outParam of
        Just (procId, argNum) -> addIOMapProcedureOut procId argNum ann (getInBox source)
        Nothing -> addOptionBox optionBox source

addBox :: Identifier -> InBox SemanticAnn -> BoxInOutMonad ()
addBox inPt inBox = do
    ST.modify (\s -> s { inBoxMap = M.insert inPt inBox (inBoxMap s) })

addIOMapFree :: Identifier -> SemanticAnn -> InBox SemanticAnn -> BoxInOutMonad ()
addIOMapFree outPt sann inBox = do
    prevIOMap <- ST.gets outputInputMaps
    case M.lookup outPt (outBoxFree prevIOMap) of
        Just frees -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxFree = M.insert outPt ((sann, inBox) : frees) (outBoxFree prevIOMap) } } ) 
        Nothing -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxFree = M.insert outPt [(sann, inBox)] (outBoxFree prevIOMap) } } )

addIOMapSend :: Identifier -> SemanticAnn -> InBox SemanticAnn -> BoxInOutMonad ()
addIOMapSend outPt sann inBox = do
    prevIOMap <- ST.gets outputInputMaps
    case M.lookup outPt (outBoxSend prevIOMap) of
        Just sends -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxSend = M.insert outPt ((sann, inBox) : sends) (outBoxSend prevIOMap) } } ) 
        Nothing -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxSend = M.insert outPt [(sann, inBox)] (outBoxSend prevIOMap) } } )

addIOMapDeque :: Identifier -> SemanticAnn -> InBox SemanticAnn -> BoxInOutMonad ()
addIOMapDeque deque sann inBox = do
    prevIOMap <- ST.gets outputInputMaps
    case M.lookup deque (outBoxDeque prevIOMap) of
        Just puts -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxDeque = M.insert deque ((sann, inBox) : puts) (outBoxDeque prevIOMap) } } )
        Nothing -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxDeque = M.insert deque [(sann, inBox)] (outBoxDeque prevIOMap) } } )

addIOMapProcedureCall :: Identifier -> Identifier -> Integer -> SemanticAnn -> InBox SemanticAnn -> BoxInOutMonad ()
addIOMapProcedureCall outPt procId argNum sann inBox = do
    let procedureCall = (outPt, procId, argNum)
    prevIOMap <- ST.gets outputInputMaps
    case M.lookup procedureCall (outBoxProcedureCall prevIOMap) of
        Just calls -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxProcedureCall = M.insert procedureCall ((sann, inBox) : calls) (outBoxProcedureCall prevIOMap) } } ) 
        Nothing -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxProcedureCall = M.insert procedureCall [(sann, inBox)] (outBoxProcedureCall prevIOMap) } } )

addIOMapProcedureOut :: Identifier -> Integer -> SemanticAnn -> InBox SemanticAnn -> BoxInOutMonad ()
addIOMapProcedureOut procId argNum sann inBox = do
    prevIOMap <- ST.gets outputInputMaps
    ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxProcedureOut = M.insertWith (++) (procId, argNum) [(sann, inBox)] (outBoxProcedureOut prevIOMap) } } )

inOutDestroyOptionMatchCases :: InOptionBox SemanticAnn -> [MatchCase SemanticAnn] -> BoxInOutMonad ()
inOutDestroyOptionMatchCases optionBoxSource [fstCase, sndCase] = do 
    let (someCase, MatchCase _ _ noneBody _) = if matchIdentifier fstCase == "Some" then (fstCase, sndCase) else (sndCase, fstCase)
    case someCase of
        MatchCase "Some" [(arg, _)] body _ -> do
            localInputScope (addBox arg (getInBox optionBoxSource) >> mapM_ inOutBasicBlock (blockBody body))
        _ -> throwError $ annotateError Internal EInvalidMatchCase
    localInputScope (mapM_ inOutBasicBlock (blockBody noneBody))
inOutDestroyOptionMatchCases optionBoxSource [MatchCase "Some" [(arg, _)] body _] =
    localInputScope (addBox arg (getInBox optionBoxSource) >> mapM_ inOutBasicBlock (blockBody body))
inOutDestroyOptionMatchCases _ [MatchCase "None" _ noneBody _] = 
    localInputScope (mapM_ inOutBasicBlock (blockBody noneBody))
inOutDestroyOptionMatchCases _ _ = throwError $ annotateError Internal EInvalidMatchCase

inOutBasicBlock :: BasicBlock SemanticAnn -> BoxInOutMonad ()
inOutBasicBlock (AllocBox obj arg ann) = do
    -- | First we need to obtain the name of the allocator port
    inPt <- getPortName obj
    optionBox <- getExprOptionBoxName arg
    fillOptionBox optionBox ann (InOptionBoxAlloc inPt ann)
inOutBasicBlock (FreeBox obj arg ann) = do
    outPt <- getPortName obj
    boxName <- getExprBoxName arg
    boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
    addIOMapFree outPt ann boxSource
inOutBasicBlock (DequeTake obj arg ann) = do
    deque <- getPortName obj
    optionBox <- getExprOptionBoxName arg
    fillOptionBox optionBox ann (InOptionBoxDeque deque ann)
inOutBasicBlock (DequePut _ obj arg ann) = do
    deque <- getPortName obj
    boxName <- getExprBoxName arg
    boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
    addIOMapDeque deque ann boxSource
inOutBasicBlock (DequeExchange kind obj arg out ann) = do
    deque <- getPortName obj
    boxName <- getExprBoxName arg
    boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
    addIOMapDeque deque ann boxSource
    optionBox <- getExprOptionBoxName out
    -- | A try_put leaves out the box it was given, and a replace one that was
    -- already in the deque
    case kind of
        DequeTryPut -> fillOptionBox optionBox ann (InOptionBoxRejected boxSource)
        DequeReplace -> fillOptionBox optionBox ann (InOptionBoxDeque deque ann)
inOutBasicBlock (IfElseBlock bTrue elseIfs bFalse _ann) =
    localInputScope $ mapM_ inOutBasicBlock (blockBody . condIfBody $ bTrue) >> mapM_ (mapM_ inOutBasicBlock) (blockBody . condElseIfBody<$> elseIfs) >> mapM_ (mapM_ inOutBasicBlock . blockBody . condElseBody) bFalse
inOutBasicBlock (ForLoopBlock _ _ _ _ _ body _) =
    -- | Only the boxes born in an iteration can be released in it
    localInputScope $ mapM_ inOutBasicBlock (blockBody body)
inOutBasicBlock (MatchBlock eMatch cases mDefaultCase _) = do
    -- | We need to check if we are destroying a box
    eTy <- getExprType eMatch
    case eTy of
        -- | If the match expression is an option box, we need to destroy the box
        -- and propagate the box to the Some case
        TOption (TBoxSubtype _) -> do
            -- | First we need to obtain the name of the option box variable, so that
            -- we can look for the port where its box came from.
            optionBox <- getExprOptionBoxName eMatch
            -- | We look for the port where the box came from
            optionBoxSource <- ST.gets (fromJust . M.lookup optionBox . inOptionBoxMap)
            inOutDestroyOptionMatchCases optionBoxSource cases
        _ -> mapM_ (mapM_ inOutBasicBlock) (blockBody . matchBody <$> cases)
    case mDefaultCase of
        Just (DefaultCase defBody _) -> mapM_ inOutBasicBlock (blockBody defBody)
        Nothing -> return ()
inOutBasicBlock (SendMessage obj arg ann) = do
    msgTy <- getExprType arg
    case msgTy of
        TBoxSubtype _ -> do
            outPt <- getPortName obj
            boxName <- getExprBoxName arg
            boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
            addIOMapSend outPt ann boxSource
        _ -> return ()
inOutBasicBlock (ProcedureInvoke obj procId args ann) = do
    -- | We need to check if we are sending a box as an argument
    argTys <- mapM getExprType args
    zipWithM_ checkArg [0..] argTys

    where
        checkArg argNum argTy = case argTy of
            TBoxSubtype _ -> do
                outPt <- getPortName obj
                boxName <- getExprBoxName (args !! argNum)
                boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
                addIOMapProcedureCall outPt procId (toInteger argNum) ann boxSource
            -- | The option-box is moved into the procedure
            TOption (TBoxSubtype _) -> do
                outPt <- getPortName obj
                optionBox <- getExprOptionBoxName (args !! argNum)
                optionBoxSource <- ST.gets (fromJust . M.lookup optionBox . inOptionBoxMap)
                addIOMapProcedureCall outPt procId (toInteger argNum) ann (getInBox optionBoxSource)
            -- | The procedure writes a box into the option-box
            TReference Mutable (TOption (TBoxSubtype _)) -> do
                outPt <- getPortName obj
                optionBox <- getExprOptionBoxName (args !! argNum)
                fillOptionBox optionBox ann (InOptionBoxProcedureOut outPt procId (toInteger argNum) ann)
            _ -> return ()
inOutBasicBlock (RegularBlock stmts) = mapM_ inOutStatement stmts
inOutBasicBlock _ = return ()

-- | Assigning Some to an option-box moves the box into it
inOutStatement :: Statement SemanticAnn -> BoxInOutMonad ()
inOutStatement (AssignmentStmt obj (MonadicVariantInitializer (Some arg) _) ann) = do
    objTy <- getObjType obj
    case objTy of
        TOption (TBoxSubtype _) -> do
            optionBox <- getObjOptionBoxName obj
            boxName <- getExprBoxName arg
            boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
            fillOptionBox optionBox ann (InOptionBoxSome boxSource)
        _ -> return ()
inOutStatement _ = return ()

inOutBasicBlocks :: Block SemanticAnn -> BoxInOutMonad ()
inOutBasicBlocks (Block blocks _) = mapM_ inOutBasicBlock blocks

inOutClassMember :: M.Map Identifier (FieldDefinition SemanticAnn) -> ClassMember SemanticAnn -> BoxInOutMonad ()
inOutClassMember _ (ClassField {}) = return ()
inOutClassMember _ (ClassMethod _ _ _ _ body _) = inOutBasicBlocks body
inOutClassMember _ (ClassViewer {}) = return ()
inOutClassMember _ (ClassAction _ _ Nothing body _) = inOutBasicBlocks body
inOutClassMember _ (ClassCase _ _ _ body _) = inOutBasicBlocks body
inOutClassMember actionsToPorts (ClassAction _ name (Just input) body _ann) = do
    clearInputScope
    case paramType input of
        (TBoxSubtype _) -> do
            let inPt = actionsToPorts M.! name
            addBox (paramIdentifier input) (InBoxInput (fieldIdentifier inPt))
            inOutBasicBlocks body
        _ -> inOutBasicBlocks body
inOutClassMember _ (ClassProcedure _ name params body _ann) = do
    clearInputScope
    zipWithM_ addInParam [0..] params
    inOutBasicBlocks body

    where

        addInParam :: Integer -> Parameter a -> BoxInOutMonad ()
        addInParam argNum param = case paramType param of
            TBoxSubtype _ -> do
                addBox (paramIdentifier param) (InBoxProcedureCall name argNum)
            TOption (TBoxSubtype _) ->
                addOptionBox (paramIdentifier param) (InOptionBoxProcedureCall name argNum)
            TReference Mutable (TOption (TBoxSubtype _)) ->
                ST.modify (\s -> s { outParamMap = M.insert (paramIdentifier param) (name, argNum) (outParamMap s) })
            _ -> return ()

inOutClass :: TypeDef SemanticAnn -> BoxInOutMonad ()
inOutClass (Class _ _ members _ _) = do
    -- | Get the map between the actions and the ports from the members list
    let actionsToPorts = M.fromList $ 
            (map (
                \case {
                    ClassField field@(FieldDefinition _ (TInPort _ action) _) -> (action, field);
                    ClassField field@(FieldDefinition _ (TSinkPort _ action) _) -> (action, field);
                    _ -> error "This should not happen"
                }) . filter (\case {
                ClassField (FieldDefinition _ (TInPort {}) _) -> True;
                ClassField (FieldDefinition _ (TSinkPort {}) _) -> True;
                _ -> False
             })) members
    mapM_ (inOutClassMember actionsToPorts) members
inOutClass _ = return ()

emptyBoxOutputInputMaps :: BoxOutputInputMaps a
emptyBoxOutputInputMaps = BoxOutputInputMaps M.empty M.empty M.empty M.empty M.empty

emptyBoxInOutState :: BoxInOutState a
emptyBoxInOutState = BoxInOutState M.empty M.empty M.empty emptyBoxOutputInputMaps

runInOutClass ::
  TypeDef SemanticAnn
  -> Either ArchitectureError (BoxOutputInputMaps SemanticAnn)
runInOutClass tyDef =
  case flip ST.runState emptyBoxInOutState . runExceptT $ inOutClass tyDef of
    (Left err, _) -> Left err
    (Right _, st) -> Right (outputInputMaps st)
