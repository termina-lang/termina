module ControlFlow.Architecture.BoxInOut where

import ControlFlow.BasicBlocks.AST
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
clearInputScope = ST.modify (\s -> s { inBoxMap = M.empty, inOptionBoxMap = M.empty }) 

addOptionBox :: Identifier -> InOptionBox SemanticAnn -> BoxInOutMonad ()
addOptionBox inPt optionBox = do
    ST.modify (\s -> s { inOptionBoxMap = M.insert inPt optionBox (inOptionBoxMap s) })

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

addIOMapProcedureCall :: Identifier -> Identifier -> Integer -> SemanticAnn -> InBox SemanticAnn -> BoxInOutMonad ()
addIOMapProcedureCall outPt procId argNum sann inBox = do
    let procedureCall = (outPt, procId, argNum)
    prevIOMap <- ST.gets outputInputMaps
    case M.lookup procedureCall (outBoxProcedureCall prevIOMap) of
        Just calls -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxProcedureCall = M.insert procedureCall ((sann, inBox) : calls) (outBoxProcedureCall prevIOMap) } } ) 
        Nothing -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxProcedureCall = M.insert procedureCall [(sann, inBox)] (outBoxProcedureCall prevIOMap) } } )

inOutDestroyOptionMatchCases :: InOptionBox SemanticAnn -> [MatchCase SemanticAnn] -> BoxInOutMonad ()
inOutDestroyOptionMatchCases optionBoxSource [fstCase, sndCase] = do 
    let (someCase, MatchCase _ _ noneBody _) = if matchIdentifier fstCase == "Some" then (fstCase, sndCase) else (sndCase, fstCase)
    case someCase of
        MatchCase "Some" [arg] body _ -> do
            localInputScope (addBox arg (getInBox optionBoxSource) >> mapM_ inOutBasicBlock (blockBody body))
        _ -> throwError $ annotateError Internal EInvalidMatchCase
    localInputScope (mapM_ inOutBasicBlock (blockBody noneBody))
inOutDestroyOptionMatchCases optionBoxSource [MatchCase "Some" [arg] body _] =
    localInputScope (addBox arg (getInBox optionBoxSource) >> mapM_ inOutBasicBlock (blockBody body))
inOutDestroyOptionMatchCases _ [MatchCase "None" _ noneBody _] = 
    localInputScope (mapM_ inOutBasicBlock (blockBody noneBody))
inOutDestroyOptionMatchCases _ _ = throwError $ annotateError Internal EInvalidMatchCase

inOutBasicBlock :: BasicBlock SemanticAnn -> BoxInOutMonad ()
inOutBasicBlock (AllocBox obj arg ann) = do
    -- | First we need to obtain the name of the allocator port
    inPt <- getPortName obj
    optionBox <- getExprOptionBoxName arg
    addOptionBox optionBox (InOptionBoxAlloc inPt ann)
inOutBasicBlock (FreeBox obj arg ann) = do
    outPt <- getPortName obj
    boxName <- getExprBoxName arg
    boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
    addIOMapFree outPt ann boxSource
inOutBasicBlock (IfElseBlock bTrue elseIfs bFalse _ann) =
    localInputScope $ mapM_ inOutBasicBlock (blockBody . condIfBody $ bTrue) >> mapM_ (mapM_ inOutBasicBlock) (blockBody . condElseIfBody<$> elseIfs) >> maybe (return ()) (mapM_ inOutBasicBlock . blockBody . condElseBody) bFalse
inOutBasicBlock (ForLoopBlock {}) =
    -- | Inside a for loop, there cannot be any box releasing
    return ()
inOutBasicBlock (MatchBlock eMatch cases mDefaultCase _) = do
    -- | We need to check if we are destroying a box
    eTy <- getExprType eMatch
    case eTy of
        -- | If the match expression is an option box, we need to destroy the box
        -- and propagate the box to the Some case
        TOption (TBoxSubtype _) -> do
            -- | First we need to obtain the name of the option box variable, so that
            -- we can look for the port where its box came from.
            optionBox <- getExprOptionBoxName eMatch
            -- | We look for the port where the box came from
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
    -- | We need to check if we are sending a box as an argument
    argTys <- mapM getExprType args
    zipWithM_ checkArg [0..] argTys

    where
        checkArg argNum argTy = case argTy of
            TBoxSubtype _ -> do
                outPt <- getPortName obj
                boxName <- getExprBoxName (args !! argNum)
                boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
                addIOMapProcedureCall outPt procId (toInteger argNum) ann boxSource
            _ -> return ()
inOutBasicBlock _ = return () 

inOutBasicBlocks :: Block SemanticAnn -> BoxInOutMonad ()
inOutBasicBlocks (Block blocks _) = mapM_ inOutBasicBlock blocks

inOutClassMember :: M.Map Identifier (FieldDefinition SemanticAnn) -> ClassMember SemanticAnn -> BoxInOutMonad ()
inOutClassMember _ (ClassField {}) = return ()
inOutClassMember _ (ClassMethod _ _ _ _ body _) = inOutBasicBlocks body
inOutClassMember _ (ClassViewer {}) = return ()
inOutClassMember _ (ClassAction _ _ Nothing _ body _) = inOutBasicBlocks body
inOutClassMember actionsToPorts (ClassAction _ name (Just input) _ body _ann) = do
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
            _ -> return ()

inOutClass :: TypeDef SemanticAnn -> BoxInOutMonad ()
inOutClass (Class _ _ members _ _) = do
    -- | Get the map between the actions and the ports from the members list
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
emptyBoxOutputInputMaps = BoxOutputInputMaps M.empty M.empty M.empty

emptyBoxInOutState :: BoxInOutState a
emptyBoxInOutState = BoxInOutState M.empty M.empty emptyBoxOutputInputMaps

runInOutClass ::
  TypeDef SemanticAnn
  -> Either ArchitectureError (BoxOutputInputMaps SemanticAnn)
runInOutClass tyDef =
  case flip ST.runState emptyBoxInOutState . runExceptT $ inOutClass tyDef of
    (Left err, _) -> Left err
    (Right _, st) -> Right (outputInputMaps st)
