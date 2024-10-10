module ControlFlow.Architecture.BoxInOut where

import ControlFlow.BasicBlocks.AST
import ControlFlow.Architecture.Types

import qualified Control.Monad.State.Strict as ST
import Control.Monad.Except

import qualified Data.Map as M
import ControlFlow.Architecture.Errors.Errors
import Semantic.Types
import ControlFlow.Architecture.Utils
import Utils.Annotations
import Data.Maybe
import qualified Data.Set as S


type BoxInOutMonad = ExceptT ArchitectureError (ST.State BoxInOutState)

localInputScope :: BoxInOutMonad a -> BoxInOutMonad a
localInputScope comp = do
  prevst <- ST.get
  res <- comp
  currst <- ST.get
  ST.put (prevst { outputInputMaps = outputInputMaps currst })
  return res

clearInputScope :: BoxInOutMonad ()
clearInputScope = ST.modify (\s -> s { inBoxMap = M.empty, inOptionBoxMap = M.empty }) 

addOptionBox :: Identifier -> InOptionBox -> BoxInOutMonad ()
addOptionBox inPt optionBox = do
    ST.modify (\s -> s { inOptionBoxMap = M.insert inPt optionBox (inOptionBoxMap s) })

addBox :: Identifier -> InBox -> BoxInOutMonad ()
addBox inPt inBox = do
    ST.modify (\s -> s { inBoxMap = M.insert inPt inBox (inBoxMap s) })

addIOMapFree :: Identifier -> InBox -> BoxInOutMonad ()
addIOMapFree outPt inBox = do
    prevIOMap <- ST.gets outputInputMaps
    case M.lookup outPt (outBoxFree prevIOMap) of
        Just frees -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxFree = M.insert outPt (S.insert inBox frees) (outBoxFree prevIOMap) } } ) 
        Nothing -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxFree = M.insert outPt (S.singleton inBox) (outBoxFree prevIOMap) } } )

addIOMapSend :: Identifier -> InBox -> BoxInOutMonad ()
addIOMapSend outPt inBox = do
    prevIOMap <- ST.gets outputInputMaps
    case M.lookup outPt (outBoxSend prevIOMap) of
        Just sends -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxSend = M.insert outPt (S.insert inBox sends) (outBoxSend prevIOMap) } } ) 
        Nothing -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxSend = M.insert outPt (S.singleton inBox) (outBoxSend prevIOMap) } } )

addIOMapProcedureCall :: Identifier -> Identifier -> Integer -> InBox -> BoxInOutMonad ()
addIOMapProcedureCall outPt procId argNum inBox = do
    let procedureCall = (outPt, procId, argNum)
    prevIOMap <- ST.gets outputInputMaps
    case M.lookup procedureCall (outBoxProcedureCall prevIOMap) of
        Just calls -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxProcedureCall = M.insert procedureCall (S.insert inBox calls) (outBoxProcedureCall prevIOMap) } } ) 
        Nothing -> ST.modify (\s -> s { outputInputMaps = prevIOMap { outBoxProcedureCall = M.insert procedureCall (S.singleton inBox) (outBoxProcedureCall prevIOMap) } } )

inOutDestroyOptionMatchCases :: InOptionBox -> [MatchCase SemanticAnn] -> BoxInOutMonad ()
inOutDestroyOptionMatchCases optionBoxSource [fstCase, sndCase] = do 
    let (someCase, MatchCase _ _ noneBody _) = if matchIdentifier fstCase == "Some" then (fstCase, sndCase) else (sndCase, fstCase)
    case someCase of
        MatchCase "Some" [arg] body _ -> do
            localInputScope (addBox arg (getInBox optionBoxSource) >> mapM_ inOutBasicBlock body)
        _ -> throwError $ annotateError Internal EUnboxingMatchCase
    localInputScope (mapM_ inOutBasicBlock noneBody) 
inOutDestroyOptionMatchCases _ _ = throwError $ annotateError Internal EUnboxingMatchCase

inOutBasicBlock :: BasicBlock SemanticAnn -> BoxInOutMonad ()
inOutBasicBlock (AllocBox obj arg _ann) = do
    -- | First we need to obtain the name of the allocator port
    inPt <- getPortName obj
    optionBox <- getExprOptionBoxName arg
    addOptionBox optionBox (InOptionBoxAlloc inPt)
inOutBasicBlock (FreeBox obj arg _ann) = do
    outPt <- getPortName obj
    boxName <- getExprBoxName arg
    boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
    addIOMapFree outPt boxSource
inOutBasicBlock (IfElseBlock _eCond bTrue elseIfs bFalse _ann) =
    localInputScope $ mapM_ inOutBasicBlock bTrue >> mapM_ (mapM_ inOutBasicBlock) (elseIfBody <$> elseIfs) >> maybe (return ()) (mapM_ inOutBasicBlock) bFalse
inOutBasicBlock (ForLoopBlock {}) =
    -- | Inside a for loop, there cannot be any box releasing
    return ()
inOutBasicBlock (MatchBlock eMatch cases _) = do
    -- | We need to check if we are destroying a box
    eTy <- getExprType eMatch
    case eTy of
        -- | If the match expression is an option box, we need to destroy the box
        -- and propagate the box to the Some case
        Option (BoxSubtype _) -> do
            -- | First we need to obtain the name of the option box variable, so that
            -- we can look for the port where its box came from.
            optionBox <- getExprOptionBoxName eMatch
            -- | We look for the port where the box came from
            optionBoxSource <- ST.gets (fromJust . M.lookup optionBox . inOptionBoxMap)
            inOutDestroyOptionMatchCases optionBoxSource cases
        _ -> mapM_ (mapM_ inOutBasicBlock) (matchBody <$> cases)
inOutBasicBlock (SendMessage obj arg _) = do
    msgTy <- getExprType arg
    case msgTy of
        BoxSubtype _ -> do
            outPt <- getPortName obj
            boxName <- getExprBoxName arg
            boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
            addIOMapSend outPt boxSource
        _ -> return ()
inOutBasicBlock (ProcedureCall obj procId args _) = do
    -- | We need to check if we are sending a box as an argument
    argTys <- mapM getExprType args
    zipWithM_ checkArg [0..] argTys

    where
        checkArg argNum argTy = case argTy of
            BoxSubtype _ -> do
                outPt <- getPortName obj
                boxName <- getExprBoxName (args !! argNum)
                boxSource <- ST.gets (fromJust . M.lookup boxName . inBoxMap)
                addIOMapProcedureCall outPt procId (toInteger argNum) boxSource
            _ -> return ()
inOutBasicBlock _ = return () 

inOutBasicBlocks :: Block SemanticAnn -> BoxInOutMonad ()
inOutBasicBlocks (Block blocks) = mapM_ inOutBasicBlock blocks

inOutClassMember :: M.Map Identifier FieldDefinition -> ClassMember SemanticAnn -> BoxInOutMonad ()
inOutClassMember _ (ClassField {}) = return ()
inOutClassMember _ (ClassMethod _ _ body _) = inOutBasicBlocks body
inOutClassMember _ (ClassViewer {}) = return ()
inOutClassMember actionsToPorts (ClassAction name input _ body _) = do
    clearInputScope
    case paramTerminaType input of
        (BoxSubtype _) -> do
            let inPt = actionsToPorts M.! name
            case fieldTerminaType inPt of
                InPort _ portName ->
                    addBox (paramIdentifier input) (InBoxInput portName)
                _ -> throwError $ annotateError Internal EUnboxingClassField
            inOutBasicBlocks body
        _ -> inOutBasicBlocks body
inOutClassMember _ (ClassProcedure name params body _) = do
    clearInputScope
    zipWithM_ addInParam [0..] params
    inOutBasicBlocks body

    where

        addInParam :: Integer -> Parameter -> BoxInOutMonad ()
        addInParam argNum param = case paramTerminaType param of
            BoxSubtype _ -> do
                addBox (paramIdentifier param) (InBoxProcedureCall name argNum)
            _ -> return ()

inOutClass :: TypeDef SemanticAnn -> BoxInOutMonad ()
inOutClass (Class _ _ members _ _) = do
    -- | Get the map between the actions and the ports from the members list
    let actionsToPorts = M.fromList $ 
            (map (
                \case {
                    ClassField field@(FieldDefinition _ (InPort _ action)) _ -> (action, field);
                    ClassField field@(FieldDefinition _ (SinkPort _ action)) _ -> (action, field);
                    _ -> error "This should not happen"
                }) . filter (\case {
                ClassField (FieldDefinition _ (InPort {})) _ -> True;
                ClassField (FieldDefinition _ (SinkPort {})) _ -> True;
                _ -> False
             })) members
    mapM_ (inOutClassMember actionsToPorts) members
inOutClass _ = return ()

emptyBoxOutputInputMaps :: BoxOutputInputMaps
emptyBoxOutputInputMaps = BoxOutputInputMaps M.empty M.empty M.empty

emptyBoxInOutState :: BoxInOutState
emptyBoxInOutState = BoxInOutState M.empty M.empty emptyBoxOutputInputMaps

runInOutClass ::
  TypeDef SemanticAnn
  -> Either ArchitectureError BoxOutputInputMaps
runInOutClass tyDef =
  case flip ST.runState emptyBoxInOutState . runExceptT $ inOutClass tyDef of
    (Left err, _) -> Left err
    (Right _, st) -> Right (outputInputMaps st)