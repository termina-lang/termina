module EFP.Schedulability.TransPath.Generator where

import ControlFlow.BasicBlocks.AST
import Semantic.Types
import EFP.Schedulability.TransPath.AST
import Utils.Annotations


mergeLocations :: Location -> Location -> Location
mergeLocations (Position name startPos _) (Position _ _ endPos) = Position name startPos endPos
-- | Other cases (built-in or internal) will be silently ignored and the first location will be kept
mergeLocations loc _ = loc

genPaths :: BasicBlock SemanticAnn -> [WCEPathBlock SemanticAnn]
genPaths (RegularBlock stmts) =
    -- | Here we assume that the regular block is well-formed and contains at least one statement
    let firstLoc = getLocation . getAnnotation . Prelude.head $ stmts
        endLoc = getLocation . getAnnotation . Prelude.last $ stmts
        blockLoc = mergeLocations firstLoc endLoc in
    [WCEPRegularBlock blockLoc]

genPaths (IfElseBlock ifBlk elifs mElse ann) =
    let ifPaths = flip WCEPathCondIf (getLocation . condIfAnnotation $ ifBlk) <$> genWCEPaths [] (blockBody . condIfBody $ ifBlk)
        elifPaths = fmap (
            \(CondElseIf _ elifBlk ann') ->
                flip WCEPathCondElseIf (getLocation ann') <$> genWCEPaths [] (blockBody elifBlk)) elifs
        elsePaths = case mElse of
            Just elseBlk -> flip WCEPathCondElse (getLocation . condElseAnnotation $ elseBlk) <$> genWCEPaths [] (blockBody . condElseBody $ elseBlk)
            Nothing -> []
    in
        case (ifPaths, elifPaths, elsePaths) of
            -- | If the path on if branch only contains one sinlge regular block,
            -- we can merge it into the current path
            ([WCEPathCondIf [WCEPRegularBlock _] _], [], []) ->
                [WCEPRegularBlock (getLocation ann)]
            -- | If the paths on if and else branches only contain one sinlge regular block,
            -- we can merge them into the current path
            ([WCEPathCondIf [WCEPRegularBlock _] _], [], [WCEPathCondElse [WCEPRegularBlock _] _]) ->
                [WCEPRegularBlock (getLocation ann)]
            -- | If the paths on all branches only contain one sinlge regular block, 
            -- we can merge them into the current path
            ([WCEPathCondIf [WCEPRegularBlock _] _], _, [WCEPathCondElse [WCEPRegularBlock _] _]) ->
                let allElifRegular = all (\case [WCEPathCondElseIf [WCEPRegularBlock _] _] -> True;
                                                _ -> False) elifPaths in
                if allElifRegular then
                    [WCEPRegularBlock (getLocation ann)]
                else
                    ifPaths ++ concat elifPaths ++ elsePaths
            _ -> ifPaths ++ concat elifPaths ++ elsePaths

genPaths (ForLoopBlock _ _ lower upper _ blk ann) =
    let loopPaths = flip (WCEPathForLoop lower upper) (getLocation ann) <$> genWCEPaths [] (blockBody blk) in
    case loopPaths of
        -- | If the path on loop body only contains one sinlge regular block,
        -- we can merge it into the current path
        [WCEPathForLoop _ _ [WCEPRegularBlock _] _] ->
            [WCEPRegularBlock (getLocation ann)]
        _ -> loopPaths
genPaths (MatchBlock _ cases mDefaultCase ann) =
    let casePaths = fmap (
            \(MatchCase _ _ caseBlk ann') ->
                flip WCEPathMatchCase (getLocation ann') <$> genWCEPaths [] (blockBody caseBlk)) cases
        defaultPaths = case mDefaultCase of
            Just (DefaultCase defBlk ann') ->
                [flip WCEPathMatchCase (getLocation ann') <$> genWCEPaths [] (blockBody defBlk)]
            Nothing -> []
        allCasesRegular = all (\case [WCEPathMatchCase [WCEPRegularBlock _] _] -> True;
                                      _ -> False) (casePaths ++ defaultPaths)
    in
        if allCasesRegular then
            [WCEPRegularBlock (getLocation ann)]
        else
            concat $ casePaths ++ defaultPaths
genPaths (SendMessage obj _ ann) = 
    let outPt = case obj of 
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in SendMessage block when generating WCE paths"
    in
        [WCEPSendMessage outPt (getLocation ann)]
genPaths (ProcedureInvoke obj procId args ann) =
    let outPt = case obj of 
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in ProcedureInvoke block when generating WCE paths"
        pts = case ann of
            SemanticAnn (ETy (AppType pts' _)) _ -> pts'
            _ -> error "Unexpected annotation in ProcedureInvoke block when generating WCE paths"
        constArgs = fst <$> filter (\case (_, Parameter _ (TConstSubtype _)) -> True; _ -> False) (zip args pts)
    in
        [WCEPProcedureInvoke outPt procId constArgs (getLocation ann)]
genPaths (AtomicLoad _obj _expr ann) =
    [WCEPRegularBlock (getLocation ann)]
genPaths (AtomicStore _obj _expr ann) =
    [WCEPRegularBlock (getLocation ann)]
genPaths (AtomicArrayLoad _obj _index _expr ann) =
    [WCEPRegularBlock (getLocation ann)]
genPaths (AtomicArrayStore _obj _index _expr ann) =
    [WCEPRegularBlock (getLocation ann)]
genPaths (AllocBox obj _args ann) =
    let outPt = case obj of 
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in AllocBox block when generating WCE paths"
    in
        [WCEPAllocBox outPt (getLocation ann)]
genPaths (FreeBox obj _args ann) =
    let outPt = case obj of
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in FreeBox block when generating WCE paths"
    in
        [WCEPFreeBox outPt (getLocation ann)]
genPaths (ReturnBlock _ ann) =
    [WCEPReturn (getLocation ann)]
genPaths (ContinueBlock expr ann) =
    case expr of 
        (MemberFunctionCall _obj ident _args _) -> 
            [WCEPContinue ident (getLocation ann)]
        (DerefMemberFunctionCall _obj ident _args _) ->
            [WCEPContinue ident (getLocation ann)]
        _ -> error "Unexpected expression in Continue block when generating WCE paths"
genPaths (RebootBlock ann) =
    [WCEPReboot (getLocation ann)]
genPaths (SystemCall _obj syscallId _args ann) =
    [WCEPSystemCall syscallId (getLocation ann)]


genWCEPaths :: [[WCEPathBlock SemanticAnn]] -> [BasicBlock SemanticAnn] -> [[WCEPathBlock SemanticAnn]]
genWCEPaths paths [] = paths
genWCEPaths paths (blk : xs) = 
    let newPaths = genPaths blk 
        appendedPaths = concatMap (\p -> mergePath p <$> paths) newPaths in
    genWCEPaths appendedPaths xs

    where

        mergePath :: WCEPathBlock SemanticAnn -> [WCEPathBlock SemanticAnn] -> [WCEPathBlock SemanticAnn]
        mergePath newPath [] = [newPath]
        mergePath (WCEPRegularBlock newLoc) (WCEPRegularBlock pathLoc : rest) = 
            let mergedLoc = mergeLocations pathLoc newLoc
            in (WCEPRegularBlock mergedLoc : rest)
        mergePath newPath paths' = newPath : paths'


genClassMemberWCEPs :: Identifier -> ClassMember SemanticAnn -> [TransactionalWCEPath SemanticAnn]
genClassMemberWCEPs className (ClassMethod _ak ident _params _mrty blk _) = 
    let wcePaths = genWCEPaths [] (blockBody blk) in
    zipWith (TransactionalWCEPath className ident) ["path" ++ show idx | idx <- [1..]] wcePaths
genClassMemberWCEPs className (ClassProcedure _ak ident _params blk _) = 
    let wcePaths = genWCEPaths [] (blockBody blk) in
    zipWith (TransactionalWCEPath className ident) ["path" ++ show idx | idx <- [1..]] wcePaths
genClassMemberWCEPs className (ClassAction _ak ident _param _mrty blk _) = 
    let wcePaths = genWCEPaths [] (blockBody blk) in
    zipWith (TransactionalWCEPath className ident) ["path" ++ show idx | idx <- [1..]] wcePaths
genClassMemberWCEPs className (ClassViewer ident _params _mrty blk _) = 
    let wcePaths = genWCEPaths [] (blockBody blk) in
    zipWith (TransactionalWCEPath className ident) ["path" ++ show idx | idx <- [1..]] wcePaths
genClassMemberWCEPs _ _ = []

genTransactionalWCEPS :: AnnotatedProgram SemanticAnn -> [TransactionalWCEPath SemanticAnn]
genTransactionalWCEPS (TypeDefinition (Class _kind ident members _ _) _ : xs) =
    let classWCEPs = concatMap (genClassMemberWCEPs ident) members
    in classWCEPs ++ genTransactionalWCEPS xs
genTransactionalWCEPS (_ : xs) = genTransactionalWCEPS xs
genTransactionalWCEPS [] = []