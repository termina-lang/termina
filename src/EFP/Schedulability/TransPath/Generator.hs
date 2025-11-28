module EFP.Schedulability.TransPath.Generator where

import ControlFlow.BasicBlocks.AST
import Semantic.Types
import EFP.Schedulability.TransPath.AST
import Utils.Annotations
import Text.Parsec.Pos


mergeLocations :: BlockPosition -> BlockPosition -> BlockPosition
mergeLocations (BlockPosition startLine startCol _ _) 
    (BlockPosition _ _ endLine endCol) = BlockPosition startLine startCol endLine endCol
-- | Other cases (built-in or internal) will be silently ignored and the first location will be kept

loc2BlockPos :: Location -> BlockPosition
loc2BlockPos (Position _ startPos endPos) =
    BlockPosition
        (toInteger $ sourceLine startPos)
        (toInteger $ sourceColumn startPos)
        (toInteger $ sourceLine endPos)
        (toInteger $ sourceColumn endPos)
loc2BlockPos _ = error "Unable to obtain source code position"

mergePath :: WCEPathBlock -> [WCEPathBlock] -> [WCEPathBlock]
mergePath newPath [] = [newPath]
mergePath (WCEPRegularBlock newLoc) (WCEPRegularBlock pathLoc : rest) =
    let mergedLoc = mergeLocations pathLoc newLoc
    in (WCEPRegularBlock mergedLoc : rest)
mergePath newPath paths' = newPath : paths'

genConstExpression :: Expression SemanticAnn -> WCEPConstExpression
genConstExpression (AccessObject (Variable ident _)) =
    WCEPConstObject ident
genConstExpression (Constant (I tInt _) _) =
    WCEPConstInt tInt
genConstExpression (BinOp op left right _) =
    WCEPConstBinOp op (genConstExpression left) (genConstExpression right)
genConstExpression _ = error "Unsupported constant expression in WCEPConstExpression generation"

genExpressionPath :: Expression SemanticAnn -> [WCEPathBlock] -> [WCEPathBlock]
genExpressionPath (MemberFunctionCall _obj ident args ann) acc =
    let pts = case ann of
            SemanticAnn (ETy (AppType pts' _)) _ -> pts'
            _ -> error "Unexpected annotation in ProcedureInvoke block when generating WCE paths"
        constArgs = genConstExpression . fst <$> filter (\case (_, Parameter _ (TConstSubtype _)) -> True; _ -> False) (zip args pts)
    in
        WCEPathMemberFunctionCall ident constArgs (loc2BlockPos . getLocation $ ann) : acc
genExpressionPath (DerefMemberFunctionCall _obj ident args ann) acc =
    let pts = case ann of
            SemanticAnn (ETy (AppType pts' _)) _ -> pts'
            _ -> error "Unexpected annotation in ProcedureInvoke block when generating WCE paths"
        constArgs = genConstExpression .fst <$> filter (\case (_, Parameter _ (TConstSubtype _)) -> True; _ -> False) (zip args pts)
    in
        WCEPathMemberFunctionCall ident constArgs (loc2BlockPos . getLocation $ ann) : acc
genExpressionPath (BinOp _op left right _) acc =
    let leftPath = genExpressionPath left acc
        rightPath = genExpressionPath right leftPath
    in rightPath
genExpressionPath (Casting expr _ _) acc =
    genExpressionPath expr acc
genExpressionPath (FunctionCall _f args _) acc =
    foldr genExpressionPath acc args
genExpressionPath (StructInitializer fields _) acc =
    foldr genFAExpressionPath acc fields

    where

        genFAExpressionPath :: FieldAssignment SemanticAnn -> [WCEPathBlock] -> [WCEPathBlock]
        genFAExpressionPath (FieldValueAssignment _ expr _) acc' = genExpressionPath expr acc'
        genFAExpressionPath (FieldAddressAssignment _ _ ann') acc' = mergePath (WCEPRegularBlock (loc2BlockPos . getLocation $ ann')) acc'
        genFAExpressionPath (FieldPortConnection _ _ _ ann') acc' = mergePath (WCEPRegularBlock (loc2BlockPos . getLocation $ ann')) acc'

genExpressionPath (EnumVariantInitializer _ _ args _) acc =
    foldr genExpressionPath acc args
genExpressionPath (ArrayInitializer value size _) acc =
    let sizePath = genExpressionPath value acc
        valuePath = genExpressionPath size sizePath
    in valuePath
genExpressionPath (ArrayExprListInitializer exprs _) acc =
    foldr genExpressionPath acc exprs
genExpressionPath (MonadicVariantInitializer ov ann) acc =
    case ov of
        Ok expr -> genExpressionPath expr acc
        Error expr -> genExpressionPath expr acc
        Some expr -> genExpressionPath expr acc
        Failure expr -> genExpressionPath expr acc
        _ -> mergePath (WCEPRegularBlock (loc2BlockPos . getLocation $ ann)) acc
genExpressionPath expr acc =
    let loc = loc2BlockPos . getLocation . getAnnotation $ expr
    in
        mergePath (WCEPRegularBlock loc) acc

genRegularBlockPath :: [WCEPathBlock] -> [Statement SemanticAnn] -> [WCEPathBlock]
genRegularBlockPath acc [] = acc
genRegularBlockPath acc (Declaration _ _ _ expr _ : xs) =
    let exprPath = genExpressionPath expr acc
    in genRegularBlockPath exprPath xs
genRegularBlockPath acc (AssignmentStmt _ expr _ : xs) =
    let exprPath = genExpressionPath expr acc
    in genRegularBlockPath exprPath xs
genRegularBlockPath acc (SingleExpStmt expr _ : xs) =
    let exprPath = genExpressionPath expr acc
    in genRegularBlockPath exprPath xs

genPaths :: BasicBlock SemanticAnn -> [WCEPathBlock]
genPaths (RegularBlock stmts) =
    genRegularBlockPath [] stmts

genPaths (IfElseBlock ifBlk elifs mElse ann) =
    let ifPaths = flip WCEPathCondIf (loc2BlockPos . getLocation . condIfAnnotation $ ifBlk) . reverse <$> genWCEPaths [] (blockBody . condIfBody $ ifBlk)
        elifPaths = fmap (
            \(CondElseIf _ elifBlk ann') ->
                flip WCEPathCondElseIf (loc2BlockPos . getLocation $ ann') . reverse <$> genWCEPaths [] (blockBody elifBlk)) elifs
        elsePaths = case mElse of
            Just elseBlk -> flip WCEPathCondElse (loc2BlockPos . getLocation . condElseAnnotation $ elseBlk) . reverse <$> genWCEPaths [] (blockBody . condElseBody $ elseBlk)
            Nothing -> []
    in
        case (ifPaths, elifPaths, elsePaths) of
            -- | If the path on if branch only contains one sinlge regular block,
            -- we can merge it into the current path
            ([WCEPathCondIf [WCEPRegularBlock _] _], [], []) ->
                [WCEPRegularBlock (loc2BlockPos . getLocation $ ann)]
            -- | If the paths on if and else branches only contain one sinlge regular block,
            -- we can merge them into the current path
            ([WCEPathCondIf [WCEPRegularBlock _] _], [], [WCEPathCondElse [WCEPRegularBlock _] _]) ->
                [WCEPRegularBlock (loc2BlockPos . getLocation $ ann)]
            -- | If the paths on all branches only contain one sinlge regular block, 
            -- we can merge them into the current path
            ([WCEPathCondIf [WCEPRegularBlock _] _], _, [WCEPathCondElse [WCEPRegularBlock _] _]) ->
                let allElifRegular = all (\case [WCEPathCondElseIf [WCEPRegularBlock _] _] -> True;
                                                _ -> False) elifPaths in
                if allElifRegular then
                    [WCEPRegularBlock (loc2BlockPos . getLocation $ ann)]
                else
                    ifPaths ++ concat elifPaths ++ elsePaths
            _ -> ifPaths ++ concat elifPaths ++ elsePaths

genPaths (ForLoopBlock _ _ lower upper _ blk ann) =
    let loopPaths = flip (WCEPathForLoop (genConstExpression lower) (genConstExpression upper)) 
                            (loc2BlockPos . getLocation $ ann) . reverse <$> genWCEPaths [] (blockBody blk) 
    in
    case loopPaths of
        -- | If the path on loop body only contains one sinlge regular block,
        -- we can merge it into the current path
        [WCEPathForLoop _ _ [WCEPRegularBlock _] _] ->
            [WCEPRegularBlock (loc2BlockPos . getLocation $ ann)]
        _ -> loopPaths
genPaths (MatchBlock _ cases mDefaultCase ann) =
    let casePaths = fmap (
            \(MatchCase _ _ caseBlk ann') ->
                flip WCEPathMatchCase (loc2BlockPos . getLocation $ ann') <$> genWCEPaths [] (blockBody caseBlk)) cases
        defaultPaths = case mDefaultCase of
            Just (DefaultCase defBlk ann') ->
                [flip WCEPathMatchCase (loc2BlockPos . getLocation $ ann') <$> genWCEPaths [] (blockBody defBlk)]
            Nothing -> []
        allCasesRegular = all (\case [WCEPathMatchCase [WCEPRegularBlock _] _] -> True;
                                      _ -> False) (casePaths ++ defaultPaths)
    in
        if allCasesRegular then
            [WCEPRegularBlock (loc2BlockPos . getLocation $ ann)]
        else
            concat $ casePaths ++ defaultPaths
genPaths (SendMessage obj _ ann) =
    let outPt = case obj of
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in SendMessage block when generating WCE paths"
    in
        [WCEPSendMessage outPt (loc2BlockPos . getLocation $ ann)]
genPaths (ProcedureInvoke obj procId args ann) =
    let outPt = case obj of
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in ProcedureInvoke block when generating WCE paths"
        pts = case ann of
            SemanticAnn (ETy (AppType pts' _)) _ -> pts'
            _ -> error "Unexpected annotation in ProcedureInvoke block when generating WCE paths"
        constArgs = genConstExpression . fst <$> filter (\case (_, Parameter _ (TConstSubtype _)) -> True; _ -> False) (zip args pts)
    in
        [WCEPProcedureInvoke outPt procId constArgs (loc2BlockPos . getLocation $ ann)]
genPaths (AtomicLoad _obj _expr ann) =
    [WCEPRegularBlock (loc2BlockPos . getLocation $ ann)]
genPaths (AtomicStore _obj _expr ann) =
    [WCEPRegularBlock (loc2BlockPos . getLocation $ ann)]
genPaths (AtomicArrayLoad _obj _index _expr ann) =
    [WCEPRegularBlock (loc2BlockPos . getLocation $ ann)]
genPaths (AtomicArrayStore _obj _index _expr ann) =
    [WCEPRegularBlock (loc2BlockPos . getLocation $ ann)]
genPaths (AllocBox obj _args ann) =
    let outPt = case obj of
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in AllocBox block when generating WCE paths"
    in
        [WCEPAllocBox outPt (loc2BlockPos . getLocation $ ann)]
genPaths (FreeBox obj _args ann) =
    let outPt = case obj of
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in FreeBox block when generating WCE paths"
    in
        [WCEPFreeBox outPt (loc2BlockPos . getLocation $ ann)]
genPaths (ReturnBlock _ ann) =
    [WCEPReturn (loc2BlockPos . getLocation $ ann)]
genPaths (ContinueBlock expr ann) =
    case expr of
        (MemberFunctionCall _obj ident _args _) ->
            [WCEPContinue ident (loc2BlockPos . getLocation $ ann)]
        (DerefMemberFunctionCall _obj ident _args _) ->
            [WCEPContinue ident (loc2BlockPos . getLocation $ ann)]
        _ -> error "Unexpected expression in Continue block when generating WCE paths"
genPaths (RebootBlock ann) =
    [WCEPReboot (loc2BlockPos . getLocation $ ann)]
genPaths (SystemCall _obj syscallId _args ann) =
    [WCEPSystemCall syscallId (loc2BlockPos . getLocation $ ann)]


genWCEPaths :: [[WCEPathBlock]] -> [BasicBlock SemanticAnn] -> [[WCEPathBlock]]
genWCEPaths paths [] = paths
genWCEPaths paths (blk : xs) =
    let newPaths = genPaths blk
        appendedPaths = if null paths then [[p] | p <- newPaths] else
            concatMap (\prevPath -> map (`mergePath` prevPath) newPaths) paths
    in
    genWCEPaths appendedPaths xs

genClassMemberWCEPs :: Identifier -> ClassMember SemanticAnn -> [TransactionalWCEPath]
genClassMemberWCEPs className (ClassMethod _ak ident params _mrty blk _) =
    let wcePaths = genWCEPaths [] (blockBody blk)
        constParams = [name | Parameter name (TConstSubtype _) <- params] in
    zipWith (flip (TransactionalWCEPath className ident) constParams) ["path" ++ show idx | idx <- [(0 :: Integer) ..]] (reverse <$> wcePaths)
genClassMemberWCEPs className (ClassProcedure _ak ident params blk _) =
    let wcePaths = genWCEPaths [] (blockBody blk)
        constParams = [name | Parameter name (TConstSubtype _) <- params] in
    zipWith (flip (TransactionalWCEPath className ident) constParams) ["path" ++ show idx | idx <- [(0 :: Integer) ..]] (reverse <$> wcePaths)
genClassMemberWCEPs className (ClassAction _ak ident _param _mrty blk _) =
    let wcePaths = genWCEPaths [] (blockBody blk) in
    zipWith (flip (TransactionalWCEPath className ident) []) ["path" ++ show idx | idx <- [(0 :: Integer) ..]] (reverse <$> wcePaths)
genClassMemberWCEPs className (ClassViewer ident _params _mrty blk _) =
    let wcePaths = genWCEPaths [] (blockBody blk)
        constParams = [name | Parameter name (TConstSubtype _) <- _params] in
    zipWith (flip (TransactionalWCEPath className ident) constParams) ["path" ++ show idx | idx <- [(0 :: Integer) ..]] (reverse <$> wcePaths)
genClassMemberWCEPs _ _ = []

genTransactionalWCEPS :: AnnotatedProgram SemanticAnn -> [TransactionalWCEPath]
genTransactionalWCEPS (TypeDefinition (Class _kind ident members _ _) _ : xs) =
    let classWCEPs = concatMap (genClassMemberWCEPs ident) members
    in
        classWCEPs ++ genTransactionalWCEPS xs
genTransactionalWCEPS (_ : xs) = genTransactionalWCEPS xs
genTransactionalWCEPS [] = []