module EFP.Schedulability.WCEPath.Generator where

import ControlFlow.BasicBlocks.AST
import qualified Semantic.Types as STYPES
import EFP.Schedulability.WCEPath.AST
import Utils.Annotations
import Text.Parsec.Pos
import EFP.Schedulability.Core.Types


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

mergePath :: WCEPathBlock GeneratorAnn -> [WCEPathBlock GeneratorAnn] -> [WCEPathBlock GeneratorAnn]
mergePath newPath [] = [newPath]
mergePath (WCEPRegularBlock newLoc _) (WCEPRegularBlock pathLoc _ : rest) =
    let mergedLoc = mergeLocations pathLoc newLoc
    in (WCEPRegularBlock mergedLoc Generated : rest)
mergePath newPath paths' = newPath : paths'

genConstExpression :: Expression STYPES.SemanticAnn -> ConstExpression GeneratorAnn
genConstExpression (AccessObject (Variable ident _)) =
    ConstObject ident Generated
genConstExpression (Constant (I tInt _) _) =
    ConstInt tInt Generated
genConstExpression (BinOp op left right _) =
    ConstBinOp op (genConstExpression left) (genConstExpression right) Generated
genConstExpression _ = error "Unsupported constant expression in ConstExpression generation"

genExpressionPath :: Expression STYPES.SemanticAnn -> [WCEPathBlock GeneratorAnn] -> [WCEPathBlock GeneratorAnn]
genExpressionPath (MemberFunctionCall _obj ident args ann) acc =
    let pts = case ann of
            STYPES.SemanticAnn (STYPES.ETy (STYPES.AppType pts' _)) _ -> pts'
            _ -> error "Unexpected annotation in ProcedureInvoke block when generating WCE paths"
        constArgs = genConstExpression . fst <$> filter (\case (_, Parameter _ (TConstSubtype _)) -> True; _ -> False) (zip args pts)
    in
        WCEPathMemberFunctionCall ident constArgs (loc2BlockPos . getLocation $ ann) Generated : acc
genExpressionPath (DerefMemberFunctionCall _obj ident args ann) acc =
    let pts = case ann of
            STYPES.SemanticAnn (STYPES.ETy (STYPES.AppType pts' _)) _ -> pts'
            _ -> error "Unexpected annotation in ProcedureInvoke block when generating WCE paths"
        constArgs = genConstExpression .fst <$> filter (\case (_, Parameter _ (TConstSubtype _)) -> True; _ -> False) (zip args pts)
    in
        WCEPathMemberFunctionCall ident constArgs (loc2BlockPos . getLocation $ ann) Generated : acc
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

        genFAExpressionPath :: FieldAssignment STYPES.SemanticAnn -> [WCEPathBlock GeneratorAnn] -> [WCEPathBlock GeneratorAnn]
        genFAExpressionPath (FieldValueAssignment _ expr _) acc' = genExpressionPath expr acc'
        genFAExpressionPath (FieldAddressAssignment _ _ ann') acc' = mergePath (WCEPRegularBlock (loc2BlockPos . getLocation $ ann') Generated) acc'
        genFAExpressionPath (FieldPortConnection _ _ _ ann') acc' = mergePath (WCEPRegularBlock (loc2BlockPos . getLocation $ ann') Generated) acc'

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
        _ -> mergePath (WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated) acc
genExpressionPath expr acc =
    let loc = loc2BlockPos . getLocation . getAnnotation $ expr
    in
        mergePath (WCEPRegularBlock loc Generated) acc

genRegularBlockPath :: [WCEPathBlock GeneratorAnn] -> [Statement STYPES.SemanticAnn] -> [WCEPathBlock GeneratorAnn]
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

uniqueRegularBlocks :: [WCEPathBlock GeneratorAnn] -> [WCEPathBlock GeneratorAnn]
uniqueRegularBlocks [] = []
uniqueRegularBlocks (WCEPRegularBlock loc ann : xs) =
    WCEPRegularBlock loc ann : filter (\case WCEPRegularBlock _ _ -> False; _ -> True) xs
uniqueRegularBlocks (x : xs) = x : uniqueRegularBlocks xs

genPaths :: BasicBlock STYPES.SemanticAnn -> [WCEPathBlock GeneratorAnn]
genPaths (RegularBlock stmts) =
    genRegularBlockPath [] stmts

genPaths (IfElseBlock ifBlk elifs mElse ann) =
    let ifPaths = 
            let paths = genWCEPaths [] (blockBody . condIfBody $ ifBlk) in
            map (\p -> 
                case p of
                    [WCEPRegularBlock {}] -> 
                        WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated
                    _ ->  WCEPathCondIf (reverse p) (loc2BlockPos . getLocation . condIfAnnotation $ ifBlk) Generated) paths
        elifPaths = concatMap (
            \(CondElseIf _ elifBlk ann') ->
                let paths = genWCEPaths [] (blockBody elifBlk) in
                map (\p -> 
                    case p of
                        [WCEPRegularBlock {}] -> 
                            WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated
                        _ -> WCEPathCondElseIf (reverse p) (loc2BlockPos . getLocation $ ann') Generated) paths
            ) elifs
        elsePaths = case mElse of
            Just elseBlk -> 
                let paths = genWCEPaths [] (blockBody . condElseBody $ elseBlk) in
                    map (\p -> 
                        case p of
                            [WCEPRegularBlock {}] -> 
                                WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated
                            _ ->
                                WCEPathCondElse (reverse p) (loc2BlockPos . getLocation . condElseAnnotation $ elseBlk) Generated) paths
            Nothing -> []
    in
        uniqueRegularBlocks (ifPaths ++ elifPaths ++ elsePaths)
    
genPaths (ForLoopBlock _ _ lower upper _ blk ann) =
    let paths = genWCEPaths [] (blockBody blk)
        loopPaths =
            map (\p -> case p of
                    [WCEPRegularBlock {}] -> WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated
                    _ -> WCEPathForLoop (genConstExpression lower) (genConstExpression upper) (reverse p) 
                            (loc2BlockPos . getLocation $ ann) Generated) paths
    in
        uniqueRegularBlocks loopPaths
    
genPaths (MatchBlock _ cases mDefaultCase ann) =
    let casePaths = map (
            \(MatchCase _ _ caseBlk ann') ->
                let paths = genWCEPaths [] (blockBody caseBlk) in
                map (\p -> case p of
                    [WCEPRegularBlock {}] -> WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated
                    _ ->
                        WCEPathMatchCase (reverse p) (loc2BlockPos . getLocation $ ann') Generated) paths
            ) cases
        defaultPaths = case mDefaultCase of
            Just (DefaultCase defBlk ann') ->
                let paths = genWCEPaths [] (blockBody defBlk) in
                [map (\p -> case p of
                    [WCEPRegularBlock {}] -> WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated
                    _ ->
                        WCEPathMatchCase (reverse p) (loc2BlockPos . getLocation $ ann') Generated) paths]
            Nothing -> []
    in
        uniqueRegularBlocks (concat casePaths ++ concat defaultPaths)

genPaths (SendMessage obj _ ann) =
    let outPt = case obj of
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in SendMessage block when generating WCE paths"
    in
        [WCEPSendMessage outPt (loc2BlockPos . getLocation $ ann) Generated]
genPaths (ProcedureInvoke obj procId args ann) =
    let outPt = case obj of
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in ProcedureInvoke block when generating WCE paths"
        pts = case ann of
            STYPES.SemanticAnn (STYPES.ETy (STYPES.AppType pts' _)) _ -> pts'
            _ -> error "Unexpected annotation in ProcedureInvoke block when generating WCE paths"
        constArgs = genConstExpression . fst <$> filter (\case (_, Parameter _ (TConstSubtype _)) -> True; _ -> False) (zip args pts)
    in
        [WCEPProcedureInvoke outPt procId constArgs (loc2BlockPos . getLocation $ ann) Generated]
genPaths (AtomicLoad _obj _expr ann) =
    [WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated]
genPaths (AtomicStore _obj _expr ann) =
    [WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated]
genPaths (AtomicArrayLoad _obj _index _expr ann) =
    [WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated]
genPaths (AtomicArrayStore _obj _index _expr ann) =
    [WCEPRegularBlock (loc2BlockPos . getLocation $ ann) Generated]
genPaths (AllocBox obj _args ann) =
    let outPt = case obj of
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in AllocBox block when generating WCE paths"
    in
        [WCEPAllocBox outPt (loc2BlockPos . getLocation $ ann) Generated]
genPaths (FreeBox obj _args ann) =
    let outPt = case obj of
            (MemberAccess _ portId _) -> portId
            (DereferenceMemberAccess _ portId _) -> portId
            _ -> error "Unexpected object in FreeBox block when generating WCE paths"
    in
        [WCEPFreeBox outPt (loc2BlockPos . getLocation $ ann) Generated]
genPaths (ReturnBlock _ ann) =
    [WCEPReturn (loc2BlockPos . getLocation $ ann) Generated]
genPaths (ContinueBlock expr ann) =
    case expr of
        (MemberFunctionCall _obj ident _args _) ->
            [WCEPContinue ident (loc2BlockPos . getLocation $ ann) Generated]
        (DerefMemberFunctionCall _obj ident _args _) ->
            [WCEPContinue ident (loc2BlockPos . getLocation $ ann) Generated]
        _ -> error "Unexpected expression in Continue block when generating WCE paths"
genPaths (RebootBlock ann) =
    [WCEPReboot (loc2BlockPos . getLocation $ ann) Generated]
genPaths (SystemCall _obj syscallId args ann) = do
    let pts = case ann of
            STYPES.SemanticAnn (STYPES.ETy (STYPES.AppType pts' _)) _ -> pts'
            _ -> error "Unexpected annotation in ProcedureInvoke block when generating WCE paths"
        constArgs = genConstExpression . fst <$> filter (\case (_, Parameter _ (TConstSubtype _)) -> True; _ -> False) (zip args pts)
    [WCEPSystemCall syscallId constArgs (loc2BlockPos . getLocation $ ann) Generated]


genWCEPaths :: [[WCEPathBlock GeneratorAnn]] -> [BasicBlock STYPES.SemanticAnn] -> [[WCEPathBlock GeneratorAnn]]
genWCEPaths paths [] = paths
genWCEPaths paths (blk : xs) =
    let newPaths = genPaths blk
        appendedPaths = if null paths then [[p] | p <- newPaths] else
            concatMap (\prevPath -> map (`mergePath` prevPath) newPaths) paths
    in
    genWCEPaths appendedPaths xs

genClassMemberWCEPs :: Identifier -> ClassMember STYPES.SemanticAnn -> [WCEPath GeneratorAnn]
genClassMemberWCEPs className (ClassMethod _ak ident params _mrty blk _) =
    let wcePaths = genWCEPaths [] (blockBody blk)
        constParams = [name | Parameter name (TConstSubtype _) <- params] in
    zipWith (\pathName path -> 
        WCEPath className ident pathName constParams path Generated) 
        ["path" ++ show idx | idx <- [(0 :: Integer) ..]] (reverse <$> wcePaths)
genClassMemberWCEPs className (ClassProcedure _ak ident params blk _) =
    let wcePaths = genWCEPaths [] (blockBody blk)
        constParams = [name | Parameter name (TConstSubtype _) <- params] in
    zipWith (\pathName path -> 
        WCEPath className ident pathName constParams path Generated) 
        ["path" ++ show idx | idx <- [(0 :: Integer) ..]] (reverse <$> wcePaths)
genClassMemberWCEPs className (ClassAction _ak ident _param _mrty blk _) =
    let wcePaths = genWCEPaths [] (blockBody blk) in
    zipWith (\pathName path -> 
        WCEPath className ident pathName [] path Generated) 
        ["path" ++ show idx | idx <- [(0 :: Integer) ..]] (reverse <$> wcePaths)
genClassMemberWCEPs className (ClassViewer ident _params _mrty blk _) =
    let wcePaths = genWCEPaths [] (blockBody blk)
        constParams = [name | Parameter name (TConstSubtype _) <- _params] in
    zipWith (\pathName path -> 
        WCEPath className ident pathName constParams path Generated) 
        ["path" ++ show idx | idx <- [(0 :: Integer) ..]] (reverse <$> wcePaths)
genClassMemberWCEPs _ _ = []

genTransactionalWCEPS :: AnnotatedProgram STYPES.SemanticAnn -> [WCEPath GeneratorAnn]
genTransactionalWCEPS (TypeDefinition (Class _kind ident members _ _) _ : xs) =
    let classWCEPs = concatMap (genClassMemberWCEPs ident) members
    in
        classWCEPs ++ genTransactionalWCEPS xs
genTransactionalWCEPS (_ : xs) = genTransactionalWCEPS xs
genTransactionalWCEPS [] = []