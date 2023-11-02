-- | Simple POC of data flo analysis to compute flow of dynamic variables.

module DataFlow.DF where

{-
At termina level, each block is a basic block.
Maybe ask Pablo about this, but functions do not change values unless are
explicit about it using the |mut| operator.

In this module, we implement a backward analysis.
That is, given a block, i.e. a sequence of statements, we go to the last
statement and build our sets backwards.
-}

-- Monad and manipulations
import DataFlow.Computation
import DataFlow.Errors

import Control.Monad
import Control.Monad.Except

import AST.Core (Identifier)
-- AST to work with.
import AST.Seman as SAST
-- We need to know the type of objects.
import Semantic.Monad (SemanticAnns(..), getTypeSAnns)
import qualified Semantic.Monad as SM (location)


useObject :: Object SemanticAnns -> UDM AnnotatedErrors ()
useObject (Variable ident ann)
  =
  let loc = SM.location ann in
  maybe
        (loc `annotateError` throwError ImpossibleError)
        (\case {
            -- We can use dynamic only once!
                DynamicSubtype _ ->
                loc `annotateError` addUseOnlyOnce ident
                ;
            -- We can use Options only once!
                Option _ ->
                loc  `annotateError` addUseOnlyOnce ident
                ;
                _ ->
                loc `annotateError` safeAddUse ident
        }) (getTypeSAnns ann)
useObject (VectorIndexExpression obj e _ann)
  = useObject obj >> useExpression e
useObject (MemberAccess obj _i _ann)
  = useObject obj
useObject (Dereference obj _ann)
  = useObject obj
useObject (DereferenceMemberAccess obj _i _ann)
  = useObject obj
useObject (VectorSliceExpression obj eB eT _ann)
  = useObject obj >> useExpression eB >> useExpression eT
-- TODO Use Object undyn?
useObject (Undyn obj _ann)
  = useObject obj

useExpression :: Expression SemanticAnns -> UDM AnnotatedErrors ()
useExpression (AccessObject obj)
  = useObject obj
useExpression (Constant _c _a)
  = return ()
useExpression (BinOp _o el er _ann)
  = useExpression el >> useExpression er
useExpression (ReferenceExpression _aK obj _a)
  = useObject obj
useExpression (Casting obj _ty _a)
  = useObject obj
useExpression (MemberFunctionAccess obj _ident args _ann)
  = useObject obj >> sequence_ useExpression args
useExpression (DerefMemberFunctionAccess obj _ident args _ann)
  = useObject obj >> sequence_ useExpression args
useExpression (VectorInitExpression e _size _ann)
  = useExpression e
useExpression (FieldAssignmentsExpression _ident fs _ann)
  = sequence_ _useFieldExpression fs
useExpression (EnumVariantExpression _ident _ident2 es _ann)
  = sequence_ useExpression es
useExpression (OptionVariantExpression ov _ann)
  = _useOptionVariant ov


useDefBlockRet :: BlockRet SemanticAnns -> UDM AnnotatedErrors ()
useDefBlockRet = _

useDefStmt :: Statement SemanticAnns -> UDM AnnotatedErrors ()
useDefStmt (Declaration ident accK tyS initE _ann)
  = _
-- All branches should have the same used Only ones.
useDefStmt (IfElseStmt eCond bTrue elseIfs bFalse ann)
  = do
  -- All sets generated for all different branches.
  sets <- runEncaps
                ([ mapM_ useDefStmt (reverse bTrue) >> get
                , mapM_ useDefStmt (reverse bFalse) >> get
                ]
                ++ map ((\l -> mapM_ useDefStmt l >> get) . reverse . elseIfBody) elseIfs)
  -- Rule here is, all branches should have the same onlyonce behaviour.
  let usedOO = map usedOnlyOnce sets
  unless (sameSets usedOO)
    (ann `annotateError` throwError DifferentOnlyOnce)
  -- We get all uses
  let normalUses = unions (map usedSet sets)
  -- We get all defined but not defined variables
  let notUsed = unions (map defV sets)
  continueWith (fst usedOO) normalUses notUsed

-- Same sets
sameSets :: [VarSet] -> Bool
sameSets [] = False -- shouldn't happen but :shrug:
sameSets (x:xs) = all ( null . difference x) xs
