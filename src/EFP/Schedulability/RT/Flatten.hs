module EFP.Schedulability.RT.Flatten (runFlattenTransaction) where

import EFP.Schedulability.RT.AST
import EFP.Schedulability.RT.Monad
import Control.Monad
import EFP.Schedulability.RT.Types
import Utils.Annotations
import Control.Monad.Except
import EFP.Schedulability.RT.Errors
import EFP.Schedulability.RT.Utils

-- | Flattens nested conditional and multicast structures within an RT transaction.
--
-- This function transforms RT transaction elements by eliminating nested conditional
-- steps. The flattening process ensures that all conditional branches are at a 
-- single level, making it easier to analyze execution paths and their associated 
-- probabilities.
--
-- = Transformation Rules
--
-- The function applies the following transformations recursively:
--
-- 1. __Nested Conditionals__: When a conditional step contains another conditional
--    as its next step, the nested structure is flattened by:
--    - Creating new branches for each combination of outer and inner conditions
--    - Computing combined probabilities: @(outer_prob * inner_prob) / 100@
--    - Validating that resulting probabilities remain in the range [1, 100]
--
-- 2. __Actions Before Conditionals__: When an action step is followed by a
--    conditional, the action is distributed across all conditional branches,
--    creating separate action-then-branch paths for each condition.
--
-- 3. __Multicast with Conditionals__: When a multicast contains steps with
--    conditionals, it transforms into a conditional where each branch contains
--    a multicast of the corresponding steps from each conditional branch.
--    The probabilities are computed by multiplying all branch probabilities
--    and dividing by 100 for each intermediate multiplication.
--
-- = Probability Calculation
--
-- Conditional expressions represent probabilities as integers in the range [1, 100]
-- (representing percentages). When combining probabilities from nested conditionals,
-- the function:
-- - Multiplies the outer and inner probability expressions
-- - Divides by 100 to normalize back to percentage form
-- - Validates the result is still within [1, 100]
--
-- = Error Handling
--
-- The function may fail with:
-- - 'EFlatConditionalExpressionOutOfRange': When a computed probability falls
--   outside the valid [1, 100] range
-- - 'EInvalidConditionalExpressionType': When a conditional expression is not
--   a constant integer
-- - 'EInvalidFlatteningTarget': When applied to a non-transaction RT element
--
-- = Example
--
-- @
-- Transaction with nested conditionals:
--   Action A -> Conditional {
--     50%: Action B -> Conditional {
--       60%: Action C
--       40%: Action D
--     }
--     50%: Action E
--   }
--
-- After flattening:
--   Conditional {
--     30%: Action A -> Action B -> Action C  -- (50 * 60) / 100
--     20%: Action A -> Action B -> Action D  -- (50 * 40) / 100
--     50%: Action A -> Action E
--   }
-- @
--
flattenTransaction :: RTElement RTSemAnn -> RTFlatMonad (RTElement RTSemAnn)
flattenTransaction (RTTransaction ident step ann) = flip (RTTransaction ident) ann <$> flattenStep step

    where

    -- | Validates that a conditional probability expression is within the valid range.
    --
    -- This function ensures that probability values computed during the flattening
    -- process remain valid. Conditional expressions must be constant integers in the
    -- range [1, 100], representing percentages.
    --
    -- ==== Parameters
    --
    -- [@loc@] The source location of the outer conditional expression, used for
    --         error reporting
    -- [@innerLoc@] The source location of the inner conditional expression being
    --              validated, used for error reporting
    -- [@expr@] The constant expression to validate, expected to be a constant integer
    --
    -- ==== Error Conditions
    --
    -- - Throws 'EFlatConditionalExpressionOutOfRange' if the expression evaluates
    --   to a value outside the range [1, 100]
    -- - Throws 'EInvalidConditionalExpressionType' if the expression is not a
    --   constant integer
    --
    -- ==== Implementation Notes
    --
    -- The validation is strict: values must be strictly greater than 0 and less
    -- than or equal to 100. This ensures that all branches have non-zero probability
    -- and that probabilities don't exceed 100%.
    checkConditionalExprRange :: Location -> Location -> ConstExpression RTSemAnn -> RTFlatMonad ()
    checkConditionalExprRange loc innerLoc expr = do
        case expr of
            ConstInt (TInteger v _) _ ->
                unless (v > 0 && v <= 100) $
                    throwError . annotateError loc $ EFlatConditionalExpressionOutOfRange v innerLoc
            _ -> throwError . annotateError Internal $ EInvalidConditionalExpressionType
    
    -- | Recursively flattens a single transaction step and all its nested steps.
    --
    -- This is the core recursive function that implements the flattening algorithm.
    -- It handles four types of transaction steps, each with specific transformation
    -- rules to eliminate nesting while preserving the transaction's semantics.
    --
    -- = Step Types and Transformations
    --
    -- 1. __Terminal Action Steps__ (@RTTransStepAction@ with no next step):
    --    - Returns the step unchanged as there's nothing to flatten
    --
    -- 2. __Action Steps with Continuation__ (@RTTransStepAction@ with next step):
    --    - Recursively flattens the next step
    --    - If the flattened next step is a conditional, distributes the action
    --      across all conditional branches
    --    - Creates a new step for each branch: Action → Branch
    --    - Otherwise, chains the action with the flattened next step
    --
    -- 3. __Conditional Steps__ (@RTTransStepConditional@):
    --    - Recursively flattens each branch of the conditional
    --    - If a flattened branch itself becomes a conditional (nested conditional),
    --      combines probabilities:
    --      * For each inner branch, computes: @(outer_prob * inner_prob) / 100@
    --      * Validates the result is in range [1, 100]
    --      * Creates flattened branches for all combinations
    --    - Returns a single-level conditional with all flattened branches
    --
    -- 4. __Multicast Steps__ (@RTTransStepMuticast@):
    --    - Recursively flattens each parallel step in the multicast
    --    - If any flattened step contains a conditional, transforms the structure:
    --      * Non-conditional steps are treated as 100% probability branches
    --      * Uses 'sequence' to generate all combinations of conditional branches
    --      * For each combination, computes combined probability by multiplying
    --        all branch probabilities and dividing by 100 iteratively
    --      * Creates a conditional where each branch is a multicast of the
    --        corresponding steps from each combination
    --    - If no conditionals exist, returns the multicast with flattened steps
    --
    -- = Probability Computation Details
    --
    -- When combining probabilities from multiple conditionals, the function uses
    -- iterative multiplication and division to maintain precision:
    --
    -- @
    -- result = (((100 * prob1) / 100 * prob2) / 100 * prob3) / 100 ...
    -- @
    --
    -- Starting with 100 and folding over the probabilities ensures proper
    -- normalization at each step.
    --
    -- = Example Transformations
    --
    -- __Nested Conditionals:__
    --
    -- @
    -- Input:  Conditional { 60%: Conditional { 50%: A, 50%: B }, 40%: C }
    -- Output: Conditional { 30%: A, 30%: B, 40%: C }
    -- @
    --
    -- __Action Before Conditional:__
    --
    -- @
    -- Input:  Action X -> Conditional { 70%: A, 30%: B }
    -- Output: Conditional { 70%: Action X -> A, 30%: Action X -> B }
    -- @
    --
    -- __Multicast with Conditionals:__
    --
    -- @
    -- Input:  Multicast [Conditional { 60%: A, 40%: B }, Action C]
    -- Output: Conditional { 60%: Multicast [A, C], 40%: Multicast [B, C] }
    -- @
    flattenStep :: RTTransStep RTSemAnn -> RTFlatMonad (RTTransStep RTSemAnn)
    flattenStep st@(RTTransStepAction _name _task _action _path Nothing _ann) = return st
    flattenStep (RTTransStepAction name task action path (Just next) ann') = do
        flatNext <- flattenStep next
        case flatNext of
            RTTransStepConditional conds _ -> 
                flip RTTransStepConditional ann' <$> mapM (\(condExpr, condStep) ->
                        return (condExpr, RTTransStepAction name task action path (Just condStep) ann')) conds
            _ -> return $ RTTransStepAction name task action path (Just flatNext) ann
    -- | If the next step is a conditional, we need to flatten each branch of the conditional
    flattenStep (RTTransStepConditional conds ann') = do
        flatConds <- forM conds $ \(condExpr, condStep) -> do
            let loc = getLocation . getAnnotation $ condExpr
            flatCondStep <- flattenStep condStep
            -- | In the original conditional, the conditional step may never be
            -- another conditional.  However, after flattening, it may be. In
            -- that case, we need to adjust the conditional expressions
            -- accordingly.
            case flatCondStep of
                RTTransStepConditional innerConds _ ->
                    -- | For each inner conditional, we need to create a new
                    -- conditional expression that is the product of the outer
                    -- and inner conditional expressions, divided by 100.
                    forM innerConds $ \(innerCondExpr, innerCondStep) -> do
                        let innerLoc = getLocation . getAnnotation $ innerCondExpr
                        newCondExpr <- evalConstExpression 
                            (ConstBinOp Division 
                                (ConstBinOp Multiplication condExpr innerCondExpr ann') 
                                (ConstInt (TInteger 100 DecRepr) ann') ann')
                        checkConditionalExprRange loc innerLoc newCondExpr
                        return (newCondExpr, innerCondStep)
                _ -> return [(condExpr, flatCondStep)]
        return $ RTTransStepConditional (concat flatConds) ann
    flattenStep (RTTransStepMuticast steps ann') = do
        -- | Flatten each step in the multicast
        flatSteps <- forM steps flattenStep
        -- | If any of the flattened steps is a conditional, we need to
        -- distribute the multicast over the conditional.
        let hasConditional = any isConditional flatSteps
        if hasConditional then do
            -- | Instead of a multicast of conditional steps, we need to create a
            -- conditional step where each branch is a multicast of the steps
            -- corresponding to that branch.
            let conditionalStepsList = map (\step' -> case step' of
                    RTTransStepConditional conds _ -> conds
                    RTTransStepMuticast {} -> error "Unexpected multicast inside multicast flattening"
                    _ -> [(ConstInt (TInteger 100 DecRepr) (getAnnotation step'), step)]) flatSteps
            -- | Now, for each combination of conditional branches, create a new multicast step
            -- We use sequence to get all combinations
            let flatConds = sequence conditionalStepsList
            newConds <- forM flatConds $ \fcnds -> do
                resultingCondExpr <- foldM (\acc (condExpr, _) -> 
                        -- | Multiply the conditional expressions and divide by 100
                        return $ ConstBinOp Division 
                                        (ConstBinOp Multiplication condExpr acc (getAnnotation condExpr)) 
                                        (ConstInt (TInteger 100 DecRepr) (RTExprTy TConstInt Internal)) 
                                        (getAnnotation condExpr)) 
                    (ConstInt (TInteger 100 DecRepr) (RTExprTy TConstInt Internal)) fcnds 
                checkConditionalExprRange (getLocation ann') (getLocation . getAnnotation $ resultingCondExpr) resultingCondExpr
                return (resultingCondExpr, RTTransStepMuticast (map snd fcnds) ann')
            return $ RTTransStepConditional newConds ann
        else
            return $ RTTransStepMuticast flatSteps ann
flattenTransaction _ = throwError . annotateError Internal $ EInvalidFlatteningTarget

runFlattenTransaction :: RTElement RTSemAnn -> Either RTErrors (RTElement RTSemAnn)
runFlattenTransaction transaction =
    runExcept (flattenTransaction transaction) 