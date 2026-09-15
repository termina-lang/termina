{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ControlFlow.ConstPropagation.Errors where

import Semantic.AST
import Semantic.Types
import Utils.Annotations
import Utils.Errors
import Utils.Printer

data Error =
    EInvariantCondition (Const SemanticAnn) -- ^ Condition with the same value on every evaluation (CPE-001)
  deriving Show

type ConstPropError = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EInvariantCondition value) =
        diagnostic "CPE-001" "invariant control expression"
            ("This condition evaluates to " <> emph (showText value) <>
                " every time it is reached, so one of the paths it guards is never taken.")

instance ErrorMessage ConstPropError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
