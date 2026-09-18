{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Errors of the scope check, which looks for local objects declared in a
-- wider block than the one their uses fall in.

module ControlFlow.VarScope.Errors where

import Core.AST (Identifier)

import Utils.Annotations
import Utils.Errors
import qualified Data.Text as T

data Error
  = EScopeCanBeReduced Identifier Location -- ^ Every use of a local falls inside the given inner block (VSE-001)
  deriving Show

type VarScopeError = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EScopeCanBeReduced ident target) =
        relatedTo target ("the block where " <> T.pack ident <> " can be declared") $
            diagnostic "VSE-001" "variable scope can be reduced"
                ("Every use of variable " <> emph (T.pack ident) <>
                    " is inside an inner block.\n" <>
                    "Declare it inside that block.")

instance ErrorMessage VarScopeError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
