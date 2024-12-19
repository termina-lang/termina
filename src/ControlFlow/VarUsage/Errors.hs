{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module With Errors

module ControlFlow.VarUsage.Errors where

import Core.AST (Identifier)

import Utils.Annotations
import ControlFlow.VarUsage.Types
import Utils.Errors
import qualified Data.Text as T
import Text.Parsec
import qualified Data.Map as M
import qualified Language.LSP.Protocol.Types as LSP

data Error
  = ESetMaxBound -- ^ The set has reached its maximum bound (Internal)  
  | EMapMaxBound -- ^ The map has reached its maximum bound (Internal)
  | EUnboxingObjectType -- ^ Error when trying to unbox an object type (Internal)
  | EUnboxingExpressionType -- ^ Error when trying to unbox an expression type (Internal)
  | EDefiningBox -- ^ Error when trying to declare variable of box type (Internal)
  | EBadAllocArg -- ^ Bad argument for alloc (Internal)
  | EBadFreeArg -- ^ Bad argument for free (Internal)
  | EBadSendArg -- ^ Bad argument for send (Internal)
  | EVarRedefinition -- ^ Variable redefinition (Internal)
  | EMalformedOptionBoxMatch -- ^ Malformed option-box match (Internal)
  | EBadOptionBoxAssignExpression -- ^ Bad expression for option-box assignment (Internal)
  | EUnboxingOptionMap -- ^ Error when trying to unbox an option map (Internal)
  | EUnboxingVariableMap -- ^ Error when trying to unbox a variable map (Internal)
  | EDefinedTwice -- ^ Variable defined twice (Internal)
  | EOptionBoxUsedInBadContext -- ^ TOption-box used in bad context (Internal)
  | EUsedIgnoredParameter Identifier -- ^ Using a variable that is ignored (VE-001)
  | ENotUsed Identifier -- ^ Variable is not used (VE-002)
  | EBoxNotMoved Identifier -- ^ Box variable is not moved (VE-003)
  | EBoxMovedTwice Identifier Location -- ^ Box variable is moved twice (VE-004)
  | EOptionBoxMovedTwice Identifier Location -- ^ TOption-box variable is moved twice (VE-005)
  | EDifferentOptionBoxUse Identifier MVars (MVars, Location) -- ^ TOption-box final state mismatch (VE-006)
  | EDifferentNewOptionBoxUse Identifier MVars -- ^ TOption-box used in conditional branch (VE-007)
  | EMissingOptionBox Identifier MVars -- ^ TOption-box unused in a branch but used previously (VE-008)
  | EMissingBoxMove Identifier Location -- ^ Box variable is not always moved (VE-009)
  | EBoxMoveConditionalBranch Identifier -- ^ Box variable moved in conditional branch (VE-010)
  | EAllocNotMoved Identifier -- ^ TOption-box allocated but not moved (VE-011)
  | EAllocTwice Identifier Location -- ^ TOption-box allocated twice (VE-012)
  | EMovedWithoutAlloc Identifier Location -- ^ TOption-box moved but not allocated (VE-013)
  deriving Show

type VarUsageError = AnnotatedError Error Location

instance ErrorMessage VarUsageError where

    errorIdent (AnnotatedError (EUsedIgnoredParameter _ident) _pos) = "VE-001"
    errorIdent (AnnotatedError (ENotUsed _ident) _pos) = "VE-002"
    errorIdent (AnnotatedError e _pos) = T.pack $ show e

    errorTitle (AnnotatedError (EUsedIgnoredParameter _ident) _pos) = "using a variable that is ignored"
    errorTitle _ = "Unknown"

    toText e@(AnnotatedError (EUsedIgnoredParameter ident) pos@(Position start _end)) files =
        let fileName = sourceName start
            sourceLines = files M.! fileName
            title = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
        in
            pprintSimpleError
                sourceLines title fileName pos
                (Just ("Parameter \x1b[31m" <> T.pack ident <>
                    "\x1b[0m is ignored and should not be used."))
    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e
    
    toDiagnostic e@(AnnotatedError (EUsedIgnoredParameter _ident) pos) _files =
        LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing
        
        where 
            text = "\x1b[31merror [" <> errorIdent e <> "]\x1b[0m: " <> errorTitle e <> "."
    toDiagnostic _ _files = 
        LSP.Diagnostic 
            emptyRange
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing
        where 
            text = T.pack "\x1b[31mUknown\x1b[0m."