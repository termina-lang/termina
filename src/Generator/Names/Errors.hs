{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Errors of the check of the names that the generated C declares.

module Generator.Names.Errors where

import Core.AST (Identifier)
import Semantic.Reserved (ReservedBy(..), reservedByText)

import Utils.Annotations
import Utils.Errors
import qualified Data.Text as T

data Error
  = EGeneratedNameReserved Identifier ReservedBy -- ^ A name of the generated C that C or the platform keeps for itself (CE-001)
  deriving Show

type CNameError = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EGeneratedNameReserved ident reservedBy) =
        diagnostic "CE-001" "generated name taken by the platform"
            ("This element becomes " <> emph (T.pack ident) <> " in the generated C, which "
                <> reservedByText reservedBy <> ".\n"
                <> "Rename the element.")

instance ErrorMessage CNameError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
