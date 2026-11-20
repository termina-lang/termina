{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Errors of the documentation comments that need the semantic information,
-- the family DE.

module Doc.Errors where

import Core.AST (Identifier)

import Utils.Annotations
import Utils.Errors
import qualified Data.Text as T

data Error
  = EUnknownDocParam Identifier Identifier [Identifier] -- ^ A documented parameter that is not in the signature: the element, the parameter and the parameters of the signature (DE-001)
  deriving Show

type DocCheckError = AnnotatedError Error Location

instance Diagnosable Error where

    describe (EUnknownDocParam owner param params) =
        diagnostic "DE-001" "documented parameter not in the signature"
            (emph (T.pack owner) <> " has no parameter " <> emph (T.pack param) <> ". "
                <> parameters)

        where

            parameters = case params of
                [] -> "It has no parameters."
                [one] -> "Its parameter is " <> emph (T.pack one) <> "."
                several -> "Its parameters are "
                    <> T.intercalate ", " (map (emph . T.pack) (init several))
                    <> " and " <> emph (T.pack (last several)) <> "."

instance ErrorMessage DocCheckError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
