{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module EFP.Schedulability.TransPath.Errors where
import Utils.Annotations
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Types as LSP
import Utils.Errors

--------------------------------------------------
-- Transactional Path Generator error handling
--------------------------------------------------

data Error
  = 
    EInvalidTransStepType -- ^ Invalid transactional step type (internal)
    | EUnknownAction -- ^ Unknown action referenced in transactional step
    deriving Show

type TRPGenErrors = AnnotatedError Error Location

instance ErrorMessage TRPGenErrors where

    errorIdent _ = "Internal"

    errorTitle (AnnotatedError _err _pos) = "internal error"

    toText (AnnotatedError e pos) _files = T.pack $ show pos ++ ": " ++ show e

    toDiagnostics e@(AnnotatedError _ pos) _files =
        [LSP.Diagnostic (loc2Range pos)
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing]
        
        where 
            text = "error [" <> errorIdent e <> "]: " <> errorTitle e <> "."