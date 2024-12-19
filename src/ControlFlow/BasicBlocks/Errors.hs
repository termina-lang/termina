{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ControlFlow.BasicBlocks.Errors where
import Utils.Errors
import qualified Language.LSP.Protocol.Types as LSP
import qualified Data.Text as T

-- | This type represents the possible errors that can occur during the generation of basic blocks.
-- In its current form, the only possible error is an internal error, which is used to signal that
-- an unexpected situation has occurred.
newtype BBGeneratorError = InternalError String
    deriving (Show)

instance ErrorMessage BBGeneratorError where

    errorIdent (InternalError _msg) = "Internal"

    errorTitle (InternalError msg) = T.pack msg

    toText (InternalError msg) _files = T.pack $ show msg
    
    toDiagnostic (InternalError msg) _files =
        LSP.Diagnostic emptyRange
            (Just LSP.DiagnosticSeverity_Error)
            Nothing Nothing Nothing
            text (Just []) Nothing Nothing
        
        where 
            text = T.pack $ show msg
