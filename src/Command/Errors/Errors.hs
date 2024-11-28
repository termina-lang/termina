{-# LANGUAGE FlexibleInstances #-}
module Command.Errors.Errors where

import Modules.Modules
import Utils.Annotations

----------------------------------------
-- Type checker error handling
----------------------------------------
newtype Error
  = 
    EImportedFileNotFound QualifiedName -- ^ Imported file not found (CE-001)
  deriving Show

type CommandErrors = AnnotatedError Error Location

instance Annotated (AnnotatedError Error) where

  getAnnotation (AnnotatedError _err ann) = ann

  updateAnnotation (AnnotatedError err _) = AnnotatedError err