{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Errors of the deque check, which makes sure that a box is only put into a
-- deque that has room for it and that every deque is both filled and taken.

module ControlFlow.DequeUsage.Errors where

import Core.AST (Identifier)

import Utils.Annotations
import Utils.Errors
import qualified Data.Text as T

-- | Why a deque may hold a box at some point.
data FullBecause
  = -- | The deque keeps its boxes between activations, so it may be full when
    -- the member starts.
    FullOnEntry Identifier
    -- | A box was put into the deque here.
  | FullAfterPut Location
    -- | A call to a method that may leave a box in the deque, with the put
    -- inside the method.
  | FullAfterCall Identifier Location Location
  deriving (Show, Eq, Ord)

data Error
  = EDequeMayBeFull Identifier [FullBecause] -- ^ Box put into a deque that may be full (DQE-001)
  | EDequeMayBeFullInCall Identifier Identifier Location [FullBecause] -- ^ Call to a method that puts into a deque that may be full (DQE-002)
  | EDequeNeverTaken Identifier -- ^ Deque filled and never taken (DQE-003)
  | EDequeNeverFilled Identifier -- ^ Deque never filled (DQE-004)
  deriving Show

type DequeUsageError = AnnotatedError Error Location

-- | The positions that explain why a deque may be full, leaving out the ones
-- the message already points at.
withReasons :: [Location] -> [FullBecause] -> Diagnostic -> Diagnostic
withReasons shown reasons d = foldr addReason d reasons

  where

    unlessShown loc f = if loc `elem` shown then id else f

    addReason (FullOnEntry _) = id
    addReason (FullAfterPut loc) = unlessShown loc (relatedTo loc "a box is put into the deque here")
    addReason (FullAfterCall m callLoc putLoc) =
      unlessShown putLoc (relatedTo putLoc ("the method " <> T.pack m <> " puts a box into the deque here"))
        . relatedTo callLoc ("and it is called here")

entrySentence :: [FullBecause] -> T.Text
entrySentence reasons =
  case [member | FullOnEntry member <- reasons] of
    (member : _) ->
      "\nThe deque keeps its boxes between activations, so it may be full when "
        <> emph (T.pack member) <> " starts. Take a box out before putting a new one, or use try_put or replace."
    [] -> ""

instance Diagnosable Error where

    describe (EDequeMayBeFull deque reasons) =
        withReasons [] reasons $
            diagnostic "DQE-001" "box put into a deque that may be full"
                ("The deque " <> emph (T.pack deque) <> " may have no room for another box here."
                    <> entrySentence reasons)
    describe (EDequeMayBeFullInCall deque method putLoc reasons) =
        relatedTo putLoc ("the method " <> T.pack method <> " puts a box into the deque here") $
        withReasons [putLoc] reasons $
            diagnostic "DQE-002" "call to a method that puts into a deque that may be full"
                ("The method " <> emph (T.pack method) <> " puts a box into the deque " <> emph (T.pack deque)
                    <> ", which may be full when the method is called here."
                    <> entrySentence reasons)
    describe (EDequeNeverTaken deque) =
        diagnostic "DQE-003" "deque never taken"
            ("The deque " <> emph (T.pack deque) <> " is filled but no box is ever taken from it, so its boxes would stay in it forever.")
    describe (EDequeNeverFilled deque) =
        unnecessary $ diagnostic "DQE-004" "deque never filled"
            ("No box is ever put into the deque " <> emph (T.pack deque) <> ", so it is always empty.")

instance ErrorMessage DequeUsageError where

    errorIdent = diagCode . describe . getError
    errorTitle = diagTitle . describe . getError
    toText = errorToText
    toDiagnostics = errorToDiagnostics
