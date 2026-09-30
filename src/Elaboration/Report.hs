{-# LANGUAGE OverloadedStrings #-}

-- | The report of the run-time checks of a build, written to
-- @output/checks.json@: each obligation of each module, and whether a prover
-- discharged it, in which case the generated code does not check it, or the
-- generated code checks it while the program runs.
--
-- Modules are listed by name and the obligations of a module by position, so
-- two builds of the same sources write the same file.
module Elaboration.Report (
    checksReport
) where

import Elaboration (ElaborationReport)
import Elaboration.Obligations (CheckKind(..))
import Elaboration.Prover (Evidence(..))
import Modules.Utils (qualifiedToModuleName)
import Utils.Annotations (Location(..), QualifiedName)

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty', keyOrder)
import qualified Data.ByteString.Lazy as BL
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Parsec.Pos (SourcePos, sourceColumn, sourceLine)

-- | The JSON of the report. The first argument is the report of each module and
-- the second the source file of each module, relative to the project.
checksReport :: M.Map QualifiedName ElaborationReport -> M.Map QualifiedName FilePath -> BL.ByteString
checksReport reports files =
  encodePretty' config $ object
    [ "summary" .= object
        [ "obligations" .= length obligations
        , "discharged" .= length [ () | (_, Just _) <- obligations ]
        , "checked" .= length [ () | (_, Nothing) <- obligations ]
        ]
    , "modules" .= map moduleValue (M.toList reports)
    ]

  where

    obligations = concat (M.elems reports)

    config = defConfig
      { confCompare = keyOrder
          [ "summary", "module", "file", "obligations", "discharged", "checked"
          , "modules", "start", "end", "line", "column"
          , "property", "status", "prover", "reason" ]
      , confTrailingNewline = True
      }

    moduleValue :: (QualifiedName, ElaborationReport) -> Value
    moduleValue (name, report) = object
      [ "module" .= T.pack (qualifiedToModuleName name)
      , "file" .= fmap T.pack (M.lookup name files)
      , "obligations" .= map obligationValue (sortOn fst report)
      ]

    obligationValue :: ((Location, CheckKind), Maybe Evidence) -> Value
    obligationValue ((loc, kind), evidence) = object $
      positionFields loc
      ++ [ "property" .= propertyName kind ]
      ++ case evidence of
        Just (Evidence prover reason) ->
          [ "status" .= ("discharged" :: T.Text)
          , "prover" .= T.pack prover
          , "reason" .= T.pack reason ]
        Nothing -> [ "status" .= ("checked" :: T.Text) ]

    positionFields (Position _ start end) = [ "start" .= positionValue start, "end" .= positionValue end ]
    positionFields _ = []

    positionValue :: SourcePos -> Value
    positionValue pos = object [ "line" .= sourceLine pos, "column" .= sourceColumn pos ]

propertyName :: CheckKind -> T.Text
propertyName IndexInBounds = "index-in-bounds"
propertyName SliceInBounds = "slice-in-bounds"
propertyName ShiftBelowWidth = "shift-below-width"
propertyName NoOverflow = "no-overflow"
propertyName NonZeroDivisor = "non-zero-divisor"
