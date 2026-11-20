{-# LANGUAGE TemplateHaskell #-}

-- | The files that the report of the coverage writes next to its pages.
module Runner.CoverageAssets (terminaMark, styleSheet) where

import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)

-- | The Termina mark, as it is in the repository of the logos
terminaMark :: BS.ByteString
terminaMark = $(embedFile "assets/coverage/termina-mark.svg")

-- | The style of the pages
styleSheet :: BS.ByteString
styleSheet = $(embedFile "assets/coverage/coverage.css")
