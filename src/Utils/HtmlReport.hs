{-# LANGUAGE TemplateHaskell #-}

-- | The frame shared by the reports that termina writes as web pages: the
-- header with the Termina mark, the stylesheet, the provenance line and the
-- explanations on hover.
module Utils.HtmlReport (
    Provenance(..), terminaMark, styleSheet, script, markFile, styleFile, scriptFile, page, provenanceLine, help
) where

import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.FileEmbed (embedFile)
import Utils.Markup (escape)

-- | Where a report comes from: the project with its version, the version of termina and when it was written
data Provenance = Provenance {
    provenanceProject :: String,
    provenanceTermina :: String,
    provenanceWritten :: String
  }

-- | The Termina mark, as it is in the repository of the logos
terminaMark :: BS.ByteString
terminaMark = $(embedFile "assets/report/termina-mark.svg")

-- | The style of the pages
styleSheet :: BS.ByteString
styleSheet = $(embedFile "assets/report/termina.css")

-- | The script that sorts the tables and folds the groups of rows
script :: BS.ByteString
script = $(embedFile "assets/report/termina.js")

-- | The file of the Termina mark, in the folder of the landing page
markFile :: FilePath
markFile = "termina-mark.svg"

-- | The file of the script, in the folder of the landing page
scriptFile :: FilePath
scriptFile = "termina.js"

-- | The file of the style of the pages of a report, in the folder of its landing page
styleFile :: String -> FilePath
styleFile report = report ++ ".css"

-- | The line under the title of a landing page
provenanceLine :: Provenance -> String
provenanceLine provenance =
    "<p class=\"meta\">" ++ escape (provenanceProject provenance) ++ " · termina " ++ escape (provenanceTermina provenance)
    ++ " · written " ++ escape (provenanceWritten provenance) ++ "</p>"

-- | A text with its explanation on hover
help :: String -> String -> String
help tip text = "<span class=\"help\" data-tip=\"" ++ escape tip ++ "\">" ++ text ++ "</span>"

-- | A page of a report with the header of Termina, given the name of the report, its stylesheets,
-- its scripts, its title, its path to the landing page and its place under it
page :: String -> [FilePath] -> [FilePath] -> String -> FilePath -> [(String, Maybe FilePath)] -> [String] -> String
page report styles scripts title toLanding crumbs body = unlines $
    [ "<!doctype html>"
    , "<html lang=\"en\"><head><meta charset=\"utf-8\">"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    , "<title>" ++ escape title ++ "</title>" ]
    ++ [ "<link rel=\"stylesheet\" href=\"" ++ toLanding ++ style ++ "\">" | style <- styles ]
    ++ [ "<script src=\"" ++ toLanding ++ file ++ "\" defer></script>" | file <- scripts ]
    ++ [ "</head><body>"
    , "<header><img src=\"" ++ toLanding ++ markFile ++ "\" alt=\"\"><span class=\"brand\"><span class=\"wordmark\">Termina</span> " ++ report ++ "</span>"
      ++ "<nav><a href=\"" ++ toLanding ++ "index.html\">" ++ capitalized ++ "</a>"
      ++ concat [ "<span>›</span>" ++ maybe (escape name) (\href -> "<a href=\"" ++ href ++ "\">" ++ escape name ++ "</a>") link
                | (name, link) <- crumbs ]
      ++ "</nav></header>"
    , "<main>" ]
    ++ body
    ++ [ "</main></body></html>" ]

  where

    capitalized = case report of
        c : rest -> toUpper c : rest
        [] -> []
