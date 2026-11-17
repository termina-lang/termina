-- | The text of the documents of XML and HTML that termina writes.
module Utils.Markup (
    escape
) where

-- | The text of an attribute or of an element of XML or HTML; the control characters that XML 1.0 does not admit are dropped
escape :: String -> String
escape = concatMap char

  where

    char c =
        case c of
            '&' -> "&amp;"
            '<' -> "&lt;"
            '>' -> "&gt;"
            '"' -> "&quot;"
            '\'' -> "&apos;"
            '\n' -> "&#10;"
            _ -> if c < ' ' && c /= '\t' then "" else [c]
