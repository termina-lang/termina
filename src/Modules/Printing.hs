-- | Module Printing Some Module stuff

module Modules.Printing where

import Modules.Errors
import Semantic.Errors (ppError)
import Errata

-- Error Printing

ppModError :: Errors -> IO ()
ppModError (ELiftTypeCheckError e src_lines) = ppError src_lines e
ppModError _ = putStrLn "Other errors during module stuff"
