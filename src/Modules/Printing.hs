-- | Module Printing Some Module stuff

module Modules.Printing where

import Modules.Errors
import Semantic.Errors (ppError)
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M

-- Error Printing

ppModError :: M.Map FilePath TL.Text -> Errors -> IO ()
ppModError toLazyText (ELiftTypeCheckError e) = ppError toLazyText e
ppModError _ _ = putStrLn "Other errors during module stuff"
