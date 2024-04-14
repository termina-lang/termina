-- | Module Printing Some Module stuff

module Modules.Printing where

import Modules.Errors
import Prettyprinter
import Semantic.Errors (ppError)

-- Error Printing

ppModError :: Errors -> Doc a
ppModError (ELiftTypeCheckError e) = ppError e
ppModError e = pretty "Other errors during module stuff" <+> viaShow e
