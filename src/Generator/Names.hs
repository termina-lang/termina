-- | Check of the names declared by the generated C against the reserved lists.
module Generator.Names (
    checkGeneratedNames
) where

import Configuration.Platform (Platform)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Control.Applicative ((<|>))

import Generator.LanguageC.AST (CFile)
import Generator.LanguageC.Names
import Generator.Names.Errors
import Semantic.Reserved (reservedName, linkedName)
import Utils.Annotations

-- | First name of a file reserved by C or the platform, termina__ names excepted.
checkGeneratedNames :: Platform -> CFile -> Maybe CNameError
checkGeneratedNames plt = listToMaybe . mapMaybe clash . declaredNames

  where

    clash (DeclaredName ident scope loc)
      | "termina__" `isPrefixOf` ident = Nothing
      | otherwise =
          annotateError loc . EGeneratedNameReserved ident <$>
            (reservedName plt ident <|> linked scope ident)

    -- | Only a file-scope name reaches the linker
    linked CGlobalName ident = linkedName plt ident
    linked CLocalName _ = Nothing
