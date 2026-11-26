-- | The check of the names that the generated C declares.
--
-- The semantic analysis checks each identifier of the source, but many names
-- of the C are made by the transpiler from several of them (the procedure next
-- of the class CCounter is CCounter__next), from types (the monomorphized
-- types) or from nothing in the source (the glue of the application). This
-- check reads every name that a file of C declares and looks it up in the same
-- lists as the semantic analysis, before the file is written.
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

-- | The first name of a file that C or the platform keeps for itself. A name
-- with the prefix termina__ is in the name space of the transpiler and of the
-- OSAL, which the language keeps out of the reach of the source. A local name
-- hides a global symbol and never reaches the linker, so only a name with file
-- scope is looked up among the symbols that the platform links.
checkGeneratedNames :: Platform -> CFile -> Maybe CNameError
checkGeneratedNames plt = listToMaybe . mapMaybe clash . declaredNames

  where

    clash (DeclaredName ident scope loc)
      | "termina__" `isPrefixOf` ident = Nothing
      | otherwise =
          annotateError loc . EGeneratedNameReserved ident <$>
            (reservedName plt ident <|> linked scope ident)

    linked CGlobalName ident = linkedName plt ident
    linked CLocalName _ = Nothing
