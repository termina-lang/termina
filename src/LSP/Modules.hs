module LSP.Modules where

import qualified Data.Text as T
import Command.Types (SemanticData)
import LSP.Index (ModuleIndex)

-- | A module of the last check of the language server.
data TerminaStoredModule = TerminaStoredModule {

  -- | Source code
  sourcecode :: T.Text,
  -- | Module's semantic AST, when the module has been type checked
  semantic :: Maybe SemanticData,
  -- | Where the module defines each name and what each use of a name points at
  moduleIndex :: ModuleIndex

}
