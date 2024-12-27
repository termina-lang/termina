module LSP.Modules where

import Modules.Modules (ModuleDependency)
import qualified Data.Text as T
import Command.Types (SemanticData, ParsingData)
import qualified Language.LSP.Protocol.Types as LSP

-- | Data type used to represented a loaded module
-- | It contains the module's name, the list of imported modules, the source code
-- | and the module's metadata.
data TerminaStoredModule = TerminaStoredModule {

  -- This is the full path where the module's source file is located
  fullPath :: !FilePath,
  -- | List of imported modules
  importedModules :: [ModuleDependency],
  -- | Source code
  sourcecode :: T.Text,

  -- | Module's error diagnostics
  diagnostics :: [LSP.Diagnostic],
  -- | Module's parsed AST
  parsing :: Maybe ParsingData,
  -- | Module's semantic AST
  semantic :: Maybe SemanticData

} 