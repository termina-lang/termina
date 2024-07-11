-- | Module about Modules!

module Modules.Modules where

-- Containers
import qualified Data.Text.Lazy as TL

type QualifiedName = FilePath

-- | Data type used to represented a loaded module
-- | It contains the module's name, the list of imported modules, the source code
-- | and the module's metadata.
data TerminaModuleData a = TerminaModuleData {
  -- | Module's qualified name
  qualifiedName :: !QualifiedName,
  -- | Root of the module
  -- This is the root path where the module is located
  rootPath :: !FilePath,
  -- | List of imported modules
  importedModules :: ![QualifiedName],
  -- | Source code
  sourcecode :: TL.Text,
  -- | Module meta-data (e.g. parsed AST)
  metadata :: a
} deriving (Show)