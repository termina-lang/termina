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
  -- | File path to the module
  -- This is the full path where the module's source file is located
  fullPath :: !FilePath,
  -- | List of imported modules
  importedModules :: ![QualifiedName],
  -- | Source code
  sourcecode :: TL.Text,
  -- | Module meta-data (e.g. parsed AST)
  metadata :: a
} deriving (Show)