{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Module about Modules!

module Modules.Modules where

-- Containers
import qualified Data.Text as T
import Data.Time
import Utils.Annotations
import Utils.Graph

data ModuleDependency = ModuleDependency QualifiedName Location
  deriving Show

instance GraphKey QualifiedName ModuleDependency where
  graphKey (ModuleDependency qname _) = qname

-- | Data type used to represented a loaded module
-- | It contains the module's name, the list of imported modules, the source code
-- | and the module's metadata.
data TerminaModuleData a = TerminaModuleData {
  -- | Module's qualified name
  qualifiedName :: !QualifiedName,
  -- | File path to the module
  -- This is the full path where the module's source file is located
  fullPath :: !FilePath,
  -- | Module's last modification time
  modificationTime :: !UTCTime,
  -- | List of imported modules
  importedModules :: [ModuleDependency],
  -- | Visible modules
  visibleModules :: [QualifiedName],
  -- | Source code
  sourcecode :: T.Text,
  -- | Module meta-data (e.g. parsed AST)
  metadata :: a
} deriving (Show)