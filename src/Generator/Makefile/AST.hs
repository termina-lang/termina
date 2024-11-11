module Generator.Makefile.AST where

newtype Makefile = Makefile [MakeStatement]
  deriving (Show)

data MakeStatement = 
    MVariable FilePath [Fragment]
    | MRule FilePath [FilePath] [MakeCommand]
    | MInclude FilePath
  deriving (Show)

data MakeCommand = 
  MakeCommand 
    Bool -- ^ Hidden command
    [Fragment]
  deriving (Show)

data Fragment =
    MFragment String
    | MSubstitution String
  deriving (Show)