module Generator.Makefile.AST where

newtype Makefile = Makefile [MakeBlock]
  deriving (Show)

newtype MakeBlock = MakeBlock [MakeStatement]
  deriving (Show)

data MVariableType = 
    MSimple
    | MRecursive
    | MAppend
  deriving (Show)

data MakeStatement = 
    MVariable MVariableType FilePath [MFragment]
    | MRule FilePath [FilePath] [MakeCommand]
    | MInclude Bool FilePath
  deriving (Show)

data MakeCommand = 
  MakeCommand 
    Bool -- ^ Hidden command
    [MFragment]
  deriving (Show)

data MFragment =
    MFragment String
    | MFunction String [MFragment]
  deriving (Show)