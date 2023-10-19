{-# Language LambdaCase #-}
-- | Util functions related to AST.Core

module Utils.AST.Core where

import           AST.Core
import qualified Data.List as L

findClassField :: Identifier -> [ ClassMember' expr lho a ] -> Maybe (TypeSpecifier, a)
findClassField i
  = fmap
  (\case {ClassMethod {} -> error "Impossible after find"
         ; ClassProcedure {} -> error "Impossible after find"
         ; ClassViewer {} -> error "Impossible after find"
         ; ClassField (FieldDefinition _ t) a -> (t, a)})
  .
  L.find (\case { ClassMethod {} -> False
                ; ClassProcedure {} -> False
                ; ClassViewer {} -> False
                ; ClassField (FieldDefinition ident _) _ -> ident == i
                ;})

findClassProcedure :: Identifier -> [ ClassMember' expr lho a ] -> Maybe ([Parameter], a)
findClassProcedure i
  = fmap
  (\case {ClassField {} -> error "Impossible after find"
         ; ClassMethod {} -> error "Impossible after find"
         ; ClassViewer {} -> error "Impossible after find"
         ; ClassProcedure _ ps _ a -> (ps,a)}
         )
  .
  L.find (\case{ClassField {} -> False
               ; ClassMethod {} -> False
               ; ClassViewer {} -> False
               ; ClassProcedure ident _ _ _ -> (ident == i)})

findClassViewerOrMethod :: Identifier -> [ ClassMember' expr lho a ] -> Maybe ([Parameter], Maybe TypeSpecifier, a)
findClassViewerOrMethod i
  = fmap
  (\case {ClassField {} -> error "Impossible after find"
         ; ClassMethod _ mty _ a  -> ([], mty, a)
         ; ClassProcedure {} -> error "Impossible after find"
         ;  ClassViewer _ ps ty _ a -> (ps, Just ty, a)}
         )
  .
  L.find (\case{ClassField {} -> False
               ; ClassMethod {} -> False
               ; ClassProcedure {} -> False
               ; ClassViewer ident _ _ _ _ -> (ident == i)})
