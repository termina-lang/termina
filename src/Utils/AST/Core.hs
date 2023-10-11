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
         ; ClassField _ t a -> (t, a)})
  .
  L.find (\case { ClassMethod {} -> False
                ; ClassProcedure {} -> False
                ; ClassViewer {} -> False
                ; ClassField ident _ _ -> ident == i
                ;})

findClassProcedure :: Identifier -> [ ClassMember' expr lho a ] -> Maybe ([Parameter], a)
findClassProcedure i
  = fmap
  (\case {ClassField {} -> error "Impossible after find"
         ; ClassMethod {} -> error "Impossible after find"
         ; ClassViewer {} -> error "Impossible after find"
         ; ClassProcedure _ ps _ _ a -> (ps,a)}
         )
  .
  L.find (\case{ClassField {} -> False
               ; ClassMethod {} -> False
               ; ClassViewer {} -> False
               ; ClassProcedure ident _ _ _ _ -> (ident == i)})

findClassViewer :: Identifier -> [ ClassMember' expr lho a ] -> Maybe ([Parameter], TypeSpecifier, a)
findClassViewer i
  = fmap
  (\case {ClassField {} -> error "Impossible after find"
         ; ClassMethod {} -> error "Impossible after find"
         ; ClassProcedure {} -> error "Impossible after find"
         ;  ClassViewer _ ps ty _ a -> (ps, ty, a)}
         )
  .
  L.find (\case{ClassField {} -> False
               ; ClassMethod {} -> False
               ; ClassProcedure {} -> False
               ; ClassViewer ident _ _ _ _ -> (ident == i)})

findClassMethod :: Identifier -> [ ClassMember' expr lho a ] -> Maybe (Maybe TypeSpecifier, a)
findClassMethod i
  = fmap
  (\case {ClassField {} -> error "Impossible after find"
         ; ClassViewer {} -> error "Impossible after find"
         ; ClassProcedure {} -> error "Impossible after find"
         ; ClassMethod _ ty _ a -> (ty, a)}
         )
  .
  L.find (\case{ClassField {} -> False
               ; ClassViewer {} -> False
               ; ClassProcedure {} -> False
               ; ClassMethod ident _ _ _ -> (ident == i)})
