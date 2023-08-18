{-# Language LambdaCase #-}
-- | Util functions related to CoreAST

module Utils.CoreAST where

import           CoreAST
import Data.List

findClassField :: Identifier -> [ ClassMember' expr lho a ] -> Maybe (TypeSpecifier, a)
findClassField i = fmap (\case {ClassMethod {} -> error "Impossible after find 1"; ClassField _ t a -> (t, a)}) . find (\case { ClassMethod {} -> False; ClassField ident _ _ -> ident == i;})

findClassMethod :: Identifier -> [ ClassMember' expr lho a ] -> Maybe ([Parameter], a)
findClassMethod i
  = fmap (\case {ClassField {} -> error "Impossible after find 2"; ClassMethod _ ps _ _ a -> (ps,a)}
         ) .
  find (\case{ClassField {} -> False; ClassMethod ident _ _ _ _ -> (ident == i)})
