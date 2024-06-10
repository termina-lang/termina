{-# LANGUAGE LambdaCase #-}
-- | Util functions related to AST.Core

module Utils.AST.Core where

import           AST.Core
import qualified Data.List  as L

-- TODO TEST
-- findWith (if eq i j then just . f' else Nothing) = fmap f . L.find (eq i)
findWith :: (a -> Maybe b) -> [a] -> Maybe b
findWith f = L.foldl' (\acc a -> maybe acc Just (f a)) Nothing

findClassField :: Identifier -> [ ClassMember' expr lho a ] -> Maybe (TypeSpecifier, a)
findClassField i
  =
  -- findWith (\case { ClassField (FieldDefinition ident t) a ->
  --                    if ident == i then Just (t,a) else Nothing
  --                   ; _ -> Nothing})
  fmap
  (\case { ClassField (FieldDefinition _ t) a -> (t, a);
           _ -> error "Impossible after find"})
  .
  L.find (\case {ClassField (FieldDefinition ident _) _ -> ident == i;
                 _ -> False;})

findClassProcedure :: Identifier -> [ ClassMember' expr lho a ] -> Maybe ([Parameter], a)
findClassProcedure i
  = fmap
  (\case {ClassProcedure _ ps _ a -> (ps,a)
         ; _ -> error "Impossible after find"})
  .
  L.find (\case{ ClassProcedure ident _ _ _ -> (ident == i)
               ; _ -> False})

findInterfaceProcedure :: Identifier -> [ InterfaceMember a ] -> Maybe ([Parameter], a)
findInterfaceProcedure i
  = fmap
  (\case {InterfaceProcedure _ ps a -> (ps, a)})
  .
  L.find (\case{InterfaceProcedure ident _ _ -> (ident == i)})

findClassViewerOrMethod :: Identifier -> [ ClassMember' expr lho a ] -> Maybe ([Parameter], Maybe TypeSpecifier, a)
findClassViewerOrMethod i
  = fmap
  (\case {
    ClassViewer _ ps ty _ a -> (ps, Just ty, a);
    ClassMethod _ ty _ a -> ([], ty, a);
    _ -> error "Impossible after find"
  })
  .
  L.find (
    \case{
      ClassViewer ident _ _ _ _ -> (ident == i);
      ClassMethod ident _ _ _ -> (ident == i);
      _ -> False
    }
  )




className :: ClassMember' expr obj a -> Identifier
className (ClassField e _)                = fieldIdentifier e
className (ClassMethod mIdent _ _ _)      = mIdent
className (ClassProcedure pIdent _ _ _) = pIdent
className (ClassViewer vIdent _ _ _ _)  = vIdent
className (ClassAction aIdent _ _ _ _)    = aIdent
----------------------------------------

glbName :: Global' expr a -> Identifier
glbName (Task tId _ _ _ _)     = tId
glbName (Resource tId _ _ _ _) = tId
glbName (Channel tId _ _ _ _)  = tId
glbName (Emitter tId _ _ _ _) = tId
glbName (Handler tId _ _ _ _)  = tId
glbName (Const tId _ _ _ _)    = tId

tyDefName :: TypeDef' expr obj a -> Identifier
tyDefName (Struct sId _ _ )= sId
tyDefName (Enum sId _ _ )=   sId
tyDefName (Class _ sId _ _ _ )=sId
tyDefName (Interface sId _ _ )=sId

globalsName :: AnnASTElement' expr obj a -> Identifier
globalsName (Function fId _ _ _ _ _) = fId
globalsName (GlobalDeclaration glb)    = glbName glb
globalsName (TypeDefinition tyDef _)   = tyDefName tyDef
