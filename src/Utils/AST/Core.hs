{-# LANGUAGE LambdaCase #-}
-- | Util functions related to AST.Core

module Utils.AST.Core where

import           AST.Core
import qualified Data.List  as L
import           Data.Maybe (maybeToList)

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


----------------------------------------
-- Invokation Dependency in a block of code.
-- Capturing the pattern `self->f()` on objects.
-- As far as I understand it, all objects should have name, and thus, we cannot
-- (or shoudln't) concatenate invocations.

-- We do not have name shadowing, so technically there cannot be two things with
-- the same name. We do not need to insepect 'self->f()', but I am afraid of
-- breaking something else.

selfInv :: Expression' obj a -> (obj a -> Bool) -> Maybe Identifier
selfInv (MemberFunctionAccess obj mident _args _ann) isSelf =
  if isSelf obj then Just mident else Nothing
selfInv (DerefMemberFunctionAccess obj mident _args _ann) isSelf =
  if isSelf obj then Just mident else Nothing
selfInv _ _isSelf = Nothing

selfInvStmt :: (obj a -> Bool) -> Statement' (Expression' obj) obj a -> [Identifier]
selfInvStmt isSelf = selfInvStmt'
 where
    isSelfExpression e = maybeToList (selfInv e isSelf)
    -- selfInvStmt' :: Statement' (Expression' obj) obj a -> [Identifier]
    selfInvStmt' (Declaration _vident _accK _type e _ann) = isSelfExpression e
    selfInvStmt' (AssignmentStmt _obj e _ann) = isSelfExpression e
    selfInvStmt' (IfElseStmt eC bIf bEls bEl _ann) =
      isSelfExpression eC
        ++
        concatMap selfInvStmt' bIf
        ++
        concatMap (\ el ->
                     isSelfExpression (elseIfCond el)
                ++ selfInvBlock isSelf (elseIfBody el)
                ) bEls
        ++
        concatMap selfInvStmt' bEl
    selfInvStmt' (ForLoopStmt _loopIdent _type initV endV cBreak body _ann) =
      isSelfExpression initV ++ isSelfExpression endV
      ++ concat (maybeToList (isSelfExpression <$> cBreak))
      ++ concatMap selfInvStmt' body
    selfInvStmt' (MatchStmt e mcases _ann) =
      isSelfExpression e
      ++  concatMap (selfInvBlock isSelf . matchBody) mcases
    selfInvStmt' (SingleExpStmt e _ann) = isSelfExpression e

selfInvBlock :: (obj a -> Bool) -> Block' (Expression' obj) obj a -> [Identifier]
selfInvBlock isSelf = concatMap (selfInvStmt isSelf)

selfInvRetStmt :: (obj a -> Bool) -> ReturnStmt' (Expression' obj) a -> [Identifier]
selfInvRetStmt isSelf = maybe [] ( maybeToList . (`selfInv` isSelf)) . returnExpression

selfInvBlockRet :: (obj a -> Bool) -> BlockRet' (Expression' obj) obj a -> [Identifier]
selfInvBlockRet isSelf bret
  = selfInvBlock isSelf (blockBody bret)
  ++ selfInvRetStmt isSelf (blockRet bret)

selfDepClass
  :: (obj a -> Bool)
  -> ClassMember' (Expression' obj) obj a
  -> Maybe (Identifier, [Identifier])
selfDepClass isSelf = selfDepClass'
 where
   -- Fields do not have self dependencies
   selfDepClass' (ClassField {}) = Nothing
   -- Methods can
   selfDepClass' (ClassMethod mId _type bRet _ann) =
     Just (mId,selfInvBlockRet isSelf bRet)
   -- Procedures can
   selfDepClass' (ClassProcedure pId _params blk _ann) =
     Just (pId, selfInvBlock isSelf blk)
   -- Viewers can
   selfDepClass' (ClassViewer vId _param _type bRet _ann) =
     Just (vId , selfInvBlockRet isSelf bRet)
   -- Actions can
   selfDepClass' (ClassAction aId _param _type bRet _ann) =
      Just (aId , selfInvBlockRet isSelf bRet)

className :: ClassMember' expr obj a -> Identifier
className (ClassField e _)              = fieldIdentifier e
className (ClassMethod mIdent _ _ _)    = mIdent
className (ClassProcedure pIdent _ _ _) = pIdent
className (ClassViewer vIdent _ _ _ _)  = vIdent
className (ClassAction aIdent _ _ _ _)  = aIdent
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
globalsName (GlobalDeclaration glb)  = glbName glb
globalsName (TypeDefinition tyDef _) = tyDefName tyDef
