module Semantic.Option where

-- Termina Semantic AST
import AST.Seman as SAST
import Utils.TypeSpecifier

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

type OptionMap = M.Map TypeSpecifier (S.Set TypeSpecifier)

insertOptionType :: Monad m
  -- | The initial map with the all the option types
  => OptionMap
  -- | The new element
  -> TypeSpecifier
  -- | The resulting map
  -> m OptionMap
insertOptionType prevMap ts =
  if isNonDynOption ts then
    maybe
      -- | if the root type is not in the map, insert it
      (return $ M.insert (rootType ts) (S.singleton ts) prevMap)
      -- | if the root type is in the map, insert the new option type in the set 
      (\ps -> return $ M.insert (rootType ts) (S.insert ts ps) prevMap) $ M.lookup (rootType ts) prevMap
  else return prevMap

mapParameterOption :: Monad m
  -- | The initial map with the all the option types
  => OptionMap
  -- | The new parameter
  -> Parameter
  -- | The resulting map
  -> m OptionMap
mapParameterOption prevMap (Parameter _ ts) = insertOptionType prevMap ts

mapMaybeOption :: Monad m
  -- | The initial map with the all the option types
  => OptionMap
  -- | The new element
  -> Maybe TypeSpecifier
  -- | The resulting map
  -> m OptionMap
mapMaybeOption prevMap (Just ts) = insertOptionType prevMap ts
mapMaybeOption prevMap Nothing = return prevMap

mapStatementOption :: Monad m
  -- | The initial map with the all the option types
  => OptionMap
  -- | The new element
  -> Statement a
  -- | The resulting map
  -> m OptionMap
mapStatementOption prevMap (Declaration _ _ ts _ _) = insertOptionType prevMap ts
mapStatementOption prevMap (IfElseStmt _ stmts elseIfs elseBlk _) =
  -- | Get the option types from the statements
  foldM mapStatementOption prevMap stmts >>=
  -- | Get the option types from the else if statements
  flip (foldM (\m (ElseIf _ stmts' _) -> foldM mapStatementOption m stmts')) elseIfs >>=
  -- | Get the option types from the else statements
  (\acc -> maybe (return acc) (foldM mapStatementOption acc) elseBlk)
mapStatementOption prevMap (ForLoopStmt _ _ _ _ _ stmts _) = foldM mapStatementOption prevMap stmts
mapStatementOption prevMap (MatchStmt _ cases _) =
  foldM (\m (MatchCase _ _ stmts _) -> foldM mapStatementOption m stmts) prevMap cases
mapStatementOption prevMap _ = return prevMap

mapInterfaceProcedureOption :: Monad m
  -- | The initial map with the all the option types
  => OptionMap
  -- | The new element
  -> InterfaceMember a
  -- | The resulting map
  -> m OptionMap
mapInterfaceProcedureOption prevMap (InterfaceProcedure _ params _) =
  -- | Get the option types from the parameters
  foldM mapParameterOption prevMap params

mapClassMemberOption :: Monad m
  -- | The initial map with the all the option types
  => OptionMap
  -- | The new element
  -> ClassMember a
  -- | The resulting map
  -> m OptionMap
mapClassMemberOption prevMap (ClassField field _) = insertOptionType prevMap (fieldTypeSpecifier field)
mapClassMemberOption prevMap (ClassMethod _ maybeRet blkRet _) =
  -- | Get the option types from the return type
  mapMaybeOption prevMap maybeRet >>=
  -- | Get the option types from the block return type
  flip (foldM mapStatementOption) (blockBody blkRet)
mapClassMemberOption prevMap (ClassProcedure _ params blk _) =
  -- | Get the option types from the parameters
  foldM mapParameterOption prevMap params >>=
  -- | Get the option types from the block return type
  flip (foldM mapStatementOption) blk
mapClassMemberOption prevMap (ClassViewer _ params maybeRet blkRet _) =
  -- | Get the option types from the return type
  mapMaybeOption prevMap maybeRet >>=
  -- | Get the option types from the parameters
  flip (foldM mapParameterOption) params >>=
  -- | Get the option types from the block return type
  flip (foldM mapStatementOption) (blockBody blkRet)
mapClassMemberOption prevMap (ClassAction _ param ret blkRet _) =
  -- | Get the option types from the parameter
  mapParameterOption prevMap param >>=
  -- | Get the option types from the return type
  flip insertOptionType ret >>=
  -- | Get the option types from the block return type
  flip (foldM mapStatementOption) (blockBody blkRet)

mapTypeDefOption :: Monad m
  -- | The initial map with the all the option types
  => OptionMap
  -- | The new element
  -> TypeDef a
  -- | The resulting map
  -> m OptionMap
mapTypeDefOption prevMap (Struct _ fields _) =
  -- | Get the option types from the fields
  foldM (\m (FieldDefinition _ ts) -> insertOptionType m ts) prevMap fields
mapTypeDefOption prevMap (Enum _ variants _) =
  -- | Get the option types from the fields
  foldM (\m (EnumVariant _ tss) -> foldM insertOptionType m tss) prevMap variants
mapTypeDefOption prevMap (Class _ _ members _ _) =
  -- | Get the option types from the class members
  foldM mapClassMemberOption prevMap members
mapTypeDefOption prevMap (Interface _ members _) =
  -- | Get the option types from the class members
  foldM mapInterfaceProcedureOption prevMap members

mapOptions :: Monad m
  -- | The initial map with the all the option types
  => OptionMap
  -- | The new element
  -> AnnASTElement a
  -- | The resulting map
  -> m OptionMap
mapOptions prevMap (Function _ params maybeRet blkRet _ _) =
  -- | Get the option types from the parameters
  foldM mapParameterOption prevMap params >>=
  -- | Get the option types from the return type (it may return a regular Option)
  flip mapMaybeOption maybeRet >>=
  -- | Get the option types from the return block
  flip (foldM mapStatementOption) (blockBody blkRet)
mapOptions prevMap (TypeDefinition typeDef _) =
  -- | Get the option types from the type definition
  mapTypeDefOption prevMap typeDef
mapOptions prevMap (GlobalDeclaration (Resource _ (MsgQueue ts _) _ _ _)) =
  -- | Get the option types from the message queue type
  insertOptionType prevMap (Option ts)
mapOptions prevMap _ = return prevMap