module Generator.Option where

-- Termina Semantic AST
import Semantic.AST as SAST
import Core.Utils

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad
import qualified Control.Monad.State.Strict as ST

type OptionMap = M.Map TypeSpecifier (S.Set TypeSpecifier)

type OptionTypesMonad = ST.State OptionMap

insertOptionType ::
  -- | The new element
  TypeSpecifier
  -> OptionTypesMonad ()
insertOptionType ts = do
  when (isNonBoxOption ts) $ ST.gets (M.lookup (rootType ts)) >>=
    maybe
      -- | if the root type is not in the map, insert it
      (ST.modify $ M.insert (rootType ts) (S.singleton ts))
      -- | if the root type is in the map, insert the new option type in the set 
      (ST.modify . M.insert (rootType ts) . S.insert ts)

mapParameterOption ::
  -- | The new parameter
  Parameter
  -> OptionTypesMonad ()
mapParameterOption (Parameter _ ts) = insertOptionType ts

mapMaybeOption ::
  -- | The new element
  Maybe TypeSpecifier
  -> OptionTypesMonad ()
mapMaybeOption (Just ts) = insertOptionType ts
mapMaybeOption Nothing = return ()

mapStatementOption ::
  -- | The new element
  Statement a
  -> OptionTypesMonad ()
mapStatementOption (Declaration _ _ ts _ _) = insertOptionType ts
mapStatementOption (IfElseStmt _ stmts elseIfs elseBlk _) =
  -- | Get the option types from the statements
  mapM_ mapStatementOption stmts >>
  -- | Get the option types from the else if statements
  mapM_ (\(ElseIf _ stmts' _) -> mapM_ mapStatementOption stmts') elseIfs >>
  -- | Get the option types from the else statements
  maybe (return ()) (mapM_ mapStatementOption) elseBlk
mapStatementOption (ForLoopStmt _ _ _ _ _ stmts _) = mapM_ mapStatementOption stmts
mapStatementOption (MatchStmt _ cases _) =
  mapM_ (\(MatchCase _ _ stmts _) -> mapM_ mapStatementOption stmts) cases
mapStatementOption _ = return ()

mapInterfaceProcedureOption ::
  -- | The new element
  InterfaceMember a
  -- | The resulting map
  -> OptionTypesMonad ()
mapInterfaceProcedureOption (InterfaceProcedure _ params _) =
  -- | Get the option types from the parameters
  mapM_ mapParameterOption params

mapClassMemberOption ::
  -- | The new element
  ClassMember a
  -> OptionTypesMonad ()
mapClassMemberOption (ClassField field _) = insertOptionType (fieldTypeSpecifier field)
mapClassMemberOption (ClassMethod _ maybeRet blkRet _) =
  -- | Get the option types from the return type
  mapMaybeOption maybeRet >>
  -- | Get the option types from the block return type
  mapM_ mapStatementOption (blockBody blkRet)
mapClassMemberOption (ClassProcedure _ params blkRet _) =
  -- | Get the option types from the parameters
  mapM_ mapParameterOption params >>
  -- | Get the option types from the block return type
  mapM_ mapStatementOption (blockBody blkRet)
mapClassMemberOption (ClassViewer _ params maybeRet blkRet _) =
  -- | Get the option types from the return type
  mapMaybeOption maybeRet >>
  -- | Get the option types from the parameters
  mapM_ mapParameterOption params >>
  -- | Get the option types from the block return type
  mapM_ mapStatementOption (blockBody blkRet)
mapClassMemberOption (ClassAction _ param ret blkRet _) =
  -- | Get the option types from the parameter
  mapParameterOption param >>
  -- | Get the option types from the return type
  insertOptionType ret >>
  -- | Get the option types from the block return type
  mapM_ mapStatementOption (blockBody blkRet)

mapTypeDefOption ::
  -- | The new element
  TypeDef a
  -> OptionTypesMonad ()
mapTypeDefOption (Struct _ fields _) =
  -- | Get the option types from the fields
  mapM_ (\(FieldDefinition _ ts) -> insertOptionType ts) fields
mapTypeDefOption (Enum _ variants _) =
  -- | Get the option types from the fields
  mapM_ (\(EnumVariant _ tss) -> mapM_ insertOptionType tss) variants
mapTypeDefOption (Class _ _ members _ _) =
  -- | Get the option types from the class members
  mapM_ mapClassMemberOption members
mapTypeDefOption (Interface _ members _) =
  -- | Get the option types from the class members
  mapM_ mapInterfaceProcedureOption members

mapOptions ::
  -- | The new element
  AnnASTElement a
  -> OptionTypesMonad ()
mapOptions (Function _ params maybeRet blkRet _ _) =
  -- | Get the option types from the parameters
  mapM_ mapParameterOption params >>
  -- | Get the option types from the return type (it may return a regular Option)
  mapMaybeOption maybeRet >>
  -- | Get the option types from the return block
  mapM_ mapStatementOption (blockBody blkRet)
mapOptions (TypeDefinition typeDef _) =
  -- | Get the option types from the type definition
  mapTypeDefOption typeDef
mapOptions (GlobalDeclaration (Resource _ (MsgQueue ts _) _ _ _)) =
  -- | Get the option types from the message queue type
  insertOptionType (Option ts)
mapOptions _ = return ()

mapOptionsAnnotatedProgram ::
  -- | The annotated program
  AnnotatedProgram a
  -> OptionTypesMonad ()
mapOptionsAnnotatedProgram = mapM_ mapOptions

runMapOptionsAnnotatedProgram ::
  -- | The initial state (i.e., the previous option map)
  OptionMap
  -- | The annotated program to map
  -> AnnotatedProgram a
  -> OptionMap
runMapOptionsAnnotatedProgram = flip $ ST.execState . mapOptionsAnnotatedProgram