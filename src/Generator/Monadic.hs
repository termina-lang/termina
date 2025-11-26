module Generator.Monadic where

-- Termina Semantic AST
import Semantic.AST as SAST

import qualified Data.Set as S

import qualified Control.Monad.State.Strict as ST
import Semantic.Types ( SemanticAnn )
import qualified Data.Map as M
import Utils.Annotations

data MonadicTypes = MonadicTypes {
    optionTypes :: S.Set (TerminaType SemanticAnn),
    statusTypes :: S.Set (TerminaType SemanticAnn),
    resultTypes :: M.Map (TerminaType SemanticAnn) (S.Set (TerminaType SemanticAnn, TerminaType SemanticAnn)),
    -- | Map between the generated struct and enum types and their corresponding source files
    generatedTypes :: M.Map (TerminaType SemanticAnn) QualifiedName
  } deriving Show

type MonadicTypesMonad = ST.State MonadicTypes

insertMonadicType ::
  -- | The new element
  TerminaType SemanticAnn 
  -> MonadicTypesMonad ()
insertMonadicType (TOption (TBoxSubtype _)) = return ()
insertMonadicType (TOption ty) = 
  ST.modify $ \st ->
    st {
      optionTypes = S.insert ty (optionTypes st)
    } 
-- | We do not need to insert the TInt32 type in the status types because it is
-- already defined by the operating system abstraction layer.
insertMonadicType (TStatus TInt32) = return ()
insertMonadicType (TStatus ty) =
  ST.modify $ \st ->
    st {
      statusTypes = S.insert ty (statusTypes st)
    }
insertMonadicType (TResult ty1 ty2) = do
  prevResultTypes <- ST.gets resultTypes
  case M.lookup ty1 prevResultTypes of
    Just tys -> 
      ST.modify $ \st ->
        st {
          resultTypes = M.insert ty1 (S.insert (ty1, ty2) tys) (resultTypes st)
        }
    Nothing ->
      ST.modify $ \st ->
        st {
          resultTypes = M.insert ty1 (S.singleton (ty1, ty2)) (resultTypes st)
        }
  case M.lookup ty2 prevResultTypes of
    Just tys -> 
      ST.modify $ \st ->
        st {
          resultTypes = M.insert ty2 (S.insert (ty1, ty2) tys) (resultTypes st)
        }
    Nothing ->
      ST.modify $ \st ->
        st {
          resultTypes = M.insert ty2 (S.singleton (ty1, ty2)) (resultTypes st)
        }
insertMonadicType _ = return ()

mapParameterMonadic ::
  -- | The new parameter
  Parameter SemanticAnn
  -> MonadicTypesMonad ()
mapParameterMonadic (Parameter _ (TReference _ ts)) = insertMonadicType ts
mapParameterMonadic (Parameter _ ts) = insertMonadicType ts

mapMaybeMonadic ::
  -- | The new element
  Maybe (TerminaType SemanticAnn)
  -> MonadicTypesMonad ()
mapMaybeMonadic (Just ts) = insertMonadicType ts
mapMaybeMonadic Nothing = return ()

mapStatementMonadic ::
  -- | The new element
  Statement SemanticAnn
  -> MonadicTypesMonad ()
mapStatementMonadic (Declaration _ _ ts _ _) = insertMonadicType ts
mapStatementMonadic (IfElseStmt condIf elseIfs elseBlk _) =
  -- | Get the monadic types types from the statements
  mapM_ mapStatementMonadic (blockBody . condIfBody $ condIf) >>
  -- | Get the monadic types types from the else if statements
  mapM_ (\(CondElseIf _ (Block stmts' _) _) -> mapM_ mapStatementMonadic stmts') elseIfs >>
  -- | Get the monadic types types from the else statements
  maybe (return ()) (mapM_ mapStatementMonadic . blockBody . condElseBody) elseBlk
mapStatementMonadic (ForLoopStmt _ _ _ _ _ (Block stmts _) _) = mapM_ mapStatementMonadic stmts
mapStatementMonadic (MatchStmt _ cases mDefaultCase _) = do
  mapM_ (\(MatchCase _ _ (Block stmts _) _) -> mapM_ mapStatementMonadic stmts) cases
  case mDefaultCase of
    Just (DefaultCase (Block stmts _) _) -> mapM_ mapStatementMonadic stmts
    Nothing -> return ()
mapStatementMonadic _ = return ()

mapInterfaceProcedureMonadic ::
  -- | The new element
  InterfaceMember SemanticAnn
  -- | The resulting map
  -> MonadicTypesMonad ()
mapInterfaceProcedureMonadic (InterfaceProcedure _ _ params _ _) =
  -- | Get the monadic types types from the parameters
  mapM_ mapParameterMonadic params

mapClassMemberMonadic ::
  -- | The new element
  ClassMember SemanticAnn
  -> MonadicTypesMonad ()
mapClassMemberMonadic (ClassField field) = insertMonadicType (fieldTerminaType field)
mapClassMemberMonadic (ClassMethod _ _ params maybeRet blkRet _) =
  -- | Get the monadic types types from the return type
  mapMaybeMonadic maybeRet >>
  -- | Get the monadic types types from the parameters
  mapM_ mapParameterMonadic params >>
  -- | Get the monadic types types from the block return type
  mapM_ mapStatementMonadic (blockBody blkRet)
mapClassMemberMonadic (ClassProcedure _ _ params blkRet _) =
  -- | Get the monadic types types from the parameters
  mapM_ mapParameterMonadic params >>
  -- | Get the monadic types types from the block return type
  mapM_ mapStatementMonadic (blockBody blkRet)
mapClassMemberMonadic (ClassViewer _ params maybeRet blkRet _) =
  -- | Get the monadic types types from the return type
  mapMaybeMonadic maybeRet >>
  -- | Get the monadic types types from the parameters
  mapM_ mapParameterMonadic params >>
  -- | Get the monadic types types from the block return type
  mapM_ mapStatementMonadic (blockBody blkRet)
mapClassMemberMonadic (ClassAction _ _ Nothing ret blkRet _) =
  -- | Get the monadic types types from the return type
  insertMonadicType ret >>
  -- | Get the monadic types types from the block return type
  mapM_ mapStatementMonadic (blockBody blkRet)
mapClassMemberMonadic (ClassAction _ _ (Just param) ret blkRet _) =
  -- | Get the monadic types types from the parameter
  mapParameterMonadic param >>
  -- | Get the monadic types types from the return type
  insertMonadicType ret >>
  -- | Get the monadic types types from the block return type
  mapM_ mapStatementMonadic (blockBody blkRet)

mapTypeDefMonadic ::
  -- | The new element
  TypeDef SemanticAnn
  -> MonadicTypesMonad ()
mapTypeDefMonadic (Struct _ fields _) =
  -- | Get the monadic types types from the fields
  mapM_ (\(FieldDefinition _ ts _) -> insertMonadicType ts) fields
mapTypeDefMonadic (Enum _ variants _) =
  -- | Get the monadic types types from the fields
  mapM_ (\(EnumVariant _ tss) -> mapM_ insertMonadicType tss) variants
mapTypeDefMonadic (Class _ _ members _ _) =
  -- | Get the monadic types types from the class members
  mapM_ mapClassMemberMonadic members
mapTypeDefMonadic (Interface _ _ _ members _) =
  -- | Get the monadic types types from the class members
  mapM_ mapInterfaceProcedureMonadic members

mapMonadicTypes ::
  -- | The new element
  AnnASTElement SemanticAnn
  -> MonadicTypesMonad ()
mapMonadicTypes (Function _ params maybeRet blkRet _ _) =
  -- | Get the monadic types types from the parameters
  mapM_ mapParameterMonadic params >>
  -- | Get the monadic types types from the return type (it may return a regular Monadic)
  mapMaybeMonadic maybeRet >>
  -- | Get the monadic types types from the return block
  mapM_ mapStatementMonadic (blockBody blkRet)
mapMonadicTypes (TypeDefinition typeDef _) =
  -- | Get the monadic types types from the type definition
  mapTypeDefMonadic typeDef
mapMonadicTypes (GlobalDeclaration (Resource _ (TMsgQueue ts _) _ _ _)) =
  -- | Get the monadic types types from the message queue type
  insertMonadicType (TOption ts)
mapMonadicTypes _ = return ()

mapMonadicTypesAnnotatedProgram ::
  -- | The annotated program
  AnnotatedProgram SemanticAnn
  -> MonadicTypesMonad ()
mapMonadicTypesAnnotatedProgram = mapM_ mapMonadicTypes

emptyMonadicTypes :: MonadicTypes
emptyMonadicTypes = MonadicTypes S.empty S.empty M.empty M.empty

runMapMonadicTypesAnnotatedProgram ::
  -- | The initial state (i.e., the previous monadic types maps)
  MonadicTypes
  -- | The annotated program to map
  -> AnnotatedProgram SemanticAnn
  -> MonadicTypes
runMapMonadicTypesAnnotatedProgram = flip $ ST.execState . mapMonadicTypesAnnotatedProgram