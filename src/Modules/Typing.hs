-- | Semantic Analysis of modules.

module Modules.Typing where

-- From ordering (and parsing) the project we have
-- Project: |Map ModuleName (... , PAST.TerminaProgram Annotation)|
-- We want to have now: |Map Module Name SAST.TerminaProgram ..|

import Modules.Modules hiding (MData)
import Modules.Errors

import Control.Monad.Except

import Data.Maybe (isJust)

-- Core inof
import AST.Core (TerminaProgram'(..))
-- Parsing data types stuff
import qualified AST.Parser as PAST
import Parser.Parsing (Annotation)

-- Semantic datat types stuff
import qualified AST.Seman as SAST
import Semantic.Monad (SAnns(..), SemanticAnns)
import Semantic.Types (GEntry)
import Semantic.TypeChecking (typeAndGetGlobals)

-- Module system imports
import qualified Data.Map.Strict as M


data ModuleAST a = MData
  { moduleDeps :: [ModuleName]
  , moduleData :: a
  }
  deriving Show

mAstFromPair :: ([ModuleName] , a , PAST.TerminaProgram Annotation) -> ModuleAST (PAST.TerminaProgram Annotation)
mAstFromPair (deps, _ , tast) = MData deps tast

data TypedModule = Typed
   { typedModule :: SAST.TerminaProgram SemanticAnns
   , defsModule  :: [ (SAST.Identifier , SAnns (GEntry SemanticAnns)) ]
   }
  deriving Show

type ParserProject = M.Map ModuleName (ModuleAST (PAST.TerminaProgram Annotation))
type SemanProject =  M.Map ModuleName (ModuleAST TypedModule)

type Environment = M.Map SAST.Identifier (SAnns (GEntry SemanticAnns))

type TProjectM = Except Errors

runTypeProject
  :: ParserProject
  -- a list indicating the order in which they need to be loaded.
  -> [ModuleName]
  -- Return a seman project (covered with effects :sweat_smile:)
  -> Either Errors SemanProject
runTypeProject p = runExcept . typeProject p

typeModule
  :: ParserProject
  -> ModuleName
  -> SemanProject
  -> Either Errors (ModuleAST TypedModule)
typeModule parserMap m typedDeps =
  case M.lookup m parserMap of
        Nothing -> Left (EModuleNotParsed m)
        Just (MData deps parsedM) ->
            addDeps deps >>= \env ->
            case typeAndGetGlobals env (frags parsedM) of
               Left err -> Left (ELiftTypeCheckError err)
               Right t ->
                 let typedModule
                        = Typed (Termina (map (fmap buildModuleName) (modules parsedM)) (fst t)) (snd t)
                 in Right (MData deps typedModule)
  where
   -- Load globals defined by dependecies and create the environment
   -- used to type the current module.
   addDeps = runExcept . foldM (flip envB) M.empty
   envB = buildEnvironment typedDeps

typeProject
  -- We have a project just parsed
  :: ParserProject
  -- a list indicating the order in which they need to be loaded.
  -> [ModuleName]
  -- Return a seman project (covered with effects :sweat_smile:)
  -> TProjectM SemanProject
typeProject parserMap mds =
  typeProject' mds M.empty
  where
    typeProject' [] typedP = return typedP
    typeProject' (m:ms) tP =
      case M.lookup m parserMap of
        Nothing -> throwError (EModuleNotParsed m)
        Just (MData deps parsedM) -> do
            -- Load globals defined by dependecies and create the environment
            -- used to type the current module.
            env <- foldM (flip (buildEnvironment tP)) M.empty deps
            -- Type the current module and get its globals.
            typedProg <- case typeAndGetGlobals env (frags parsedM) of
                           Left err -> throwError (ELiftTypeCheckError err)
                           Right t -> return t
            -- Load stuff into project map
            let typedModule = Typed (Termina (map (fmap buildModuleName) (modules parsedM)) (fst typedProg)) (snd typedProg)
            -- Add it to the project and continue typing
            typeProject' ms (M.insert m (MData deps typedModule) tP)

buildEnvironment
  :: SemanProject -> ModuleName -> Environment -> TProjectM Environment
buildEnvironment tP dName env =
  case M.lookup dName tP of
     --
     Nothing -> throwError (EDependencyNotTyped dName)
     --
     Just typedM ->
       foldM
        (\e (i,defs) ->
           when (isJust (M.lookup i e)) (throwError (ENameCollition dName i))
           >> return (M.insert i defs e))
        env
        (defsModule (moduleData typedM))
