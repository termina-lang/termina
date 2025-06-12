{-# LANGUAGE OverloadedStrings #-}
module LSP.Utils  where

import LSP.Monad
import Data.Text
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import Language.LSP.VFS
import System.IO.Error
import Data.Functor
import Command.Types
import Text.Parsec
import Parser.Parsing
import qualified Data.Text as T
import Utils.Annotations
import Parser.Errors
import qualified Data.Map as M
import Utils.Errors (ErrorMessage(toDiagnostics))
import Control.Monad.State
import Parser.AST
import LSP.Logging
import System.Directory
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Diagnostics as LSP
import LSP.Modules
import qualified Data.SortedList as SL
import Modules.Modules (ModuleDependency (..))
import Semantic.TypeChecking
import System.FilePath
import Parser.Types
import qualified Parser.AST as PAST
import Modules.Utils
import Semantic.Environment
import Command.Utils (getModuleDependencyList)
import qualified Data.Set as S

filePathToUri :: MonadIO m => FilePath -> m LSP.Uri
filePathToUri = liftIO . (LSP.filePathToUri <$>) . canonicalizePath

filePathToNormalizedUri :: MonadIO m => FilePath -> m LSP.NormalizedUri
filePathToNormalizedUri = liftIO . (LSP.toNormalizedUri <$>) . filePathToUri

getModuleImports :: Maybe FilePath -> PAST.TerminaModule ParserAnn -> HandlerM (Either ParsingErrors [ModuleDependency])
getModuleImports (Just srcPath) m =
    mapM buildAndTest (modules m) <&> sequence
    where

        buildAndTest :: PAST.ModuleImport ParserAnn -> HandlerM (Either ParsingErrors ModuleDependency)
        buildAndTest (ModuleImport modName ann) = do
            let mname = buildModuleName ann modName
            case mname of
                Left err -> return $ Left err
                Right qname -> do
                    srcAbsPath <- liftIO $ canonicalizePath srcPath
                    let importedPath = srcAbsPath </> qname <.> "fin"
                    exists <- liftIO $ doesFileExist importedPath
                    if exists
                        then return $ Right (ModuleDependency importedPath ann)
                        else
                            return $ Left (annotateError ann (EImportedFileNotFound importedPath))
getModuleImports Nothing m =
    case modules m of
        [] -> return $ Right []
        ((ModuleImport modName ann):_) -> do
            let mname = buildModuleName ann modName
            case mname of
                Left err -> return $ Left err
                Right qname -> do
                    return $ Left (annotateError ann (EImportedFileNotFound (qname <.> "fin")))

emitDiagnostics :: QualifiedName -> [LSP.Diagnostic] -> HandlerM ()
emitDiagnostics filePath mdiag = do
    let maxDiags = 100
        version = Just 0
        normUri = LSP.toNormalizedUri . LSP.filePathToUri $ filePath
    case mdiag of
      [] -> LSP.publishDiagnostics maxDiags normUri version (M.singleton Nothing (SL.toSortedList []))
      diags -> LSP.publishDiagnostics maxDiags normUri version (LSP.partitionBySource diags)

-- | Load the modules of the project
loadModules
  :: [ModuleDependency]
  -> FilePath
  -> HandlerM ()
loadModules imported srcPath = do
  -- | Load and parse the project.
  -- The main application module has been already loaded. We need to load the
  -- rest of the modules.
  loadModules' imported

  where

  loadModules' ::
    -- Modules to load
    [ModuleDependency]
    -> HandlerM ()
  loadModules' [] = return ()
  loadModules' ((ModuleDependency absPath _):fss) = do
    fsLoaded <- gets project_modules
    if M.member absPath fsLoaded
    -- Nothing to do, skip to the next one. It could be the case of a module
    -- imported from several files.
    then loadModules' fss
    -- Import and load it.
    else do
      mLoadedModule <- loadTerminaModule absPath (Just srcPath)
      case mLoadedModule of 
        Nothing -> loadModules' fss
        Just loadedModule -> do
          let deps = importedModules loadedModule
          loadModules' (fss ++ deps)

-- | Load Termina file 
loadTerminaModule ::
  -- | Path of the file to load
  FilePath
  -- | Path of the source folder that stores the imported modules
  -> Maybe FilePath
  -> HandlerM (Maybe TerminaStoredModule)
loadTerminaModule fullP srcPath = do
  -- read it
  esrc_code <- loadVSFile fullP
  case esrc_code of
    Left _err -> do
      errorM ("Error when loading file: " <> T.pack (show fullP))
      return Nothing
    Right src_code ->
      -- parse it
      case runParser terminaModuleParser fullP fullP (T.unpack src_code) of
        Left err -> do
          let pErr = annotateError (Position fullP (errorPos err) (errorPos err)) (EParseError err)
              fileMap = M.singleton fullP src_code
              newModule = TerminaStoredModule fullP [] src_code (toDiagnostics pErr fileMap) Nothing Nothing
          modify (\s -> 
            s { 
              project_modules = M.insert fullP 
                  newModule
                  (project_modules s) })
          return $ Just newModule
        Right term -> do
          mimports <- getModuleImports srcPath term
          case mimports of
            Left err -> do
              let fileMap = M.singleton fullP src_code
                  newModule = TerminaStoredModule fullP [] src_code (toDiagnostics err fileMap) Nothing Nothing
              modify (\s -> 
                s { 
                  project_modules = M.insert fullP 
                    newModule
                    (project_modules s) })
              return $ Just newModule
            Right imports -> do
              let newModule = TerminaStoredModule fullP imports src_code [] (Just $ ParsingData . frags $ term) Nothing
              modify (\s -> s { project_modules = M.insert fullP 
                newModule
                (project_modules s) })
              return $ Just newModule

loadVSFile :: 
  -- | Path of the file to load
  FilePath
  -- | Path of the source folder that stores the imported modules
  -> HandlerM (Either IOError Text)
loadVSFile filePath = do
  -- First, we check if the file is loaded into the VFS
  let normUri = LSP.toNormalizedUri . LSP.filePathToUri $ filePath
  vfile <- getVirtualFile normUri
  case virtualFileText <$> vfile of
    Nothing ->
      liftIO $ catchIOError (TIO.readFile filePath <&> Right)
        (return . Left)
    Just fileContents -> return $ Right fileContents

typeModules :: FilePath -> M.Map QualifiedName [QualifiedName] -> Environment -> [QualifiedName] -> HandlerM Environment
typeModules _srcPath _prevModsMap finalState [] = pure finalState
typeModules srcPath prevModsMap prevState (m:ms) = do
  parsedProject <- gets project_modules
  let parsedModule = parsedProject M.! m
  let moduleDependencies = S.fromList $ getModuleDependencyList prevModsMap (importedModules parsedModule)
  case parsing parsedModule of
    Nothing -> 
      -- This means that the module has not been parsed or that the parsing failed
      return prevState
    Just parsingData -> do
      let result = runTypeChecking prevState (typeTerminaModule (S.insert m moduleDependencies) . parsedAST $ parsingData)
      case result of
        (Left err) -> do
          -- | Create the source files map. This map will be used to obtain the source files that
          -- will be feed to the error pretty printer. The source files map must use as key the
          -- path of the source file and as element the text of the source file.
          let sourceFilesMap = 
                M.foldrWithKey (\_ item prevmap -> M.insert (fullPath item) (sourcecode item) prevmap) 
                    M.empty parsedProject
          -- Store the diagnostics 
          modify (\s -> 
            s { 
              project_modules = M.insert m 
                  parsedModule { 
                    diagnostics = toDiagnostics err sourceFilesMap,
                    semantic = Nothing }
                  (project_modules s) })
          return prevState
        (Right (typedProgram, newState)) -> do
          let semanticData = SemanticData typedProgram
          let newModsMap = M.insert m (S.toList moduleDependencies) prevModsMap
          modify (\s -> 
            s { 
              project_modules = M.insert m 
                  parsedModule { semantic = Just semanticData }
                  (project_modules s) })          
          -- | We need to update the project store
          typeModules srcPath newModsMap newState ms