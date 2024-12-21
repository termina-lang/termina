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
import Utils.Errors (ErrorMessage(toDiagnostic))
import Control.Monad.State
import Command.Utils (getModuleImports)
import Parser.AST
import LSP.Logging
import System.Directory ( canonicalizePath )
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Diagnostics as LSP
import LSP.Modules
import qualified Data.SortedList as SL

filePathToUri :: MonadIO m => FilePath -> m LSP.Uri
filePathToUri = liftIO . (LSP.filePathToUri <$>) . canonicalizePath

filePathToNormalizedUri :: MonadIO m => FilePath -> m LSP.NormalizedUri
filePathToNormalizedUri = liftIO . (LSP.toNormalizedUri <$>) . filePathToUri

emitDiagnostics :: LSP.NormalizedUri -> Maybe LSP.Diagnostic -> HandlerM ()
emitDiagnostics uri mdiag = do
    let maxDiags = 1
        version = Just 0
    case mdiag of
      Nothing -> LSP.publishDiagnostics maxDiags uri version (M.singleton Nothing (SL.toSortedList []))
      Just diag -> LSP.publishDiagnostics maxDiags uri version (LSP.partitionBySource [diag])

-- | Load Termina file 
loadTerminaModule ::
  -- | Path of the file to load
  FilePath
  -- | Path of the source folder that stores the imported modules
  -> FilePath
  -> HandlerM ()
loadTerminaModule fullP srcPath = do
  uri <- filePathToNormalizedUri fullP
  -- read it
  esrc_code <- loadVSFile fullP
  case esrc_code of
    Left _err -> do
      errorM ("Error when loading file: " <> T.pack (show fullP))
    Right src_code ->
      -- parse it
      case runParser terminaModuleParser () fullP (T.unpack src_code) of
        Left err -> do
          let pErr = annotateError (Position (errorPos err) (errorPos err)) (EParseError err)
              fileMap = M.singleton fullP src_code
          modify (\s -> 
            s { 
              project_modules = M.insert uri 
                  (TerminaStoredModule fullP [] src_code (Just (toDiagnostic pErr fileMap)) Nothing Nothing)
                  (project_modules s) })
        Right term -> do
          mimports <- getModuleImports srcPath term
          case mimports of
            Left err -> do
              let fileMap = M.singleton fullP src_code
              modify (\s -> 
                s { 
                  project_modules = M.insert uri 
                    (TerminaStoredModule fullP [] src_code (Just (toDiagnostic err fileMap)) Nothing Nothing)
                    (project_modules s) })
            Right imports ->
              modify (\s -> s { project_modules = M.insert uri 
                (TerminaStoredModule fullP imports src_code Nothing (Just $ ParsingData . frags $ term) Nothing) 
                (project_modules s) })

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
