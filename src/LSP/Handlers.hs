{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module LSP.Handlers where

import Control.Lens ((^.))
import Language.LSP.Server
import Language.LSP.Protocol.Message
import LSP.Monad
import Language.LSP.Protocol.Types
import Control.Monad.State

import qualified Language.LSP.Protocol.Lens as J
import Configuration.Platform
import Configuration.Configuration
import LSP.Logging
import System.Directory
import qualified Data.Text as T
import Command.Utils
import LSP.Utils
import qualified Data.Map.Strict as M
import System.FilePath
import LSP.Modules
import LSP.Index
import Utils.Errors (loc2Location)
import Utils.Annotations (QualifiedName)
import qualified Utils.Annotations as Ann
import Generator.Environment (getPlatformInitialGlobalEnv)
import Data.Functor (void)
import Semantic.Environment
import Command.Types (typedAST)
import Data.Maybe (fromMaybe, mapMaybe)

initializeHandler :: TMessage Method_Initialize -> HandlerM ()
initializeHandler _req = do
    infoM "Loading termina.yaml..."
    ecfg <- loadConfig
    case ecfg of
      Left err ->
        errorM $ "Error when parsing termina.yaml: " <> T.pack (show err)
      Right cfg -> do
        -- We have loaded the configuration file. Then we must check that the platform is OK
        -- Decode the selected platform field
        infoM "Loaded termina.yaml"
        case checkPlatform (T.unpack (platform cfg)) of
          Nothing ->
            errorM $ "Unsupported platform: \"" <> T.pack (show (platform cfg)) <> "\""
          Just plt -> do
            put $ ServerState (Just cfg) mempty
            -- The platform is OK
            -- Then we have to check the folder's structure
            existSourceFolder <- liftIO $ doesDirectoryExist (sourceModulesFolder cfg)
            existAppFolder <- liftIO $ doesDirectoryExist appFolder
            if not existSourceFolder then
              errorM ("Source folder \"" <> T.pack (sourceModulesFolder cfg) <> "\" does not exist")
            else if not existAppFolder then
              errorM ("Application folder \"" <> T.pack appFolder <> "\" does not exist")
            else do
              -- At this point, no files have been loaded into the VFS, so we must read all
              -- the files directly from the file system.
              absPath <- liftIO $ canonicalizePath appFolder
              let fullP = absPath </> appFilename <.> "fin"
              mappModule <- loadTerminaModule fullP (Just (sourceModulesFolder cfg))
              case mappModule of
                Nothing -> return ()
                Just appModule -> do
                  -- Load the modules of the project
                  loadModules (importedModules appModule) (sourceModulesFolder cfg)
                  -- | Once all files have been loaded, we may proceed to type check them
                  -- We need to first obtain the order in which the modules must be type checked
                  parsedProject <- gets project_modules
                  let projectDependencies = fmap importedModules parsedProject
                  either
                    (\_loop ->
                      -- TODO: Generate diagnostics
                      return ())
                    (\orderedDependencies ->
                      let initialGlobalEnv = makeInitialGlobalEnv (Just cfg) plt (getPlatformInitialGlobalEnv cfg plt) in
                      void $ typeModules (sourceModulesFolder cfg) M.empty initialGlobalEnv orderedDependencies)
                    $ sortProjectDepsOrLoop projectDependencies
              return ()

typeProject :: HandlerM ()
typeProject = do
  parsedProject <- gets project_modules
  cfg <- gets config
  let projectDependencies = M.map importedModules parsedProject
  either
    (\_loop ->
      -- TODO: Generate diagnostics
      return ())
    (\orderedDependencies -> do
      let lspPlt = fromMaybe
            TestPlatform (cfg >>= checkPlatform . T.unpack . platform)
          pltInitialGlbEnv = case cfg of
            Nothing -> []
            Just cfg' -> case checkPlatform (T.unpack (platform cfg')) of
              Nothing -> []
              Just plt -> getPlatformInitialGlobalEnv cfg' plt
      let initialGlobalEnv = makeInitialGlobalEnv cfg lspPlt pltInitialGlbEnv
      case cfg of
        Nothing -> void $ typeModules "" M.empty initialGlobalEnv orderedDependencies
        Just cfg' -> void $ typeModules (sourceModulesFolder cfg') M.empty initialGlobalEnv orderedDependencies)
    $ sortProjectDepsOrLoop projectDependencies
  diags <- gets project_modules
  mapM_ (uncurry emitDiagnostics) (M.toList (diagnostics <$> diags))

documentChange :: Handlers HandlerM
documentChange  = notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
  let fileURI = msg ^. J.params . J.textDocument . J.uri
  case uriToFilePath fileURI of
    Nothing ->
      sendNotification SMethod_WindowShowMessage
        (ShowMessageParams MessageType_Error $ T.pack ("Internal error: unknown file: " ++ show (uriToFilePath fileURI)))
    Just filePath -> do
      cfg <- gets config
      _ <- loadTerminaModule filePath (sourceModulesFolder <$> cfg)
      typeProject

requestSymbols :: Handlers HandlerM
requestSymbols = requestHandler SMethod_TextDocumentDocumentSymbol $ \req responder -> do
  let fileURI = req ^. J.params . J.textDocument . J.uri
  case uriToFilePath fileURI of
    Nothing ->
      sendNotification SMethod_WindowShowMessage
        (ShowMessageParams MessageType_Error $ T.pack ("Internal error: unknown file: " ++ show (uriToFilePath fileURI)))
    Just filePath -> do
      typed_modules <- gets project_modules
      let mLoadedModule = M.lookup filePath typed_modules
      case mLoadedModule of
        Nothing -> do
          responder (Right (InL []))
        Just loadedModule -> do
          case semantic loadedModule of
            Nothing -> responder (Right (InL []))
            Just semanticData -> do
              let symbols = getDocumentSymbols (typedAST semanticData)
              responder (Right (InR (InL symbols)))

-- | Where the name under the cursor is defined. The use the walk of the module
-- found wins; a position the walk does not reach, such as the name of a type
-- inside a declaration, falls back to the word written there, which is looked
-- up among the definitions of the project.
requestDefinition :: Handlers HandlerM
requestDefinition = requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
  let fileURI = req ^. J.params . J.textDocument . J.uri
      pos = req ^. J.params . J.position
      here = (fromIntegral (pos ^. J.line) + 1, fromIntegral (pos ^. J.character) + 1)
  case uriToFilePath fileURI of
    Nothing -> responder (Right (InR (InR Null)))
    Just filePath -> do
      modules <- gets project_modules
      case M.lookup filePath modules of
        Nothing -> responder (Right (InR (InR Null)))
        Just loadedModule -> do
          let idx = moduleIndex loadedModule
              target = case referenceAt idx here of
                Just found -> Just found
                Nothing -> TopLevel <$> wordAt (sourcecode loadedModule) here
          responder (Right (maybe (InR (InR Null)) InL (target >>= locate modules idx)))

  where

    -- | A local is already resolved; a name is looked up in the module that
    -- holds the cursor and, failing that, in the rest of the project, which is
    -- where an imported name lives.
    locate :: M.Map QualifiedName TerminaStoredModule -> ModuleIndex -> Target -> Maybe Definition
    locate _ _ (Local loc) = definitionOf loc
    locate modules idx (TopLevel ident) =
      lookupAcross modules (M.lookup ident . indexTopLevel) idx
    locate modules idx (Member owner ident) =
      lookupAcross modules (M.lookup (owner, ident) . indexMembers) idx

    lookupAcross :: M.Map QualifiedName TerminaStoredModule
      -> (ModuleIndex -> Maybe Ann.Location) -> ModuleIndex -> Maybe Definition
    lookupAcross modules search idx =
      case search idx of
        Just loc -> definitionOf loc
        Nothing ->
          case mapMaybe (search . moduleIndex) (M.elems modules) of
            (loc:_) -> definitionOf loc
            [] -> Nothing

    definitionOf :: Ann.Location -> Maybe Definition
    definitionOf loc = Definition . InL <$> loc2Location loc

-- | Every use of the name under the cursor, across the project. Two uses are
-- the same name when they resolve to the same target, which is what makes a
-- local of one body different from a local of another with the same name.
requestReferences :: Handlers HandlerM
requestReferences = requestHandler SMethod_TextDocumentReferences $ \req responder -> do
  let fileURI = req ^. J.params . J.textDocument . J.uri
      pos = req ^. J.params . J.position
      here = (fromIntegral (pos ^. J.line) + 1, fromIntegral (pos ^. J.character) + 1)
  modules <- gets project_modules
  case uriToFilePath fileURI >>= flip M.lookup modules of
    Nothing -> responder (Right (InR Null))
    Just loadedModule ->
      case referenceAt (moduleIndex loadedModule) here of
        Nothing -> responder (Right (InR Null))
        Just target ->
          responder (Right (InL (usesOf modules target)))

-- | The uses of a target in every module of the project.
usesOf :: M.Map QualifiedName TerminaStoredModule -> Target -> [Location]
usesOf modules target =
  [ lspLoc
  | loadedModule <- M.elems modules
  , (loc, found) <- indexRefs (moduleIndex loadedModule)
  , found == target
  , Just lspLoc <- [loc2Location loc] ]

-- | The same name highlighted wherever it appears in the file being read.
requestHighlight :: Handlers HandlerM
requestHighlight = requestHandler SMethod_TextDocumentDocumentHighlight $ \req responder -> do
  let fileURI = req ^. J.params . J.textDocument . J.uri
      pos = req ^. J.params . J.position
      here = (fromIntegral (pos ^. J.line) + 1, fromIntegral (pos ^. J.character) + 1)
  modules <- gets project_modules
  case uriToFilePath fileURI >>= flip M.lookup modules of
    Nothing -> responder (Right (InR Null))
    Just loadedModule -> do
      let idx = moduleIndex loadedModule
      case referenceAt idx here of
        Nothing -> responder (Right (InR Null))
        Just target ->
          responder (Right (InL
            [ DocumentHighlight range (Just DocumentHighlightKind_Text)
            | (loc, found) <- indexRefs idx
            , found == target
            , Just (Location _ range) <- [loc2Location loc] ]))

-- | The definitions of the project whose name matches what is typed, which is
-- what answers the "go to symbol in workspace" of the editor.
requestWorkspaceSymbols :: Handlers HandlerM
requestWorkspaceSymbols = requestHandler SMethod_WorkspaceSymbol $ \req responder -> do
  let query = T.toLower (req ^. J.params . J.query)
  modules <- gets project_modules
  responder (Right (InL
    [ SymbolInformation (T.pack ident) SymbolKind_Object Nothing Nothing Nothing lspLoc
    | loadedModule <- M.elems modules
    , (ident, loc) <- M.toList (indexTopLevel (moduleIndex loadedModule))
    , T.null query || query `T.isInfixOf` T.toLower (T.pack ident)
    , Just lspLoc <- [loc2Location loc] ]))

-- | The type of what the cursor rests on.
requestHover :: Handlers HandlerM
requestHover = requestHandler SMethod_TextDocumentHover $ \req responder -> do
  let fileURI = req ^. J.params . J.textDocument . J.uri
      pos = req ^. J.params . J.position
      here = (fromIntegral (pos ^. J.line) + 1, fromIntegral (pos ^. J.character) + 1)
  modules <- gets project_modules
  case uriToFilePath fileURI >>= flip M.lookup modules of
    Nothing -> responder (Right (InR Null))
    Just loadedModule ->
      case typeAt (moduleIndex loadedModule) here of
        Nothing -> responder (Right (InR Null))
        Just rendered ->
          responder (Right (InL (Hover
            (InL (MarkupContent MarkupKind_Markdown ("```termina\n" <> rendered <> "\n```")))
            Nothing)))

initialized :: Handlers HandlerM
initialized = notificationHandler SMethod_Initialized $ \_msg -> do
  diags <- gets project_modules
  mapM_ (uncurry emitDiagnostics) (M.toList (diagnostics <$> diags))

didOpen :: Handlers HandlerM
didOpen = notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
    let fileURI = msg ^. J.params . J.textDocument . J.uri
    case uriToFilePath fileURI of
      Nothing ->
        sendNotification SMethod_WindowShowMessage
          (ShowMessageParams MessageType_Error $ T.pack ("Internal error: unknown file: " ++ show (uriToFilePath fileURI)))
      Just filePath -> do
        -- TODO: See if we can improve this. Now we load the module in all cases, so that we
        -- ensure that, even if the module was not previously loaded, it is now present as part
        -- of the project. Modules are loaded at the beginning, but only those that are
        -- referenced (either directly or indirectly, by the main app module). 
        cfg <- gets config
        _ <- loadTerminaModule filePath (sourceModulesFolder <$> cfg)
        typeProject

handlers :: Handlers HandlerM
handlers =
  mconcat
    [
      notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_not ->
        return ()
      , initialized
      , didOpen
      , requestDefinition
      , requestReferences
      , requestHighlight
      , requestWorkspaceSymbols
      , requestHover
      , documentChange
      , requestSymbols
    ]


