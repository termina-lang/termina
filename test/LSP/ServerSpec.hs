{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The language server, driven as an editor drives it: a session opens the
-- files of a project, changes them and asks about them, and checks what the
-- server answers.
module LSP.ServerSpec (spec) where

import LSP.Server (lspRunServerWith)

import Control.Applicative.Combinators (skipManyTill)
import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Row ((.==))
import qualified Data.Text as T
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Message (SMethod(SMethod_TextDocumentPublishDiagnostics))
import Language.LSP.Protocol.Types
import Language.LSP.Test
import System.Directory
import System.FilePath
import System.Process (createPipe)
import Test.Hspec

-- | The main module of the application, which calls a function of src/ and
-- one of the library of the local folder ../lib.
appModule :: T.Text
appModule = T.unlines
  [ "import util.twice;"
  , "import lib.half;"
  , ""
  , "handler class CInit {"
  , "    count : u32;"
  , "    init_ev : sink TimeVal triggers go;"
  , ""
  , "    action go(&priv self, _now : TimeVal) {"
  , "        self->count = twice(half(self->count));"
  , "        return;"
  , "    }"
  , "};"
  , ""
  , "handler init : CInit = { count = 0, init_ev <- system_init };" ]

-- | An application in app/ and a library in lib/, which the application
-- declares as a local folder.
project :: [(FilePath, T.Text)]
project =
  [ ("app/termina.yaml", T.unlines
      [ "project:", "  name: app", "  version: 0.1.0", "  manifest-version: 1"
      , "app:", "  platform: posix-gcc", "  profile: release"
      , "dependencies:", "  - path: ../lib" ])
  , ("app/src/util/twice.fin", "function twice(x : u32) -> u32 {\n    return x + x;\n}\n")
  , ("app/app/app.fin", appModule)
  , ("lib/termina.yaml", T.unlines
      [ "project:", "  name: lib", "  version: 0.1.0", "  manifest-version: 1"
      , "exports: [lib]" ])
  , ("lib/src/lib/half.fin", "function half(x : u32) -> u32 {\n    return x / 2;\n}\n") ]

-- | Run a session against a server on the application of the project.
withServer :: (FilePath -> Session a) -> IO a
withServer = withServerIn "app"

-- | Run a session against a server on a folder of the project, written to a
-- temporary folder. The server runs in this process, through two pipes, with
-- that folder as its working folder, as the editor starts it. The session is
-- given the folder that holds the project.
withServerIn :: FilePath -> (FilePath -> Session a) -> IO a
withServerIn folder session = do
    tmp <- getTemporaryDirectory >>= canonicalizePath
    let base = tmp </> "termina-lsp-spec"
        root = base </> folder
    removePathForcibly base
    forM_ project $ \(path, text) -> do
        createDirectoryIfMissing True (base </> takeDirectory path)
        writeFile (base </> path) (T.unpack text)
    withCurrentDirectory root $ do
        (serverIn, clientOut) <- createPipe
        (clientIn, serverOut) <- createPipe
        bracket (forkIO (lspRunServerWith serverIn serverOut)) killThread $ \_ ->
            runSessionWithHandles clientOut clientIn defaultConfig { messageTimeout = 10 } fullCaps root (session base)

-- | A change that replaces the whole text of a document.
replaceAll :: T.Text -> TextDocumentContentChangeEvent
replaceAll text = TextDocumentContentChangeEvent (InR (#text .== text))

-- | The next diagnostics that the server publishes for a document.
diagnosticsOf :: TextDocumentIdentifier -> Session [Diagnostic]
diagnosticsOf doc = do
    published <- skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics)
    let params = published ^. L.params
    if params ^. L.uri == doc ^. L.uri
        then return (params ^. L.diagnostics)
        else diagnosticsOf doc

-- | The codes of the first diagnostics that the server publishes for a
-- document and that are not empty.
errorCodesOf :: TextDocumentIdentifier -> Session [T.Text]
errorCodesOf doc = do
    diags <- diagnosticsOf doc
    if null diags
        then errorCodesOf doc
        else return [ code | Just (InR code) <- map (^. L.code) diags ]

-- | The position of the first occurrence of a piece of text in a text.
positionOf :: T.Text -> T.Text -> Position
positionOf needle text =
    case [ (n, T.length prefix) | (n, line) <- zip [0 ..] (T.lines text), let (prefix, rest) = T.breakOn needle line, not (T.null rest) ] of
        ((n, column) : _) -> Position n (fromIntegral column)
        [] -> error ("no " ++ T.unpack needle)

spec :: Spec
spec = describe "language server" $ do

  it "marks a type error in a file that is not saved, and clears it once it is fixed" $
    withServer $ \_ -> do
      twice <- openDoc "src/util/twice.fin" "termina"
      changeDoc twice [replaceAll "function twice(x : u32) -> u32 {\n    return true;\n}\n"]
      codes <- errorCodesOf twice
      changeDoc twice [replaceAll "function twice(x : u32) -> u32 {\n    return x + x;\n}\n"]
      cleared <- diagnosticsOf twice
      liftIO $ do
        codes `shouldBe` ["SE-238"]
        cleared `shouldBe` []

  it "keeps the text of a file saved and closed, read again from disk" $
    withServer $ \base -> do
      let broken = "function twice(x : u32) -> u32 {\n    return true;\n}\n"
      twice <- openDoc "src/util/twice.fin" "termina"
      changeDoc twice [replaceAll broken]
      _ <- errorCodesOf twice
      liftIO (writeFile (base </> "app" </> "src" </> "util" </> "twice.fin") (T.unpack broken))
      closeDoc twice
      codes <- errorCodesOf twice
      liftIO (codes `shouldBe` ["SE-238"])

  it "marks an error in a module of a local folder" $
    withServer $ \base -> do
      half <- openDoc (base </> "lib" </> "src" </> "lib" </> "half.fin") "termina"
      changeDoc half [replaceAll "function half(x : u32) -> u32 {\n    return true;\n}\n"]
      codes <- errorCodesOf half
      liftIO (codes `shouldBe` ["SE-238"])

  it "marks a type error in an exported module of a library" $
    withServerIn "lib" $ \_ -> do
      half <- openDoc "src/lib/half.fin" "termina"
      changeDoc half [replaceAll "function half(x : u32) -> u32 {\n    return true;\n}\n"]
      codes <- errorCodesOf half
      liftIO (codes `shouldBe` ["SE-238"])

  it "marks an import of a module that no folder of the build has" $
    withServer $ \_ -> do
      app <- openDoc "app/app.fin" "termina"
      changeDoc app [replaceAll ("import util.nothing;\n" <> appModule)]
      codes <- errorCodesOf app
      liftIO (codes `shouldBe` ["PE-003"])

  it "marks an error of the architecture" $
    withServer $ \_ -> do
      app <- openDoc "app/app.fin" "termina"
      changeDoc app [replaceAll (appModule <> "handler other : CInit = { count = 0, init_ev <- system_init };\n")]
      codes <- errorCodesOf app
      liftIO (codes `shouldBe` ["AE-001"])

  it "gives the type of a field on a hover" $
    withServer $ \_ -> do
      app <- openDoc "app/app.fin" "termina"
      assigned <- getHover app (positionOf "count = twice" appModule)
      passed <- getHover app (positionOf "count))" appModule)
      onSelf <- getHover app (positionOf "self->count = twice" appModule)
      liftIO $ do
        fmap (^. L.contents) assigned `shouldBe` Just (InL (MarkupContent MarkupKind_Markdown "```termina\nu32\n```"))
        fmap (^. L.contents) passed `shouldBe` Just (InL (MarkupContent MarkupKind_Markdown "```termina\nu32\n```"))
        fmap (^. L.contents) onSelf `shouldNotBe` fmap (^. L.contents) assigned

  it "keeps the hover of a module while it has a type error" $
    withServer $ \_ -> do
      app <- openDoc "app/app.fin" "termina"
      let broken = T.replace "twice(half" "thrice(half" appModule
      changeDoc app [replaceAll broken]
      codes <- errorCodesOf app
      kept <- getHover app (positionOf "count = thrice" broken)
      liftIO $ do
        codes `shouldBe` ["SE-092"]
        fmap (^. L.contents) kept `shouldBe` Just (InL (MarkupContent MarkupKind_Markdown "```termina\nu32\n```"))

  it "goes to the definition of a function of a local folder" $
    withServer $ \base -> do
      app <- openDoc "app/app.fin" "termina"
      definitions <- getDefinitions app (positionOf "half(" appModule)
      liftIO $ case definitions of
        InL (Definition (InL location)) ->
          uriToFilePath (location ^. L.uri) `shouldBe` Just (base </> "lib" </> "src" </> "lib" </> "half.fin")
        other -> expectationFailure ("no single definition: " ++ show other)
