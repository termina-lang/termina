{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The language server, driven as an editor drives it: a session opens the
-- files of a project, changes them and asks about them, and checks what the
-- server answers.
module LSP.ServerSpec (spec) where

import LSP.Server (lspRunServerWith)

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Applicative.Combinators (skipManyTill)
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

-- | An application with a function in src/ that app.fin calls.
application :: [(FilePath, String)]
application =
  [ ("termina.yaml", unlines
      [ "project:", "  name: app", "  version: 0.1.0", "  manifest-version: 1"
      , "app:", "  platform: posix-gcc", "  profile: release" ])
  , ("src/util/twice.fin", unlines
      [ "function twice(x : u32) -> u32 {"
      , "    return x + x;"
      , "}" ])
  , ("app/app.fin", unlines
      [ "import util.twice;"
      , ""
      , "handler class CInit {"
      , "    count : u32;"
      , "    init_ev : sink TimeVal triggers go;"
      , ""
      , "    action go(&priv self, _now : TimeVal) {"
      , "        self->count = twice(self->count);"
      , "        return;"
      , "    }"
      , "};"
      , ""
      , "handler init : CInit = { count = 0, init_ev <- system_init };" ]) ]

-- | Run a session against a server on a project, written to a temporary
-- folder. The server runs in this process, through two pipes, with the folder
-- of the project as its working folder, as the editor starts it.
withServerIn :: [(FilePath, String)] -> Session a -> IO a
withServerIn files session = do
    tmp <- getTemporaryDirectory >>= canonicalizePath
    let root = tmp </> "termina-lsp-spec"
    removePathForcibly root
    forM_ files $ \(path, text) -> do
        createDirectoryIfMissing True (root </> takeDirectory path)
        writeFile (root </> path) text
    withCurrentDirectory root $ do
        (serverIn, clientOut) <- createPipe
        (clientIn, serverOut) <- createPipe
        bracket (forkIO (lspRunServerWith serverIn serverOut)) killThread $ \_ ->
            runSessionWithHandles clientOut clientIn defaultConfig { messageTimeout = 10 } fullCaps root session

-- | A change that replaces the whole text of a document.
replaceAll :: T.Text -> TextDocumentContentChangeEvent
replaceAll text = TextDocumentContentChangeEvent (InR (#text .== text))

-- | The codes of the first diagnostics that the server publishes for a
-- document and that are not empty.
errorCodesOf :: TextDocumentIdentifier -> Session [T.Text]
errorCodesOf doc = do
    published <- skipManyTill anyMessage (message SMethod_TextDocumentPublishDiagnostics)
    let params = published ^. L.params
    if params ^. L.uri == doc ^. L.uri && not (null (params ^. L.diagnostics))
        then return [ code | Just (InR code) <- map (^. L.code) (params ^. L.diagnostics) ]
        else errorCodesOf doc

spec :: Spec
spec = describe "language server" $ do

  it "publishes no diagnostics for a project without errors" $
    withServerIn application $ do
      _ <- openDoc "app/app.fin" "termina"
      diags <- waitForDiagnostics
      liftIO (diags `shouldBe` [])

  it "marks a type error in a file that is not saved" $
    withServerIn application $ do
      _ <- openDoc "app/app.fin" "termina"
      twice <- openDoc "src/util/twice.fin" "termina"
      changeDoc twice [replaceAll "function twice(x : u32) -> u32 {\n    return true;\n}\n"]
      codes <- errorCodesOf twice
      liftIO (codes `shouldBe` ["SE-238"])
