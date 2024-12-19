module LSP.Utils  where

import LSP.Monad
import Data.Text
import qualified Language.LSP.Protocol.Types as LSP
import Language.LSP.Server
import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO
import Language.LSP.VFS

loadVSFile :: FilePath -> HandlerM Text
loadVSFile filePath = do
  -- First, we check if the file is loaded into the VFS
  let normUri = LSP.toNormalizedUri . LSP.filePathToUri $ filePath
  vfile <- getVirtualFile normUri
  case virtualFileText <$> vfile of
    Nothing -> liftIO $ TIO.readFile filePath
    Just contents -> return contents
