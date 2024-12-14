module LSP.Logging where

import Colog.Core (Severity (..), WithSeverity (..), (<&))
import Control.Monad (when)
import qualified Data.Text as T
import Language.LSP.Logging (logToLogMessage, logToShowMessage)
import Language.LSP.Server (MonadLsp)

-- Original code: 
-- https://github.com/fwcd/curry-language-server/blob/c230297065b8fa1fc085c14c1e1e1b22899d5636/src/Curry/LanguageServer/Utils/Logging.hs

logAt :: MonadLsp c m => Severity -> T.Text -> m ()
logAt sev msg = do
    when (sev >= Info) $
        logToLogMessage <& WithSeverity msg sev

showAt :: MonadLsp c m => Severity -> T.Text -> m ()
showAt sev msg = logToShowMessage <& WithSeverity msg sev

errorM :: MonadLsp c m => T.Text -> m ()
errorM = showAt Error

warnM :: MonadLsp c m => T.Text -> m ()
warnM = logAt Warning

infoM :: MonadLsp c m => T.Text -> m ()
infoM = logAt Info