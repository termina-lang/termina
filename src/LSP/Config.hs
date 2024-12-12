module LSP.Config where
    
import LSP.Monad
import Configuration.Configuration
import qualified Data.Yaml as YAML
import Control.Monad.IO.Class

loadConfig :: HandlerM (Either String TerminaConfig)
loadConfig = do
    config <- liftIO $ YAML.decodeFileEither "termina.yaml"
    case config of
        Left (YAML.InvalidYaml (Just (YAML.YamlException err))) -> return $ Left err
        Left err -> return $ Left (show err)
        Right c -> return $ Right c