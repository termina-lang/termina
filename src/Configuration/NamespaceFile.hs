{-# LANGUAGE OverloadedStrings #-}
-- | The file that describes a namespace, "namespace.yaml", in the folder of
-- its modules, with one field, the description of what the namespace holds.
module Configuration.NamespaceFile (
    namespaceFileName,
    readNamespaceDescription
) where

import Data.Aeson.Types (parseEither, withObject, (.:))
import qualified Data.Text as T
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Configuration.Manifest (onlyFields)

-- | The name of the file that describes a namespace.
namespaceFileName :: FilePath
namespaceFileName = "namespace.yaml"

-- | The description of a namespace, given the folder of its modules: none
-- when the folder has no "namespace.yaml", and an error when the file cannot
-- be read.
readNamespaceDescription :: FilePath -> IO (Either String (Maybe T.Text))
readNamespaceDescription folder = do
    let path = folder </> namespaceFileName
    exists <- doesFileExist path
    if not exists
        then return (Right Nothing)
        else do
            decoded <- decodeFileEither path
            return $ case decoded of
                Left err -> Left (path ++ ": " ++ prettyPrintParseException err)
                Right value -> either (\err -> Left (path ++ ": " ++ err)) (Right . Just) $
                    parseEither (withObject "namespace" $ \o -> onlyFields ["description"] o >> o .: "description") value
