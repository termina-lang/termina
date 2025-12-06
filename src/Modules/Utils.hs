module Modules.Utils where
import Parser.Errors
import Utils.Annotations
import System.FilePath
import qualified Data.List as L


buildModuleName :: Location -> [String] -> Either ParsingErrors QualifiedName
buildModuleName loc [] = Left $ annotateError loc EEmptyModuleName
buildModuleName loc [x] = Left $ annotateError loc (EInvalidModuleName x)
buildModuleName loc fs = buildModuleName' fs

  where

    buildModuleName' :: [String] -> Either ParsingErrors QualifiedName
    buildModuleName' [] = Left $ annotateError loc EEmptyModuleName
    buildModuleName' [x] = Right x
    buildModuleName' (x:xs) = (x </>) <$> buildModuleName' xs

qualifiedToModuleName :: QualifiedName -> String
qualifiedToModuleName qname = L.intercalate "." (splitDirectories qname)