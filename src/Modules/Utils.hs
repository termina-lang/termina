module Modules.Utils where
import Parser.Errors
import Modules.Modules
import Utils.Annotations
import System.FilePath


buildModuleName :: Location -> [String] -> Either ParsingErrors QualifiedName
buildModuleName loc [] = Left $ annotateError loc EEmptyModuleName
buildModuleName loc [x] = Left $ annotateError loc (EInvalidModuleName x)
buildModuleName loc fs = buildModuleName' fs

  where

    buildModuleName' :: [String] -> Either ParsingErrors QualifiedName
    buildModuleName' [] = Left $ annotateError loc EEmptyModuleName
    buildModuleName' [x] = Right x
    buildModuleName' (x:xs) = (x </>) <$> buildModuleName' xs