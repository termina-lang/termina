{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Option where

import Generator.LanguageC.AST
import Generator.CodeGen.Common
import Control.Monad.Reader
import System.Path
import Generator.CodeGen.TypeDefinition
import Semantic.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import AST.Seman
import Utils.Annotations

genOptionPathName :: FilePath
genOptionPathName = toUnrootedFilePath (fragment "option" <.> FileExt "h")

genSimpleOptionDefinition :: TypeSpecifier -> CHeaderGenerator [CFileItem]
genSimpleOptionDefinition = genOptionStruct (Located (STy SimpleStmtType) Internal)

genOptionHeaderFile :: CHeaderGenerator CFile
genOptionHeaderFile = do
    let defineLabel = "__OPTION_H__"
    optionMap <- ask
    items <- concat <$> mapM genSimpleOptionDefinition (concatMap S.toList (M.elems optionMap))
    return $ CHeaderFile genOptionPathName $
        [
            CPPDirective (CPPIfNDef defineLabel) (Located (CPPDirectiveAnn False) Internal),
            CPPDirective (CPPDefine defineLabel Nothing) (Located (CPPDirectiveAnn False) Internal),
            CPPDirective (CPPInclude True "termina.h") (Located (CPPDirectiveAnn True) Internal)
        ]
        ++ items ++ [
            CPPDirective CPPEndif (Located (CPPDirectiveAnn True) Internal)
        ]

runGenOptionHeaderFile :: OptionTypes -> Either CGeneratorError CFile
runGenOptionHeaderFile = runReaderT genOptionHeaderFile