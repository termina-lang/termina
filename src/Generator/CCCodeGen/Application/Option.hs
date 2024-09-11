{-# LANGUAGE FlexibleContexts #-}

module Generator.CCCodeGen.Application.Option where

import Generator.LanguageC.CompCertC
import Generator.CCCodeGen.Common
import Control.Monad.Reader
import System.Path
import Generator.CCCodeGen.TypeDefinition
import Semantic.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import AST.Seman
import Utils.Annotations

genOptionPathName :: FilePath
genOptionPathName = toUnrootedFilePath (fragment "option" <.> FileExt "h")

genSimpleOptionDefinition :: TypeSpecifier -> CHeaderGenerator [CFileItem]
genSimpleOptionDefinition ts = do
    optionStruct <- genOptionStruct (Located (STy SimpleStmtType) Internal) ts
    return $ CExtDecl <$> optionStruct

genOptionHeaderFile :: CHeaderGenerator CFile
genOptionHeaderFile = do
    let defineLabel = "__OPTION_H__"
    optionMap <- ask
    items <- concat <$> mapM genSimpleOptionDefinition (concatMap S.toList (M.elems optionMap))
    return $ CHeaderFile genOptionPathName $
        [
            CPPDirective $ CPPIfNDef defineLabel (Located (CPPDirectiveAnn False) Internal),
            CPPDirective $ CPPDefine defineLabel Nothing (Located (CPPDirectiveAnn False) Internal),
            CPPDirective $ CPPInclude True "termina.h" (Located (CPPDirectiveAnn True) Internal)
        ]
        ++ items ++ [
            CPPDirective $ CPPEndif (Located (CPPDirectiveAnn True) Internal)
        ]

runGenOptionHeaderFile :: OptionTypes -> Either CGeneratorError CFile
runGenOptionHeaderFile = runReaderT genOptionHeaderFile