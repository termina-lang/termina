{-# LANGUAGE FlexibleContexts #-}

module Generator.Application.Option where

import Generator.LanguageC.AST
import Generator.Common
import Parser.Parsing
import Control.Monad.Reader
import System.Path
import AST.Core ( TypeSpecifier(Option) )
import Generator.TypeDefinition
import Semantic.Monad
import qualified Data.Set as S
import qualified Data.Map as M

genOptionPathName :: FilePath
genOptionPathName = toUnrootedFilePath (fragment "option" <.> FileExt "h")

genSimpleOptionDefinition :: TypeSpecifier -> CHeaderGenerator [CFileItem]
genSimpleOptionDefinition ts = do
    optionStruct <- genOptionStruct (SemAnn Internal STy) ts
    return $ CExtDecl <$> optionStruct

genOptionHeaderFile :: CHeaderGenerator CFile
genOptionHeaderFile = do
    let defineLabel = "__OPTION_H__"
    optionMap <- ask
    items <- concat <$> mapM genSimpleOptionDefinition (concatMap S.toList (M.elems optionMap))
    return $ CHeaderFile genOptionPathName $
        [
            CPPDirective $ CPPIfNDef defineLabel (CAnnotations Internal (CPPDirectiveAnn False)),
            CPPDirective $ CPPDefine defineLabel Nothing (CAnnotations Internal (CPPDirectiveAnn False)),
            CPPDirective $ CPPInclude True "termina.h" (CAnnotations Internal (CPPDirectiveAnn True))
        ]
        ++ items ++ [
            CPPDirective $ CPPEndif (CAnnotations Internal (CPPDirectiveAnn True))
        ]
