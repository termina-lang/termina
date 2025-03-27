{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Option where

import Generator.LanguageC.AST
import Generator.CodeGen.Common
import Control.Monad.Reader
import System.FilePath
import Generator.CodeGen.TypeDefinition
import Semantic.Types
import qualified Data.Set as S
import qualified Data.Map as M
import ControlFlow.BasicBlocks.AST
import Utils.Annotations
import Control.Monad.Except
import Configuration.Configuration
import Generator.CodeGen.SystemCall

genOptionPathName :: FilePath
genOptionPathName = "option" <.> "h"

genSimpleOptionDefinition :: TerminaType -> CGenerator [CFileItem]
genSimpleOptionDefinition = genOptionStruct (LocatedElement (STy SimpleStmtType) Internal)

genOptionHeaderFile :: CGenerator CFile
genOptionHeaderFile = do
    let defineLabel = "__OPTION_H__"
    optionMap <- asks optionTypes
    items <- concat <$> mapM genSimpleOptionDefinition (concatMap S.toList (M.elems optionMap))
    return $ CHeaderFile genOptionPathName $
        [
            CPPDirective (CPPIfNDef defineLabel) (LocatedElement (CPPDirectiveAnn False) Internal),
            CPPDirective (CPPDefine defineLabel Nothing) (LocatedElement (CPPDirectiveAnn False) Internal),
            CPPDirective (CPPInclude True "termina.h") (LocatedElement (CPPDirectiveAnn True) Internal)
        ]
        ++ items ++ [
            CPPDirective CPPEndif (LocatedElement (CPPDirectiveAnn True) Internal)
        ]

runGenOptionHeaderFile :: 
    TerminaConfig 
    -> M.Map Identifier Integer
    -> OptionTypes 
    -> Either CGeneratorError CFile
runGenOptionHeaderFile config irqMap opts = 
    runReader (runExceptT genOptionHeaderFile) 
        (CGeneratorEnv opts config syscallFunctionsMap irqMap)