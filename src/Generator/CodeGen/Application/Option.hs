{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Option where

import Generator.LanguageC.AST
import Generator.CodeGen.Common
import System.FilePath
import Generator.CodeGen.TypeDefinition
import qualified Data.Set as S
import qualified Data.Map as M
import ControlFlow.BasicBlocks.AST
import Utils.Annotations
import Control.Monad.Except
import Configuration.Configuration
import Generator.Monadic
import Control.Monad.State
import Modules.Modules

genOptionPathName :: FilePath
genOptionPathName = "option" <.> "h"

genOptionHeaderFile :: CGenerator CFile
genOptionHeaderFile = do
    let defineLabel = "__OPTION_H__"
    optionSet <- gets (S.filter (\case {
        TStruct _ -> False;
        TEnum _ -> False;
        _ -> True;
        }) . optionTypes . monadicTypes)
    items <- concat <$> traverse genOptionStruct (S.toList optionSet)
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
    -> QualifiedName
    -> MonadicTypes
    -> Either CGeneratorError CFile
runGenOptionHeaderFile config irqMap optionFileName opts =
    case runState (runExceptT genOptionHeaderFile)
        (CGeneratorEnv optionFileName S.empty opts config irqMap) of
    (Left err, _) -> Left err
    (Right file, _) -> Right file