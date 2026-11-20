{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Option where

import Generator.LanguageC.AST
import Generator.CodeGen.Common
import Configuration.Platform (Platform)
import System.FilePath
import Generator.CodeGen.TypeDefinition
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Elaboration.AST
import Utils.Annotations
import Control.Monad.Except
import Configuration.Configuration
import Generator.Monadic
import Control.Monad.State

genOptionPathName :: FilePath
genOptionPathName = "option" <.> "h"

genOptionHeaderFile :: CGenerator CFile
genOptionHeaderFile = do
    let defineLabel = "OPTION_H__"
    optionSet <- gets (S.filter (\case {
        TStruct _ -> False;
        TEnum _ -> False;
        _ -> True;
        }) . optionTypes . monadicTypes)
    items <- concat <$> traverse genOptionStruct (S.toList optionSet)
    return $ CHeaderFile genOptionPathName $
        [
            CPPDirective (CPPIfNDef defineLabel) (LocatedElement (CPPDirectiveAnn False Nothing) Internal),
            CPPDirective (CPPDefine defineLabel Nothing) (LocatedElement (CPPDirectiveAnn False Nothing) Internal),
            CPPDirective (CPPInclude True "termina.h") (LocatedElement (CPPDirectiveAnn True Nothing) Internal)
        ]
        ++ items ++ [
            CPPDirective CPPEndif (LocatedElement (CPPDirectiveAnn True Nothing) Internal)
        ]

runGenOptionHeaderFile ::
    TerminaConfig
    -> Platform
    -> QualifiedName
    -> MonadicTypes
    -> Either CGeneratorError CFile
runGenOptionHeaderFile config plt optionFileName opts =
    case runState (runExceptT genOptionHeaderFile)
        (CGeneratorEnv optionFileName S.empty opts config plt False Nothing M.empty) of
    (Left err, _) -> Left err
    (Right file, _) -> Right file