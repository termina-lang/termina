{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Status where

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

genStatusPathName :: FilePath
genStatusPathName = "status" <.> "h"

genStatusHeaderFile :: CGenerator CFile
genStatusHeaderFile = do
    let defineLabel = "__STATUS_H__"
    statusSet <- gets (S.filter (\case {
        TStruct _ -> False;
        TEnum _ -> False;
        _ -> True;
        }) . statusTypes . monadicTypes)
    items <- concat <$> traverse genStatusStruct (S.toList statusSet)
    return $ CHeaderFile genStatusPathName $
        [
            CPPDirective (CPPIfNDef defineLabel) (LocatedElement (CPPDirectiveAnn False) Internal),
            CPPDirective (CPPDefine defineLabel Nothing) (LocatedElement (CPPDirectiveAnn False) Internal),
            CPPDirective (CPPInclude True "termina.h") (LocatedElement (CPPDirectiveAnn True) Internal)
        ]
        ++ items ++ [
            CPPDirective CPPEndif (LocatedElement (CPPDirectiveAnn True) Internal)
        ]

runGenStatusHeaderFile ::
    TerminaConfig
    -> M.Map Identifier Integer
    -> QualifiedName
    -> MonadicTypes
    -> Either CGeneratorError CFile
runGenStatusHeaderFile config irqMap optionFileName opts =
    case runState (runExceptT genStatusHeaderFile)
        (CGeneratorEnv optionFileName S.empty opts config irqMap) of
    (Left err, _) -> Left err
    (Right file, _) -> Right file