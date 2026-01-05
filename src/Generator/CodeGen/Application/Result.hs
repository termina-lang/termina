{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Result where

import Generator.LanguageC.AST
import Generator.CodeGen.Common
import System.FilePath
import Generator.CodeGen.TypeDefinition
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import ControlFlow.BasicBlocks.AST
import Utils.Annotations
import Control.Monad.Except
import Configuration.Configuration
import Generator.Monadic
import Control.Monad.State

genResultPathName :: FilePath
genResultPathName = "result" <.> "h"

genResultHeaderFile :: CGenerator CFile
genResultHeaderFile = do
    let defineLabel = "__RESULT_H__"
    resultSet <- gets (S.unions . M.elems . M.filterWithKey (\k _ -> case k of {
        TStruct _ -> False;
        TEnum _ -> False;
        _ -> True;
        }) . resultTypes . monadicTypes)
    items <- concat <$> traverse (uncurry genResultStruct) (S.toList resultSet)
    return $ CHeaderFile genResultPathName $
        [
            CPPDirective (CPPIfNDef defineLabel) (LocatedElement (CPPDirectiveAnn False) Internal),
            CPPDirective (CPPDefine defineLabel Nothing) (LocatedElement (CPPDirectiveAnn False) Internal),
            CPPDirective (CPPInclude True "termina.h") (LocatedElement (CPPDirectiveAnn True) Internal)
        ]
        ++ items ++ [
            CPPDirective CPPEndif (LocatedElement (CPPDirectiveAnn True) Internal)
        ]

runGenResultHeaderFile ::
    TerminaConfig
    -> M.Map Identifier Integer
    -> QualifiedName
    -> MonadicTypes
    -> Either CGeneratorError CFile
runGenResultHeaderFile config irqMap resultFileName monadicTys =
    case runState (runExceptT genResultHeaderFile)
        (CGeneratorEnv resultFileName S.empty monadicTys config irqMap) of
    (Left err, _) -> Left err
    (Right file, _) -> Right file