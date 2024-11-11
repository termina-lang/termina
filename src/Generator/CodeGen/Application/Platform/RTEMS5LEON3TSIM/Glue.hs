{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Generator.CodeGen.Application.Platform.RTEMS5LEON3TSIM.Glue where

import Generator.LanguageC.AST
import qualified Data.Map as M
import Generator.CodeGen.Common
import Control.Monad.Reader (runReader)
import Modules.Modules (QualifiedName)
import Semantic.Types
import ControlFlow.Architecture.Types
import Generator.CodeGen.Application.OS.RTEMS.RTEMS5.Glue
import Control.Monad.Except
import Configuration.Configuration

runGenMainFile :: TerminaConfig -> QualifiedName -> TerminaProgArch SemanticAnn -> Either CGeneratorError CFile
runGenMainFile config mainFilePath progArchitecture = runReader (runExceptT (genMainFile mainFilePath progArchitecture)) (CGeneratorEnv M.empty M.empty config)
