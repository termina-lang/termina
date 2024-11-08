{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Initialization where

import Generator.LanguageC.AST
import Generator.CodeGen.Common
import System.FilePath
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import Generator.CodeGen.Statement
import Modules.Modules
import qualified Data.Map as M
import Control.Monad.Reader (runReader)
import Control.Monad.Except (runExceptT)
import Command.Configuration

genInitializeObj :: Bool -> Global SemanticAnn -> CGenerator [CCompoundBlockItem]
genInitializeObj before (Resource identifier _ (Just expr) _ _) = do
    let cObj = CVar identifier (CTTypeDef identifier noqual)
    genStructInitialization before 0 cObj expr
genInitializeObj before (Task identifier _ (Just expr) _ _) = do
    let cObj = CVar identifier (CTTypeDef identifier noqual)
    genStructInitialization before 0 cObj expr
genInitializeObj before (Handler identifier _ (Just expr) _ _) = do
    let cObj = CVar identifier (CTTypeDef identifier noqual)
    genStructInitialization before 0 cObj expr
genInitializeObj before (Emitter identifier _ (Just expr) _ _) = do
    let cObj = CVar identifier (CTTypeDef identifier noqual)
    genStructInitialization before 0 cObj expr
genInitializeObj _ _ = return []

genInitFile :: QualifiedName -> [(QualifiedName, AnnotatedProgram SemanticAnn)] -> CGenerator CFile
genInitFile mName prjprogs = do
    items <- genItems (concat [objs | (_, objs) <- globals])
    let cStmtAnn = internalAnn (CStatementAnn True False)
        cReturn = [CBlockStmt $ CSReturn Nothing cStmtAnn]
        initFunction = [CFunctionDef Nothing (CFunction (CTVoid noqual) initFunctionName []
            (CSCompound (items ++ cReturn) (internalAnn (CCompoundAnn False True))))
            (internalAnn (CDeclarationAnn True))]
    return $ CSourceFile mName (includeTermina : includes ++ initFunction)

    where
        globals = map (\(mn, elems) -> (mn, [g | (GlobalDeclaration g) <- elems])) prjprogs
        modsWithGlobals = filter (\(_, objs) -> not (null objs)) globals
        incs = map fst modsWithGlobals
        includes = map (\nm -> CPPDirective (CPPInclude False (nm <.> "h")) (internalAnn (CPPDirectiveAnn True))) incs

        initFunctionName = namefy $ "termina_app" <::> "init_globals"

        includeTermina = CPPDirective (CPPInclude True ("termina" <.> "h")) (internalAnn (CPPDirectiveAnn True))

        genItems :: [Global SemanticAnn] -> CGenerator [CCompoundBlockItem]
        genItems [] = return []
        genItems (obj:objs) = do
            items <- genInitializeObj True obj
            rest <- concat <$> mapM (genInitializeObj False) objs
            return $ items ++ rest

runGenInitFile :: TerminaConfig -> FilePath -> [(QualifiedName, AnnotatedProgram SemanticAnn)] -> Either CGeneratorError CFile 
runGenInitFile params initFilePath prjprogs = runReader (runExceptT (genInitFile initFilePath prjprogs)) (CGeneratorEnv M.empty M.empty params)