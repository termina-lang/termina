{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Initialization where

import Generator.LanguageC.AST
import Generator.CodeGen.Common
import System.FilePath
import Semantic.Monad
import AST.Seman
import Generator.CodeGen.Statement
import Modules.Modules
import qualified Data.Map as M
import Control.Monad.Reader (runReaderT)

genInitializeObj :: Bool -> Global SemanticAnn -> CSourceGenerator [CCompoundBlockItem]
genInitializeObj before (Resource identifier _ (Just expr) _ ann) = do
    let cAnn = buildGenericAnn ann
        cObj = CVar identifier cAnn
    fmap CBlockStmt <$> genStructInitialization before 0 cObj expr
genInitializeObj before (Task identifier _ (Just expr) _ ann) = do
    let cAnn = buildGenericAnn ann
        cObj = CVar identifier cAnn
    fmap CBlockStmt <$> genStructInitialization before 0 cObj expr
genInitializeObj before (Handler identifier _ (Just expr) _ ann) = do
    let cAnn = buildGenericAnn ann
        cObj = CVar identifier cAnn
    fmap CBlockStmt <$> genStructInitialization before 0 cObj expr
genInitializeObj before (Emitter identifier _ (Just expr) _ ann) = do
    let cAnn = buildGenericAnn ann
        cObj = CVar identifier cAnn
    fmap CBlockStmt <$> genStructInitialization before 0 cObj expr
genInitializeObj _ _ = return []

genInitFile :: QualifiedName -> [(QualifiedName, AnnotatedProgram SemanticAnn)] -> CSourceGenerator CFile
genInitFile mName prjprogs = do
    items <- genItems (concat [objs | (_, objs) <- globals])
    let cAnn = internalAnn CGenericAnn
        cStmtAnn = internalAnn (CStatementAnn True False)
        retTypeDecl = [CTypeSpec CVoidType]
        cReturn = [CBlockStmt $ CReturn Nothing cStmtAnn]
        initFunction = [ CExtDecl $ CFDefExt $ CFunDef retTypeDecl
            (CDeclarator (Just initFunctionName) [CFunDeclr [] [] cAnn] [] cAnn)
            (CCompound (items ++ cReturn) (internalAnn (CCompoundAnn False True)))
            (internalAnn (CDeclarationAnn True))]
    return $ CSourceFile mName (includeTermina : includes ++ initFunction)

    where
        globals = map (\(mn, elems) -> (mn, [g | (GlobalDeclaration g) <- elems])) prjprogs
        modsWithGlobals = filter (\(_, objs) -> not (null objs)) globals
        incs = map fst modsWithGlobals
        includes = map (\nm -> CPPDirective $ CPPInclude False (nm <.> "h") (internalAnn (CPPDirectiveAnn True))) incs

        initFunctionName = namefy $ "termina_app" <::> "init_globals"

        includeTermina = CPPDirective $ CPPInclude True ("termina" <.> "h") (internalAnn (CPPDirectiveAnn True))

        genItems :: [Global SemanticAnn] -> CSourceGenerator [CCompoundBlockItem]
        genItems [] = return []
        genItems (obj:objs) = do
            items <- genInitializeObj True obj
            rest <- concat <$> mapM (genInitializeObj False) objs
            return $ items ++ rest

runGenInitFile :: FilePath -> [(QualifiedName, AnnotatedProgram SemanticAnn)] -> Either CGeneratorError CFile 
runGenInitFile initFilePath prjprogs = runReaderT (genInitFile initFilePath prjprogs) M.empty