{-# LANGUAGE FlexibleContexts #-}

module Generator.Application.Initialization where

import Generator.LanguageC.AST
import Generator.Common
import Parser.Parsing
import System.Path
import Semantic.Monad
import AST.Seman
import Generator.Statement
import Modules.Modules
import Generator.Module

genInitializeObj :: Bool -> Global SemanticAnns -> CSourceGenerator [CCompoundBlockItem]
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

genInitFile :: ModuleName -> [(ModuleName, ModuleMode, AnnotatedProgram SemanticAnns)] -> CSourceGenerator CFile
genInitFile mName prjprogs = do
    items <- genItems (concat [objs | (_, _, objs) <- globals])
    let cAnn = CAnnotations Internal CGenericAnn
        cStmtAnn = CAnnotations Internal (CStatementAnn True False)
        retTypeDecl = [CTypeSpec CVoidType]
        cReturn = [CBlockStmt $ CReturn Nothing cStmtAnn]
        initFunction = [ CExtDecl $ CFDefExt $ CFunDef retTypeDecl
            (CDeclarator (Just initFunctionName) [CFunDeclr [] [] cAnn] [] cAnn)
            (CCompound (items ++ cReturn) (CAnnotations Internal (CCompoundAnn False True)))
            (CAnnotations Internal (CDeclarationAnn True))]
    return $ CSourceFile (genModulePathName mName SrcFile) (includeTermina : includes ++ initFunction)

    where
        globals = map (\(mn, mm, elems) -> (mn, mm, [g | (GlobalDeclaration g) <- elems])) prjprogs
        modsWithGlobals = filter (\(_, _, objs) -> not (null objs)) globals
        incs = map (\(nm, mm, _) -> (nm, mm)) modsWithGlobals
        includes = map (\(nm, _) -> CPPDirective $ CPPInclude False (toUnrootedFilePath (nm <.> FileExt "h")) (CAnnotations Internal (CPPDirectiveAnn True))) incs

        initFunctionName = namefy $ "termina_app" <::> "init_globals"

        includeTermina = CPPDirective $ CPPInclude True (toUnrootedFilePath (fragment "termina" <.> FileExt "h")) (CAnnotations Internal (CPPDirectiveAnn True))

        genItems :: [Global SemanticAnns] -> CSourceGenerator [CCompoundBlockItem]
        genItems [] = return []
        genItems (obj:objs) = do
            items <- genInitializeObj True obj
            rest <- concat <$> mapM (genInitializeObj False) objs
            return $ items ++ rest
