{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Application.Initialization where

import Generator.LanguageC.AST
import Generator.CodeGen.Common
import System.FilePath
import ControlFlow.BasicBlocks.AST
import Semantic.Types
import Generator.CodeGen.Statement
import qualified Data.Map as M
import Control.Monad.Except (runExceptT)
import Configuration.Configuration
import Generator.LanguageC.Embedded
import Generator.Monadic (emptyMonadicTypes)
import Control.Monad.State
import qualified Data.Set as S
import Utils.Annotations
import Generator.CodeGen.Expression

genInitializeObj :: Location -> Bool -> Global SemanticAnn -> CGenerator [CCompoundBlockItem]
genInitializeObj loc before (Resource identifier ty@(TAtomic {}) (Just expr) _ _) = do
    cType <- genType noqual ty
    let cObj = identifier @: cType
    genAtomicInitialization loc before 0 cObj expr
genInitializeObj loc before (Resource identifier ty@(TAtomicArray {}) (Just expr) _ _) = do
    cType <- genType noqual ty
    let cObj = identifier @: cType
    genAtomicArrayInitialization loc before 0 cObj expr
genInitializeObj loc before (Resource identifier _ (Just expr) _ _) = do
    let cObj = identifier @: typeDef identifier
    genStructInitialization loc before 0 cObj expr
genInitializeObj loc before (Task identifier _ (Just expr) _ _) = do
    let cObj = identifier @: typeDef identifier
    genStructInitialization loc before 0 cObj expr
genInitializeObj loc before (Handler identifier _ (Just expr) _ _) = do
    let cObj = identifier @: typeDef identifier
    genStructInitialization loc before 0 cObj expr
genInitializeObj loc before (Emitter identifier _ (Just expr) _ _) = do
    let cObj = identifier @: typeDef identifier
    genStructInitialization loc before 0 cObj expr
genInitializeObj _ _ _ = return []

genInitFile :: QualifiedName -> [(QualifiedName, AnnotatedProgram SemanticAnn)] -> CGenerator CFile
genInitFile mName prjprogs = do
    items <- genItems (concat [objs | (_, objs) <- globals])
    let initFunction = [
                pre_cr $ function initFunctionName [] @-> void $ 
                    trail_cr . block $
                        items ++ [pre_cr (_return Nothing)]
            ]
    return $ CSourceFile mName (includeTermina : includes ++ initFunction)

    where
        globals = map (\(mn, elems) -> (mn, [g | (GlobalDeclaration g) <- elems])) prjprogs
        modsWithGlobals = filter (\(_, objs) -> not (null objs)) globals
        incs = map fst modsWithGlobals
        includes = genIncludes incs

        initFunctionName = namefy $ "termina_app" <::> "init_globals"

        includeTermina = CPPDirective (CPPInclude True ("termina" <.> "h")) (internalAnn (CPPDirectiveAnn True))

        -- Generate include preprocessor directives for the modules
        genIncludes :: [QualifiedName] -> [CFileItem]
        genIncludes [] = []
        genIncludes (inc:xincs) = 
            CPPDirective (CPPInclude False (inc <.> "h")) (internalAnn (CPPDirectiveAnn True)) : genIncludes' xincs
        
        genIncludes' :: [QualifiedName] -> [CFileItem]
        genIncludes' [] = []
        genIncludes' (inc:xincs) =
            CPPDirective (CPPInclude False (inc <.> "h")) (internalAnn (CPPDirectiveAnn False)) : genIncludes' xincs

        genItems :: [Global SemanticAnn] -> CGenerator [CCompoundBlockItem]
        genItems [] = return []
        genItems (obj:objs) = do
            items <- genInitializeObj Internal True obj
            rest <- concat <$> traverse (genInitializeObj Internal False) objs
            return $ items ++ rest

runGenInitFile :: 
    TerminaConfig 
    -> M.Map Identifier Integer
    -> QualifiedName 
    -> [(QualifiedName, AnnotatedProgram SemanticAnn)] -> Either CGeneratorError CFile 
runGenInitFile config irqMap initFilePath prjprogs = 
    case runState (runExceptT (genInitFile initFilePath prjprogs)) 
        (CGeneratorEnv initFilePath S.empty emptyMonadicTypes config irqMap) of
    (Left err, _) -> Left err
    (Right file, _) -> Right file