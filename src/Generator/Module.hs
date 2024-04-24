{-# LANGUAGE FlexibleContexts #-}

module Generator.Module where

import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Generator.Common
import Generator.TypeDefinition
import Generator.Global
import Generator.Function
import Modules.Modules
import System.Path
import Parser.Parsing
import Data.Text (unpack, pack, intercalate, replace, toUpper)


genModulePathName :: ModuleName -> ModuleMode -> FilePath
genModulePathName mn DirMod = toUnrootedFilePath (mn </> fragment "header")
genModulePathName mn SrcFile = toUnrootedFilePath mn

genModuleDefineLabel :: ModuleName -> ModuleMode -> String
genModuleDefineLabel mn mm =
    let filePath = case mm of
            DirMod -> map (pack . toUnrootedFilePath) (splitFragments (mn </> fragment "header" <.> FileExt "h"))
            SrcFile -> map (pack . toUnrootedFilePath) (splitFragments (mn <.> FileExt "h"))
    in
    unpack $ pack "__" <> intercalate (pack "__") (map (toUpper . replace (pack ".") (pack "_")) filePath) <> pack "__"

genInclude :: (ModuleName, ModuleMode) -> Bool -> CPreprocessorDirective
genInclude (mName, DirMod) before = CPPInclude False (toUnrootedFilePath (mName </> fragment "header" <.> FileExt "h")) (CAnnotations Internal (CPPDirectiveAnn before))
genInclude (mName, SrcFile) before = CPPInclude False (toUnrootedFilePath (mName <.> FileExt "h")) (CAnnotations Internal (CPPDirectiveAnn before))

genHeaderASTElement :: AnnASTElement SemanticAnns -> CHeaderGenerator [CFileItem]
genHeaderASTElement typedef@(TypeDefinition {}) = do
    cTypeDef <- genTypeDefinitionDecl typedef
    return $ CExtDecl <$> cTypeDef
genHeaderASTElement glb@(GlobalDeclaration {}) = genGlobalDecl glb
genHeaderASTElement func@(Function {}) = do
    cFunc <- genFunctionDecl func
    return $ CExtDecl <$> cFunc

genSourceASTElement :: AnnASTElement SemanticAnns -> CSourceGenerator [CFileItem]
genSourceASTElement typedef@(TypeDefinition (Class {}) _) = do
    cTypeDef <- genClassDefinition typedef
    return $ CExtDecl <$> cTypeDef
genSourceASTElement (TypeDefinition {}) = return []
genSourceASTElement glb@(GlobalDeclaration {}) = do
    cGlobal <- genGlobal glb
    return $ CExtDecl <$> cGlobal
genSourceASTElement func@(Function {}) = do
    cFunc <- genFunction func
    return $ CExtDecl <$> cFunc

genHeaderFile ::
    -- | Include option.h
    Bool
    -- | Module name
    -> ModuleName
    -- | Module mode
    -> ModuleMode
    -- | Import list
    -> [(ModuleName, ModuleMode)]
    -> AnnotatedProgram SemanticAnns
    -> CHeaderGenerator CFile
genHeaderFile includeOptionH mName mMode imports program = do
    let defineLabel = genModuleDefineLabel mName mMode
        includeList = map (CPPDirective . flip genInclude False) imports
    items <- concat <$> mapM genHeaderASTElement program
    return $ CHeaderFile (genModulePathName mName mMode) $
        [
            CPPDirective $ CPPIfNDef defineLabel (CAnnotations Internal (CPPDirectiveAnn False)),
            CPPDirective $ CPPDefine defineLabel Nothing (CAnnotations Internal (CPPDirectiveAnn False)),
            CPPDirective $ CPPInclude True "termina.h" (CAnnotations Internal (CPPDirectiveAnn True))
        ] ++ ([CPPDirective $ CPPInclude False "option.h" (CAnnotations Internal (CPPDirectiveAnn True)) | includeOptionH]) 
        ++ includeList ++ items ++ [
            CPPDirective $ CPPEndif (CAnnotations Internal (CPPDirectiveAnn True))
        ]

genSourceFile ::
    -- | Module name
    ModuleName
    -- | Typed Termina program
    -> AnnotatedProgram SemanticAnns
    -> CSourceGenerator CFile
genSourceFile mName program = do
    items <- concat <$> mapM genSourceASTElement program
    return $ CSourceFile (genModulePathName mName SrcFile) $
        CPPDirective (CPPInclude False (toUnrootedFilePath (mName <.> FileExt "h")) (CAnnotations Internal (CPPDirectiveAnn True)))
        : items