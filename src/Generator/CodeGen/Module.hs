{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Module where

import AST.Seman
import Generator.LanguageC.AST
import Semantic.Monad
import Generator.CodeGen.Common
import Generator.CodeGen.TypeDefinition
import Generator.CodeGen.Global
import Generator.CodeGen.Function
import Modules.Modules
import Parser.Parsing
import Data.Text (unpack, pack, intercalate, replace, toUpper)
import System.FilePath
import qualified Data.Map as M
import Control.Monad.Reader (runReaderT)


genModuleDefineLabel :: ModuleName -> String
genModuleDefineLabel mn =
    let filePath = map (pack . dropTrailingPathSeparator) (splitPath (mn <.> "h"))
    in
    unpack $ pack "__" <> intercalate (pack "__") (map (toUpper . replace (pack ".") (pack "_")) filePath) <> pack "__"

genInclude :: ModuleName -> Bool -> CPreprocessorDirective
genInclude mName before = CPPInclude False (mName <.> "h") (CAnnotations Internal (CPPDirectiveAnn before))

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
    -- | Import list
    -> [ModuleName]
    -> AnnotatedProgram SemanticAnns
    -> CHeaderGenerator CFile
genHeaderFile includeOptionH mName imports program = do
    let defineLabel = genModuleDefineLabel mName
        includeList = map (CPPDirective . flip genInclude False) imports
    items <- concat <$> mapM genHeaderASTElement program
    return $ CHeaderFile mName $
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
    return $ CSourceFile mName $
        CPPDirective (CPPInclude False (mName <.> "h") (CAnnotations Internal (CPPDirectiveAnn True)))
        : items

runGenSourceFile :: ModuleName -> AnnotatedProgram SemanticAnns -> Either CGeneratorError CFile
runGenSourceFile mName program = runReaderT (genSourceFile mName program) M.empty

runGenHeaderFile :: Bool -> ModuleName -> [ModuleName] -> AnnotatedProgram SemanticAnns -> OptionTypes -> Either CGeneratorError CFile
runGenHeaderFile includeOptionH mName imports program = runReaderT (genHeaderFile includeOptionH mName imports program)
