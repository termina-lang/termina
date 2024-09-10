{-# LANGUAGE FlexibleContexts #-}

module Generator.CCCodeGen.Module where

import AST.Seman
import Generator.LanguageC.CompCertC
import Semantic.Monad
import Generator.CCCodeGen.Common
import Generator.CCCodeGen.TypeDefinition
import Generator.CCCodeGen.Global
import Generator.CCCodeGen.Function
import Modules.Modules
import Data.Text (unpack, pack, intercalate, replace, toUpper)
import System.FilePath
import qualified Data.Map as M
import Control.Monad.Reader (runReaderT)
import Utils.Annotations


genModuleDefineLabel :: QualifiedName -> String
genModuleDefineLabel mn =
    let filePath = map (pack . dropTrailingPathSeparator) (splitPath (mn <.> "h"))
    in
    unpack $ pack "__" <> intercalate (pack "__") (map (toUpper . replace (pack ".") (pack "_")) filePath) <> pack "__"

genInclude :: QualifiedName -> Bool -> CPreprocessorDirective
genInclude mName before = CPPInclude False (mName <.> "h") (Located (CPPDirectiveAnn before) Internal)

genHeaderASTElement :: AnnASTElement SemanticAnn -> CHeaderGenerator [CFileItem]
genHeaderASTElement typedef@(TypeDefinition {}) = genTypeDefinitionDecl typedef
genHeaderASTElement glb@(GlobalDeclaration {}) = genGlobalDecl glb
genHeaderASTElement func@(Function {}) = do
    cFunc <- genFunctionDecl func
    return $ CExtDecl <$> cFunc

genSourceASTElement :: AnnASTElement SemanticAnn -> CSourceGenerator [CFileItem]
genSourceASTElement typedef@(TypeDefinition (Class {}) _) = genClassDefinition typedef
genSourceASTElement (TypeDefinition {}) = return []
genSourceASTElement glb@(GlobalDeclaration {}) = genGlobal glb
genSourceASTElement func@(Function {}) = genFunction func

genHeaderFile ::
    -- | Include option.h
    Bool
    -- | Module name
    -> QualifiedName
    -- | Import list
    -> [QualifiedName]
    -> AnnotatedProgram SemanticAnn
    -> CHeaderGenerator CFile
genHeaderFile includeOptionH mName imports program = do
    let defineLabel = genModuleDefineLabel mName
        includeList = map (CPPDirective . flip genInclude False) imports
    items <- concat <$> mapM genHeaderASTElement program
    return $ CHeaderFile mName $
        [
            CPPDirective $ CPPIfNDef defineLabel (Located (CPPDirectiveAnn False) Internal),
            CPPDirective $ CPPDefine defineLabel Nothing (Located (CPPDirectiveAnn False) Internal),
            CPPDirective $ CPPInclude True "termina.h" (Located (CPPDirectiveAnn True) Internal)
        ] ++ ([CPPDirective $ CPPInclude False "option.h" (Located (CPPDirectiveAnn True) Internal) | includeOptionH]) 
        ++ includeList ++ items ++ [
            CPPDirective $ CPPEndif (Located (CPPDirectiveAnn True) Internal)
        ]

genSourceFile ::
    -- | Module name
    QualifiedName
    -- | Typed Termina program
    -> AnnotatedProgram SemanticAnn
    -> CSourceGenerator CFile
genSourceFile mName program = do
    items <- concat <$> mapM genSourceASTElement program
    return $ CSourceFile mName $
        CPPDirective (CPPInclude False (mName <.> "h") (Located (CPPDirectiveAnn True) Internal))
        : items

runGenSourceFile :: QualifiedName -> AnnotatedProgram SemanticAnn -> Either CGeneratorError CFile
runGenSourceFile mName program = runReaderT (genSourceFile mName program) M.empty

runGenHeaderFile :: Bool -> QualifiedName -> [QualifiedName] -> AnnotatedProgram SemanticAnn -> OptionTypes -> Either CGeneratorError CFile
runGenHeaderFile includeOptionH mName imports program = runReaderT (genHeaderFile includeOptionH mName imports program)
