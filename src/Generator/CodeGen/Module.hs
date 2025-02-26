{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Module where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Semantic.Types
import Generator.CodeGen.Common
import Generator.CodeGen.TypeDefinition
import Generator.CodeGen.Global
import Generator.CodeGen.Function
import Modules.Modules
import Data.Text (unpack, pack, intercalate, replace, toUpper)
import System.FilePath
import qualified Data.Map as M
import Control.Monad.Reader (runReader)
import Utils.Annotations
import Control.Monad.Except
import Configuration.Configuration
import Generator.CodeGen.SystemCall (syscallFunctionsMap)


genModuleDefineLabel :: QualifiedName -> String
genModuleDefineLabel mn =
    let filePath = map (pack . dropTrailingPathSeparator) (splitPath (mn <.> "h"))
    in
    unpack $ pack "__" <> intercalate (pack "__") (map (toUpper . replace (pack ".") (pack "_")) filePath) <> pack "__"

genInclude :: QualifiedName -> Bool -> CFileItem
genInclude mName before = CPPDirective (CPPInclude False (mName <.> "h")) (LocatedElement (CPPDirectiveAnn before) Internal)

genHeaderASTElement :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genHeaderASTElement typedef@(TypeDefinition {}) = genTypeDefinitionDecl typedef
genHeaderASTElement glb@(GlobalDeclaration {}) = genGlobalDecl glb
genHeaderASTElement func@(Function {}) = do
    genFunctionDecl func

genSourceASTElement :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
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
    -> CGenerator CFile
genHeaderFile includeOptionH mName imports program = do
    let defineLabel = genModuleDefineLabel mName
        includeList = map (`genInclude` False) imports
    items <- concat <$> mapM genHeaderASTElement program
    return $ CHeaderFile mName $
        [
            CPPDirective (CPPIfNDef defineLabel) (LocatedElement (CPPDirectiveAnn False) Internal),
            CPPDirective (CPPDefine defineLabel Nothing) (LocatedElement (CPPDirectiveAnn False) Internal),
            CPPDirective (CPPInclude True "termina.h") (LocatedElement (CPPDirectiveAnn True) Internal)
        ] ++ ([CPPDirective (CPPInclude False "option.h") (LocatedElement (CPPDirectiveAnn True) Internal) | includeOptionH])
        ++ includeList ++ items ++ [
            CPPDirective CPPEndif (LocatedElement (CPPDirectiveAnn True) Internal)
        ]

genSourceFile ::
    -- | Module name
    QualifiedName
    -- | Typed Termina program
    -> AnnotatedProgram SemanticAnn
    -> CGenerator CFile
genSourceFile mName program = do
    items <- concat <$> mapM genSourceASTElement program
    return $ CSourceFile mName $
        CPPDirective (CPPInclude False (mName <.> "h")) (LocatedElement (CPPDirectiveAnn True) Internal)
        : items

runGenSourceFile :: TerminaConfig -> QualifiedName -> AnnotatedProgram SemanticAnn -> Either CGeneratorError CFile
runGenSourceFile config mName program = 
    runReader (runExceptT (genSourceFile mName program)) (CGeneratorEnv M.empty config syscallFunctionsMap) 

runGenHeaderFile :: TerminaConfig -> Bool -> QualifiedName -> [QualifiedName] -> AnnotatedProgram SemanticAnn -> OptionTypes -> Either CGeneratorError CFile
runGenHeaderFile config includeOptionH mName imports program opts = 
    runReader (runExceptT (genHeaderFile includeOptionH mName imports program)) 
        (CGeneratorEnv opts config syscallFunctionsMap)
