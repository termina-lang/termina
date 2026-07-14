{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Module where

import ControlFlow.BasicBlocks.AST
import Generator.LanguageC.AST
import Semantic.Types
import Generator.CodeGen.Common
import Configuration.Platform (Platform, maxIdentifierLength)
import Generator.CodeGen.TypeDefinition
import Generator.CodeGen.Global
import Generator.CodeGen.Function
import Data.Text (unpack, pack, intercalate, replace, toUpper)
import System.FilePath
import qualified Data.Map.Strict as M
import Utils.Annotations
import Control.Monad.Except
import Configuration.Configuration
import Generator.Monadic
import Control.Monad.State
import qualified Data.Set as S


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
    -- | Include status.h
    -> Bool
    -- | Include result.h
    -> Bool
    -- | Module name
    -> QualifiedName
    -- | Import list
    -> [QualifiedName]
    -> AnnotatedProgram SemanticAnn
    -> CGenerator CFile
genHeaderFile includeOptionH includeStatusH includeResultH mName imports program = do
    let defineLabel = genModuleDefineLabel mName
    items <- concat <$> traverse genHeaderASTElement program
    extra <- gets extraImports
    let includeList = genIncludeList (S.toList (S.union (S.fromList imports) extra))
    let file = CHeaderFile mName $
            [
                CPPDirective (CPPIfNDef defineLabel) (LocatedElement (CPPDirectiveAnn False) Internal),
                CPPDirective (CPPDefine defineLabel Nothing) (LocatedElement (CPPDirectiveAnn False) Internal),
                CPPDirective (CPPInclude True "termina.h") (LocatedElement (CPPDirectiveAnn True) Internal)
            ] ++ includeList
            ++ ([CPPDirective (CPPInclude False "option.h") (LocatedElement (CPPDirectiveAnn True) Internal) | includeOptionH])
            ++ ([CPPDirective (CPPInclude False "status.h") (LocatedElement (CPPDirectiveAnn False) Internal) | includeStatusH])
            ++ ([CPPDirective (CPPInclude False "result.h") (LocatedElement (CPPDirectiveAnn False) Internal) | includeResultH])
            ++ items
            ++ [
                CPPDirective CPPEndif (LocatedElement (CPPDirectiveAnn True) Internal)
            ]
    plt <- gets targetPlatform
    checkIdentifierLengths (maxIdentifierLength plt) file
    return file

    where

        genIncludeList :: [QualifiedName] -> [CFileItem]
        genIncludeList [] = []
        genIncludeList (x:xs) = genInclude x True : map (`genInclude` False) xs

genSourceFile ::
    -- | Module name
    QualifiedName
    -- | Typed Termina program
    -> AnnotatedProgram SemanticAnn
    -> CGenerator CFile
genSourceFile mName program = do
    items <- concat <$> traverse genSourceASTElement program
    let file = CSourceFile mName $
            CPPDirective (CPPInclude False (mName <.> "h")) (LocatedElement (CPPDirectiveAnn True) Internal)
            : items
    plt <- gets targetPlatform
    checkIdentifierLengths (maxIdentifierLength plt) file
    return file

runGenSourceFile :: 
    TerminaConfig 
    -> Platform
    -> QualifiedName 
    -> AnnotatedProgram SemanticAnn 
    -> Either CGeneratorError CFile
runGenSourceFile config plt mName program = 
    case runState (runExceptT (genSourceFile mName program)) (CGeneratorEnv mName S.empty emptyMonadicTypes config plt) of
    (Left err, _) -> Left err
    (Right file, _) -> Right file

runGenHeaderFile :: 
    TerminaConfig 
    -> Platform
    -> QualifiedName 
    -> [QualifiedName] 
    -> AnnotatedProgram SemanticAnn 
    -> MonadicTypes 
    -> Either CGeneratorError (CFile, MonadicTypes)
runGenHeaderFile config plt mName imports program monadicTys = 
    let includeOptionH = not (S.null (S.filter (\case {
            TStruct _ -> False;
            TEnum _ -> False;
            _ -> True;
            }) (optionTypes monadicTys)))
        includeStatusH = not (S.null (S.filter (\case {
            TStruct _ -> False;
            TEnum _ -> False;
            _ -> True;
            }) (statusTypes monadicTys)))
        includeResultH = not (S.null (S.unions . M.elems . M.filterWithKey (\k _ -> case k of {
            TStruct _ -> False;
            TEnum _ -> False;
            _ -> True;
            }) $ resultTypes monadicTys))
    in
    case runState (runExceptT (genHeaderFile includeOptionH includeStatusH includeResultH mName imports program)) 
        (CGeneratorEnv mName S.empty monadicTys config plt) of
    (Left err, _) -> Left err
    (Right file, env) -> Right (file, monadicTypes env)
