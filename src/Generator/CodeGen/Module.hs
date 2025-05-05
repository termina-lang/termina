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
    items <- concat <$> mapM genHeaderASTElement program
    extra <- gets extraImports
    let includeList = genIncludeList (S.toList (S.union (S.fromList imports) extra))
    return $ CHeaderFile mName $
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
    items <- concat <$> mapM genSourceASTElement program
    return $ CSourceFile mName $
        CPPDirective (CPPInclude False (mName <.> "h")) (LocatedElement (CPPDirectiveAnn True) Internal)
        : items

runGenSourceFile :: 
    TerminaConfig 
    -> M.Map Identifier Integer
    -> QualifiedName 
    -> AnnotatedProgram SemanticAnn 
    -> Either CGeneratorError CFile
runGenSourceFile config irqMap mName program = 
    case runState (runExceptT (genSourceFile mName program)) (CGeneratorEnv mName S.empty emptyMonadicTypes config irqMap) of
    (Left err, _) -> Left err
    (Right file, _) -> Right file

runGenHeaderFile :: 
    TerminaConfig 
    -> M.Map Identifier Integer
    -> QualifiedName 
    -> [QualifiedName] 
    -> AnnotatedProgram SemanticAnn 
    -> MonadicTypes 
    -> Either CGeneratorError (CFile, MonadicTypes)
runGenHeaderFile config irqMap mName imports program monadicTys = 
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
        (CGeneratorEnv mName S.empty monadicTys config irqMap) of
    (Left err, _) -> Left err
    (Right file, env) -> Right (file, monadicTypes env)
