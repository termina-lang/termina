{-# LANGUAGE FlexibleContexts #-}

module Generator.CodeGen.Module where

import Elaboration.AST
import Generator.LanguageC.AST
import Semantic.Types
import Generator.CodeGen.Common
import Configuration.Platform (Platform, maxIdentifierLength)
import Generator.CodeGen.TypeDefinition
import Generator.CodeGen.Global
import Generator.CodeGen.Function
import Generator.CodeGen.Coverage (coverageItems)
import Generator.CodeGen.Application.Option (genOptionPathName)
import Generator.CodeGen.Application.Status (genStatusPathName)
import Generator.CodeGen.Application.Result (genResultPathName)
import Data.Text (unpack, pack, intercalate, replace, toUpper)
import System.FilePath
import qualified Data.Map.Strict as M
import Utils.Annotations
import Control.Monad.Except
import Configuration.Configuration
import Generator.Monadic
import Control.Monad.State
import qualified Data.Set as S
import Doc.Facts (DocFact)
import Generator.CodeGen.Doc (genFileDoc, genSourceFileDoc)


genModuleDefineLabel :: QualifiedName -> String
genModuleDefineLabel mn =
    let filePath = map (pack . dropTrailingPathSeparator) (splitPath (mn <.> "h"))
    in
    unpack $ intercalate (pack "__") (map (toUpper . replace (pack ".") (pack "_")) filePath) <> pack "__"

genInclude :: QualifiedName -> Bool -> CFileItem
genInclude mName before = CPPDirective (CPPInclude False (mName <.> "h")) (LocatedElement (CPPDirectiveAnn before Nothing) Internal)

genHeaderASTElement :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genHeaderASTElement typedef@(TypeDefinition {}) = genTypeDefinitionDecl typedef
genHeaderASTElement glb@(GlobalDeclaration {}) = genGlobalDecl glb
genHeaderASTElement func@(Function {}) = do
    genFunctionDecl func
genHeaderASTElement func@(ExternFunction {}) = genFunctionDecl func

genSourceASTElement :: AnnASTElement SemanticAnn -> CGenerator [CFileItem]
genSourceASTElement typedef@(TypeDefinition (Class {}) _) = genClassDefinition typedef
genSourceASTElement (TypeDefinition {}) = return []
genSourceASTElement glb@(GlobalDeclaration {}) = genGlobal glb
genSourceASTElement func@(Function {}) = genFunction func
genSourceASTElement (ExternFunction {}) = return []

genHeaderFile ::
    -- | Include option.h
    Bool
    -- | Include status.h
    -> Bool
    -- | Include result.h
    -> Bool
    -- | Module name
    -> QualifiedName
    -- | Import list
    -> [QualifiedName]
    -- | Comment of the module
    -> Maybe DocComment
    -> AnnotatedProgram SemanticAnn
    -> CGenerator CFile
genHeaderFile includeOptionH includeStatusH includeResultH mName imports comment program = do
    let defineLabel = genModuleDefineLabel mName
    fileDoc <- genFileDoc mName comment
    items <- concat <$> traverse genHeaderASTElement program
    extra <- gets extraImports
    let includeList = genIncludeList (S.toList (S.union (S.fromList imports) extra))
    let file = CHeaderFile mName $
            [
                CPPDirective (CPPIfNDef defineLabel) (LocatedElement (CPPDirectiveAnn False (Just fileDoc)) Internal),
                CPPDirective (CPPDefine defineLabel Nothing) (LocatedElement (CPPDirectiveAnn False Nothing) Internal),
                CPPDirective (CPPInclude True "termina.h") (LocatedElement (CPPDirectiveAnn True Nothing) Internal)
            ] ++ includeList
            ++ ([CPPDirective (CPPInclude False genOptionPathName) (LocatedElement (CPPDirectiveAnn True Nothing) Internal) | includeOptionH])
            ++ ([CPPDirective (CPPInclude False genStatusPathName) (LocatedElement (CPPDirectiveAnn False Nothing) Internal) | includeStatusH])
            ++ ([CPPDirective (CPPInclude False genResultPathName) (LocatedElement (CPPDirectiveAnn False Nothing) Internal) | includeResultH])
            ++ items
            ++ [
                CPPDirective CPPEndif (LocatedElement (CPPDirectiveAnn True Nothing) Internal)
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
    let fileDoc = genSourceFileDoc mName
    items <- concat <$> traverse genSourceASTElement program
    -- | The arrays of the coverage, whose sizes are known once the items are generated
    covItems <- coverageItems
    let file = CSourceFile mName $
            CPPDirective (CPPInclude False (mName <.> "h")) (LocatedElement (CPPDirectiveAnn True (Just fileDoc)) Internal)
            : covItems ++ items
    plt <- gets targetPlatform
    checkIdentifierLengths (maxIdentifierLength plt) file
    return file

-- | The C of a module and the points of its coverage, if any.
runGenSourceFile ::
    TerminaConfig
    -> Platform
    -> QualifiedName
    -> AnnotatedProgram SemanticAnn
    -> Either CGeneratorError (CFile, [CoveragePoint])
runGenSourceFile config plt mName program =
    let cov = if coverage config then Just emptyCoverageState else Nothing in
    case runState (runExceptT (genSourceFile mName program)) (CGeneratorEnv mName S.empty emptyMonadicTypes config plt False cov M.empty) of
    (Left err, _) -> Left err
    (Right file, env) -> Right (file, maybe [] (reverse . coveragePoints) (coverageState env))

runGenHeaderFile :: 
    TerminaConfig 
    -> Platform
    -> QualifiedName 
    -> [QualifiedName]
    -> Maybe DocComment
    -> AnnotatedProgram SemanticAnn
    -> MonadicTypes
    -- | The facts of the instances of the deployment
    -> M.Map Identifier [DocFact SemanticAnn]
    -> Either CGeneratorError (CFile, MonadicTypes)
runGenHeaderFile config plt mName imports comment program monadicTys facts =
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
    case runState (runExceptT (genHeaderFile includeOptionH includeStatusH includeResultH mName imports comment program))
        (CGeneratorEnv mName S.empty monadicTys config plt False Nothing facts) of
    (Left err, _) -> Left err
    (Right file, env) -> Right (file, monadicTypes env)
