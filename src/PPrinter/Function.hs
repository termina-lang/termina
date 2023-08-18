module PPrinter.Function where

import Prettyprinter

import SemanAST
import PPrinter.Common
import Semantic.Monad (SemanticAnns)
import PPrinter.Statement
import PPrinter.Expression (Substitutions)
import Data.Map

ppBlockRet :: Substitutions -> DocStyle -> BlockRet SemanticAnns -> DocStyle
ppBlockRet subs identifier (BlockRet body ret) =
  braces' (
    indentTab . align $
      line <> vsep [ppStatement subs s <> line | s <- body]
      <> line <> ppReturnStmt identifier ret <> line
  )

ppParameterSubstitutions :: [Parameter] -> Substitutions
ppParameterSubstitutions parameters =
  fromList [(pid, pretty pid <> pretty ".array") | (Parameter pid (Vector {})) <- parameters]

ppFunction :: AnnASTElement SemanticAnns -> DocStyle
ppFunction (Function identifier parameters rTS blk _ _) =
  ppCFunctionDeclaration (pretty identifier)
    (ppParameterDeclaration (pretty identifier) <$> parameters)
    (ppReturnType (pretty identifier) <$> rTS)
    <+> ppBlockRet (ppParameterSubstitutions parameters) (pretty identifier) blk
ppFunction _ = error "AST element is not a function"

ppFunctionDeclaration :: AnnASTElement SemanticAnns -> DocStyle
ppFunctionDeclaration (Function identifier parameters rTS _ _ _) =
  vsep $ 
  ([ppParameterVectorValueStructureDecl (pretty identifier) (pretty pid) ts <> line | (Parameter pid ts@(Vector {})) <- parameters]) ++ 
  [
    ppCFunctionDeclaration (pretty identifier)
      (ppParameterDeclaration (pretty identifier) <$> parameters)
      (ppReturnType (pretty identifier) <$> rTS) <> semi
  ]
ppFunctionDeclaration _ = error "AST element is not a function"