module PPrinter.Task where

import Prettyprinter

import AST.Seman
import PPrinter.Common
import Semantic.Monad (SemanticAnns)
import PPrinter.Function

ppTask :: AnnASTElement SemanticAnns -> DocStyle
ppTask (Task identifier parameters rTS blk _ _) =
  ppCFunctionPrototype (pretty identifier)
    (ppParameterDeclaration (pretty identifier) <$> parameters)
    (Just (ppReturnType (pretty identifier) rTS))
    <+> ppBlockRet (ppParameterSubstitutions parameters) (pretty identifier) blk
ppTask _ = error "AST element is not a task: " -- This should never happen

ppTaskDeclaration :: AnnASTElement SemanticAnns -> DocStyle
ppTaskDeclaration (Task identifier parameters rTS _ _ _) =
  vsep $ 
  ([ppParameterVectorValueStructureDecl (pretty identifier) (pretty pid) ts <> line | (Parameter pid ts@(Vector {})) <- parameters]) ++ 
  [
    ppCFunctionPrototype (pretty identifier)
      (ppParameterDeclaration (pretty identifier) <$> parameters)
      (Just (ppReturnType (pretty identifier) rTS)) <> semi
  ]
ppTaskDeclaration _ = error "AST element is not a function"
