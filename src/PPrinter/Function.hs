module PPrinter.Function where

import Prettyprinter

import AST.Seman
import PPrinter.Common
import Semantic.Monad (SemanticAnns)
import PPrinter.Statement

ppFunction :: AnnASTElement SemanticAnns -> DocStyle
ppFunction (Function identifier parameters rTS blk _ _) =
  ppCFunctionPrototype (pretty identifier)
    (ppParameterDeclaration (pretty identifier) <$> parameters)
    (ppReturnType (pretty identifier) <$> rTS)
    <+> ppBlockRet (ppParameterSubstitutions parameters) (pretty identifier) blk
ppFunction _ = error "AST element is not a function"

ppFunctionDeclaration :: AnnASTElement SemanticAnns -> DocStyle
ppFunctionDeclaration (Function identifier parameters rTS _ _ _) =
  vsep $ 
  ([ppParameterVectorValueStructureDecl (pretty identifier) (pretty pid) ts <> line | (Parameter pid ts@(Vector {})) <- parameters]) ++
  (case rTS of
    Just ts@(Vector {}) -> [ppReturnVectorValueStructureDecl (pretty identifier) ts <> line]
    _ -> []) ++
  [
    ppCFunctionPrototype (pretty identifier)
      (ppParameterDeclaration (pretty identifier) <$> parameters)
      (ppReturnType (pretty identifier) <$> rTS) <> semi,
    emptyDoc
  ]
ppFunctionDeclaration _ = error "AST element is not a function"
