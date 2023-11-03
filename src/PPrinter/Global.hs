module PPrinter.Global where

import Prettyprinter

import AST.Seman
import PPrinter.Common
import Semantic.Monad (SemanticAnns)

ppGlobalDeclaration :: Global SemanticAnns -> DocStyle
ppGlobalDeclaration (Resource identifier ts _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDeclaration (Task identifier ts _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDeclaration (Handler identifier ts _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDeclaration decl = error $ "unsupported global declaration: " ++ show decl

ppGlobalDefinition :: Global SemanticAnns -> DocStyle
ppGlobalDefinition (Resource identifier ts _ _ _) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDefinition (Task identifier ts _ _ _) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDefinition (Handler identifier ts _ _ _) = ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDefinition decl = error $ "unsupported global declaration: " ++ show decl
