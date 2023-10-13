module PPrinter.Global where

import Prettyprinter

import AST.Seman ( Global'(Resource), Global )
import PPrinter.Common
import Semantic.Monad (SemanticAnns)

ppGlobalDeclaration :: Global SemanticAnns -> DocStyle
ppGlobalDeclaration (Resource identifier ts _ _ _) = externC <+> ppTypeSpecifier ts <+> pretty identifier <> ppDimension ts <> semi <> line
ppGlobalDeclaration decl = error $ "unsupported global declaration: " ++ show decl
